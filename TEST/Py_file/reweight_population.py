"""
reweight_population.py
Calibrate perwt via entropy tilting (no external packages beyond scipy).
Finds the minimum-KL-divergence weights w_i = d_i * exp(lambda @ x_i)
that satisfy all count targets simultaneously.

Targets:
  1. Population by fine age band x sex   (SSPopJan_TR2023)
  2. Married / divorced fractions        (SSPopJan_TR2023)
  3. SS beneficiary                      (SSA supplement 5.A4)
  4. Workers above / below OASDI taxmax  (SSA supplement 4.B1)
"""

import numpy as np
import pandas as pd
from scipy.optimize import minimize

BASE_DIR         = r"C:\Users\kchanwong\Documents\GitHub\social_security_cato_model\TEST"
SUPPLEMENT25     = rf"{BASE_DIR}\Data_Input\supplement25 (2).xlsx"
BASE_YEAR = 2008

# ─────────────────────────────────────────────────────────────────────────────
# 1. LOAD
# ─────────────────────────────────────────────────────────────────────────────
pop = pd.read_csv(f"{BASE_DIR}/Data_Output/initial_simulation.csv")
N   = len(pop)

for _col in ["perwt_orig", "_rake_"]:
    if _col in pop.columns:
        pop = pop.drop(columns=[_col])

ssa = pd.read_csv(f"{BASE_DIR}/Data_Input/SSPopJan_TR2023 (1).csv")
ssa.columns = ssa.columns.str.replace(" ", ".", regex=False)
s08 = ssa[ssa["Year"] == BASE_YEAR]

US_POP            = s08["Total"].sum()
TAX_MAX_2008      = 102_000
WORKERS_ABOVE_MAX = 9_764_000
WORKERS_TOTAL     = 162_532_000
SS_BENES          = 50_898_244
AWI_2008          = 41_334.97

# ─────────────────────────────────────────────────────────────────────────────
# 2. BUILD CALIBRATION VARIABLES
# ─────────────────────────────────────────────────────────────────────────────
AGE_BINS   = [-1,  0, 14, 17, 24, 34, 44, 54, 64, 74, 999]
AGE_LABELS = ["0","1-14","15-17","18-24","25-34","35-44","45-54","55-64","65-74","75+"]

pop["age_band"] = pd.cut(pop["age"], bins=AGE_BINS, labels=AGE_LABELS).astype(str)
pop["age_sex"]  = np.where(pop["sex"] == 1, "M_", "F_") + pop["age_band"]

pop["marst_simp"] = np.where(pop["marst"] == "married",  "married",
                    np.where(pop["marst"] == "divorced", "divorced", "other"))

pop["ss_bene"]    = pop["receive_ss"]

pop["tax_max_cat"] = np.where(
    (pop["employed"] == "yes") & (pop["incwage"] > TAX_MAX_2008), "above_max",
    np.where(pop["employed"] == "yes", "worker_below_max", "not_worker"),
)

# ─────────────────────────────────────────────────────────────────────────────
# 3. BUILD CONSTRAINT MATRIX AND TARGET VECTOR
# Each constraint j: sum_i w_i * x_ij = T_j  (absolute population counts)
# ─────────────────────────────────────────────────────────────────────────────
AGE_LO = [0,  1,  15, 18, 25, 35, 45, 55, 65, 75]
AGE_HI = [0, 14,  17, 24, 34, 44, 54, 64, 74, 999]

rows, tgts, names = [], [], []

def _add(indicator, target, name):
    rows.append(indicator.astype(float))
    tgts.append(float(target))
    names.append(name)

# Age × sex
for lo, hi, label in zip(AGE_LO, AGE_HI, AGE_LABELS):
    _add(pop["age_sex"] == f"M_{label}",
         s08[s08["Age"].between(lo, hi)]["M.Tot"].sum(), f"M_{label}")
    _add(pop["age_sex"] == f"F_{label}",
         s08[s08["Age"].between(lo, hi)]["F.Tot"].sum(), f"F_{label}")

# Marital status
mar_tot = s08["M.Mar"].sum() + s08["F.Mar"].sum()
div_tot = s08["M.Div"].sum() + s08["F.Div"].sum()
_add(pop["marst_simp"] == "married",  mar_tot,               "married")
_add(pop["marst_simp"] == "divorced", div_tot,               "divorced")
_add(pop["marst_simp"] == "other",    US_POP - mar_tot - div_tot, "marst_other")

# Tax max / worker status
_add(pop["tax_max_cat"] == "above_max",        WORKERS_ABOVE_MAX,                   "above_max")
_add(pop["tax_max_cat"] == "worker_below_max", WORKERS_TOTAL - WORKERS_ABOVE_MAX,   "below_max")
_add(pop["tax_max_cat"] == "not_worker",       US_POP - WORKERS_TOTAL,              "not_worker")

# Total wages (linear constraint that pins AWI when combined with worker count)
# x_i = incwage if employed with positive wage, else 0
wg_arr    = pop["incwage"].fillna(0).values.astype(float)
has_wage  = ((pop["employed"] == "yes") & (wg_arr > 0)).values
TOTAL_WAGES = AWI_2008 * WORKERS_TOTAL
_add(wg_arr * has_wage, TOTAL_WAGES, "total_wages")

X = np.column_stack(rows)   # shape (N, K), binary indicators
T = np.array(tgts)          # shape (K,), absolute population counts
K = len(T)
d = US_POP / N              # uniform base weight per person

# Reparameterize: mu_j = lam_j * T_j  =>  x_norm_ij = x_ij / T_j
# At mu=0 the gradient ~= d * count_j / T_j - 1, which is O(1).
# This prevents L-BFGS-B from taking enormous first steps.
X_norm = X / T[None, :]     # shape (N, K)

print(f"Calibrating {N:,} records to {K} constraints.  US pop: {US_POP/1e6:.1f}M")
for i, (nm, ratio) in enumerate(zip(names, d * X.sum(axis=0) / T)):
    print(f"  {nm:<20s}  sample/target = {ratio:.3f}")

# ─────────────────────────────────────────────────────────────────────────────
# 4. ENTROPY TILTING  (reparameterized dual)
# L(mu) = d * sum_i exp(X_norm[i] @ mu) - sum(mu)
# At optimum: d * X_norm.T @ w = 1  =>  sum_i w_i * x_ij = T_j
# ─────────────────────────────────────────────────────────────────────────────
def dual_obj_grad(mu):
    eta  = np.clip(X_norm @ mu, -50, 50)
    w    = d * np.exp(eta)
    obj  = w.sum() - mu.sum()
    grad = X_norm.T @ w - 1.0
    return float(obj), grad

result = minimize(
    dual_obj_grad,
    x0=np.zeros(K),
    method="L-BFGS-B",
    jac=True,
    options={"maxiter": 20000, "ftol": 1e-20, "gtol": 1e-10},
)

if not result.success:
    print(f"  Warning: {result.message}")

new_w = d * np.exp(np.clip(X_norm @ result.x, -50, 50))
new_w *= US_POP / new_w.sum()   # exact normalization

print(f"  Converged: {result.success}  |  f-calls: {result.nfev}")
print(f"  Weight range: [{new_w.min():.1f}, {new_w.max():.1f}]")

# ─────────────────────────────────────────────────────────────────────────────
# 4b. GINI TARGET FROM SSA TABLE 4.B7
# Groups: 1-9999, 10k-19999, 20k-39999, 40k-59999, 60k-79999, 80k-99999,
#         100k-taxmax (below), at-or-above taxmax.
# Above-max midpoint is solved so the group-weighted average equals AWI.
# This gives GINI of actual covered wages, anchored to SSA data.
# ─────────────────────────────────────────────────────────────────────────────
def compute_4b7_gini(path: str, year: int, awi: float) -> float:
    import openpyxl
    wb  = openpyxl.load_workbook(path, read_only=True, data_only=True)
    ws  = wb["4.B7"]

    counts = None
    in_all = False
    for row in ws.iter_rows(values_only=True):
        if row[2] == "All wage and salary workers":
            in_all = True
            continue
        if in_all and row[2] == "Men":
            break
        if in_all and row[0] == year:
            # cols 3..9 = below-max bands; col 13 = at-max
            def _num(v):
                return float(v) if isinstance(v, (int, float)) else 0.0
            counts = np.array([_num(row[3]), _num(row[4]), _num(row[5]),
                               _num(row[6]), _num(row[7]), _num(row[8]),
                               _num(row[9]), _num(row[13])], dtype=float)
            break
    if counts is None:
        raise ValueError(f"Table 4.B7 row for year {year} not found")

    # Fixed midpoints for below-max bands (last band 100k-taxmax ≈ 101k mid)
    mids_base = np.array([5000, 15000, 30000, 50000, 70000, 90000, 101000], dtype=float)
    # Solve for above-max midpoint that makes weighted avg == AWI
    below_wages = (counts[:7] * mids_base).sum()
    below_n     = counts[:7].sum()
    above_n     = counts[7]
    # (below_wages + above_n * m_above) / (below_n + above_n) = awi
    m_above = (awi * (below_n + above_n) - below_wages) / above_n
    mids = np.concatenate([mids_base, [m_above]])

    # Grouped GINI (trapezoidal Lorenz)
    idx    = np.argsort(mids)
    n_s, y_s = counts[idx], mids[idx]
    N      = n_s.sum()
    Y_tot  = (n_s * y_s).sum()
    p_c    = np.cumsum(n_s) / N
    s_c    = np.cumsum(n_s * y_s) / Y_tot
    p_lag  = np.concatenate([[0.0], p_c[:-1]])
    s_lag  = np.concatenate([[0.0], s_c[:-1]])
    return float(1.0 - np.sum((p_c - p_lag) * (s_c + s_lag)))

GINI_TARGET = compute_4b7_gini(SUPPLEMENT25, BASE_YEAR, AWI_2008)
print(f"  GINI target (4.B7, AWI-anchored): {GINI_TARGET:.4f}")

# ─────────────────────────────────────────────────────────────────────────────
# 5. INCOME ADJUSTMENT
# a) Proportional scale to exactly match AWI (preserves distribution shape / GINI)
# b) Power transform around the mean to match GINI (monotonic → preserves rank)
#    Then rescale to restore AWI.
# ─────────────────────────────────────────────────────────────────────────────
w   = new_w
wg  = pop["incwage"].fillna(0).values.astype(float)
emp = (pop["employed"] == "yes").values
has_wage = emp & (wg > 0)

# a) Proportional scalar to match AWI (preserves rank order)
sim_AWI   = (w * wg * has_wage).sum() / (w * has_wage).sum()
awi_scale = AWI_2008 / sim_AWI
wg_adj    = wg.copy()
wg_adj[has_wage] *= awi_scale
print(f"\n  AWI scalar: {awi_scale:.4f}  ({sim_AWI:.0f} -> {AWI_2008:.0f})")

# b) Weighted GINI helper
def weighted_gini(y, wts):
    order = np.argsort(y)
    y, wts = y[order], wts[order]
    W  = wts.sum()
    mu = (wts * y).sum() / W
    cum = np.cumsum(wts) - wts / 2
    return 2.0 * (cum * wts * y).sum() / (W**2 * mu) - 1.0

y_wk = wg_adj[has_wage]
w_wk = w[has_wage]
mu_wk = (w_wk * y_wk).sum() / w_wk.sum()
gini_before = weighted_gini(y_wk, w_wk)
print(f"  GINI before power transform: {gini_before:.4f}")

# c) Two-parameter joint optimisation: (alpha, log_tau).
#    alpha : overall power exponent — GINI is monotone and scale-invariant here.
#    tau   : multiplicative scale applied to workers already above TAX_MAX in
#            transformed space (tau < 1 compresses top tail, tau > 1 expands it).
#    Final AWI rescale preserves AWI by construction.
#    We solve the 2×2 system: GINI == GINI_TARGET, above_max == WORKERS_ABOVE_MAX.
from scipy.optimize import fsolve

y_wk0 = y_wk.copy()   # snapshot after AWI scalar, before transform

def _transform2(alpha, log_tau):
    tau  = np.exp(np.clip(log_tau, -10, 10))
    safe = np.clip(y_wk0 / mu_wk, 1e-15, None)
    y_t  = mu_wk * safe ** np.clip(alpha, 0.05, 15.0)
    y_t2 = y_t.copy()
    y_t2[y_t > TAX_MAX_2008] *= tau
    mu_t = (w_wk * y_t2).sum() / w_wk.sum()
    if mu_t <= 0 or not np.isfinite(mu_t):
        return np.full_like(y_t, AWI_2008)
    return y_t2 * (AWI_2008 / mu_t)

def _resid2d(params):
    alpha, log_tau = params
    y_sc   = _transform2(alpha, log_tau)
    wg_t   = wg_adj.copy()
    wg_t[has_wage] = y_sc
    g_sim  = weighted_gini(y_sc, w_wk)
    ab_sim = (w * emp * (wg_t > TAX_MAX_2008)).sum()
    return [(g_sim  - GINI_TARGET),
            (ab_sim - WORKERS_ABOVE_MAX) / WORKERS_ABOVE_MAX]

# Warm start: binary-search for alpha_init such that GINI(alpha_init, tau=1) = GINI_TARGET,
# then let fsolve adjust tau to fix above-max.
lo_a, hi_a = 0.05, 15.0
for _ in range(60):
    mid_a = (lo_a + hi_a) / 2.0
    g_mid = weighted_gini(_transform2(mid_a, 0.0), w_wk)
    if g_mid < GINI_TARGET:
        lo_a = mid_a
    else:
        hi_a = mid_a
alpha_init = (lo_a + hi_a) / 2.0

x0  = np.array([alpha_init, 0.0])
sol, info, ier, msg = fsolve(_resid2d, x0, full_output=True)
if ier != 1:
    print(f"  Warning (fsolve 2D): {msg}")
alpha_opt, log_tau_opt = sol
tau_opt = np.exp(log_tau_opt)

y_final = _transform2(alpha_opt, log_tau_opt)
wg_adj[has_wage] = y_final

gini_after = weighted_gini(wg_adj[has_wage], w_wk)
awi_after  = (w * wg_adj * has_wage).sum() / (w * has_wage).sum()
above_after = (w * emp * (wg_adj > TAX_MAX_2008)).sum()
print(f"  alpha={alpha_opt:.4f}  tau={tau_opt:.4f}")
print(f"  GINI after  transform:       {gini_after:.4f}  (target {GINI_TARGET:.4f})")
print(f"  AWI  after  rescale:         {awi_after:.2f}")
print(f"  Above tax max after:         {above_after/1e6:.3f}M  (target {WORKERS_ABOVE_MAX/1e6:.3f}M)")

# ─────────────────────────────────────────────────────────────────────────────
# 6. VALIDATION
# ─────────────────────────────────────────────────────────────────────────────
def _check(label, sim_val, ssa_val, unit=""):
    err  = (sim_val - ssa_val) / (abs(ssa_val) + 1e-9) * 100
    flag = " <<<" if abs(err) > 10 else ""
    print(f"  {label:<30s} sim={sim_val:>12.3f}{unit}  ssa={ssa_val:>12.3f}{unit}  err={err:+.1f}%{flag}")

age = pop["age"].values
mst = pop["marst"].values
ss  = (pop["receive_ss"] == "yes").values

ssa_cbr = s08[s08["Age"] == 0]["Total"].values[0] / US_POP * 1000
ssa_dep = ((s08[s08["Age"] <  18]["Total"].sum() + s08[s08["Age"] >= 65]["Total"].sum()) /
            s08[s08["Age"].between(18, 64)]["Total"].sum())

print("\n-- Validation (calibrated weights + adjusted income) --")
_check("Total pop (M)",      w.sum()/1e6,    US_POP/1e6)
_check("CBR (/1000)",        (w*(age==0)).sum()/w.sum()*1000, ssa_cbr)
_check("Pct married",        (w*(mst=="married")).sum()/w.sum(),
                              (s08["M.Mar"].sum()+s08["F.Mar"].sum())/US_POP)
_check("Pct divorced",       (w*(mst=="divorced")).sum()/w.sum(),
                              (s08["M.Div"].sum()+s08["F.Div"].sum())/US_POP)
_check("Dep ratio",          ((w*(age<18)).sum()+(w*(age>=65)).sum())
                             /(w*((age>=18)&(age<=64))).sum(), ssa_dep)
_check("AWI ($)",            awi_after, AWI_2008)
_check("GINI",               gini_after, GINI_TARGET)
_check("Above tax max (M)",  (w*emp*(wg_adj>TAX_MAX_2008)).sum()/1e6, WORKERS_ABOVE_MAX/1e6)
_check("SS benes (M)",       (w*ss).sum()/1e6, SS_BENES/1e6)

# ─────────────────────────────────────────────────────────────────────────────
# 7. SAVE
# ─────────────────────────────────────────────────────────────────────────────
drop_cols = ["age_band", "age_sex", "marst_simp", "ss_bene", "tax_max_cat"]
pop = pop.drop(columns=[c for c in drop_cols if c in pop.columns])
pop["perwt"]   = new_w
pop["incwage"] = wg_adj
pop.to_csv(f"{BASE_DIR}/Data_Output/initial_simulation.csv", index=False)
print(f"\nSaved -> Data_Output/initial_simulation.csv  (perwt + incwage updated)")
