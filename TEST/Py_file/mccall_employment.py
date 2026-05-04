"""
mccall_employment.py
====================
McCall job-search model with separation, calibrated to CPS unemployment rates.

Three public entry points:
  calibrate_mccall(unemp_csv, sep_csv, wage_csv)  → cell_params dict
  apply_employment(pop, proj_year, cell_params, rng) → updated pop
  wage_growth_factors(wage_csv)                    → {year: {cell: factor}}
"""

import numpy as np
import pandas as pd

BASE_YEAR = 2008

# ─────────────────────────────────────────────────────────────────────────────
# McCall Bellman solver (log utility for numerical stability with $-wages)
# ─────────────────────────────────────────────────────────────────────────────

def _solve_mccall(alpha_m: float, gamma_m: float,
                  wages_annual: np.ndarray, probs: np.ndarray,
                  beta_annual: float = 0.98, c_annual: float | None = None,
                  tol: float = 1e-8, max_iter: int = 2000) -> tuple:
    """
    Bellman iteration for McCall with separation at MONTHLY frequency.

    alpha_m  : monthly separation probability  (= alpha_annual / 12)
    gamma_m  : monthly offer arrival rate      (calibrated via bisection)
    wages_annual : annual wage grid in $
    beta_annual  : annual discount factor → converted to monthly internally

    Returns
    -------
    w_bar_annual : float — reservation wage in annual $ units
    p_accept     : float — P(offer >= w_bar) under the offer distribution
    """
    # Convert to monthly quantities
    beta_m    = beta_annual ** (1.0 / 12.0)
    wages_m   = wages_annual / 12.0
    w_med_m   = np.median(wages_m)
    wn        = wages_m / w_med_m
    c_m       = (c_annual / 12.0 / w_med_m) if c_annual is not None else 0.4

    u_w = np.log(np.maximum(wn, 1e-12))
    u_c = np.log(max(c_m, 1e-12))

    V = u_w / (1.0 - beta_m)
    U = u_c / (1.0 - beta_m)

    for _ in range(max_iter):
        V_new = u_w + beta_m * ((1.0 - alpha_m) * V + alpha_m * U)
        EV    = float(np.dot(np.maximum(U, V), probs))
        U_new = u_c + beta_m * (1.0 - gamma_m) * U + beta_m * gamma_m * EV

        if np.max(np.abs(V_new - V)) < tol and abs(U_new - U) < tol:
            V, U = V_new, U_new
            break
        V, U = V_new, U_new

    accept_mask = V >= U
    if not accept_mask.any():
        return np.inf, 0.0

    # Convert reservation wage back to annual
    w_bar_annual = wages_annual[np.argmax(accept_mask)]
    p_accept     = float(probs[accept_mask].sum())
    return w_bar_annual, p_accept


def _steady_state_u(alpha_m: float, gamma_m: float,
                    wages_annual: np.ndarray, probs: np.ndarray,
                    beta_annual: float, c_annual: float | None) -> float:
    """Monthly steady-state: u* = alpha_m / (alpha_m + lambda_m)."""
    _, p_accept = _solve_mccall(alpha_m, gamma_m, wages_annual, probs, beta_annual, c_annual)
    lam_m = gamma_m * p_accept
    if lam_m + alpha_m < 1e-12:
        return 1.0
    return alpha_m / (alpha_m + lam_m)


def _calibrate_gamma(alpha_annual: float, target_u: float,
                     wages_annual: np.ndarray, probs: np.ndarray,
                     beta_annual: float = 0.98,
                     c_annual: float | None = None) -> float:
    """
    Bisect on monthly gamma to hit target_u in monthly steady state.
    All internal math is monthly; returns gamma_m in (0, 1).
    """
    alpha_m = alpha_annual / 12.0
    lo, hi  = 1e-4, 0.999

    u_lo = _steady_state_u(alpha_m, lo, wages_annual, probs, beta_annual, c_annual)
    u_hi = _steady_state_u(alpha_m, hi, wages_annual, probs, beta_annual, c_annual)

    if target_u >= u_lo:
        return lo
    if target_u <= u_hi:
        return hi

    for _ in range(60):
        mid   = (lo + hi) / 2.0
        u_mid = _steady_state_u(alpha_m, mid, wages_annual, probs, beta_annual, c_annual)
        if abs(u_mid - target_u) < 1e-6:
            return mid
        if u_mid > target_u:
            lo = mid
        else:
            hi = mid

    return (lo + hi) / 2.0


# ─────────────────────────────────────────────────────────────────────────────
# Worker function (module-level so ProcessPoolExecutor can pickle it)
# ─────────────────────────────────────────────────────────────────────────────

def _calibrate_cell(args):
    """Calibrate a single (sex, educ, age_band, year) cell."""
    key, alpha_annual, target_u, wages, probs, beta = args
    alpha_m     = alpha_annual / 12.0
    gamma_m     = _calibrate_gamma(alpha_annual, target_u, wages, probs, beta)
    w_bar, p_ac = _solve_mccall(alpha_m, gamma_m, wages, probs, beta)

    # Annual simulation probabilities via Poisson compounding of monthly rates
    p_sep_annual  = 1.0 - (1.0 - alpha_m) ** 12
    lam_m         = gamma_m * p_ac
    p_find_annual = 1.0 - (1.0 - lam_m) ** 12

    # Steady-state employment rate (for new NILF→LF entrants)
    ss_u     = alpha_m / (alpha_m + lam_m) if (alpha_m + lam_m) > 1e-12 else target_u
    emp_rate = 1.0 - ss_u

    return key, {
        "alpha_m":       alpha_m,
        "gamma_m":       gamma_m,
        "w_bar":         w_bar,
        "p_accept":      p_ac,
        "p_sep_annual":  p_sep_annual,
        "p_find_annual": p_find_annual,
        "emp_rate":      emp_rate,
        "wages":         wages,
        "probs":         probs,
    }


# ─────────────────────────────────────────────────────────────────────────────
# Public: calibrate all cells
# ─────────────────────────────────────────────────────────────────────────────

def calibrate_mccall(unemp_csv: str, sep_csv: str, wage_csv: str,
                     beta: float = 0.98,
                     n_workers: int = 4) -> dict:
    """
    Calibrate McCall parameters for every (sex, educ, age_band, year) cell.

    Parameters
    ----------
    unemp_csv : path to unemp_rates.csv
    sep_csv   : path to separation_rates.csv
    wage_csv  : path to wage_distributions.csv
    beta      : discount factor
    n_workers : parallel workers for ProcessPoolExecutor

    Returns
    -------
    cell_params : dict  keyed by (sex, educ, age_band, year)
                  values: {alpha, gamma, w_bar, p_accept, wages, probs}
    """
    unemp = pd.read_csv(unemp_csv)
    sep   = pd.read_csv(sep_csv)
    wage  = pd.read_csv(wage_csv)

    # Year-specific alpha per (sex, educ, age_band, year); fall back to all-year average
    alpha_yr = (
        sep.set_index(["sex", "educ", "age_band", "year"])["sep_rate"]
        .to_dict()
    )
    alpha_avg = (
        sep.groupby(["sex", "educ", "age_band"])["sep_rate"]
        .mean()
        .to_dict()
    )

    # Pivot wage distributions to array form per cell-year
    wage_pivot = (
        wage.sort_values("quantile")
        .groupby(["year", "sex", "educ", "age_band"])
        .apply(lambda g: (g["wage_q"].values, np.full(len(g), 1.0 / len(g))),
               include_groups=False)
        .to_dict()
    )

    # Build task list
    tasks = []
    for _, row in unemp.iterrows():
        key       = (int(row["sex"]), row["educ"], row["age_band"], int(row["year"]))
        demo_key  = (int(row["sex"]), row["educ"], row["age_band"])
        yr_key    = (int(row["sex"]), row["educ"], row["age_band"], int(row["year"]))
        alpha     = alpha_yr.get(yr_key) or alpha_avg.get(demo_key, 0.07)
        target_u  = float(row["unemp_rate"])
        target_u  = max(0.005, min(0.50, target_u))  # clamp to sane range

        wk = (int(row["year"]), int(row["sex"]), row["educ"], row["age_band"])
        if wk not in wage_pivot:
            continue
        wages, probs = wage_pivot[wk]

        # Drop NaN quantile entries
        ok = np.isfinite(wages)
        if ok.sum() < 5:
            continue
        wages = wages[ok].astype(float)
        probs = np.full(len(wages), 1.0 / len(wages))

        tasks.append((key, alpha, target_u, wages, probs, beta))

    print(f"Calibrating {len(tasks)} McCall cells ...")

    cell_params = {}
    for i, task in enumerate(tasks):
        key, params = _calibrate_cell(task)
        cell_params[key] = params
        if (i + 1) % 100 == 0:
            print(f"  {i+1}/{len(tasks)} cells done")

    print(f"Calibration complete: {len(cell_params)} cells.")
    return cell_params


# ─────────────────────────────────────────────────────────────────────────────
# Public: wage growth index
# ─────────────────────────────────────────────────────────────────────────────

def wage_growth_factors(wage_csv: str) -> dict:
    """
    Returns {(sex, educ, age_band, year): growth_factor} where growth_factor
    = median_wage(year) / median_wage(BASE_YEAR).
    Used to scale the offer distribution in future projection years.
    """
    wage = pd.read_csv(wage_csv)
    medians = (
        wage[wage["quantile"].between(0.499, 0.501)]
        .groupby(["year", "sex", "educ", "age_band"])["wage_q"]
        .mean()
        .reset_index()
    )

    base = (
        medians[medians["year"] == BASE_YEAR]
        .set_index(["sex", "educ", "age_band"])["wage_q"]
        .to_dict()
    )

    factors = {}
    for _, row in medians.iterrows():
        demo = (int(row["sex"]), row["educ"], row["age_band"])
        base_w = base.get(demo, np.nan)
        if base_w and base_w > 0:
            factors[(demo[0], demo[1], demo[2], int(row["year"]))] = row["wage_q"] / base_w
    return factors


# ─────────────────────────────────────────────────────────────────────────────
# Helpers for simulation lookup
# ─────────────────────────────────────────────────────────────────────────────

def _age_band(age: int) -> str:
    if   age <= 24: return "16-24"
    elif age <= 34: return "25-34"
    elif age <= 44: return "35-44"
    elif age <= 54: return "45-54"
    elif age <= 64: return "55-64"
    else:           return "65+"


def _nearest_year(year: int, available: set) -> int:
    return min(available, key=lambda y: abs(y - year))


# ─────────────────────────────────────────────────────────────────────────────
# Public: assign initial employment to new NILF→LF entrants
# ─────────────────────────────────────────────────────────────────────────────

def assign_new_lf_employment(pop: pd.DataFrame,
                              new_lf_mask: np.ndarray,
                              cell_params: dict,
                              proj_year: int,
                              rng: np.random.Generator) -> pd.DataFrame:
    """
    For people who just entered the labor force (NILF→LF this year), draw their
    initial employment status from the cell steady-state employment rate (1 - u*).
    This avoids treating all new entrants as unemployed, which would overstate the
    unemployment stock.

    new_lf_mask : boolean array of length len(pop), True for new entrants.
    """
    idx = np.where(new_lf_mask)[0]
    if len(idx) == 0:
        return pop

    available_years = {k[3] for k in cell_params}
    use_year = _nearest_year(proj_year, available_years)

    pop      = pop.copy()
    emp_arr  = pop["employed"].values.copy()
    wage_arr = pop["incwage"].values.astype(float).copy()
    age_arr  = pop["age"].values
    sex_arr  = pop["sex"].values
    educ_arr = pop["educ"].values

    draws_emp  = rng.random(len(idx))
    draws_wage = rng.random(len(idx))

    for j, i in enumerate(idx):
        key = (int(sex_arr[i]), str(educ_arr[i]),
               _age_band(int(age_arr[i])), use_year)
        params = cell_params.get(key)
        if params is None:
            continue
        if draws_emp[j] < params["emp_rate"]:
            acc_mask = params["wages"] >= params["w_bar"]
            if not acc_mask.any():
                continue
            acc_wages = params["wages"][acc_mask]
            acc_probs = params["probs"][acc_mask]
            acc_probs = acc_probs / acc_probs.sum()
            cdf   = np.cumsum(acc_probs)
            w_idx = min(np.searchsorted(cdf, draws_wage[j]), len(acc_wages) - 1)
            emp_arr[i]  = "yes"
            wage_arr[i] = float(acc_wages[w_idx])

    pop["employed"] = emp_arr
    pop["incwage"]  = wage_arr
    return pop


# ─────────────────────────────────────────────────────────────────────────────
# Public: apply employment transitions for one projection year
# ─────────────────────────────────────────────────────────────────────────────

def apply_employment(pop: pd.DataFrame,
                     proj_year: int,
                     cell_params: dict,
                     wage_factors: dict,
                     rng: np.random.Generator) -> pd.DataFrame:
    """
    Apply one year of McCall employment dynamics.

    For each person in the labor force:
      - If employed:   separate with prob alpha → set employed='no', incwage=0
      - If unemployed: receive offer with prob gamma; if offered wage >= w_bar
                       (scaled by wage growth), set employed='yes', incwage=wage

    People outside the labor force have employed set to 'no'.
    """
    available_years = {k[3] for k in cell_params}
    use_year        = _nearest_year(proj_year, available_years)

    pop       = pop.copy()
    age_arr   = pop["age"].values
    sex_arr   = pop["sex"].values
    educ_arr  = pop["educ"].values
    lf_arr    = pop["labforce"].values
    emp_arr   = pop["employed"].values.copy()
    wage_arr  = pop["incwage"].values.astype(float).copy()

    n = len(pop)

    # Precompute cell key per person
    cell_keys = [
        (int(sex_arr[i]), str(educ_arr[i]), _age_band(int(age_arr[i])), use_year)
        for i in range(n)
    ]

    draws_sep   = rng.random(n)
    draws_find  = rng.random(n)
    draws_wage  = rng.random(n)

    # ── Separation: employed in LF ────────────────────────────────────────────
    for i in range(n):
        if lf_arr[i] != "yes" or emp_arr[i] != "yes":
            continue
        params   = cell_params.get(cell_keys[i])
        p_sep    = params["p_sep_annual"] if params else 0.068
        if draws_sep[i] < p_sep:
            emp_arr[i]  = "no"
            wage_arr[i] = 0.0

    # ── Job finding: unemployed in LF ─────────────────────────────────────────
    for i in range(n):
        if lf_arr[i] != "yes" or emp_arr[i] != "no":
            continue
        params = cell_params.get(cell_keys[i])
        if params is None:
            continue
        if draws_find[i] >= params["p_find_annual"]:
            continue

        # p_find_annual = P(acceptable offer in 12 months); offer already accepted.
        # Draw wage from the truncated (w >= w_bar) distribution only —
        # no second acceptance check, which would double-count rejection.
        gf_key = (int(sex_arr[i]), str(educ_arr[i]),
                  _age_band(int(age_arr[i])), proj_year)
        growth = wage_factors.get(gf_key, 1.0)
        wages  = params["wages"] * growth
        w_bar  = params["w_bar"] * growth

        accept_mask = wages >= w_bar
        if not accept_mask.any():
            continue
        acc_wages = wages[accept_mask]
        acc_probs = params["probs"][accept_mask]
        acc_probs = acc_probs / acc_probs.sum()
        cdf   = np.cumsum(acc_probs)
        w_idx = min(np.searchsorted(cdf, draws_wage[i]), len(acc_wages) - 1)
        emp_arr[i]  = "yes"
        wage_arr[i] = float(acc_wages[w_idx])

    # ── NILF: force employed = no ─────────────────────────────────────────────
    nilf_mask = lf_arr != "yes"
    emp_arr[nilf_mask] = "no"

    pop["employed"] = emp_arr
    pop["incwage"]  = wage_arr
    return pop
