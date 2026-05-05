"""
project_multi_year.py
Projects initial_simulation.csv (base year 2008) forward year by year through 2024.

Each year:
  1. Age everyone by 1
  2. Education transitions
  3. Deaths (period life tables + Chetty income gradient)
  4. Births (actual CDC ASFR)
  5. Immigration (cohort-component residual vs. SSA targets)
  6. Marriage / divorce
  7. Household formation

Prints a year-by-year table comparing key demographic moments to SSA targets.
"""

import sys
import pickle
import numpy as np
import pandas as pd

sys.path.insert(0, r"C:\Users\kritc\OneDrive\Documents\GitHub\social_security_cato_model\TEST\Py_file")

from death_and_births    import load_life_tables, load_income_mortality_gradient, apply_deaths, make_babies, load_asfr
from household_formation import form_households
from mccall_employment   import (calibrate_mccall, apply_employment, wage_growth_factors,
                                  assign_new_lf_employment,
                                  _age_band as _mccall_age_band)

BASE_DIR  = r"C:\Users\kritc\OneDrive\Documents\GitHub\social_security_cato_model\TEST"
RNG       = np.random.default_rng(20080101)
BASE_YEAR = 2008
END_YEAR  = 2024

# ─────────────────────────────────────────────────────────────────────────────
# LOAD STATIC INPUTS
# ─────────────────────────────────────────────────────────────────────────────
print("Loading static inputs ...")
pop = pd.read_csv(f"{BASE_DIR}/Data_Output/initial_simulation.csv")
pop["year"]          = BASE_YEAR
pop["ped"]           = 0.0
pop["perm_shock"]    = 0.0
pop["ped_initialized"] = 0
pop["aime"]          = 0.0
pop["predict_hat"]   = np.nan

edu_trans = pd.read_csv(f"{BASE_DIR}/Data_Output/edu_transition_probs_cohort.csv")
lt        = load_life_tables()
grad      = load_income_mortality_gradient()

ssa_all = pd.read_csv(f"{BASE_DIR}/Data_Input/SSPopJan_TR2023 (1).csv")
ssa_all.columns = ssa_all.columns.str.replace(" ", ".", regex=False)

lf_trans = pd.read_csv(f"{BASE_DIR}/Data_Output/lf_transition_prob.csv")

mccall_params = calibrate_mccall(
    unemp_csv  = f"{BASE_DIR}/Data_Output/unemp_rates.csv",
    sep_csv    = f"{BASE_DIR}/Data_Output/separation_rates.csv",
    wage_csv   = f"{BASE_DIR}/Data_Output/wage_distributions.csv",
    n_workers  = 4,
)
_wage_factors = wage_growth_factors(f"{BASE_DIR}/Data_Output/wage_distributions.csv")
with open(f"{BASE_DIR}/Data_Output/wage_model_cbo.pkl", "rb") as _f:
    wage_model = pickle.load(_f)["wage_model"]
# All-year average fallback for (sex, age, lf_lag) cells absent in a given year
lf_fallback = (
    lf_trans
    .groupby(["sex", "age", "lf_lag"])["prob_lf_yes"]
    .mean()
    .reset_index()
    .rename(columns={"prob_lf_yes": "prob_lf_yes_fb"})
)

# BLS LNS11300060 December (end-of-year) values (prime-age 25-54, seasonally adjusted, %)
_bls_raw = pd.read_csv(f"{BASE_DIR}/Data_Input/LNS11300060.csv", parse_dates=["observation_date"])
_bls_dec = _bls_raw[_bls_raw["observation_date"].dt.month == 12].copy()
_bls_dec["year"] = _bls_dec["observation_date"].dt.year
_BLS_LFPR_25_54: dict[int, float] = _bls_dec.set_index("year")["LNS11300060"].to_dict()

# BLS education-specific unemployment rates (December, end-of-year), %
# LNS14027660 (fredgraph): HS grad, no college, 25+   -> "hs"
# SCND2564              : Some college, no degree, 25-64  -> "some_college"
# LNS14027662 (fredgraph): BA+, 25+                   -> "ba_plus"
_bls_u_raw = pd.read_csv(f"{BASE_DIR}/Data_Input/fredgraph.csv", parse_dates=["observation_date"])
_bls_u_dec = _bls_u_raw[_bls_u_raw["observation_date"].dt.month == 12].copy()
_bls_u_dec["year"] = _bls_u_dec["observation_date"].dt.year
_bls_u_idx = _bls_u_dec.set_index("year")

_bls_sc_raw = pd.read_csv(f"{BASE_DIR}/Data_Input/SCND2564.csv", parse_dates=["observation_date"])
_bls_sc_dec = _bls_sc_raw[_bls_sc_raw["observation_date"].dt.month == 12].copy()
_bls_sc_dec["year"] = _bls_sc_dec["observation_date"].dt.year
_bls_sc_idx = _bls_sc_dec.set_index("year")["SCND2564"].to_dict()

_BLS_U_BY_EDUC: dict[tuple[str, int], float] = {}
for _yr, _row in _bls_u_idx.iterrows():
    _yr = int(_yr)
    _BLS_U_BY_EDUC[("hs",           _yr)] = float(_row["LNS14027660"])
    _BLS_U_BY_EDUC[("some_college", _yr)] = float(_bls_sc_idx.get(_yr, (_row["LNS14027660"] + _row["LNS14027662"]) / 2))
    _BLS_U_BY_EDUC[("ba_plus",      _yr)] = float(_row["LNS14027662"])

# ── Calibrate base-year labforce to BLS Dec-2008 LFPR 25-54 ──────────────────
# The CPS initial_simulation encodes raw LABFORCE which is ~4pp above BLS.
# Randomly flip excess "yes" → "no" within the prime-age group until the
# weighted LFPR matches the BLS December benchmark.
_bls_base = _BLS_LFPR_25_54.get(BASE_YEAR, 82.8) / 100.0
_mask_pa  = pop["age"].between(25, 54)
_wt_pa    = pop.loc[_mask_pa, "perwt"].sum()
_wt_lf_pa = pop.loc[_mask_pa & (pop["labforce"] == "yes"), "perwt"].sum()
_sim_base = _wt_lf_pa / _wt_pa
if _sim_base > _bls_base:
    # Probability of keeping each "yes" record to hit target
    _keep_p  = _bls_base / _sim_base
    _lf_yes_idx = pop.index[_mask_pa & (pop["labforce"] == "yes")]
    _flip = RNG.random(len(_lf_yes_idx)) > _keep_p
    pop.loc[_lf_yes_idx[_flip], "labforce"] = "no"
    _new_lfpr = (pop.loc[_mask_pa & (pop["labforce"] == "yes"), "perwt"].sum() /
                 pop.loc[_mask_pa, "perwt"].sum() * 100)
    print(f"Base-year LFPR 25-54 calibrated: {_sim_base*100:.1f}% -> {_new_lfpr:.1f}%  "
          f"(target {_bls_base*100:.1f}%)")

print(f"Base population: {len(pop):,} records.  Projecting {BASE_YEAR+1} to {END_YEAR}.")

# ─────────────────────────────────────────────────────────────────────────────
# HELPERS — EDUCATION TRANSITIONS
# ─────────────────────────────────────────────────────────────────────────────

def _cohort_grp(birth_year: pd.Series) -> pd.Series:
    bins   = list(range(1895, 2011, 5))
    labels = [f"{lo}-{lo+4}" for lo in range(1895, 2006, 5)]
    return pd.cut(birth_year, bins=bins, labels=labels, right=True).astype(str)


def _age_grp(age: pd.Series) -> pd.Series:
    breaks = [18, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90]
    labels = ["19-25","26-30","31-35","36-40","41-45","46-50",
              "51-55","56-60","61-65","66-70","71-75","76-80","81-85","86-90"]
    return pd.cut(age, bins=breaks, labels=labels, right=True).astype(str)


edu_lookup = (
    edu_trans
    .groupby(["sex", "cohort_grp", "age_grp", "educ"], observed=True)["prob_upgrade"]
    .mean()
    .reset_index()
)

# ── Graduation-attainment targets ────────────────────────────────────────────
# At age 22 we draw final education from a cohort-sex target distribution.
# This corrects for young cohorts who start the model at "hs" with only a few
# years of annual transitions before their first observed age (the annual rates
# are calibrated for marginal late-starters, not full trajectories).
#
# Targets are derived from:
#   • observed base-year distributions for cohorts born ≤1975 (age 33+ in 2008)
#   • ACS/CPS trends for younger cohorts
#
# Format: (birth_cohort_5yr, sex) → (prob_ba_plus, prob_some_college_given_not_ba)
_GRAD_TARGETS: dict[tuple, tuple] = {
    # prob_ba_plus at age 22 (NOT final attainment at 30+).
    # Back-solved from ACS 2024 attainment targets using:
    #   T = (ba_final - P*frac) / (1 - P*frac)
    # where frac = 1 - 0.95^avg_years_of_sc→ba_transitions_to_2024
    # Format: (birth_cohort_5yr, sex) → (prob_ba_plus, prob_sc_given_not_ba)
    (1940, 1): (0.110, 0.250), (1940, 2): (0.090, 0.280),
    (1945, 1): (0.140, 0.300), (1945, 2): (0.115, 0.310),
    (1950, 1): (0.140, 0.340), (1950, 2): (0.140, 0.360),
    (1955, 1): (0.140, 0.320), (1955, 2): (0.145, 0.370),
    (1960, 1): (0.148, 0.340), (1960, 2): (0.160, 0.400),
    (1965, 1): (0.158, 0.345), (1965, 2): (0.175, 0.405),
    (1970, 1): (0.165, 0.360), (1970, 2): (0.200, 0.420),
    (1975, 1): (0.157, 0.360), (1975, 2): (0.195, 0.430),
    (1980, 1): (0.165, 0.370), (1980, 2): (0.215, 0.440),
    # Cohorts turning 22 during projection (2009-2024): back-solved targets
    (1985, 1): (0.168, 0.370), (1985, 2): (0.237, 0.445),
    (1990, 1): (0.231, 0.380), (1990, 2): (0.305, 0.450),
    (1995, 1): (0.265, 0.390), (1995, 2): (0.354, 0.455),
    (2000, 1): (0.284, 0.395), (2000, 2): (0.363, 0.460),
    (2005, 1): (0.284, 0.395), (2005, 2): (0.363, 0.460),
}
_GRAD_TARGET_KEYS = sorted({k[0] for k in _GRAD_TARGETS.keys()})


def _grad_target(birth_cohort5: int, sex: int) -> tuple:
    """Return (prob_ba, prob_sc_given_not_ba) for the nearest available cohort."""
    nearest = min(_GRAD_TARGET_KEYS, key=lambda c: abs(c - birth_cohort5))
    return _GRAD_TARGETS.get((nearest, sex), (0.33, 0.42))


def _assign_graduation_educ(pop: pd.DataFrame, proj_year: int) -> pd.DataFrame:
    """
    One-time education draw for people who just turned 22.
    Draws final education from cohort-sex attainment targets; monotone (no
    downgrade).  Called before the annual transition step each year.
    """
    mask_22 = pop["age"].values == 22
    if not mask_22.any():
        return pop

    pop = pop.copy()
    birth_cohort5 = ((proj_year - 22) // 5) * 5

    educ_arr = pop["educ"].values.copy()
    sex_arr  = pop["sex"].values

    for sex in [1, 2]:
        prob_ba, prob_sc_gn = _grad_target(birth_cohort5, sex)
        idx = np.where(mask_22 & (sex_arr == sex))[0]
        if idx.size == 0:
            continue
        d1 = RNG.random(idx.size)
        d2 = RNG.random(idx.size)
        is_ba = d1 < prob_ba
        is_sc = (~is_ba) & (d2 < prob_sc_gn)
        # Monotone: never downgrade
        cur = educ_arr[idx]
        educ_arr[idx] = np.where(is_ba, "ba_plus",
                         np.where(is_sc, "some_college", cur))
        # If current was some_college or ba_plus and draw said hs → keep current
        educ_arr[idx] = np.where(
            (~is_ba) & (~is_sc) & np.isin(cur, ["some_college", "ba_plus"]),
            cur, educ_arr[idx]
        )

    pop["educ"] = educ_arr
    return pop


def _apply_edu_transitions(pop: pd.DataFrame, proj_year: int) -> pd.DataFrame:
    # Only some_college→ba_plus (late degree completion, ages 18-35).
    # hs→some_college is handled exclusively by _assign_graduation_educ at age 22.
    # Cap at 35: CPS-derived rates for age 36+ (~3%/yr) reflect noise/misclassification,
    # not genuine BA completions; applying them over 15+ years inflates ba_plus ~45pp.
    upgradeable = pop[
        (pop["educ"] == "some_college") & pop["age"].between(18, 35)
    ].copy()
    if upgradeable.empty:
        return pop

    orig_idx = upgradeable.index  # save before merge resets it to 0-based
    upgradeable["cohort_grp"] = _cohort_grp(proj_year - upgradeable["age"])
    upgradeable["age_grp"]    = _age_grp(upgradeable["age"])
    upgradeable = upgradeable.merge(
        edu_lookup[edu_lookup["educ"] == "some_college"],
        on=["sex","cohort_grp","age_grp","educ"], how="left"
    )
    upgradeable["prob_upgrade"] = upgradeable["prob_upgrade"].fillna(0.0)

    draws   = RNG.random(len(upgradeable))
    upgrade = orig_idx[draws < upgradeable["prob_upgrade"].values]
    pop = pop.copy()
    pop.loc[upgrade, "educ"] = "ba_plus"
    return pop


# ─────────────────────────────────────────────────────────────────────────────
# HELPERS — LABOR FORCE PARTICIPATION TRANSITIONS
# ─────────────────────────────────────────────────────────────────────────────

def _apply_lf_transitions(pop: pd.DataFrame, proj_year: int):
    """
    Apply annual LF participation transitions using CPS-derived rates.

    year_t in the CSV is the start-of-transition year; use proj_year - 1.
    Ages < 16 are always kept out of the LF.
    Missing (sex, age, lf_lag, year) cells fall back to the all-year average.

    Returns (updated_pop, new_lf_entrant_mask) where new_lf_entrant_mask is a
    boolean array marking people who transitioned NILF→LF this year.
    """
    year_t = max(2007, min(2023, proj_year - 1))
    lf_yr  = lf_trans[lf_trans["year_t"] == year_t][["sex", "age", "lf_lag", "prob_lf_yes"]]

    age_arr     = pop["age"].values
    age_clipped = np.clip(age_arr, 16, 85)
    prev_lf     = pop["labforce"].values.copy()   # save before update

    work = pd.DataFrame({
        "pos":     np.arange(len(pop), dtype=np.int64),
        "sex":     pop["sex"].values,
        "age":     age_clipped,
        "lf_lag":  prev_lf,
        "real_age": age_arr,
    })

    work = work.merge(lf_yr, on=["sex", "age", "lf_lag"], how="left")

    missing = work["prob_lf_yes"].isna()
    if missing.any():
        fb_vals = (
            work.loc[missing, ["sex", "age", "lf_lag"]]
            .merge(lf_fallback, on=["sex", "age", "lf_lag"], how="left")["prob_lf_yes_fb"]
            .values
        )
        work.loc[missing, "prob_lf_yes"] = fb_vals

    # Under-16 never participate; any remaining NaN → 0
    work.loc[work["real_age"] < 16, "prob_lf_yes"] = 0.0
    probs = work["prob_lf_yes"].fillna(0.0).values

    draws  = RNG.random(len(work))
    new_lf = np.where(draws < probs, "yes", "no")

    pop    = pop.copy()
    lf_arr = pop["labforce"].values.copy()
    lf_arr[work["pos"].values] = new_lf
    pop["labforce"] = lf_arr

    new_entrant_mask = (prev_lf == "no") & (lf_arr == "yes")
    return pop, new_entrant_mask


# ─────────────────────────────────────────────────────────────────────────────
# POST-McCALL UNEMPLOYMENT CALIBRATION — anchor to BLS education-specific rates
# ─────────────────────────────────────────────────────────────────────────────

def _calibrate_unemp_to_bls(pop: pd.DataFrame,
                              proj_year: int,
                              cell_params: dict,
                              wage_factors: dict) -> pd.DataFrame:
    """
    After McCall employment transitions, stochastically adjust employment so
    that simulated prime-age (25-54) unemployment by education matches BLS
    December targets from fredgraph.csv.

    Over-employed  (sim_u > BLS): randomly re-employ unemployed workers.
    Under-employed (sim_u < BLS): randomly un-employ employed workers.
    Workers aged 16-24 and 55+ are not touched — we only have BLS targets
    for 25+ by education.
    """
    available_years = {k[3] for k in cell_params}
    use_year = min(available_years, key=lambda y: abs(y - proj_year))

    pop      = pop.copy()
    emp_arr  = pop["employed"].values.copy()
    wage_arr = pop["incwage"].values.astype(float).copy()
    lf_arr   = pop["labforce"].values
    age_arr  = pop["age"].values
    educ_arr = pop["educ"].values
    sex_arr  = pop["sex"].values
    wt_arr   = pop["perwt"].values

    for educ_group in ["hs", "some_college", "ba_plus"]:
        target_u = _BLS_U_BY_EDUC.get((educ_group, proj_year))
        if target_u is None:
            continue
        target_u /= 100.0

        # Prime-age (25-54) in LF for this education group
        mask_lf = (lf_arr == "yes") & (age_arr >= 25) & (age_arr <= 54) & (educ_arr == educ_group)
        wt_lf   = wt_arr[mask_lf].sum()
        if wt_lf < 1e-6:
            continue

        mask_unemp = mask_lf & (emp_arr == "no")
        mask_emp   = mask_lf & (emp_arr == "yes")
        sim_u      = wt_arr[mask_unemp].sum() / wt_lf

        if sim_u > target_u:
            # Too many unemployed → re-employ some
            excess_wt = (sim_u - target_u) * wt_lf
            unemp_idx = np.where(mask_unemp)[0]
            if len(unemp_idx) == 0:
                continue
            perm = RNG.permutation(len(unemp_idx))
            cum_wt = 0.0
            for j in perm:
                if cum_wt >= excess_wt:
                    break
                i = unemp_idx[j]
                # Draw wage from McCall acceptable distribution (scaled by growth)
                key = (int(sex_arr[i]), str(educ_arr[i]),
                       _mccall_age_band(int(age_arr[i])), use_year)
                params = cell_params.get(key)
                if params is None:
                    continue
                gf_key = (int(sex_arr[i]), str(educ_arr[i]),
                          _mccall_age_band(int(age_arr[i])), proj_year)
                growth = wage_factors.get(gf_key, 1.0)
                wages  = params["wages"] * growth
                w_bar  = params["w_bar"] * growth
                acc    = wages >= w_bar
                if not acc.any():
                    continue
                acc_wages = wages[acc]
                acc_probs = params["probs"][acc] / params["probs"][acc].sum()
                cdf = np.cumsum(acc_probs)
                w_idx = min(np.searchsorted(cdf, RNG.random()), len(acc_wages) - 1)
                emp_arr[i]  = "yes"
                wage_arr[i] = float(acc_wages[w_idx])
                cum_wt += float(wt_arr[i])

        elif sim_u < target_u:
            # Too few unemployed → un-employ some
            deficit_wt = (target_u - sim_u) * wt_lf
            emp_idx = np.where(mask_emp)[0]
            if len(emp_idx) == 0:
                continue
            perm = RNG.permutation(len(emp_idx))
            cum_wt = 0.0
            for j in perm:
                if cum_wt >= deficit_wt:
                    break
                i = emp_idx[j]
                emp_arr[i]  = "no"
                wage_arr[i] = 0.0
                cum_wt += float(wt_arr[i])

        new_sim_u = wt_arr[(lf_arr == "yes") & (age_arr >= 25) & (age_arr <= 54) &
                           (educ_arr == educ_group) & (emp_arr == "no")].sum() / wt_lf
        print(f"    unemp {educ_group:12s}: sim={sim_u*100:.1f}% -> {new_sim_u*100:.1f}%  bls={target_u*100:.1f}%")

    pop["employed"] = emp_arr
    pop["incwage"]  = wage_arr
    return pop


# ─────────────────────────────────────────────────────────────────────────────
# PERWT RECALIBRATION — IPF vs SSA age×sex targets
# ─────────────────────────────────────────────────────────────────────────────

def recalibrate_ipf(pop: pd.DataFrame,
                    ssa_all: pd.DataFrame,
                    proj_year: int,
                    max_iter: int = 50,
                    tol: float = 1e-6) -> pd.DataFrame:
    """
    Iterative proportional fitting: reweight existing records to match SSA
    age×sex population counts.  No new records are added.

    Fully vectorized via np.bincount: one pass per iteration over all cells,
    no Python loop over cells.  Typically converges in <30 iterations.
    """
    ssa_yr = ssa_all[ssa_all["Year"] == proj_year]
    if ssa_yr.empty:
        return pop

    N_AGE   = 121
    N_CELLS = 2 * N_AGE
    tgt_vec = np.zeros(N_CELLS)
    m_tgt   = ssa_yr.set_index("Age")["M.Tot"]
    f_tgt   = ssa_yr.set_index("Age")["F.Tot"]
    for age in range(N_AGE):
        tgt_vec[age]         = float(m_tgt.get(age, 0.0))
        tgt_vec[N_AGE + age] = float(f_tgt.get(age, 0.0))
    has_tgt = tgt_vec > 0

    pop    = pop.copy()
    wt     = pop["perwt"].values.astype(np.float64)
    sex_v  = pop["sex"].values.astype(np.int64)
    age_v  = pop["age"].values.astype(np.int64).clip(0, N_AGE - 1)
    cell_v = (sex_v - 1) * N_AGE + age_v

    scale_vec = np.ones(N_CELLS)
    for _ in range(max_iter):
        sim_vec = np.bincount(cell_v, weights=wt, minlength=N_CELLS)
        valid   = has_tgt & (sim_vec > 0)
        scale_vec[:] = 1.0
        scale_vec[valid] = tgt_vec[valid] / sim_vec[valid]
        wt *= scale_vec[cell_v]
        if np.abs(scale_vec[valid] - 1.0).max() < tol:
            break

    pop["perwt"] = wt
    return pop


# ─────────────────────────────────────────────────────────────────────────────
# VALIDATION
# ─────────────────────────────────────────────────────────────────────────────

def _validate(pop: pd.DataFrame, ssa_yr: pd.DataFrame, year: int) -> dict:
    # SSA values are in persons
    ssa_tot = float(ssa_yr["Total"].sum())

    cbr_ssa  = float(ssa_yr[ssa_yr["Age"] == 0]["Total"].values[0]) / ssa_tot * 1000.0
    mar_ssa  = float(ssa_yr["M.Mar"].sum() + ssa_yr["F.Mar"].sum()) / ssa_tot
    div_ssa  = float(ssa_yr["M.Div"].sum() + ssa_yr["F.Div"].sum()) / ssa_tot
    dep_ssa  = (float(ssa_yr[ssa_yr["Age"] <  18]["Total"].sum() +
                      ssa_yr[ssa_yr["Age"] >= 65]["Total"].sum()) /
                float(ssa_yr[ssa_yr["Age"].between(18, 64)]["Total"].sum()))
    lfpr_bls = _BLS_LFPR_25_54.get(year, np.nan)

    wt      = pop["perwt"].values.astype(float)
    sim_tot = wt.sum()
    age_v   = pop["age"].values
    lf_v    = pop["labforce"].values == "yes"

    cbr_sim  = (wt * (age_v == 0)).sum() / sim_tot * 1000.0
    mar_sim  = (wt * (pop["marst"].values == "married")).sum() / sim_tot
    div_sim  = (wt * (pop["marst"].values == "divorced")).sum() / sim_tot
    dep_sim  = ((wt * (age_v < 18)).sum() + (wt * (age_v >= 65)).sum()) / \
                (wt * ((age_v >= 18) & (age_v <= 64))).sum()

    mask_pa   = (age_v >= 25) & (age_v <= 54)
    wt_pa     = (wt * mask_pa).sum()
    lfpr_sim  = (wt * mask_pa * lf_v).sum() / wt_pa * 100.0

    return dict(
        year=year,
        cbr_sim=cbr_sim,   cbr_ssa=cbr_ssa,
        mar_sim=mar_sim,   mar_ssa=mar_ssa,
        div_sim=div_sim,   div_ssa=div_ssa,
        dep_sim=dep_sim,   dep_ssa=dep_ssa,
        lfpr_sim=lfpr_sim, lfpr_bls=lfpr_bls,
        pop_sim=sim_tot / 1e6,
        pop_ssa=ssa_tot / 1e6,
    )


# ─────────────────────────────────────────────────────────────────────────────
# CBOLT EARNINGS MODULE  (Schwabish & Topoleski 2013, eq 9)
# ─────────────────────────────────────────────────────────────────────────────

# Figure 4 variances (σ = sqrt of the tabled variance, stratified by sex × age band)
_PERM_SD = {
    1: {"25_34": 0.026**0.5, "35_44": 0.024**0.5, "45_60": 0.033**0.5},
    2: {"25_34": 0.022**0.5, "35_44": 0.020**0.5, "45_60": 0.023**0.5},
}
_TRANS_SD = {
    1: {"25_34": 0.108**0.5, "35_44": 0.097**0.5, "45_60": 0.093**0.5},
    2: {"25_34": 0.094**0.5, "35_44": 0.073**0.5, "45_60": 0.057**0.5},
}
# Scale factor applied to both σ_N and σ_V each year (reduce variance to limit drift).
_CBOLT_SHOCK_SCALE = 0.5

# Pareto right-tail correction for CPS top-coding: the CPS caps wages at ~$150K,
# compressing the PED distribution for high earners.  Stretch PED values above the
# 90th percentile by this factor at initialization so the right tail matches the
# true (Pareto) wage distribution and TP ratio tracks SSA targets.
_PED_PARETO_STRETCH = 1.25

# SSA Average Wage Index (AWI) — nominal dollars, historical + preliminary 2024
_AWI = {
    2007: 40_405.48, 2008: 41_334.97, 2009: 40_711.61, 2010: 41_673.83,
    2011: 42_979.61, 2012: 44_321.67, 2013: 44_888.16, 2014: 46_481.52,
    2015: 48_098.63, 2016: 48_642.15, 2017: 50_321.89, 2018: 52_145.80,
    2019: 54_099.99, 2020: 55_628.60, 2021: 60_575.07, 2022: 63_795.13,
    2023: 66_621.80, 2024: 69_000.00,
}
_AWI_BASE  = _AWI[BASE_YEAR]
_WAGE_FLOOR = 4_200.0

# OASDI taxable maximum (wage base), nominal dollars
_TAX_MAX = {
    2009: 106_800, 2010: 106_800, 2011: 106_800, 2012: 110_100,
    2013: 113_700, 2014: 117_000, 2015: 118_500, 2016: 118_500,
    2017: 127_200, 2018: 128_400, 2019: 132_900, 2020: 137_700,
    2021: 142_800, 2022: 147_000, 2023: 160_200, 2024: 168_600,
}

# SSA taxable payroll ratio (covered taxable wages / total wages) — Trustees Reports
_SSA_TP_RATIO = {
    2009: 0.834, 2010: 0.833, 2011: 0.832, 2012: 0.830, 2013: 0.827,
    2014: 0.825, 2015: 0.823, 2016: 0.821, 2017: 0.820, 2018: 0.819,
    2019: 0.818, 2020: 0.828, 2021: 0.818, 2022: 0.823, 2023: 0.822,
    2024: 0.821,
}

# SSA % of workers above tax max — Trustees Reports (approx)
_SSA_PCT_ABOVE_MAX = {
    2009: 5.4, 2010: 5.4, 2011: 5.5, 2012: 5.6, 2013: 5.7,
    2014: 5.9, 2015: 6.0, 2016: 6.1, 2017: 6.2, 2018: 6.3,
    2019: 6.4, 2020: 5.8, 2021: 6.2, 2022: 6.5, 2023: 6.1,
    2024: 5.9,
}


def _weighted_gini_topcoded(wages: np.ndarray, weights: np.ndarray,
                             tax_max: float) -> float:
    """
    Gini coefficient on wages top-coded at the OASDI taxable maximum.
    Uses trapezoidal Lorenz curve integration with survey weights.
    """
    v = np.minimum(wages, tax_max)
    w = weights.astype(np.float64)
    idx = np.argsort(v)
    v, w = v[idx], w[idx]
    n    = w.sum()
    wm   = np.average(v, weights=w)
    if wm <= 0:
        return 0.0
    cumW  = np.concatenate([[0.0], np.cumsum(w) / n])
    cumX  = np.concatenate([[0.0], np.cumsum(w * v) / (n * wm)])
    B     = float(np.dot(np.diff(cumW), (cumX[:-1] + cumX[1:]) / 2))
    return 1.0 - 2.0 * B


def _dist_stats(pop: pd.DataFrame, year: int) -> dict:
    """
    Compute wage distribution statistics for employed workers (age > 18).
    Gini is computed on wages top-coded at the OASDI taxable maximum.
    """
    tax_max  = _TAX_MAX.get(year, 168_600)
    wk       = (pop["employed"].values == "yes") & (pop["age"].values > 18)
    if not wk.any():
        return {}
    wages = pop["incwage"].values[wk].astype(float)
    wts   = pop["perwt"].values[wk].astype(float)

    gini_tc  = _weighted_gini_topcoded(wages, wts, tax_max)
    pct_over = float((wts[wages > tax_max]).sum() / wts.sum() * 100.0)
    tp_ratio = float((wts * np.minimum(wages, tax_max)).sum() /
                     (wts * wages).sum()) if (wts * wages).sum() > 0 else np.nan
    return dict(
        gini_tc=gini_tc,
        pct_over_max=pct_over,
        tp_ratio=tp_ratio,
        pct_over_ssa=_SSA_PCT_ABOVE_MAX.get(year, np.nan),
        tp_ratio_ssa=_SSA_TP_RATIO.get(year, np.nan),
    )


def _tp_calibrated_level_match(log_E_raw: np.ndarray, worker_mask: np.ndarray,
                               wts: np.ndarray, year: int) -> np.ndarray:
    """
    Bisect on a top-tail stretch factor s so that the final wage distribution hits
    _SSA_TP_RATIO[year] exactly, then shift the whole distribution so the weighted
    arithmetic mean of worker wages equals SSA AWI_t.

    Threshold = p90 of workers' current log wages (adapts each year).
    For workers above threshold: log_E += (log_E - threshold) * (s - 1).
    s = 1.0 means no additional stretch; s > 1 spreads the right tail further.
    """
    target_tp = _SSA_TP_RATIO.get(year)
    tax_max   = _TAX_MAX.get(year, 168_600)
    awi_t     = _AWI.get(year, _AWI_BASE * (1.035 ** (year - BASE_YEAR)))
    log_awi   = np.log(awi_t)
    w_indices = np.where(worker_mask)[0]
    wt_w      = wts[worker_mask]
    w_raw     = log_E_raw[worker_mask]
    threshold = float(np.percentile(w_raw, 90))

    def _evaluate(s: float) -> tuple:
        log_s  = log_E_raw.copy()
        excess = w_raw - threshold
        top_m  = excess > 0
        if s != 1.0 and top_m.any():
            log_s[w_indices[top_m]] += excess[top_m] * (s - 1.0)
        mean_a = np.average(np.exp(log_s[worker_mask]), weights=wt_w)
        log_s += log_awi - np.log(mean_a)
        wages  = np.exp(log_s[worker_mask])
        tp     = float((wt_w * np.minimum(wages, tax_max)).sum() / (wt_w * wages).sum())
        return tp, log_s

    if target_tp is None:                    # no SSA target: AWI level-match only
        _, out = _evaluate(1.0)
        return out

    tp0, log0 = _evaluate(1.0)
    if abs(tp0 - target_tp) < 1e-4:
        return log0

    # tp < target → distribution too spread (too many above max) → compress (s < 1)
    # tp > target → distribution too compressed → stretch (s > 1)
    lo, hi = (0.0, 1.0) if tp0 < target_tp else (1.0, 10.0)

    for _ in range(50):
        mid = (lo + hi) / 2.0
        tp_mid, log_mid = _evaluate(mid)
        if abs(tp_mid - target_tp) < 1e-4:
            return log_mid
        if tp_mid < target_tp:               # too spread → compress more (lower s)
            hi = mid
        else:                                # too compressed → stretch more (higher s)
            lo = mid

    _, out = _evaluate((lo + hi) / 2.0)
    return out


def _stretch_ped_top_tail(ped_arr: np.ndarray, p90: float) -> np.ndarray:
    """Stretch PED values above p90 by _PED_PARETO_STRETCH to recreate the Pareto right tail
    suppressed by CPS top-coding.  Values at or below p90 are unchanged."""
    excess = ped_arr - p90
    top    = excess > 0
    out    = ped_arr.copy()
    out[top] = p90 + excess[top] * _PED_PARETO_STRETCH
    return out


def _predict_log_wage(wm: dict, pop: pd.DataFrame) -> np.ndarray:
    score = (float(wm["beta_age"])    * pop["age"].values.astype(float)
           + float(wm["beta_cohort"]) * pop["cohort"].values.astype(float))
    fe_sex  = {str(k): float(v) for k, v in wm.get("fixef_sex",  {}).items()}
    fe_educ = {str(k): float(v) for k, v in wm.get("fixef_educ", {}).items()}
    fe_ss   = {str(k): float(v) for k, v in wm.get("fixef_ss",   {}).items()}
    score += np.array([fe_sex .get(str(int(s)), 0.0) for s in pop["sex"].values])
    score += np.array([fe_educ.get(str(e),      0.0) for e in pop["educ"].values])
    score += np.array([fe_ss  .get(str(s),      0.0) for s in pop["receive_ss"].values])
    return score


def _cbolt_age_grp(ages: np.ndarray) -> np.ndarray:
    return np.where(ages < 35, "25_34", np.where(ages < 45, "35_44", "45_60"))


def build_ped_donors(pop: pd.DataFrame, wm: dict, p90: float | None = None) -> dict:
    """
    PED donor pools by (sex, educ) from base-year employed workers ages 21-31.
    If p90 is provided, applies Pareto right-tail stretching for CPS top-coding correction.
    Returns {(sex_int, educ_str): np.ndarray of residuals}.
    """
    pred = _predict_log_wage(wm, pop)
    worker_mask = ((pop["employed"].values == "yes") &
                   (pop["incwage"].values   >  _WAGE_FLOOR) &
                   (pop["age"].values       >  18))
    donor_mask  = worker_mask & pop["age"].between(21, 31).values
    donors: dict = {}
    for sv in [1, 2]:
        for ev in ["hs", "some_college", "ba_plus"]:
            m   = donor_mask & (pop["sex"].values == sv) & (pop["educ"].values == ev)
            m_s = donor_mask & (pop["sex"].values == sv)   # sex-only fallback
            if m.sum() > 0:
                pool = np.log(pop["incwage"].values[m]) - pred[m]
            elif m_s.sum() > 0:
                pool = np.log(pop["incwage"].values[m_s]) - pred[m_s]
            else:
                pool = np.array([0.0])
            if p90 is not None:
                pool = _stretch_ped_top_tail(pool, p90)
            donors[(sv, ev)] = pool
    return donors


def init_ped(pop: pd.DataFrame, wm: dict, donors: dict,
             p90: float | None = None) -> tuple:
    """
    Base-year PED (e_i) initialization.  Returns (ped, initialized).

    Career started (initialized=1):
      - Current workers: e_i = ln(w) - ln(ŵ), optionally Pareto-stretched above p90
      - Non-workers age >= 22: draw from (already-stretched) donor pool
    Career not yet started (initialized=0):
      - Age < 22, not currently employed: e_i = 0, drawn at first LF entry
    """
    pred        = _predict_log_wage(wm, pop)
    ped         = np.zeros(len(pop))
    initialized = np.zeros(len(pop), dtype=np.int8)

    worker_mask = ((pop["employed"].values == "yes") &
                   (pop["incwage"].values   >  _WAGE_FLOOR) &
                   (pop["age"].values       >  18))
    raw = np.log(pop["incwage"].values[worker_mask]) - pred[worker_mask]
    ped[worker_mask] = _stretch_ped_top_tail(raw, p90) if p90 is not None else raw
    initialized[worker_mask] = 1

    # Non-workers with career started (age >= 22): draw from (already-stretched) donor pool
    career_nw = (~worker_mask) & (pop["age"].values >= 22)
    for sv in [1, 2]:
        for ev in ["hs", "some_college", "ba_plus"]:
            m = career_nw & (pop["sex"].values == sv) & (pop["educ"].values == ev)
            if m.sum() > 0:
                pool = donors.get((sv, ev), donors.get((sv, "hs"), np.array([0.0])))
                ped[m] = RNG.choice(pool, size=m.sum(), replace=True)
                initialized[m] = 1

    return ped, initialized


def redraw_ped_on_upgrade(pop: pd.DataFrame, upgrade_mask: np.ndarray,
                          donors: dict) -> tuple:
    """
    Education upgrade → fresh e_i draw from (sex, new_educ) donor pool.
    Also resets perm_shock to 0: education upgrade starts a fresh earnings trajectory.
    Returns (new_ped, new_perm_shock).
    """
    ped        = pop["ped"].values.copy()
    perm_shock = pop["perm_shock"].values.copy()
    for sv in [1, 2]:
        for ev in ["some_college", "ba_plus"]:
            m = upgrade_mask & (pop["sex"].values == sv) & (pop["educ"].values == ev)
            if m.sum() > 0:
                pool = donors.get((sv, ev), np.array([0.0]))
                ped[m]        = RNG.choice(pool, size=m.sum(), replace=True)
                perm_shock[m] = 0.0
    return ped, perm_shock


def update_wages_cbolt(pop: pd.DataFrame, wm: dict, year: int) -> pd.DataFrame:
    """
    Annual CBOLT earnings update (Schwabish & Topoleski 2013, eq 9).

    ln E_it = ln(Ê_it) + e_i + p_it + v_it

      e_i   = pop["ped"]        — permanent differential, fixed at career start
      p_it  = pop["perm_shock"] — accumulated permanent shock (random walk)
      v_it  = beta drawn here   — transitory shock, iid each year

    p_it ← p_{i,t-1} + α·σ_N   [random walk accumulates each year employed]
    AWI level-match: shift entire distribution so weighted arithmetic mean of
    worker wages equals SSA AWI_t.
    """
    pop      = pop.copy()
    pred_log = _predict_log_wage(wm, pop)

    worker_mask = (pop["employed"].values == "yes") & (pop["age"].values > 18)
    ages    = pop["age"].values
    age_grp = _cbolt_age_grp(ages)
    n       = len(pop)
    alpha   = np.zeros(n)   # permanent shock increment ε_it
    beta_v  = np.zeros(n)   # transitory shock v_it

    for sv in [1, 2]:
        sx = pop["sex"].values == sv
        for ag in ["25_34", "35_44", "45_60"]:
            m = worker_mask & sx & (age_grp == ag)
            if m.sum() == 0:
                continue
            alpha[m]  = RNG.standard_normal(m.sum()) * _PERM_SD[sv][ag]  * _CBOLT_SHOCK_SCALE
            beta_v[m] = RNG.standard_normal(m.sum()) * _TRANS_SD[sv][ag] * _CBOLT_SHOCK_SCALE

    # p_it accumulates permanent shocks (random walk); e_i never changes here
    pop["perm_shock"] = pop["perm_shock"].values + alpha

    log_E_raw = pred_log + pop["ped"].values + pop["perm_shock"].values + beta_v

    # TP-calibrated level-match: bisect top-tail stretch to hit SSA TP ratio,
    # then shift so weighted arithmetic mean of worker wages = SSA AWI_t.
    if worker_mask.sum() > 0:
        log_E_raw = _tp_calibrated_level_match(
            log_E_raw, worker_mask, pop["perwt"].values, year
        )

    new_wages              = np.exp(log_E_raw)
    wage_arr               = pop["incwage"].values.copy().astype(float)
    wage_arr[ worker_mask] = np.maximum(new_wages[worker_mask], _WAGE_FLOOR)
    wage_arr[~worker_mask] = 0.0
    pop["incwage"]         = wage_arr
    return pop


# ── One-time base-year PED (e_i) initialization ──────────────────────────────
print("Initializing CBOLT PED from base-year wage residuals ...")
# Pre-compute raw worker PED to get the global p90 threshold for Pareto stretching
_raw_pred = _predict_log_wage(wage_model, pop)
_wm_p90   = ((pop["employed"].values == "yes") &
             (pop["incwage"].values   >  _WAGE_FLOOR) &
             (pop["age"].values       >  18))
_raw_ped  = np.log(pop["incwage"].values[_wm_p90]) - _raw_pred[_wm_p90]
_PED_P90  = float(np.percentile(_raw_ped, 90))
print(f"  PED p90 threshold = {_PED_P90:.3f}  (stretch={_PED_PARETO_STRETCH}x above this)")

ped_donors = build_ped_donors(pop, wage_model, p90=_PED_P90)
pop["ped"], init_flags = init_ped(pop, wage_model, ped_donors, p90=_PED_P90)
pop["ped_initialized"]  = init_flags
workers_init = ((pop["employed"] == "yes") & (pop["age"] > 18)).sum()
print(f"  PED initialized: {workers_init:,} workers  "
      f"mean={pop['ped'].mean():.3f}  sd={pop['ped'].std():.3f}  "
      f"n_uninitialized={int((pop['ped_initialized']==0).sum()):,}")

# ─────────────────────────────────────────────────────────────────────────────
# MAIN PROJECTION LOOP
# ─────────────────────────────────────────────────────────────────────────────
results = []

for proj_year in range(BASE_YEAR + 1, END_YEAR + 1):
    print(f"\n-- {proj_year} -----------------------------------")

    # 1. Age + year
    pop_prev = pop.copy()
    pop = pop.copy()
    pop["age"]  += 1
    pop["year"]  = proj_year

    # 2a. Graduation draw (age 22): one-time cohort attainment assignment
    educ_before = pop["educ"].values.copy()
    pop = _assign_graduation_educ(pop, proj_year)

    # 2b. Annual education transitions (marginal late-starters)
    pop = _apply_edu_transitions(pop, proj_year)

    # 2c. Education upgrade → fresh e_i draw + reset perm_shock
    upgrade_mask = pop["educ"].values != educ_before
    if upgrade_mask.any():
        pop["ped"], pop["perm_shock"] = redraw_ped_on_upgrade(pop, upgrade_mask, ped_donors)

    # 3. Deaths
    n0  = len(pop)
    pop = apply_deaths(pop, lt, grad, proj_year)
    print(f"  Deaths: {n0 - len(pop):,}")

    # 4. Births
    n0  = len(pop)
    pop = make_babies(pop, proj_year, load_asfr(proj_year))
    print(f"  Births: {len(pop) - n0:,}")

    # 5. Recalibrate perwt via IPF to match SSA age×sex targets
    pop = recalibrate_ipf(pop, ssa_all, proj_year)
    print(f"  IPF done  (pop ~= {pop['perwt'].sum()/1e6:.1f}M)")

    # 6. Household formation (includes college departures, marriages, divorces, other-adult moves)
    pop_prev_aligned = pop_prev.reindex(pop.index)
    pop = form_households(pop, pop_prev_aligned, proj_year, RNG)

    # 7. Labor force participation transitions
    pop, new_lf_mask = _apply_lf_transitions(pop, proj_year)
    lf_yes = (pop["labforce"] == "yes")
    mask_pa = pop["age"].between(25, 54)
    wt_pa   = pop.loc[mask_pa, "perwt"].sum()
    lfpr_pa = (pop.loc[mask_pa & lf_yes, "perwt"].sum() / wt_pa * 100.0) if wt_pa > 0 else np.nan
    print(f"  LFPR 25-54: sim={lfpr_pa:.1f}%  bls={_BLS_LFPR_25_54.get(proj_year, float('nan')):.1f}%  "
          f"err={lfpr_pa - _BLS_LFPR_25_54.get(proj_year, lfpr_pa):+.1f}pp")

    # 7b. Career-start PED draw for first-ever LF entrants (ped_initialized == 0)
    new_career_mask = new_lf_mask & (pop["ped_initialized"].values == 0)
    if new_career_mask.any():
        ped_arr  = pop["ped"].values.copy()
        init_arr = pop["ped_initialized"].values.copy()
        for sv in [1, 2]:
            for ev in ["hs", "some_college", "ba_plus"]:
                m = new_career_mask & (pop["sex"].values == sv) & (pop["educ"].values == ev)
                if m.sum() > 0:
                    pool = ped_donors.get((sv, ev), ped_donors.get((sv, "hs"), np.array([0.0])))
                    ped_arr[m]  = RNG.choice(pool, size=m.sum(), replace=True)
                    init_arr[m] = 1
        pop["ped"]             = ped_arr
        pop["ped_initialized"] = init_arr

    # 7c. Assign initial employment to new NILF→LF entrants (steady-state draw)
    #     McCall wage scaled by exp(PED) so high-PED entrants earn appropriately
    pop = assign_new_lf_employment(pop, new_lf_mask, mccall_params, proj_year, RNG,
                                   ped_vec=pop["ped"].values)

    # 8. Employment transitions (McCall) — separation + job-finding for existing LF
    #    Offered wages scaled by exp(PED_i) to reflect individual permanent quality
    pop = apply_employment(pop, proj_year, mccall_params, _wage_factors, RNG,
                           ped_vec=pop["ped"].values)

    # 8b. Post-calibrate prime-age (25-54) unemployment by education to BLS targets
    pop = _calibrate_unemp_to_bls(pop, proj_year, mccall_params, _wage_factors)

    # 8c. CBOLT earnings update — apply permanent/transitory shocks, AWI level-match
    #     PED (e_i) is fixed; only perm_shock (p_it) accumulates each year
    pop = update_wages_cbolt(pop, wage_model, proj_year)
    workers_cbolt = (pop["employed"] == "yes") & (pop["age"] > 18)
    if workers_cbolt.any():
        w_wages = pop.loc[workers_cbolt, "incwage"].values
        w_wts   = pop.loc[workers_cbolt, "perwt"].values
        mean_w  = np.average(w_wages, weights=w_wts)
        awi_t   = _AWI.get(proj_year, _AWI_BASE * (1.035 ** (proj_year - BASE_YEAR)))
        print(f"  CBOLT wages: mean={mean_w:,.0f}  AWI={awi_t:,.0f}  err={mean_w/awi_t-1:+.3f}")

    in_lf    = pop["labforce"] == "yes"
    employed = pop["employed"] == "yes"
    wt_lf    = pop.loc[in_lf, "perwt"].sum()
    urate    = (pop.loc[in_lf & ~employed, "perwt"].sum() / wt_lf * 100) if wt_lf > 0 else np.nan
    print(f"  Unemployment rate (overall LF): {urate:.1f}%")

    # 8d. Wage distribution stats (Gini top-coded at tax max)
    ds = _dist_stats(pop, proj_year)
    if ds:
        print(f"  Gini(TC) {ds['gini_tc']:.3f}  "
              f"Pct>Max sim={ds['pct_over_max']:.1f}% ssa={ds['pct_over_ssa']:.1f}%  "
              f"TP sim={ds['tp_ratio']:.3f} ssa={ds['tp_ratio_ssa']:.3f}")

    # 9. Validate
    ssa_yr = ssa_all[ssa_all["Year"] == proj_year]
    # Save microdata for this year
    out_cols = ["famunit","perwt","hhwt","marst","age","school","sex","educ",
                "labforce","employed","retired","incwage","receive_ss","cohort",
                "relate","year","ped","perm_shock","ped_initialized","aime","predict_hat"]
    out_cols = [c for c in out_cols if c in pop.columns]
    pop[out_cols].to_csv(f"{BASE_DIR}/Data_Output/projected_{proj_year}.csv", index=False)

    if not ssa_yr.empty:
        row = _validate(pop, ssa_yr, proj_year)
        row.update(ds)
        results.append(row)
        print(f"  pop  sim={row['pop_sim']:.1f}M  ssa={row['pop_ssa']:.1f}M  "
              f"err={row['pop_sim']-row['pop_ssa']:+.1f}M")
        print(f"  CBR  sim={row['cbr_sim']:.2f}  ssa={row['cbr_ssa']:.2f}  "
              f"err={row['cbr_sim']-row['cbr_ssa']:+.2f}")
        print(f"  %Mar sim={row['mar_sim']:.4f}  ssa={row['mar_ssa']:.4f}  "
              f"err={row['mar_sim']-row['mar_ssa']:+.4f}")
        print(f"  %Div sim={row['div_sim']:.4f}  ssa={row['div_ssa']:.4f}  "
              f"err={row['div_sim']-row['div_ssa']:+.4f}")
        print(f"  DepR sim={row['dep_sim']:.4f}  ssa={row['dep_ssa']:.4f}  "
              f"err={row['dep_sim']-row['dep_ssa']:+.4f}")

# ─────────────────────────────────────────────────────────────────────────────
# SUMMARY TABLE
# ─────────────────────────────────────────────────────────────────────────────
if results:
    df_res = pd.DataFrame(results)
    df_res["cbr_err"]  = df_res["cbr_sim"]  - df_res["cbr_ssa"]
    df_res["mar_err"]  = df_res["mar_sim"]  - df_res["mar_ssa"]
    df_res["div_err"]  = df_res["div_sim"]  - df_res["div_ssa"]
    df_res["dep_err"]  = df_res["dep_sim"]  - df_res["dep_ssa"]
    df_res["pop_err"]  = df_res["pop_sim"]  - df_res["pop_ssa"]
    df_res["lfpr_err"] = df_res["lfpr_sim"] - df_res["lfpr_bls"]

    print("\n" + "="*130)
    print(f"{'Year':>4}  "
          f"{'Pop sim':>7} {'err':>5}  "
          f"{'CBR sim':>7} {'err':>5}  "
          f"{'DepR sim':>8} {'err':>6}  "
          f"{'LFPR sim':>8} {'err':>6}  "
          f"{'Gini(TC)':>8}  "
          f"{'Pct>Max':>7} {'ssa':>5}  "
          f"{'TP_sim':>6} {'TP_ssa':>6}")
    print("-"*130)
    for _, r in df_res.iterrows():
        gini_s = f"{r['gini_tc']:.3f}" if "gini_tc" in r and not pd.isna(r.get("gini_tc", np.nan)) else "  n/a"
        pom_s  = f"{r['pct_over_max']:.1f}%" if "pct_over_max" in r else "  n/a"
        posa_s = f"{r['pct_over_ssa']:.1f}%" if "pct_over_ssa" in r else "  n/a"
        tp_s   = f"{r['tp_ratio']:.3f}" if "tp_ratio" in r and not pd.isna(r.get("tp_ratio", np.nan)) else " n/a"
        tpa_s  = f"{r['tp_ratio_ssa']:.3f}" if "tp_ratio_ssa" in r and not pd.isna(r.get("tp_ratio_ssa", np.nan)) else " n/a"
        print(f"{int(r['year']):>4}  "
              f"{r['pop_sim']:>7.1f} {r['pop_err']:>+5.1f}  "
              f"{r['cbr_sim']:>7.2f} {r['cbr_err']:>+5.2f}  "
              f"{r['dep_sim']:>8.4f} {r['dep_err']:>+6.4f}  "
              f"{r['lfpr_sim']:>8.1f} {r['lfpr_err']:>+6.1f}  "
              f"{gini_s:>8}  "
              f"{pom_s:>7} {posa_s:>5}  "
              f"{tp_s:>6} {tpa_s:>6}")
    print("="*130)

    print(f"\nMean absolute errors:")
    print(f"  Population : {df_res['pop_err'].abs().mean():.1f}M")
    print(f"  CBR        : {df_res['cbr_err'].abs().mean():.3f} /1000")
    print(f"  %Mar       : {df_res['mar_err'].abs().mean():.4f}")
    print(f"  %Div       : {df_res['div_err'].abs().mean():.4f}")
    print(f"  DepR       : {df_res['dep_err'].abs().mean():.4f}")
    print(f"  LFPR 25-54 : {df_res['lfpr_err'].abs().mean():.2f}pp")
    if "pct_over_max" in df_res.columns and "pct_over_ssa" in df_res.columns:
        print(f"  Pct>TaxMax : {(df_res['pct_over_max'] - df_res['pct_over_ssa']).abs().mean():.2f}pp")
    if "tp_ratio" in df_res.columns and "tp_ratio_ssa" in df_res.columns:
        print(f"  TP ratio   : {(df_res['tp_ratio'] - df_res['tp_ratio_ssa']).abs().mean():.4f}")

    out_path = f"{BASE_DIR}/Data_Output/multi_year_validation.csv"
    df_res.to_csv(out_path, index=False)
    print(f"\nSaved -> {out_path}")
