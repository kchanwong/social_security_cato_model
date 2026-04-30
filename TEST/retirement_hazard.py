#!/usr/bin/env python3
"""
retirement_hazard.py — Social Security Wealth and retirement hazard module.

All SSW measures are EXPECTED (forward-looking), not immediate:

  E[SSW(T)]  = PV of benefits the worker expects to collect
               if they keep working until age T and then claim,
               given their current AIME and the age-earnings
               profile implied by the wage model.

  Accrual(a) = E[SSW(a+1)] − E[SSW(a)]
               One-year forward difference in expected wealth;
               positive → financially rewarding to delay retirement.

  PeakValue  = max_{T ∈ [a,70]} E[SSW(T)] − E[SSW(a)]
               Surplus expected wealth from working until the optimal
               future claiming age (Gruber & Wise 1998).

The age-efficiency profile enters via the wage model's beta_age coefficient:
  E[ln w_{i,a+t}] = beta_age×(age_i+t) + [other fixed terms] + PED_i
PED is held at its current value (rational expectations: zero drift).
AWI is projected at its long-run real growth rate (AWI_GROWTH_RATE).

Depends on:
  - pop["aime"]       : monthly AIME in real 2008 dollars
  - pop["ped"]        : persistent earnings deviation (log scale)
  - pop["age"], pop["sex"], pop["educ"], pop["marst"]
  - lt                : period life-table DataFrame
  - wage_model        : R feols coef dict (for age-efficiency slope)
"""

import numpy as np
import pandas as pd

# ─────────────────────────────────────────────────────────────────────────────
# PIA BEND POINTS  (monthly AIME, nominal $)
# Source: SSA OASDI Trustees Reports, Table V.C1
# ─────────────────────────────────────────────────────────────────────────────
PIA_BEND = {
    2008: (711,  4288),  2009: (744,  4483),  2010: (761,  4586),
    2011: (749,  4517),  2012: (767,  4624),  2013: (791,  4768),
    2014: (816,  4917),  2015: (826,  4980),  2016: (856,  5157),
    2017: (885,  5336),  2018: (895,  5397),  2019: (926,  5583),
    2020: (960,  5785),  2021: (996,  6002),  2022: (1024, 6172),
    2023: (1115, 6721),  2024: (1174, 7078),
}

# AWI for indexing earnings (nominal)
AWI = {
    2007: 40_405.48, 2008: 41_334.97, 2009: 40_711.61, 2010: 41_673.83,
    2011: 42_979.61, 2012: 44_321.67, 2013: 44_888.16, 2014: 46_481.52,
    2015: 48_098.63, 2016: 48_642.15, 2017: 50_321.89, 2018: 52_145.80,
    2019: 54_099.99, 2020: 55_628.60, 2021: 60_575.07, 2022: 63_795.13,
    2023: 66_621.80, 2024: 69_000.00,
}
AWI_BASE        = AWI[2008]
AWI_GROWTH_RATE = 0.010    # long-run real AWI growth for projection beyond data
DISCOUNT_RATE   = 0.030    # real personal discount rate for SSW PV
MAX_AGE         = 110      # actuarial cutoff


# ─────────────────────────────────────────────────────────────────────────────
# PIA FORMULA
# ─────────────────────────────────────────────────────────────────────────────

def compute_pia_monthly(aime_monthly: np.ndarray, year: int) -> np.ndarray:
    """
    Monthly PIA from monthly AIME using the bend-point schedule for 'year'.
    PIA = 0.90 × min(AIME, b1)
        + 0.32 × max(0, min(AIME−b1, b2−b1))
        + 0.15 × max(0, AIME−b2)
    """
    b1, b2 = PIA_BEND.get(year, PIA_BEND[max(k for k in PIA_BEND if k <= year)])
    return (0.90 * np.minimum(aime_monthly, b1)
          + 0.32 * np.clip(aime_monthly - b1, 0.0, b2 - b1)
          + 0.15 * np.maximum(0.0, aime_monthly - b2))


# ─────────────────────────────────────────────────────────────────────────────
# FULL RETIREMENT AGE  (by birth cohort)
# ─────────────────────────────────────────────────────────────────────────────

def full_retirement_age(birth_year: np.ndarray) -> np.ndarray:
    """
    FRA in decimal years.
    ≤1937: 65  |  1938-1942: 65+2mo/yr  |  1943-1954: 66
    1955-1959: 66+2mo/yr  |  ≥1960: 67
    """
    return np.where(birth_year <= 1937, 65.0,
           np.where(birth_year <= 1942, 65 + (birth_year - 1937) * 2 / 12,
           np.where(birth_year <= 1954, 66.0,
           np.where(birth_year <= 1959, 66 + (birth_year - 1954) * 2 / 12,
                                        67.0))))


# ─────────────────────────────────────────────────────────────────────────────
# CLAIMING ADJUSTMENT
# ─────────────────────────────────────────────────────────────────────────────

def claiming_adjustment(claim_age: np.ndarray, fra: np.ndarray) -> np.ndarray:
    """
    Benefit as fraction of PIA.
    Early:   −5/9 %/month first 36 months; −5/12 %/month thereafter.
    Delayed: +8 %/year (DRC), capped at 70.
    """
    months_diff = np.round((claim_age - fra) * 12).astype(int)
    m_early     = np.maximum(0, -months_diff)
    early_adj   = np.where(
        m_early <= 36,
        1.0 - (5/9/100)  * m_early,
        1.0 - (5/9/100)  * 36 - (5/12/100) * (m_early - 36),
    )
    delayed_adj = 1.0 + 0.08 * np.maximum(0, months_diff) / 12
    return np.clip(np.where(months_diff >= 0, delayed_adj, early_adj), 0.40, 1.32)


# ─────────────────────────────────────────────────────────────────────────────
# ANNUITY TABLE
# ─────────────────────────────────────────────────────────────────────────────

def build_annuity_table(lt: pd.DataFrame, year: int,
                        discount_rate: float = DISCOUNT_RATE) -> dict:
    """
    Pre-compute the actuarial PV annuity factor for every (sex, start_age):
      A(sex, a) = Σ_{t=0}^{MAX_AGE−a} (1+r)^{−t} × S(sex, a, a+t)
    where S is cumulative survival from age a to a+t.
    Called once per simulation year.  Returns dict: (sex, start_age) → float.
    """
    avail  = lt[lt["year"] <= year]["year"].unique()
    lt_yr  = (lt[lt["year"] == max(avail)]
              .set_index(["sex", "age"])["q"]
              .to_dict())
    table = {}
    for sex in [1, 2]:
        for start_age in range(55, MAX_AGE):
            pv = surv = disc = 1.0
            pv = 0.0
            surv = 1.0
            disc = 1.0
            for age in range(start_age, MAX_AGE + 1):
                pv   += surv * disc
                q     = lt_yr.get((sex, age), 0.0)
                surv *= (1.0 - q)
                disc /= (1.0 + discount_rate)
                if surv < 1e-6:
                    break
            table[(sex, start_age)] = pv
    return table


# ─────────────────────────────────────────────────────────────────────────────
# AWI PROJECTION  (beyond data horizon)
# ─────────────────────────────────────────────────────────────────────────────

def _awi(year: int) -> float:
    """Nominal AWI for 'year', extrapolating if beyond table."""
    if year in AWI:
        return AWI[year]
    last_yr = max(AWI)
    return AWI[last_yr] * ((1 + AWI_GROWTH_RATE) ** (year - last_yr))


# ─────────────────────────────────────────────────────────────────────────────
# EXPECTED AIME PROJECTION
#
# Core idea: starting from current AIME, project the worker's future earnings
# along their age-efficiency profile (beta_age from the wage model) plus their
# current PED (held fixed in expectation). Convert each future year's projected
# real wage into a real 2008-dollar AWI-indexed monthly figure, then update the
# 35-year rolling AIME average step-by-step.
#
# Result: a (N, n_horizon) array of projected AIMEs — one column per claiming
# age T in {current_age+1, ..., 70}.
# ─────────────────────────────────────────────────────────────────────────────

def _project_aime_matrix(pop: pd.DataFrame, wage_model: dict,
                         current_year: int) -> np.ndarray:
    """
    Returns aime_proj[i, t] = expected AIME of individual i if they work
    through claiming age (current_age + t + 1), for t in 0..(70 - current_age).

    t=0 → claiming at current age (= current AIME, no projection needed)
    t=1 → work one more year then claim
    ...

    Shape: (N, max_t+1)  where max_t = 70 - pop["age"].values  (clipped ≥ 0)
    """
    beta_age    = float(wage_model["beta_age"])
    age_v       = pop["age"].values.astype(float)
    ped_v       = pop["ped"].values.astype(float)
    aime_v      = pop["aime"].values.astype(float)

    # Current fitted log-wage (population component without PED)
    # We need just the age-driven component for projection; other fixed effects
    # remain constant, so the change in projected log-wage over t years is
    # purely beta_age × t.
    N           = len(pop)
    max_horizon = int((70 - age_v).clip(min=0).max()) + 1

    aime_proj   = np.zeros((N, max_horizon), dtype=float)
    aime_proj[:, 0] = aime_v   # t=0: claim now at current AIME

    # Running state for the rolling 35-year average
    running_aime = aime_v.copy()
    running_n    = np.clip(age_v - 21.0, 1.0, 35.0)

    for t in range(1, max_horizon):
        future_age  = age_v + t
        future_year = current_year + t
        awi_future  = _awi(future_year)

        # Projected real wage: exp(base_log_wage + beta_age*t + PED)
        # The "+beta_age*t" captures the age-efficiency gain from staying employed
        # one more year.  PED is held fixed (zero expected drift).
        proj_log_wage = beta_age * t + ped_v   # incremental change only
        proj_real_wage = np.exp(proj_log_wage) * pop["incwage"].values.astype(float)
        # Convert to AWI-indexed monthly (real 2008 $)
        indexed_monthly = proj_real_wage * AWI_BASE / awi_future / 12.0

        # Workers still employed update AIME; for this projection we assume
        # everyone works until the horizon (that's the point: expected SSW
        # under continued employment).
        new_n           = np.minimum(running_n + 1.0, 35.0)
        running_aime    = (running_aime * (new_n - 1.0) + indexed_monthly) / new_n
        running_n       = new_n

        # Only fill in columns where this t is still ≤ 70 - current_age
        valid = (70 - age_v) >= t
        aime_proj[valid, t] = running_aime[valid]
        # For individuals already at/past 70, column stays 0 (masked out later)

    return aime_proj   # shape (N, max_horizon)


# ─────────────────────────────────────────────────────────────────────────────
# EXPECTED SSW AT EACH POSSIBLE CLAIMING AGE
# ─────────────────────────────────────────────────────────────────────────────

def compute_expected_ssw_matrix(pop: pd.DataFrame, wage_model: dict,
                                 current_year: int,
                                 annuity_table: dict) -> np.ndarray:
    """
    Returns ssw_matrix[i, t] = E[SSW] for individual i if they claim at
    age (current_age_i + t), for t in {0, 1, ..., 70 - current_age_i}.

    t=0  → claim today at current AIME (immediate SSW)
    t=1  → work one more year, claim at current_age+1
    ...
    t=k  → claim at min(current_age+k, 70)

    For t beyond (70 - current_age), the column is set to SSW at age 70
    (everyone forced to claim by then).

    Shape: (N, max_horizon)
    """
    age_v      = pop["age"].values.astype(int)
    sex_v      = pop["sex"].values.astype(int)
    N          = len(pop)
    max_horizon = int((70 - age_v.clip(max=70)).max()) + 1

    aime_proj  = _project_aime_matrix(pop, wage_model, current_year)
    birth_year = current_year - age_v

    ssw_matrix = np.zeros((N, max_horizon), dtype=float)

    for t in range(max_horizon):
        claim_age  = np.clip(age_v + t, 62, 70).astype(float)
        fra        = full_retirement_age(birth_year)
        adj        = claiming_adjustment(claim_age, fra)
        # Bend points: use year when individual reaches claiming age
        claim_year = current_year + t

        # AIME at this horizon
        aime_t = aime_proj[:, min(t, aime_proj.shape[1] - 1)]
        pia_t  = compute_pia_monthly(aime_t, min(claim_year, max(PIA_BEND)))

        annual_ben = pia_t * 12.0 * adj

        # Annuity factor at claiming age — no discounting yet from now to T
        # (the annuity table already discounts from claim_age onwards)
        ca_i  = claim_age.astype(int).clip(55, MAX_AGE - 1)
        af    = np.array([annuity_table.get((s, a), 0.0)
                          for s, a in zip(sex_v, ca_i)])

        # Discount PV back from claiming horizon to now
        disc_to_now = (1.0 + DISCOUNT_RATE) ** (-t)

        ssw_matrix[:, t] = annual_ben * af * disc_to_now

    return ssw_matrix   # (N, max_horizon); each column is E[SSW] at claiming age+t


# ─────────────────────────────────────────────────────────────────────────────
# INCENTIVE MEASURES  (scalars per individual)
# ─────────────────────────────────────────────────────────────────────────────

def compute_expected_ssw(pop: pd.DataFrame, wage_model: dict,
                          current_year: int, annuity_table: dict) -> pd.Series:
    """
    E[SSW] if claiming at current age (t=0 column of the matrix).
    This is the level of expected SS wealth under immediate retirement.
    """
    mat = compute_expected_ssw_matrix(pop, wage_model, current_year, annuity_table)
    return pd.Series(mat[:, 0], index=pop.index)


def compute_accrual(pop: pd.DataFrame, wage_model: dict,
                    current_year: int, annuity_table: dict) -> pd.Series:
    """
    One-year accrual = E[SSW(a+1)] − E[SSW(a)].
    Positive → working one more year raises expected SS wealth.
    """
    mat = compute_expected_ssw_matrix(pop, wage_model, current_year, annuity_table)
    age_v = pop["age"].values.astype(int)
    # t=1 gives SSW if working one more year and claiming then
    ssw_now  = mat[:, 0]
    ssw_next = np.where(70 - age_v >= 1, mat[:, 1], mat[:, 0])
    return pd.Series(ssw_next - ssw_now, index=pop.index)


def compute_peak_value(pop: pd.DataFrame, wage_model: dict,
                        current_year: int, annuity_table: dict) -> pd.Series:
    """
    Peak Value (Gruber & Wise 1998):
      PV_i = max_{T ≥ a} E[SSW(T)] − E[SSW(a)]

    Captures the full option value of delaying, not just one-year accrual.
    Negative → no future claiming age improves on immediate retirement.
    """
    mat     = compute_expected_ssw_matrix(pop, wage_model, current_year, annuity_table)
    ssw_now = mat[:, 0]
    peak    = mat.max(axis=1)
    return pd.Series(peak - ssw_now, index=pop.index)


# ─────────────────────────────────────────────────────────────────────────────
# HAZARD PREDICTION  (from R feglm coef dict)
# ─────────────────────────────────────────────────────────────────────────────

def predict_retirement_prob(pop: pd.DataFrame,
                             hazard_model: dict,
                             ssw: pd.Series,
                             accrual: pd.Series,
                             peak_value: pd.Series = None) -> np.ndarray:
    """
    Pr(retire this year) from R feglm logit coefficient dict.

    Linear predictor:
      η = β_ssw_log       × log(SSW)
        + β_accrual_rate  × (accrual / SSW)       [accrual as % of wealth]
        [+ β_peak_value_rate × (peak_value / SSW)  if supplied]
        + β_age           × age
        + β_age2          × age²
        + α_sex + α_educ + α_marst
    """
    ssw_vals     = np.maximum(ssw.values, 1.0)
    ssw_log      = np.log(ssw_vals)
    accrual_rate = accrual.values / ssw_vals
    age_v        = pop["age"].values.astype(float)

    score = (float(hazard_model["beta_ssw_log"])      * ssw_log
           + float(hazard_model["beta_accrual_rate"]) * accrual_rate
           + float(hazard_model["beta_age"])           * age_v
           + float(hazard_model.get("beta_age2", 0.0)) * age_v ** 2)

    if peak_value is not None and "beta_peak_value_rate" in hazard_model:
        pv_rate = peak_value.values / ssw_vals
        score  += float(hazard_model["beta_peak_value_rate"]) * pv_rate

    fe_sex   = {str(k): float(v) for k, v in hazard_model.get("fixef_sex",  {}).items()}
    fe_educ  = {str(k): float(v) for k, v in hazard_model.get("fixef_educ", {}).items()}
    fe_marst = {str(k): float(v) for k, v in hazard_model.get("fixef_marst",{}).items()}

    score += np.array([fe_sex  .get(str(int(s)), 0.0) for s in pop["sex"].values])
    score += np.array([fe_educ .get(str(e),      0.0) for e in pop["educ"].values])
    score += np.array([fe_marst.get(str(m),      0.0) for m in pop["marst"].values])

    return 1.0 / (1.0 + np.exp(-score))


# ─────────────────────────────────────────────────────────────────────────────
# APPLY RETIREMENT TRANSITIONS
# ─────────────────────────────────────────────────────────────────────────────

def apply_retirement_transitions(pop: pd.DataFrame,
                                  hazard_model: dict,
                                  wage_model: dict,
                                  year: int,
                                  lt: pd.DataFrame,
                                  rng) -> pd.DataFrame:
    """
    Draw stochastic retirement events for individuals aged 62–70.

    Eligible set: aged 62–70, not yet retired, not yet receiving SS.
    For each eligible individual:
      1. Build annuity table for this year's life table
      2. Compute expected SSW (immediate), accrual, and peak value
      3. Predict Pr(retire) from the logit hazard model
      4. Draw Bernoulli with that probability

    Age 70: force-retire everyone not yet retired (universal DRC cap).

    Returns updated pop with 'retired' and 'receive_ss' columns updated.
    """
    pop = pop.copy()

    # Build annuity table once for this year
    ann_tbl = build_annuity_table(lt, year)

    # ── Stochastic hazard for ages 62–69 ──────────────────────────────────
    eligible = (pop["age"].between(62, 69)
                & (pop["retired"]    == "no")
                & (pop["receive_ss"] == "no"))

    if eligible.sum() > 0:
        sub       = pop[eligible].copy()
        ssw       = compute_expected_ssw(sub, wage_model, year, ann_tbl)
        accrual   = compute_accrual(sub, wage_model, year, ann_tbl)
        peak_val  = compute_peak_value(sub, wage_model, year, ann_tbl)

        pr_ret    = predict_retirement_prob(sub, hazard_model,
                                            ssw, accrual, peak_val)
        retire    = rng.random(len(sub)) < pr_ret
        new_ret   = sub.index[retire]
        pop.loc[new_ret, "receive_ss"] = "yes"
        pop.loc[new_ret, "retired"]    = "yes"

    # ── Force-retire at 70 (DRC cap; near-universal claiming) ─────────────
    force = (pop["age"] >= 70) & (pop["retired"] == "no")
    pop.loc[force, "receive_ss"] = "yes"
    pop.loc[force, "retired"]    = "yes"

    return pop


# ─────────────────────────────────────────────────────────────────────────────
# AIME TRACKING HELPERS  (called from simulation.py)
# ─────────────────────────────────────────────────────────────────────────────

def init_aime(pop: pd.DataFrame) -> pd.Series:
    """
    Initialise AIME for the base-year ACS population.
    Workers: career-average ≈ 72 % of current wage for men, 62 % for women
             (calibrated to SSA-reported average AIME ratios at ages 50-65).
    Non-workers: imputed from same-sex worker median at their ±5-year age band.
    AIME is monthly real 2008 dollars.
    """
    aime = pd.Series(0.0, index=pop.index)

    career_factor = np.where(pop["sex"] == 1, 0.72, 0.62)
    worker_mask   = (pop["employed"] == "yes") & (pop["incwage"] > 0)
    aime[worker_mask] = (pop.loc[worker_mask, "incwage"].values
                         * career_factor[worker_mask] / 12.0)

    # Non-workers: median AIME of same-sex workers in ±5-year age band
    non_worker_idx = pop.index[~worker_mask]
    for idx in non_worker_idx:
        row    = pop.loc[idx]
        donors = aime[
            worker_mask &
            (pop["sex"] == row["sex"]) &
            pop["age"].between(row["age"] - 5, row["age"] + 5)
        ]
        aime.at[idx] = donors.median() if len(donors) > 0 else 0.0

    return aime


def update_aime(pop: pd.DataFrame, year: int) -> pd.Series:
    """
    Rolling 35-year average of AWI-indexed earnings (running update).

    AIME_{t} = ((n−1) × AIME_{t−1} + indexed_monthly_wage_t) / n
    where n = min(age − 21, 35).

    Indexed wage = incwage × AWI_2008 / AWI_year / 12  (real 2008 $, monthly).
    Non-workers contribute $0 for the current year.
    """
    awi_t  = _awi(year)
    n      = np.clip(pop["age"].values - 21.0, 1.0, 35.0)

    wage_indexed = np.where(
        pop["employed"] == "yes",
        pop["incwage"].values.astype(float) * AWI_BASE / awi_t / 12.0,
        0.0,
    )

    new_aime = (pop["aime"].values * (n - 1.0) + wage_indexed) / n
    return pd.Series(new_aime, index=pop.index)
