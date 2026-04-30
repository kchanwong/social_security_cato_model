#!/usr/bin/env python3
"""
fit_models.py — One-time model fitting for the SS microsimulation.

Run this script once before running simulation.py.  It:
  1. Loads and calibrates the base-year (2008) ACS population
     - Recalibrates receive_ss to match empirical 2008 OASDI claiming rates
     - Post-stratifies labforce / employed to BLS 2008 LFPR / unemployment targets
     - Computes initial Permanent Earnings Differentials (PED)
  2. Fits three sklearn models on the calibrated population:
     - Wage model   : OLS  ln(incwage) ~ age + cohort + sex + educ + receive_ss
     - LFP model    : Logistic  Pr(labforce=yes) — used for within-cell ranking only
     - Employment   : Logistic  Pr(employed=yes | labforce=yes) — same purpose
  3. Builds TR2025 implied LFPR projections for years 2025–2100 (Option 2)
  4. Saves everything to:
       fitted_models.pkl          — dict with wage_model, lfp_model, emp_model,
                                    their feature column lists, and tr_lfpr_proj
       initial_pop_calibrated.csv — base-year population ready for the sim loop

Usage:
    python fit_models.py
"""

import pickle
import numpy as np
import pandas as pd
from sklearn.linear_model import LinearRegression, LogisticRegression
import warnings
warnings.filterwarnings("ignore")

# ─────────────────────────────────────────────────────────────────────────────
# PATHS
# ─────────────────────────────────────────────────────────────────────────────
BASE_DIR    = "/sessions/amazing-sharp-feynman/mnt/TEST"
UPLOAD_DIR  = "/sessions/amazing-sharp-feynman/mnt/uploads"

INIT_SIM    = f"{BASE_DIR}/initial_simulation.csv"
TR25_SINGLE = f"{UPLOAD_DIR}/SingleYearTRTables_TR2025 (2).xlsx"
TR25_SUPP   = f"{UPLOAD_DIR}/supplement25 (1).xlsx"

MODELS_OUT  = f"{BASE_DIR}/fitted_models.pkl"
POP_OUT     = f"{BASE_DIR}/initial_pop_calibrated.csv"

RNG_SEED = 42
rng = np.random.default_rng(RNG_SEED)

# ─────────────────────────────────────────────────────────────────────────────
# BLS LFPR / UNEMPLOYMENT TARGETS  (2008 base year only — used for calibration)
# ─────────────────────────────────────────────────────────────────────────────
BLS_LFPR_2008 = {
    (1, "20_24"): 0.730, (1, "25_34"): 0.913, (1, "35_44"): 0.912,
    (1, "45_54"): 0.879, (1, "55_64"): 0.696, (1, "65p"):   0.215,
    (2, "20_24"): 0.648, (2, "25_34"): 0.742, (2, "35_44"): 0.758,
    (2, "45_54"): 0.751, (2, "55_64"): 0.580, (2, "65p"):   0.135,
}
BLS_UNEMP_2008 = {
    (1, "20_24"): 0.113, (1, "25_34"): 0.055, (1, "35_44"): 0.042,
    (1, "45_54"): 0.037, (1, "55_64"): 0.033, (1, "65p"):   0.028,
    (2, "20_24"): 0.091, (2, "25_34"): 0.046, (2, "35_44"): 0.040,
    (2, "45_54"): 0.036, (2, "55_64"): 0.030, (2, "65p"):   0.025,
}

# Full BLS tables (all years) — imported by simulation.py from here
BLS_LFPR = {
    (1, "20_24"): {2008:0.730,2009:0.709,2010:0.706,2011:0.704,2012:0.706,
                   2013:0.698,2014:0.697,2015:0.706,2016:0.711,2017:0.712,
                   2018:0.716,2019:0.720,2020:0.690,2021:0.697,2022:0.710,
                   2023:0.716,2024:0.714},
    (1, "25_34"): {2008:0.913,2009:0.900,2010:0.896,2011:0.894,2012:0.894,
                   2013:0.890,2014:0.890,2015:0.893,2016:0.893,2017:0.893,
                   2018:0.896,2019:0.898,2020:0.872,2021:0.878,2022:0.890,
                   2023:0.894,2024:0.893},
    (1, "35_44"): {2008:0.912,2009:0.901,2010:0.896,2011:0.893,2012:0.893,
                   2013:0.890,2014:0.888,2015:0.889,2016:0.890,2017:0.891,
                   2018:0.893,2019:0.895,2020:0.869,2021:0.876,2022:0.891,
                   2023:0.895,2024:0.894},
    (1, "45_54"): {2008:0.879,2009:0.868,2010:0.862,2011:0.858,2012:0.856,
                   2013:0.851,2014:0.848,2015:0.848,2016:0.847,2017:0.847,
                   2018:0.849,2019:0.851,2020:0.824,2021:0.832,2022:0.845,
                   2023:0.849,2024:0.848},
    (1, "55_64"): {2008:0.696,2009:0.685,2010:0.680,2011:0.679,2012:0.678,
                   2013:0.675,2014:0.674,2015:0.676,2016:0.678,2017:0.679,
                   2018:0.682,2019:0.686,2020:0.657,2021:0.658,2022:0.673,
                   2023:0.679,2024:0.679},
    (1, "65p"):   {2008:0.215,2009:0.212,2010:0.215,2011:0.218,2012:0.221,
                   2013:0.224,2014:0.226,2015:0.228,2016:0.230,2017:0.233,
                   2018:0.237,2019:0.241,2020:0.222,2021:0.226,2022:0.236,
                   2023:0.241,2024:0.243},
    (2, "20_24"): {2008:0.648,2009:0.627,2010:0.621,2011:0.617,2012:0.617,
                   2013:0.612,2014:0.611,2015:0.616,2016:0.620,2017:0.621,
                   2018:0.626,2019:0.630,2020:0.595,2021:0.603,2022:0.618,
                   2023:0.625,2024:0.624},
    (2, "25_34"): {2008:0.742,2009:0.733,2010:0.731,2011:0.731,2012:0.733,
                   2013:0.731,2014:0.733,2015:0.737,2016:0.741,2017:0.745,
                   2018:0.752,2019:0.757,2020:0.726,2021:0.736,2022:0.757,
                   2023:0.764,2024:0.768},
    (2, "35_44"): {2008:0.758,2009:0.750,2010:0.748,2011:0.748,2012:0.749,
                   2013:0.748,2014:0.749,2015:0.753,2016:0.756,2017:0.758,
                   2018:0.763,2019:0.768,2020:0.737,2021:0.746,2022:0.764,
                   2023:0.771,2024:0.774},
    (2, "45_54"): {2008:0.751,2009:0.742,2010:0.740,2011:0.739,2012:0.739,
                   2013:0.737,2014:0.736,2015:0.738,2016:0.739,2017:0.740,
                   2018:0.742,2019:0.745,2020:0.715,2021:0.722,2022:0.737,
                   2023:0.742,2024:0.742},
    (2, "55_64"): {2008:0.580,2009:0.574,2010:0.572,2011:0.572,2012:0.573,
                   2013:0.572,2014:0.572,2015:0.576,2016:0.578,2017:0.581,
                   2018:0.585,2019:0.590,2020:0.563,2021:0.566,2022:0.578,
                   2023:0.584,2024:0.585},
    (2, "65p"):   {2008:0.135,2009:0.134,2010:0.136,2011:0.138,2012:0.141,
                   2013:0.143,2014:0.145,2015:0.148,2016:0.150,2017:0.153,
                   2018:0.157,2019:0.160,2020:0.147,2021:0.149,2022:0.156,
                   2023:0.160,2024:0.162},
}
BLS_UNEMP = {
    (1, "20_24"): {2008:0.113,2009:0.175,2010:0.175,2011:0.167,2012:0.155,
                   2013:0.145,2014:0.130,2015:0.114,2016:0.102,2017:0.093,
                   2018:0.079,2019:0.073,2020:0.130,2021:0.107,2022:0.076,
                   2023:0.068,2024:0.072},
    (1, "25_34"): {2008:0.055,2009:0.098,2010:0.101,2011:0.090,2012:0.079,
                   2013:0.070,2014:0.059,2015:0.051,2016:0.045,2017:0.041,
                   2018:0.034,2019:0.032,2020:0.083,2021:0.067,2022:0.034,
                   2023:0.033,2024:0.036},
    (1, "35_44"): {2008:0.042,2009:0.082,2010:0.084,2011:0.077,2012:0.067,
                   2013:0.059,2014:0.050,2015:0.042,2016:0.037,2017:0.033,
                   2018:0.028,2019:0.026,2020:0.072,2021:0.057,2022:0.027,
                   2023:0.026,2024:0.029},
    (1, "45_54"): {2008:0.037,2009:0.073,2010:0.077,2011:0.069,2012:0.060,
                   2013:0.053,2014:0.044,2015:0.037,2016:0.033,2017:0.030,
                   2018:0.025,2019:0.023,2020:0.066,2021:0.053,2022:0.024,
                   2023:0.024,2024:0.027},
    (1, "55_64"): {2008:0.033,2009:0.063,2010:0.072,2011:0.065,2012:0.057,
                   2013:0.051,2014:0.043,2015:0.036,2016:0.032,2017:0.029,
                   2018:0.025,2019:0.023,2020:0.063,2021:0.052,2022:0.025,
                   2023:0.025,2024:0.027},
    (1, "65p"):   {2008:0.028,2009:0.050,2010:0.057,2011:0.053,2012:0.047,
                   2013:0.043,2014:0.038,2015:0.033,2016:0.030,2017:0.028,
                   2018:0.025,2019:0.023,2020:0.059,2021:0.048,2022:0.025,
                   2023:0.026,2024:0.028},
    (2, "20_24"): {2008:0.091,2009:0.133,2010:0.138,2011:0.131,2012:0.121,
                   2013:0.112,2014:0.099,2015:0.087,2016:0.077,2017:0.071,
                   2018:0.060,2019:0.056,2020:0.110,2021:0.090,2022:0.060,
                   2023:0.055,2024:0.058},
    (2, "25_34"): {2008:0.046,2009:0.080,2010:0.083,2011:0.076,2012:0.067,
                   2013:0.059,2014:0.050,2015:0.043,2016:0.038,2017:0.035,
                   2018:0.030,2019:0.028,2020:0.076,2021:0.062,2022:0.030,
                   2023:0.029,2024:0.031},
    (2, "35_44"): {2008:0.040,2009:0.073,2010:0.075,2011:0.068,2012:0.060,
                   2013:0.052,2014:0.044,2015:0.038,2016:0.034,2017:0.031,
                   2018:0.026,2019:0.024,2020:0.070,2021:0.055,2022:0.026,
                   2023:0.026,2024:0.028},
    (2, "45_54"): {2008:0.036,2009:0.067,2010:0.070,2011:0.063,2012:0.055,
                   2013:0.049,2014:0.042,2015:0.035,2016:0.031,2017:0.028,
                   2018:0.024,2019:0.022,2020:0.064,2021:0.051,2022:0.024,
                   2023:0.024,2024:0.026},
    (2, "55_64"): {2008:0.030,2009:0.058,2010:0.064,2011:0.058,2012:0.051,
                   2013:0.046,2014:0.040,2015:0.034,2016:0.030,2017:0.027,
                   2018:0.023,2019:0.021,2020:0.059,2021:0.048,2022:0.023,
                   2023:0.023,2024:0.025},
    (2, "65p"):   {2008:0.025,2009:0.046,2010:0.051,2011:0.047,2012:0.041,
                   2013:0.038,2014:0.033,2015:0.029,2016:0.027,2017:0.025,
                   2018:0.022,2019:0.020,2020:0.055,2021:0.045,2022:0.022,
                   2023:0.022,2024:0.024},
}


# ─────────────────────────────────────────────────────────────────────────────
# HELPERS
# ─────────────────────────────────────────────────────────────────────────────

def _age_band_lfpr(age: int) -> str:
    if age < 20:   return None
    elif age < 25: return "20_24"
    elif age < 35: return "25_34"
    elif age < 45: return "35_44"
    elif age < 55: return "45_54"
    elif age < 65: return "55_64"
    else:          return "65p"


def _post_stratify(scores: np.ndarray, target_rate: float) -> np.ndarray:
    """Return boolean mask: top round(target_rate * n) by propensity score."""
    n     = len(scores)
    n_yes = max(0, min(n, round(target_rate * n)))
    mask  = np.zeros(n, dtype=bool)
    mask[np.argsort(-scores)[:n_yes]] = True
    return mask


def _safe_scalar(df: pd.DataFrame, year_val: int, col: str, fallback=None):
    rows = df[df.index == year_val]
    if rows.empty:
        return fallback
    try:
        return float(rows.iloc[0][col])
    except (TypeError, ValueError):
        return fallback


# ─────────────────────────────────────────────────────────────────────────────
# SECTION 1 — WAGE MODEL
# ─────────────────────────────────────────────────────────────────────────────

def fit_wage_model(pop: pd.DataFrame):
    """
    OLS log-wage on workers with incwage > 4200.
      ln(incwage) ~ age + cohort + sex_f + educ_sc + educ_ba + ss_yes
    Returns (fitted LinearRegression, feature_cols list).
    """
    workers = pop[
        (pop["employed"] == "yes") &
        (pop["incwage"]  > 4200) &
        (pop["age"]      > 18)
    ].copy()
    workers["log_wage"] = np.log(workers["incwage"])
    workers["sex_f"]    = (workers["sex"]        == 2).astype(int)
    workers["educ_sc"]  = (workers["educ"]       == "some_college").astype(int)
    workers["educ_ba"]  = (workers["educ"]       == "ba_plus").astype(int)
    workers["ss_yes"]   = (workers["receive_ss"] == "yes").astype(int)

    feat_cols = ["age", "cohort", "sex_f", "educ_sc", "educ_ba", "ss_yes"]
    X = workers[feat_cols].values
    y = workers["log_wage"].values
    model = LinearRegression().fit(X, y)
    print(f"  Wage model R²={model.score(X, y):.3f}  "
          f"intercept={model.intercept_:.3f}  n={len(workers):,}")
    return model, feat_cols


def predict_log_wage(model, feat_cols: list, pop: pd.DataFrame) -> pd.Series:
    """Apply fitted wage model to any population DataFrame."""
    X = pd.DataFrame({
        "age":     pop["age"],
        "cohort":  pop["cohort"],
        "sex_f":   (pop["sex"]        == 2).astype(int),
        "educ_sc": (pop["educ"]       == "some_college").astype(int),
        "educ_ba": (pop["educ"]       == "ba_plus").astype(int),
        "ss_yes":  (pop["receive_ss"] == "yes").astype(int),
    })
    return pd.Series(model.predict(X[feat_cols].values), index=pop.index)


def compute_ped(pop: pd.DataFrame, wage_model, wage_feat_cols: list) -> pd.Series:
    """
    Permanent Earnings Differential = ln(actual) − ln(predicted).
    Workers with incwage > 4200: direct residual.
    Everyone else: sampled from same-sex young-worker PED pool (CBOLT approach).
    """
    pred_log = predict_log_wage(wage_model, wage_feat_cols, pop)
    ped = pd.Series(np.nan, index=pop.index)

    worker_mask = (pop["employed"] == "yes") & (pop["incwage"] > 4200) & (pop["age"] > 18)
    ped[worker_mask] = np.log(pop.loc[worker_mask, "incwage"]) - pred_log[worker_mask]

    donor_mask = worker_mask & pop["age"].between(21, 31)
    for sex_val in [1, 2]:
        nw   = (~worker_mask) & (pop["sex"] == sex_val)
        don  = donor_mask     & (pop["sex"] == sex_val)
        if nw.sum() > 0 and don.sum() > 0:
            drawn = rng.choice(ped[don].dropna().values, size=nw.sum(), replace=True)
            ped.loc[nw[nw].index] = drawn
        elif nw.sum() > 0:
            ped.loc[nw[nw].index] = 0.0

    return ped.fillna(0.0)


# ─────────────────────────────────────────────────────────────────────────────
# SECTION 2 — LFP & EMPLOYMENT MODELS
# ─────────────────────────────────────────────────────────────────────────────

def _build_lfp_features(pop: pd.DataFrame, ref_cols: list = None) -> pd.DataFrame:
    """Build feature matrix for LFP / employment logistic models."""
    dummies = pd.get_dummies(pop[["sex", "educ", "marst"]], drop_first=True)
    X = pd.concat(
        [pop[["age", "cohort"]].reset_index(drop=True),
         dummies.reset_index(drop=True)],
        axis=1
    ).fillna(0)
    if ref_cols is not None:
        for col in ref_cols:
            if col not in X.columns:
                X[col] = 0
        X = X[ref_cols]
    return X


def fit_lfp_model(pop: pd.DataFrame):
    """
    Logistic regression for within-cell LFP propensity ranking.
    The model is NOT used for level predictions — only to rank individuals
    within each (sex, age_band) cell. BLS targets set the cell-level rates.
    Returns (fitted LogisticRegression, feature_cols list).
    """
    df = pop[
        pop["age"].between(18, 75) &
        pop["labforce"].isin(["yes", "no"])
    ].copy()
    df["lfp"] = (df["labforce"] == "yes").astype(int)
    X = _build_lfp_features(df)
    y = df["lfp"].values
    model = LogisticRegression(max_iter=500, solver="lbfgs").fit(X, y)
    print(f"  LFP model  accuracy={model.score(X, y):.3f}  n={len(df):,}")
    return model, X.columns.tolist()


def fit_employment_model(pop: pd.DataFrame):
    """
    Logistic regression for within-cell employment propensity ranking
    (conditional on being in the labour force).
    Returns (fitted LogisticRegression, feature_cols list).
    """
    df = pop[
        (pop["labforce"] == "yes") &
        pop["age"].between(18, 75) &
        pop["employed"].isin(["yes", "no"])
    ].copy()
    df["emp"] = (df["employed"] == "yes").astype(int)
    X = _build_lfp_features(df)
    y = df["emp"].values
    model = LogisticRegression(max_iter=500, solver="lbfgs").fit(X, y)
    print(f"  Emp model  accuracy={model.score(X, y):.3f}  n={len(df):,}")
    return model, X.columns.tolist()


def predict_lfp_scores(model, feat_cols: list, pop: pd.DataFrame) -> np.ndarray:
    """Return Pr(yes) propensity scores for ranking — not level prediction."""
    X = _build_lfp_features(pop, ref_cols=feat_cols)
    return model.predict_proba(X.values)[:, 1]


# ─────────────────────────────────────────────────────────────────────────────
# SECTION 3 — TR2025 LFPR PROJECTIONS (2025–2100)
# ─────────────────────────────────────────────────────────────────────────────

def build_tr_lfpr_projections() -> dict:
    """
    Derive implied LFPR targets for projection years (2025+) from TR2025
    intermediate assumptions (V.B2 labour-force growth + V.A3 population).
    Returns {year: {(sex, ab): (lfpr_target, unemp_rate)}} for 2025–2100.
    """
    xl1 = pd.ExcelFile(TR25_SINGLE)

    # V.B2 — keep first occurrence of each year (= intermediate scenario)
    vb2 = xl1.parse("V.B2", header=None).iloc[8:, :3].copy()
    vb2.columns = ["year", "unemp_rate", "lf_growth_pct"]
    vb2["year"]          = pd.to_numeric(vb2["year"],          errors="coerce")
    vb2["unemp_rate"]    = pd.to_numeric(vb2["unemp_rate"],    errors="coerce")
    vb2["lf_growth_pct"] = pd.to_numeric(vb2["lf_growth_pct"], errors="coerce")
    vb2 = (vb2.dropna(subset=["year"])
              .drop_duplicates(subset=["year"], keep="first")
              [lambda d: d["year"].between(2024, 2100)]
              .set_index("year"))

    # V.A3 — pop 20-64 (thousands)
    va3 = xl1.parse("V.A3", header=None).iloc[8:, :4].copy()
    va3.columns = ["year", "pop_u20", "pop_2064", "pop_65plus"]
    va3["year"]     = pd.to_numeric(va3["year"],     errors="coerce")
    va3["pop_2064"] = pd.to_numeric(va3["pop_2064"], errors="coerce")
    va3 = (va3.dropna(subset=["year"])
              .drop_duplicates(subset=["year"], keep="first")
              .set_index("year"))

    ab_list = ["20_24", "25_34", "35_44", "45_54", "55_64", "65p"]
    lfpr_2024 = {(s, ab): BLS_LFPR[(s, ab)][2024] for s in [1, 2] for ab in ab_list}
    unemp_2024 = {(s, ab): BLS_UNEMP[(s, ab)][2024] for s in [1, 2] for ab in ab_list}

    core = [(s, ab) for s in [1, 2] for ab in ["20_24", "25_34", "35_44", "45_54", "55_64"]]
    lfpr_agg_2024 = np.mean([lfpr_2024[c] for c in core])

    pop_2064_2024 = _safe_scalar(va3, 2024, "pop_2064", fallback=195_000.0)
    lf_t = lfpr_agg_2024 * pop_2064_2024

    proj = {}
    for year in range(2025, 2101):
        g = (_safe_scalar(vb2, year, "lf_growth_pct", None) or 0.3) / 100.0
        u = (_safe_scalar(vb2, year, "unemp_rate",    None) or 4.5) / 100.0
        lf_t *= (1.0 + g)
        pop_t = _safe_scalar(va3, year, "pop_2064",
                             fallback=pop_2064_2024 * (1.002 ** (year - 2024)))
        scale = np.clip(lf_t / pop_t, 0.40, 0.90) / lfpr_agg_2024
        proj[year] = {
            (s, ab): (
                float(np.clip(lfpr_2024[(s, ab)] * scale, 0.05, 0.99)),
                float(np.clip(unemp_2024[(s, ab)] * (u / 0.045), 0.01, 0.30)),
            )
            for s in [1, 2] for ab in ab_list
        }

    return proj


# ─────────────────────────────────────────────────────────────────────────────
# SECTION 4 — BASE-YEAR POPULATION CALIBRATION
# ─────────────────────────────────────────────────────────────────────────────

def calibrate_initial_population(pop: pd.DataFrame,
                                  wage_model, wage_feat_cols: list,
                                  lfp_model,  lfp_feat_cols: list,
                                  emp_model,  emp_feat_cols: list) -> pd.DataFrame:
    """
    Apply one-time calibration to the 2008 ACS bootstrap population:
      1. Recalibrate receive_ss to match empirical 2008 OASDI claiming rates
      2. Post-stratify labforce / employed to BLS 2008 cell targets
      3. Compute initial PED for all individuals
    """
    pop = pop.copy()

    # ── 1. SS claiming recalibration ─────────────────────────────────────
    pop["receive_ss"] = "no"
    # 65+: ~93% claim (retired + survivors)
    m65 = pop["age"] >= 65
    pop.loc[pop[m65].index[rng.random(m65.sum()) < 0.93], "receive_ss"] = "yes"
    # 62-64 non-employed: ~65% early retirees
    m6264 = pop["age"].between(62, 64) & (pop["employed"] == "no")
    pop.loc[pop[m6264].index[rng.random(m6264.sum()) < 0.65], "receive_ss"] = "yes"
    # 25-61 workers: ~4.8% DI proxy
    mdi = pop["age"].between(25, 61) & (pop["employed"] == "yes") & (pop["receive_ss"] == "no")
    pop.loc[pop[mdi].index[rng.random(mdi.sum()) < 0.048], "receive_ss"] = "yes"
    # Retirement flag
    pop["retired"] = "no"
    pop.loc[(pop["age"] >= 62) & (pop["receive_ss"] == "yes") &
            (pop["employed"] == "no"), "retired"] = "yes"

    # ── 2. LFPR post-stratification to BLS 2008 targets ──────────────────
    wa_mask = pop["age"].between(19, 80) & (pop["retired"] == "no")
    wa      = pop[wa_mask].copy()

    lfp_scores = predict_lfp_scores(lfp_model, lfp_feat_cols, wa)
    emp_scores = predict_lfp_scores(emp_model, emp_feat_cols, wa)

    # Pre-compute age bands once
    wa_ab = wa["age"].map(_age_band_lfpr)

    new_lf  = pd.Series("no", index=wa.index)
    new_emp = pd.Series("no", index=wa.index)

    for sex_val in [1, 2]:
        for ab in ["20_24", "25_34", "35_44", "45_54", "55_64", "65p"]:
            mask     = (wa["sex"] == sex_val) & (wa_ab == ab)
            cell_idx = wa.index[mask]
            if len(cell_idx) == 0:
                continue
            pos      = np.where(mask.values)[0]
            lfpr_t   = BLS_LFPR_2008.get((sex_val, ab), 0.65)
            unemp_t  = BLS_UNEMP_2008.get((sex_val, ab), 0.05)

            in_lf    = _post_stratify(lfp_scores[pos], lfpr_t)
            new_lf.loc[cell_idx] = np.where(in_lf, "yes", "no")

            lf_pos   = pos[in_lf]
            lf_idx   = cell_idx[in_lf]
            if len(lf_idx) == 0:
                continue
            in_emp   = _post_stratify(emp_scores[lf_pos], 1.0 - unemp_t)
            new_emp.loc[lf_idx] = np.where(in_emp, "yes", "no")

    pop.loc[wa_mask, "labforce"] = new_lf.values
    pop.loc[wa_mask, "employed"] = new_emp.values
    pop.loc[~wa_mask, "labforce"] = "no"
    pop.loc[~wa_mask, "employed"] = "no"

    # ── 3. Initial PED ────────────────────────────────────────────────────
    pop["ped"] = compute_ped(pop, wage_model, wage_feat_cols)

    return pop


# ─────────────────────────────────────────────────────────────────────────────
# MAIN
# ─────────────────────────────────────────────────────────────────────────────

if __name__ == "__main__":
    print("=" * 60)
    print("  fit_models.py — one-time model fitting")
    print("=" * 60)

    # Load raw initial population
    print("\n[1] Loading initial population...")
    pop = pd.read_csv(INIT_SIM)
    pop = pop.fillna({"ped": 0.0, "predict_hat": float("nan")})
    pop["sex"]     = pop["sex"].astype(int)
    pop["age"]     = pop["age"].astype(int)
    pop["incwage"] = pd.to_numeric(pop["incwage"], errors="coerce").fillna(0.0)
    pop["year"]    = 2008
    print(f"   {len(pop):,} individuals loaded")

    # Fit wage model (needs raw incwage before LFPR recalibration)
    print("\n[2] Fitting wage model...")
    wage_model, wage_feat_cols = fit_wage_model(pop)

    # Fit LFP / employment models (use raw labforce/employed from ACS)
    print("\n[3] Fitting LFP model...")
    lfp_model, lfp_feat_cols = fit_lfp_model(pop)

    print("\n[4] Fitting employment model...")
    emp_model, emp_feat_cols = fit_employment_model(pop)

    # Build TR2025 LFPR projections
    print("\n[5] Building TR2025 LFPR projections (2025–2100)...")
    tr_lfpr_proj = build_tr_lfpr_projections()
    print(f"   Built for years {min(tr_lfpr_proj)}–{max(tr_lfpr_proj)}")

    # Calibrate initial population
    print("\n[6] Calibrating base-year population...")
    pop_cal = calibrate_initial_population(
        pop,
        wage_model, wage_feat_cols,
        lfp_model,  lfp_feat_cols,
        emp_model,  emp_feat_cols,
    )
    scale = 310_565_000 / len(pop_cal)
    n_ss  = (pop_cal["receive_ss"] == "yes").sum()
    lfpr  = ((pop_cal["labforce"] == "yes") & pop_cal["age"].between(20, 64)).sum() / \
            max(pop_cal["age"].between(20, 64).sum(), 1)
    print(f"   SS benes  : {n_ss * scale / 1e6:.1f}M  (target ~50.9M)")
    print(f"   LFPR 20-64: {lfpr:.3f}          (BLS 2008: 0.662)")

    # Save
    print("\n[7] Saving outputs...")
    bundle = {
        "wage_model":     wage_model,
        "wage_feat_cols": wage_feat_cols,
        "lfp_model":      lfp_model,
        "lfp_feat_cols":  lfp_feat_cols,
        "emp_model":      emp_model,
        "emp_feat_cols":  emp_feat_cols,
        "tr_lfpr_proj":   tr_lfpr_proj,
    }
    with open(MODELS_OUT, "wb") as f:
        pickle.dump(bundle, f)
    pop_cal.to_csv(POP_OUT, index=False)

    print(f"   {MODELS_OUT}")
    print(f"   {POP_OUT}")
    print("\nDone. Run simulation.py next.")
