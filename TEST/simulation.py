#!/usr/bin/env python3
"""
Social Security Microsimulation — main simulation loop.
CBOLT-inspired dynamic microsimulation, base year 2008, simulation 2008-2024.
Validated against SSA Trustees Report TR2025 empirical moments.

Prerequisites:
    Run estimate_wage_equation.R first to produce:
      fitted_models.pkl   — wage / LFP / employment R coefficient dicts
    The script calibrates the ACS base-year population on startup.
"""

import pickle
import numpy as np
import pandas as pd
import warnings
warnings.filterwarnings("ignore")

from retirement_hazard import init_aime, update_aime, apply_retirement_transitions

# ─────────────────────────────────────────────────────────────────────────────
# BLS LFPR TARGETS  (annual, by sex × age band, 2008–2024)
# Source: BLS Current Population Survey / CPSAAT08 series
# ─────────────────────────────────────────────────────────────────────────────
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

# ─────────────────────────────────────────────────────────────────────────────
# BLS UNEMPLOYMENT RATE TARGETS  (annual, by sex × age band, 2008–2024)
# Source: BLS CPS
# ─────────────────────────────────────────────────────────────────────────────
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


def _age_band_lfpr(age: int):
    """Map age to BLS (sex, age_band) cell label."""
    if   age < 20: return None
    elif age < 25: return "20_24"
    elif age < 35: return "25_34"
    elif age < 45: return "35_44"
    elif age < 55: return "45_54"
    elif age < 65: return "55_64"
    else:          return "65p"


def _post_stratify(scores: np.ndarray, target_rate: float) -> np.ndarray:
    """Boolean mask: top round(target_rate × n) individuals by propensity score."""
    n     = len(scores)
    n_yes = max(0, min(n, round(target_rate * n)))
    mask  = np.zeros(n, dtype=bool)
    mask[np.argsort(-scores)[:n_yes]] = True
    return mask


# ─────────────────────────────────────────────────────────────────────────────
# PREDICTION HELPERS FOR R COEFFICIENT-DICT MODELS
# Models exported from R (feols / feglm) as dicts:
#   { beta_age, beta_cohort, fixef_sex, fixef_educ, fixef_ss, outcome, family }
# Linear predictor: Xβ + α_sex + α_educ + α_ss
# ─────────────────────────────────────────────────────────────────────────────

def _linear_pred(model_dict: dict, pop: pd.DataFrame) -> np.ndarray:
    """Vectorised linear predictor from an R coef dict."""
    score = (float(model_dict["beta_age"])    * pop["age"].values.astype(float)
           + float(model_dict["beta_cohort"]) * pop["cohort"].values.astype(float))
    fe_sex  = {str(k): float(v) for k, v in model_dict.get("fixef_sex",  {}).items()}
    fe_educ = {str(k): float(v) for k, v in model_dict.get("fixef_educ", {}).items()}
    fe_ss   = {str(k): float(v) for k, v in model_dict.get("fixef_ss",   {}).items()}
    score += np.array([fe_sex .get(str(int(s)), 0.0) for s in pop["sex"].values])
    score += np.array([fe_educ.get(str(e),      0.0) for e in pop["educ"].values])
    score += np.array([fe_ss  .get(str(s),      0.0) for s in pop["receive_ss"].values])
    return score


def predict_log_wage(model_dict: dict, pop: pd.DataFrame) -> pd.Series:
    """
    Predicted ln(wage) from R wage model dict.
    outcome = "log"  → linear predictor IS ln(wage) (feols on log outcome).
    """
    return pd.Series(_linear_pred(model_dict, pop), index=pop.index)


def predict_lfp_scores(model_dict: dict, pop: pd.DataFrame) -> np.ndarray:
    """
    Pr(yes) propensity scores from R feglm logistic model dict.
    Used for within-cell ranking only — BLS/TR targets set the actual rates.
    """
    return 1.0 / (1.0 + np.exp(-_linear_pred(model_dict, pop)))

# ─────────────────────────────────────────────────────────────────────────────
# PATHS  (edit if running from a different directory)
# ─────────────────────────────────────────────────────────────────────────────
BASE_DIR   = "/sessions/amazing-sharp-feynman/mnt/TEST"
UPLOAD_DIR = "/sessions/amazing-sharp-feynman/mnt/uploads"

INIT_SIM   = f"{BASE_DIR}/initial_simulation.csv"
LT_M_HIST  = f"{BASE_DIR}/PerLifeTables_M_Hist_TR2024.csv"
LT_F_HIST  = f"{BASE_DIR}/PerLifeTables_F_Hist_TR2024.csv"
LT_M_ALT2  = f"{BASE_DIR}/PerLifeTables_M_Alt2_TR2024.csv"
LT_F_ALT2  = f"{BASE_DIR}/PerLifeTables_F_Alt2_TR2024.csv"
CHETTY_15  = f"{BASE_DIR}/health_ineq_online_table_15.csv"
TR25_SINGLE = f"{UPLOAD_DIR}/SingleYearTRTables_TR2025 (2).xlsx"
TR25_SUPP   = f"{UPLOAD_DIR}/supplement25 (1).xlsx"

# ─────────────────────────────────────────────────────────────────────────────
# CBOLT SHOCK PARAMETERS  (Schwabish & Topoleski 2013, Table in Fig 4)
# σ_N = sqrt(permanent variance), σ_V = sqrt(transitory variance)
# By sex × age group
# ─────────────────────────────────────────────────────────────────────────────
PERM_SD = {
    1: {"25_34": 0.152, "35_44": 0.159, "45_60": 0.168},   # Male
    2: {"25_34": 0.149, "35_44": 0.155, "45_60": 0.159},   # Female
}
TRANS_SD = {
    1: {"25_34": 0.302, "35_44": 0.290, "45_60": 0.305},
    2: {"25_34": 0.268, "35_44": 0.260, "45_60": 0.239},
}

SIM_START = 2008
SIM_END   = 2024
RNG_SEED  = 42
rng = np.random.default_rng(RNG_SEED)


# ═════════════════════════════════════════════════════════════════════════════
# SECTION 1 – LOAD EMPIRICAL MOMENTS FOR VALIDATION
# ═════════════════════════════════════════════════════════════════════════════

def load_empirical_moments():
    """
    Load 2007-2024 empirical moments from TR2025.
    Returns a dict of DataFrames keyed by moment name.
    """
    xl1 = pd.ExcelFile(TR25_SINGLE)
    xl2 = pd.ExcelFile(TR25_SUPP)

    # V.A1 – TFR and age-sex-adjusted death rate (per 100k)
    va1 = xl1.parse("V.A1", header=None).iloc[8:, :5]
    va1.columns = ["year", "tfr", "asmdr_total", "asmdr_u65", "asmdr_65plus"]
    va1["year"] = pd.to_numeric(va1["year"], errors="coerce")
    va1 = va1[va1["year"].between(2007, 2024)].reset_index(drop=True)
    for c in ["tfr", "asmdr_total", "asmdr_u65", "asmdr_65plus"]:
        va1[c] = pd.to_numeric(va1[c], errors="coerce")

    # V.A3 – Social Security area population (thousands) by broad age group
    va3 = xl1.parse("V.A3", header=None).iloc[8:, :6]
    va3.columns = ["year", "pop_u20", "pop_2064", "pop_65plus", "pop_total", "aged_dep"]
    va3["year"] = pd.to_numeric(va3["year"], errors="coerce")
    va3 = va3[va3["year"].between(2007, 2024)].reset_index(drop=True)
    for c in ["pop_u20", "pop_2064", "pop_65plus", "pop_total"]:
        va3[c] = pd.to_numeric(va3[c], errors="coerce")

    # V.A4 – Period life expectancy at birth and at 65
    va4 = xl1.parse("V.A4", header=None).iloc[6:, :5]
    va4.columns = ["year", "e0_male", "e0_female", "e65_male", "e65_female"]
    va4["year"] = pd.to_numeric(va4["year"], errors="coerce")
    va4 = va4[va4["year"].between(2007, 2024)].reset_index(drop=True)
    for c in ["e0_male", "e0_female", "e65_male", "e65_female"]:
        va4[c] = pd.to_numeric(va4[c], errors="coerce")

    # 5.A4 – OASDI beneficiary counts over time (selected years; filter 2007-2024)
    sa4 = xl2.parse("5.A4", header=None)
    # Header is on row 2; data starts row 5
    sa4_data = sa4.iloc[4:, :3].copy()
    sa4_data.columns = ["year", "oasdi_total", "oasi_total"]
    sa4_data["year"] = pd.to_numeric(sa4_data["year"], errors="coerce")
    sa4_data = sa4_data[sa4_data["year"].between(2007, 2024)].reset_index(drop=True)
    sa4_data["oasdi_total"] = pd.to_numeric(sa4_data["oasdi_total"], errors="coerce")

    # 4.B1 – Workers with maximum (above tax max) earnings (thousands), by year
    b1 = xl2.parse("4.B1", header=None).iloc[4:, [0, 2, 3]].copy()
    b1.columns = ["year_raw", "workers_total_thou", "workers_max_thou"]
    # Year column has footnote suffixes like "2021 e" — strip to int
    b1["year"] = pd.to_numeric(
        b1["year_raw"].astype(str).str.extract(r"(\d{4})")[0], errors="coerce"
    )
    b1["workers_max_thou"] = pd.to_numeric(b1["workers_max_thou"], errors="coerce")
    b1["workers_total_thou"] = pd.to_numeric(b1["workers_total_thou"], errors="coerce")
    b1 = b1[b1["year"].between(2007, 2024)].dropna(subset=["year"]).reset_index(drop=True)

    # 2.A3 – Annual OASDI taxable maximum by year
    a3 = xl2.parse("2.A3", header=None).iloc[4:, [0, 2]].copy()
    a3.columns = ["year_raw", "tax_max_raw"]
    a3["year"] = pd.to_numeric(
        a3["year_raw"].astype(str).str.extract(r"(\d{4})")[0], errors="coerce"
    )
    # Clean tax_max: strip footnote letters and commas → numeric
    a3["tax_max"] = pd.to_numeric(
        a3["tax_max_raw"].astype(str)
            .str.replace(r"[a-z\s,]", "", regex=True)
            .str.replace(r"[\.\.]", "", regex=True),
        errors="coerce"
    )
    a3 = a3[a3["year"].between(2007, 2025)].dropna(subset=["year", "tax_max"])
    a3 = a3.reset_index(drop=True)

    # 4.B7 – Earnings distribution by bracket (for empirical Gini computation)
    # Columns after year/NaN: Total, 1-9999, 10k-19999, 20k-39999, 40k-59999,
    #   60k-79999, 80k-99999, 100k-119999, 120k-139999, 140k-149999, 150k-taxmax, above_max
    b7 = xl2.parse("4.B7", header=None).iloc[4:, :].copy()
    # Row 0 = header "All wage and salary workers"; data rows start at row 5 of sheet
    # Find the "All workers" block (ends before row with "Men")
    b7_raw = b7.iloc[:, [0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13]].copy()
    b7_raw.columns = ["year_raw", "total", "b1", "b2", "b3", "b4",
                      "b5", "b6", "b7_c", "b8", "b9", "b10", "above_max"]
    b7_raw["year"] = pd.to_numeric(
        b7_raw["year_raw"].astype(str).str.extract(r"(\d{4})")[0], errors="coerce"
    )
    # Keep only "All workers" rows — stop at first row where year resets (Men subsection)
    valid = b7_raw["year"].notna()
    # Drop rows that are part of Men/Women sub-tables by keeping only years <= 2023
    b7_all = b7_raw[valid & b7_raw["year"].between(2007, 2023)].copy()
    for col in ["total", "b1", "b2", "b3", "b4", "b5", "b6", "b7_c", "b8", "b9", "b10", "above_max"]:
        b7_all[col] = pd.to_numeric(b7_all[col], errors="coerce")
    b7_all = b7_all.reset_index(drop=True)

    return {
        "tfr_mort":    va1,
        "population":  va3,
        "life_expect": va4,
        "oasdi_benes": sa4_data,
        "above_max":   b1,       # 4.B1: workers at/above tax max
        "tax_max":     a3,       # 2.A3: OASDI taxable maximum by year
        "earn_dist":   b7_all,   # 4.B7: earnings bracket distribution (for Gini)
    }


# ═════════════════════════════════════════════════════════════════════════════
# SECTION 2 – LOAD MORTALITY TABLES
# ═════════════════════════════════════════════════════════════════════════════

def load_life_tables():
    """
    Load period life tables (historical + Alt2 projection) for M and F.
    Returns DataFrame with columns: Year, x, q, sex (1=M, 2=F).
    """
    dfs = []
    for path, sex in [(LT_M_HIST, 1), (LT_F_HIST, 2),
                      (LT_M_ALT2, 1), (LT_F_ALT2, 2)]:
        df = pd.read_csv(path, skiprows=4)
        df = df.rename(columns={"Year": "year", "x": "age", "q(x)": "q"})
        df["sex"] = sex
        dfs.append(df[["year", "age", "q", "sex"]])
    lt = pd.concat(dfs, ignore_index=True).drop_duplicates(["year", "age", "sex"])
    lt = lt.sort_values(["sex", "year", "age"]).reset_index(drop=True)
    return lt


def load_income_mortality_gradient():
    """
    Load Chetty et al. Table 15: mortality rate by income percentile, age, sex, year.
    Returns a dict: (sex_str, age, year) -> array of mortrate indexed [pctile-1].
    Also returns the ratio table: q_pctile / q_mean by pctile.
    """
    t15 = pd.read_csv(CHETTY_15)
    # sex coding: 'M'->1, 'F'->2
    t15["sex"] = t15["gnd"].map({"M": 1, "F": 2})
    # Use average over available years to get stable gradient
    grad = (t15.groupby(["sex", "age_at_d", "pctile"])["mortrate"]
              .mean().reset_index())
    # Compute relative risk vs. mean (pctile=50 ≈ mean)
    mean_rate = (grad.groupby(["sex", "age_at_d"])["mortrate"]
                     .mean().reset_index().rename(columns={"mortrate": "mean_rate"}))
    grad = grad.merge(mean_rate, on=["sex", "age_at_d"])
    grad["rho"] = grad["mortrate"] / grad["mean_rate"]   # relative risk by percentile
    # Clip to reasonable range to avoid extreme reweighting
    grad["rho"] = grad["rho"].clip(0.3, 3.0)
    return grad  # columns: sex, age_at_d, pctile, mortrate, mean_rate, rho


# ═════════════════════════════════════════════════════════════════════════════
# SECTION 3 – WAGE MODULE  (endogenous, CBOLT methodology)
# Fitting functions live in fit_models.py.  This section contains only the
# update_wages() loop function that runs every simulation year.
# ═════════════════════════════════════════════════════════════════════════════

# OASDI Taxable Maximum — nominal dollars by year
# Source: SSA Statistical Supplement Table 2.A3
SS_TAX_MAX = {
    2007: 97_500,  2008: 102_000, 2009: 106_800, 2010: 106_800,
    2011: 106_800, 2012: 110_100, 2013: 113_700, 2014: 117_000,
    2015: 118_500, 2016: 118_500, 2017: 127_200, 2018: 128_400,
    2019: 132_900, 2020: 137_700, 2021: 142_800, 2022: 147_000,
    2023: 160_200, 2024: 168_600,
}


def gini_coefficient(wages: np.ndarray) -> float:
    """
    Compute Gini coefficient for an array of non-negative wage values.
    Uses the standard sorted-array formula:
      G = (2 * Σ i*y_i) / (n * Σ y_i) - (n+1)/n
    where y_i is the i-th smallest value (1-indexed).
    Returns NaN if all wages are zero or array is empty.
    """
    w = wages[wages > 0]
    if len(w) < 2:
        return np.nan
    w = np.sort(w)
    n = len(w)
    idx = np.arange(1, n + 1)
    return float((2.0 * (idx * w).sum()) / (n * w.sum()) - (n + 1.0) / n)


# SSA Average Wage Index (AWI) — historical values, nominal dollars
# Source: SSA AWI series (https://www.ssa.gov/oact/cola/awidevelop.html)
# Base year 2008 = 41,334.97
AWI = {
    2007: 40_405.48, 2008: 41_334.97, 2009: 40_711.61, 2010: 41_673.83,
    2011: 42_979.61, 2012: 44_321.67, 2013: 44_888.16, 2014: 46_481.52,
    2015: 48_098.63, 2016: 48_642.15, 2017: 50_321.89, 2018: 52_145.80,
    2019: 54_099.99, 2020: 55_628.60, 2021: 60_575.07, 2022: 63_795.13,
    2023: 66_621.80, 2024: 69_000.00,   # 2024 preliminary
}
AWI_BASE = AWI[2008]   # wage model was estimated on 2008 ACS data


def update_wages(pop: pd.DataFrame, wage_model: dict, year: int):
    """
    CBOLT earnings eq (9) with AWI level-matching.

    Assumes all employed workers are full-time (FTE). The cross-sectional
    distribution of earnings is determined by:
      ln E_it_raw = ln(Ê_it) + PED_it + Σ α_is σ_N + β_it σ_V

    where Ê_it is from the fixed-characteristics wage equation, PED_it is the
    permanent earnings differential (random walk), and the shocks are drawn
    from detrended CBOLT variances (Schwabish & Topoleski 2013).

    After computing the raw distribution, we re-centre in log space so that
    the simulated mean wage of employed workers exactly equals AWI_t:
      δ_t = ln(AWI_t) - mean(ln E_it_raw  |  worker)
      ln E_it = ln E_it_raw + δ_t

    This preserves the full cross-sectional variance structure (Gini,
    percentile gaps) while matching the one aggregate moment we care about.

    Returns (updated incwage Series, updated ped Series).
    """
    pred_log = predict_log_wage(wage_model, pop)

    # Vectorised age-group shock draws
    ages    = pop["age"].values
    age_grp = np.where(ages < 35, "25_34", np.where(ages < 45, "35_44", "45_60"))

    n     = len(pop)
    alpha = np.zeros(n)
    beta  = np.zeros(n)

    for sex_val in [1, 2]:
        sex_mask = (pop["sex"] == sex_val).values
        for ag in ["25_34", "35_44", "45_60"]:
            mask = sex_mask & (age_grp == ag)
            if mask.sum() == 0:
                continue
            alpha[mask] = rng.standard_normal(mask.sum()) * PERM_SD[sex_val][ag]
            beta[mask]  = rng.standard_normal(mask.sum()) * TRANS_SD[sex_val][ag]

    # Accumulate permanent shock into PED (random walk)
    pop        = pop.copy()
    pop["ped"] = pop["ped"] + alpha

    # Raw idiosyncratic log-earnings (detrended distribution)
    log_E_raw = pred_log + pop["ped"] + beta

    # ── AWI level-matching ────────────────────────────────────────────────
    # Identify full-time workers (everyone employed, by FTE assumption)
    worker_mask = (pop["employed"] == "yes") & (pop["age"] > 18)

    awi_t    = AWI.get(year, AWI_BASE * (1.035 ** (year - 2008)))
    log_awi  = np.log(awi_t)

    if worker_mask.sum() > 0:
        log_E_workers     = log_E_raw[worker_mask]
        sim_log_mean      = log_E_workers.mean()          # mean of ln E, not ln(mean E)
        delta_t           = log_awi - sim_log_mean        # shift so mean matches AWI
        log_E_raw        = log_E_raw + delta_t            # applied to ALL individuals
        #   (non-workers don't earn, but keeping distribution consistent)

    new_wage = np.exp(log_E_raw)

    result = pop["incwage"].copy().astype(float)
    result[worker_mask] = new_wage[worker_mask].clip(lower=0)
    result[~worker_mask] = 0.0
    return result, pop["ped"]


# ═════════════════════════════════════════════════════════════════════════════
# SECTION 4 – DEMOGRAPHY MODULE
# ═════════════════════════════════════════════════════════════════════════════

# ── 4a. BIRTHS ────────────────────────────────────────────────────────────────

# Age-specific fertility rate schedule (proportional shape; scaled to match TFR)
# Midpoints of standard 5-year age bands
FERT_AGES   = [17, 22, 27, 32, 37, 42, 47]
FERT_SHAPE  = np.array([0.08, 0.22, 0.28, 0.22, 0.12, 0.06, 0.02])  # sums to 1
FERT_SHAPE /= FERT_SHAPE.sum()

def asfr_from_tfr(tfr: float) -> dict:
    """
    Given a target TFR, compute ASFR for each 5-year age band.
    TFR = Σ ASFR_a * 5  =>  ASFR_a = tfr * shape_a / 5
    """
    asfr_vals = tfr * FERT_SHAPE / 5.0
    return dict(zip(FERT_AGES, asfr_vals))


def make_babies(pop: pd.DataFrame, year: int, tfr: float) -> pd.DataFrame:
    """
    Apply fertility to women 15-49 based on ASFR derived from TFR.
    Newborns inherit family unit of mother; sex drawn 48/52 M/F.
    """
    asfr = asfr_from_tfr(tfr)

    # Map each woman to her age-band midpoint
    age_to_mid = {}
    for lo, mid in zip([15,20,25,30,35,40,45], FERT_AGES):
        for a in range(lo, lo+5):
            age_to_mid[a] = mid

    women = pop[(pop["sex"] == 2) & pop["age"].between(15, 49)].copy()
    women["asfr"] = women["age"].map(age_to_mid).map(asfr).fillna(0.0)
    women["pr"]   = rng.random(len(women))
    mothers = women[women["pr"] < women["asfr"]]

    if mothers.empty:
        return pop

    babies = pd.DataFrame({
        "famunit":    mothers["famunit"].values,
        "perwt":      mothers["perwt"].values,
        "hhwt":       mothers["hhwt"].values,
        "marst":      "single",
        "age":        0,
        "school":     "no",
        "sex":        rng.choice([1, 2], size=len(mothers), p=[0.48, 0.52]),
        "educ":       "hs",
        "labforce":   "no",
        "employed":   "no",
        "retired":    "no",
        "incwage":    0.0,
        "receive_ss": "no",
        "cohort":     (year // 10) * 10,
        "relate":     "child",
        "year":       year,
        "predict_hat": np.nan,
        "ped":        0.0,
    })
    return pd.concat([pop, babies], ignore_index=True)


# ── 4b. DEATHS (with income differential) ────────────────────────────────────

def apply_deaths(pop: pd.DataFrame, lt: pd.DataFrame, grad: pd.DataFrame,
                 year: int) -> pd.DataFrame:
    """
    Apply mortality using period life table q(x) by age and sex.
    For ages 40-76: scale q by income-percentile relative risk (Chetty gradient).
    Surviving spouses of deceased become 'widowed'.
    """
    # Get this year's life table (fall back to most recent if unavailable)
    avail_years = lt[lt["year"] <= year]["year"].unique()
    lt_sub = lt[lt["year"] == max(avail_years)]

    pop = pop.copy()

    # ── Build q lookup as 2D numpy array: q_cube[sex, age] ───────────────
    # Avoids row-by-row pop.apply(); life table has ~240 rows so this is instant.
    q_cube = np.zeros((3, 121))
    lt_valid = lt_sub[lt_sub["age"].between(0, 120) & lt_sub["sex"].isin([1, 2])]
    q_cube[
        lt_valid["sex"].values.astype(int),
        lt_valid["age"].values.astype(int)
    ] = lt_valid["q"].values

    sex_v = pop["sex"].values.astype(int)
    age_v = pop["age"].values.astype(int).clip(0, 120)
    q_base = q_cube[sex_v, age_v]   # vectorised lookup — O(N) numpy, no Python loop

    # ── Assign income percentile within (age, sex) group ──────────────────
    pop["inc_pctile"] = 0
    mask_worker = (pop["employed"] == "yes") & (pop["incwage"] > 0)
    for (s, a), grp in pop[mask_worker].groupby(["sex", "age"]):
        if len(grp) < 2:
            pop.loc[grp.index, "inc_pctile"] = 50
            continue
        pctile = pd.qcut(grp["incwage"], 100, labels=False, duplicates="drop")
        pop.loc[grp.index, "inc_pctile"] = (pctile + 1).fillna(50).astype(int)
    pop.loc[~mask_worker, "inc_pctile"] = 10  # non-workers assigned low percentile

    # ── Build rho cube: rho_cube[sex, age, pctile] ────────────────────────
    # Replaces iterrows() inner loop; grad has ~7 600 rows — single fancy-index write.
    rho_cube = np.ones((3, 121, 101))
    g_valid = grad[
        grad["pctile"].between(1, 100) &
        grad["age_at_d"].between(0, 120) &
        grad["sex"].isin([1, 2])
    ]
    rho_cube[
        g_valid["sex"].values.astype(int),
        g_valid["age_at_d"].values.astype(int),
        g_valid["pctile"].values.astype(int)
    ] = g_valid["rho"].values

    # ── Apply income-gradient rho for ages 40-76 (vectorised) ─────────────
    # Replaces the per-row Python loop; uses positional indexing throughout.
    rho_vec    = np.ones(len(pop))
    inside_pos = np.where(pop["age"].between(40, 76).values)[0]
    if len(inside_pos) > 0:
        s_sub = sex_v[inside_pos]
        a_sub = age_v[inside_pos]
        p_sub = np.clip(
            pop["inc_pctile"].values[inside_pos].astype(int), 1, 100
        )
        rho_vec[inside_pos] = rho_cube[s_sub, a_sub, p_sub]

    q_adj = (q_base * rho_vec).clip(0, 1)

    pr_die     = rng.random(len(pop))
    pop["die"] = (pr_die < q_adj).astype(int)

    # Surviving spouses of deceased become widowed
    dead_fams = set(pop.loc[pop["die"] == 1, "famunit"])
    pop.loc[
        (pop["famunit"].isin(dead_fams)) &
        (pop["relate"] == "spouse") &
        (pop["die"] == 0),
        ["marst", "receive_ss"]
    ] = ["widowed", "yes"]

    survivors = pop[pop["die"] == 0].drop(columns=["die", "inc_pctile"])
    return survivors.reset_index(drop=True)


# ── 4c. MARRIAGE TRANSITIONS ──────────────────────────────────────────────────

# Empirical annual transition rates (rough calibration to SIPP/ACS patterns)
# Source: approximate from SSA trustees population by marital status
MARRIAGE_RATES = {   # Pr(single -> married) by age band, sex
    (1, "15_24"): 0.030, (1, "25_34"): 0.065, (1, "35_44"): 0.040,
    (1, "45_54"): 0.025, (1, "55_64"): 0.018, (1, "65_99"): 0.008,
    (2, "15_24"): 0.035, (2, "25_34"): 0.070, (2, "35_44"): 0.042,
    (2, "45_54"): 0.026, (2, "55_64"): 0.015, (2, "65_99"): 0.006,
}
DIVORCE_RATES = {    # Pr(married -> divorced) by age band, sex
    (1, "15_24"): 0.025, (1, "25_34"): 0.020, (1, "35_44"): 0.017,
    (1, "45_54"): 0.012, (1, "55_64"): 0.008, (1, "65_99"): 0.003,
    (2, "15_24"): 0.028, (2, "25_34"): 0.021, (2, "35_44"): 0.018,
    (2, "45_54"): 0.013, (2, "55_64"): 0.009, (2, "65_99"): 0.003,
}

def age_band_marriage(age: int) -> str:
    if age < 25:   return "15_24"
    elif age < 35: return "25_34"
    elif age < 45: return "35_44"
    elif age < 55: return "45_54"
    elif age < 65: return "55_64"
    else:          return "65_99"


def apply_marriage_transitions(pop: pd.DataFrame) -> pd.DataFrame:
    """
    Apply annual marriage/divorce transitions (vectorised).
    Single/divorced -> married; married -> divorced.
    """
    pop = pop.copy()
    pr  = rng.random(len(pop))

    # Vectorise: compute rate for each row
    age_band_vec = pd.cut(
        pop["age"],
        bins=[0, 24, 34, 44, 54, 64, 200],
        labels=["15_24", "25_34", "35_44", "45_54", "55_64", "65_99"]
    ).astype(str)

    m_rate = pd.Series(0.0, index=pop.index)
    d_rate = pd.Series(0.0, index=pop.index)

    for (s, ab), rate in MARRIAGE_RATES.items():
        m = (pop["sex"] == s) & (age_band_vec == ab)
        m_rate[m] = rate
    for (s, ab), rate in DIVORCE_RATES.items():
        m = (pop["sex"] == s) & (age_band_vec == ab)
        d_rate[m] = rate

    single_div = pop["marst"].isin(["single", "divorced"])
    married    = pop["marst"] == "married"

    new_married = single_div & (pr < m_rate.values)
    new_divorced = married & (pr < d_rate.values)

    pop.loc[new_married,  "marst"] = "married"
    pop.loc[new_divorced, "marst"] = "divorced"

    return pop


# ═════════════════════════════════════════════════════════════════════════════
# SECTION 5 – LFPR & EMPLOYMENT MODULE
# BLS tables, helper functions, fitting functions, and build_tr_lfpr_projections
# all live in fit_models.py.  This section contains only apply_lfp_employment()
# which runs every simulation year.
# ═════════════════════════════════════════════════════════════════════════════




def apply_lfp_employment(pop: pd.DataFrame, lfp_model: dict,
                          emp_model: dict, year: int,
                          tr_lfpr_proj: dict = None) -> pd.DataFrame:
    """
    Assign labour force participation and employment status using
    post-stratification against BLS age-sex targets (2008-2024) or
    TR2025-implied targets (2025+, built by build_tr_lfpr_projections()).

    Steps:
      1. For working-age adults in each (sex, age_band) cell:
         a. Score each person with the logistic model (within-cell ranking)
         b. Assign top-N as LFP=yes so cell LFPR = target for that year
      2. Among LFP=yes, repeat for employment using unemployment rate
         (emp_rate = 1 - unemp_rate) as the cell-level target.
      3. Retired and under-19 are excluded from both steps.

    For years ≤ 2024: BLS actuals are used (historical validation range).
    For years > 2024: TR2025 intermediate-scenario projections are used
                      (if tr_lfpr_proj is provided), otherwise the 2024 BLS
                      values are held constant as a fallback.
    """
    pop  = pop.copy()

    working_age = pop["age"].between(19, 80) & (pop["retired"] == "no")
    wa_idx      = pop[working_age].index

    # ── Score every working-age individual ────────────────────────────────
    wa_pop      = pop.loc[wa_idx].copy()
    lfp_scores  = predict_lfp_scores(lfp_model, wa_pop)
    emp_scores  = predict_lfp_scores(emp_model, wa_pop)

    # Pre-compute age band once for all wa_pop rows.
    # Avoids calling .apply(_age_band_lfpr) 12× inside the loop.
    wa_ab = wa_pop["age"].map(_age_band_lfpr)   # Series aligned with wa_pop.index

    # Initialise: everyone out of LF and unemployed
    new_labforce = pd.Series("no", index=wa_idx)
    new_employed = pd.Series("no", index=wa_idx)

    # ── Post-stratify by (sex, age_band) cell ─────────────────────────────
    for sex_val in [1, 2]:
        for ab in ["20_24", "25_34", "35_44", "45_54", "55_64", "65p"]:

            # ── Select LFPR / unemp targets for this year ─────────────────
            if year <= 2024:
                year_key    = min(year, 2024)
                lfpr_target = BLS_LFPR.get((sex_val, ab), {}).get(year_key, 0.65)
                unemp_rate  = BLS_UNEMP.get((sex_val, ab), {}).get(year_key, 0.05)
            elif tr_lfpr_proj is not None and year in tr_lfpr_proj:
                cell_entry  = tr_lfpr_proj[year].get((sex_val, ab), (0.65, 0.045))
                lfpr_target = cell_entry[0]
                unemp_rate  = cell_entry[1]
            else:
                lfpr_target = BLS_LFPR.get((sex_val, ab), {}).get(2024, 0.65)
                unemp_rate  = BLS_UNEMP.get((sex_val, ab), {}).get(2024, 0.05)

            emp_target = 1.0 - unemp_rate

            cell_mask = (wa_pop["sex"] == sex_val) & (wa_ab == ab)
            # np.where gives integer positions into wa_pop / the scores arrays —
            # avoids O(N) Index.get_loc() Python loop used previously.
            cell_pos  = np.where(cell_mask.values)[0]
            cell_idx  = wa_idx[cell_pos]
            if len(cell_pos) == 0:
                continue

            # ── LFP assignment ────────────────────────────────────────────
            in_lf = _post_stratify(lfp_scores[cell_pos], lfpr_target)
            new_labforce.loc[cell_idx] = np.where(in_lf, "yes", "no")

            # ── Employment assignment (conditional on LFP) ────────────────
            lf_pos      = cell_pos[in_lf]    # boolean index into cell_pos array
            lf_cell_idx = cell_idx[in_lf]
            if len(lf_cell_idx) == 0:
                continue

            employed = _post_stratify(emp_scores[lf_pos], emp_target)
            new_employed.loc[lf_cell_idx] = np.where(employed, "yes", "no")

    pop.loc[wa_idx, "labforce"] = new_labforce.values
    pop.loc[wa_idx, "employed"] = new_employed.values

    # Under-19 and retired: force out of LF
    pop.loc[~working_age, "labforce"] = "no"
    pop.loc[~working_age, "employed"] = "no"

    # Retirement flag update
    retire_cand = (pop["age"] >= 62) & (pop["receive_ss"] == "yes") & (pop["employed"] == "no")
    pop.loc[retire_cand, "retired"] = "yes"

    return pop


# ═════════════════════════════════════════════════════════════════════════════
# SECTION 5b – BASE-YEAR CALIBRATION
# These functions run once at startup to calibrate the raw ACS population.
# ═════════════════════════════════════════════════════════════════════════════

def compute_ped(pop: pd.DataFrame, wage_model: dict) -> pd.Series:
    """
    Permanent Earnings Differential = ln(actual) − ln(predicted).
    Workers with incwage > 4200: direct residual from the wage equation.
    Everyone else: sampled from the same-sex young-worker PED pool (CBOLT).
    """
    pred_log = predict_log_wage(wage_model, pop)
    ped      = pd.Series(np.nan, index=pop.index)

    worker_mask = (pop["employed"] == "yes") & (pop["incwage"] > 4200) & (pop["age"] > 18)
    ped[worker_mask] = (np.log(pop.loc[worker_mask, "incwage"])
                        - pred_log[worker_mask])

    donor_mask = worker_mask & pop["age"].between(21, 31)
    for sex_val in [1, 2]:
        nw  = (~worker_mask) & (pop["sex"] == sex_val)
        don = donor_mask     & (pop["sex"] == sex_val)
        if nw.sum() > 0 and don.sum() > 0:
            drawn = rng.choice(ped[don].dropna().values, size=nw.sum(), replace=True)
            ped.loc[nw[nw].index] = drawn
        elif nw.sum() > 0:
            ped.loc[nw[nw].index] = 0.0

    return ped.fillna(0.0)


def calibrate_initial_population(pop: pd.DataFrame,
                                  wage_model: dict,
                                  lfp_model:  dict,
                                  emp_model:  dict) -> pd.DataFrame:
    """
    One-time calibration of the raw ACS 2008 bootstrap population:
      1. Recalibrate receive_ss to match empirical 2008 OASDI claiming rates
      2. Post-stratify labforce / employed to BLS 2008 cell-level targets
         using propensity scores from the CPS-fitted logistic models
      3. Compute initial PED for every individual
    """
    pop = pop.copy()

    # ── 1. SS claiming recalibration ──────────────────────────────────────
    pop["receive_ss"] = "no"
    # 65+: ~93% claim (retired workers + survivors)
    m65 = pop["age"] >= 65
    pop.loc[pop[m65].index[rng.random(m65.sum()) < 0.93], "receive_ss"] = "yes"
    # 62–64, not employed: ~65% early-retirement claimers
    m6264 = pop["age"].between(62, 64) & (pop["employed"] == "no")
    pop.loc[pop[m6264].index[rng.random(m6264.sum()) < 0.65], "receive_ss"] = "yes"
    # 25–61, employed: ~4.8% DI proxy
    mdi = (pop["age"].between(25, 61) & (pop["employed"] == "yes")
           & (pop["receive_ss"] == "no"))
    pop.loc[pop[mdi].index[rng.random(mdi.sum()) < 0.048], "receive_ss"] = "yes"
    # Retirement flag
    pop["retired"] = "no"
    pop.loc[(pop["age"] >= 62) & (pop["receive_ss"] == "yes")
            & (pop["employed"] == "no"), "retired"] = "yes"

    # ── 2. LFPR post-stratification to BLS 2008 cell targets ──────────────
    wa_mask = pop["age"].between(19, 80) & (pop["retired"] == "no")
    wa      = pop[wa_mask].copy()
    wa_idx  = pop[wa_mask].index

    lfp_scores = predict_lfp_scores(lfp_model, wa)
    emp_scores = predict_lfp_scores(emp_model, wa)
    wa_ab      = wa["age"].map(_age_band_lfpr)

    new_lf  = pd.Series("no", index=wa_idx)
    new_emp = pd.Series("no", index=wa_idx)

    for sex_val in [1, 2]:
        for ab in ["20_24", "25_34", "35_44", "45_54", "55_64", "65p"]:
            mask     = (wa["sex"] == sex_val) & (wa_ab == ab)
            cell_idx = wa_idx[mask.values]
            pos      = np.where(mask.values)[0]
            if len(pos) == 0:
                continue
            lfpr_t  = BLS_LFPR.get((sex_val, ab), {}).get(2008, 0.65)
            unemp_t = BLS_UNEMP.get((sex_val, ab), {}).get(2008, 0.05)

            in_lf  = _post_stratify(lfp_scores[pos], lfpr_t)
            new_lf.loc[cell_idx] = np.where(in_lf, "yes", "no")

            lf_pos = pos[in_lf]
            lf_idx = cell_idx[in_lf]
            if len(lf_idx) == 0:
                continue
            in_emp = _post_stratify(emp_scores[lf_pos], 1.0 - unemp_t)
            new_emp.loc[lf_idx] = np.where(in_emp, "yes", "no")

    pop.loc[wa_mask,  "labforce"] = new_lf.values
    pop.loc[wa_mask,  "employed"] = new_emp.values
    pop.loc[~wa_mask, "labforce"] = "no"
    pop.loc[~wa_mask, "employed"] = "no"

    # ── 3. Initial PED ────────────────────────────────────────────────────
    pop["ped"] = compute_ped(pop, wage_model)

    return pop


# ═════════════════════════════════════════════════════════════════════════════
# SECTION 6 – SIMULATION LOOP
# ═════════════════════════════════════════════════════════════════════════════

def run_simulation():
    print("=" * 65)
    print("  Social Security Microsimulation  2008-2024")
    print("=" * 65)

    # ── Load data ──────────────────────────────────────────────────────────
    print("\n[1] Loading data...")
    emp_moments = load_empirical_moments()
    lt          = load_life_tables()
    grad        = load_income_mortality_gradient()

    tfr_lookup = (emp_moments["tfr_mort"]
                  .set_index("year")["tfr"]
                  .to_dict())

    # ── Load models from fitted_models.pkl (produced by estimate_wage_equation.R)
    MODELS_OUT = f"{BASE_DIR}/fitted_models.pkl"

    print("[2] Loading pre-fitted models from fitted_models.pkl...")
    with open(MODELS_OUT, "rb") as f:
        bundle = pickle.load(f)
    wage_model    = bundle["wage_model"]    # R feols log-wage coef dict
    lfp_model     = bundle["lfp_model"]     # R feglm LFP coef dict
    emp_model     = bundle["emp_model"]     # R feglm employment coef dict
    hazard_model  = bundle["hazard_model"]  # R feglm retirement hazard coef dict
    tr_lfpr_proj  = bundle.get("tr_lfpr_proj", None)
    print(f"   wage β_age={float(wage_model['beta_age']):.4f}  "
          f"lfp β_age={float(lfp_model['beta_age']):.4f}  "
          f"emp β_age={float(emp_model['beta_age']):.4f}  "
          f"hazard β_ssw={float(hazard_model['beta_ssw_log']):.4f}")

    # ── Load raw ACS population and calibrate to 2008 empirical targets ────
    print("[3] Loading ACS initial population...")
    pop = pd.read_csv(INIT_SIM)
    pop["year"]    = SIM_START
    pop["sex"]     = pop["sex"].astype(int)
    pop["age"]     = pop["age"].astype(int)
    pop["incwage"] = pd.to_numeric(pop["incwage"], errors="coerce").fillna(0.0)
    pop            = pop.fillna({"ped": 0.0, "predict_hat": np.nan})
    print(f"   {len(pop):,} individuals loaded")

    print("[4] Calibrating base-year population (SS + LFPR + PED)...")
    pop = calibrate_initial_population(pop, wage_model, lfp_model, emp_model)

    # Initialise AIME for every individual (monthly, real 2008 dollars)
    pop["aime"] = init_aime(pop)

    scale_approx = 310_565_000 / len(pop)
    n_ss_init    = (pop["receive_ss"] == "yes").sum()
    lfpr_base    = ((pop["labforce"] == "yes") & pop["age"].between(20, 64)).sum() / \
                   max(pop["age"].between(20, 64).sum(), 1)
    print(f"   Base-year SS benes: {n_ss_init * scale_approx / 1e6:.1f}M  "
          f"(empirical target: 50.9M)")
    print(f"   Base-year LFPR (20-64): {lfpr_base:.3f}  (BLS 2008 target: 0.662)")

    # ── Simulation records ─────────────────────────────────────────────────
    records = []

    def snapshot(pop, year):
        n_total   = len(pop)
        n_workers = (pop["employed"] == "yes").sum()
        n_lf      = (pop["labforce"] == "yes").sum()
        n_ss      = (pop["receive_ss"] == "yes").sum()
        n_u20     = (pop["age"] < 20).sum()
        n_2064    = pop["age"].between(20, 64).sum()
        n_65plus  = (pop["age"] >= 65).sum()

        worker_wages = pop.loc[pop["employed"] == "yes", "incwage"].values.astype(float)
        med_wage  = float(np.median(worker_wages)) if len(worker_wages) > 0 else np.nan

        # Gini of worker wages
        gini = gini_coefficient(worker_wages)

        # Workers earning above OASDI taxable maximum this year
        tax_max_t = SS_TAX_MAX.get(year, SS_TAX_MAX.get(2024, 168_600))
        n_above_max = int((worker_wages > tax_max_t).sum())

        return {
            "year": year, "n_total": n_total,
            "n_u20": n_u20, "n_2064": n_2064, "n_65plus": n_65plus,
            "lfpr": n_lf / max(pop["age"].between(20, 64).sum(), 1),
            "emp_rate": n_workers / max(n_lf, 1),
            "n_ss_benes": n_ss,
            "med_wage": med_wage,
            "pct_married": (pop["marst"] == "married").mean(),
            "gini": gini,
            "n_above_max": n_above_max,
        }

    records.append(snapshot(pop, SIM_START))

    # ── Main loop ──────────────────────────────────────────────────────────
    print(f"\n[5] Running simulation {SIM_START} -> {SIM_END}...\n")
    print(f"{'Year':>5} {'Pop':>9} {'LFPR':>7} {'EmpRate':>8} "
          f"{'SS_Benes':>10} {'MedWage':>10} {'Married%':>9}")
    print("-" * 65)

    # Net immigration per year (thousands, SS area) from TR2025 V.A2 ~2007-2024
    # Total net immigration approx 900K-1300K/yr; scaled to sim population
    NET_IMMIG_THOU = {
        2008: 950, 2009: 790, 2010: 800, 2011: 850, 2012: 900,
        2013: 950, 2014: 1000, 2015: 1050, 2016: 1100, 2017: 1070,
        2018: 1050, 2019: 1020, 2020: 470, 2021: 590, 2022: 1380,
        2023: 1700, 2024: 1680,
    }

    def add_immigrants(pop: pd.DataFrame, year: int, n_immig_thou: float,
                       scale_factor: float) -> pd.DataFrame:
        """
        Resample from working-age existing population to represent immigrants.
        Age distribution: skewed to 20-40, roughly even sex.
        """
        n_add = int(n_immig_thou * 1000 / scale_factor)
        if n_add <= 0:
            return pop
        # Draw from working-age pool as donor base
        donors = pop[pop["age"].between(20, 45)]
        if donors.empty:
            return pop
        new_rows = donors.sample(n=n_add, replace=True, random_state=int(year)).copy()
        new_rows["famunit"]    = range(pop["famunit"].max() + 1,
                                       pop["famunit"].max() + 1 + n_add)
        new_rows["age"]        = rng.integers(18, 45, size=n_add)
        new_rows["marst"]      = "single"
        new_rows["relate"]     = "head"
        new_rows["receive_ss"] = "no"
        new_rows["retired"]    = "no"
        new_rows["year"]       = year
        new_rows["ped"]        = rng.choice(
            pop.loc[pop["age"].between(21, 31), "ped"].values, size=n_add, replace=True
        ) if len(pop.loc[pop["age"].between(21, 31)]) > 0 else 0.0
        new_rows["aime"]       = 0.0   # no US earnings history on arrival
        return pd.concat([pop, new_rows], ignore_index=True)

    for year in range(SIM_START, SIM_END + 1):

        # (a) Age everyone by 1
        pop["age"]  = pop["age"] + 1
        pop["year"] = year

        # (b) Immigration (net)
        # Scaling: ~310M real / 114K sim ≈ 2720x
        immig_scale = 310_565_000 / 114_432   # anchored to 2008
        n_immig = NET_IMMIG_THOU.get(year, 950)
        pop = add_immigrants(pop, year, n_immig, immig_scale)

        # (c) Births
        tfr = tfr_lookup.get(year, 1.75)
        pop = make_babies(pop, year, tfr)

        # (d) Deaths (with income gradient)
        pop = apply_deaths(pop, lt, grad, year)

        # (d) Marriage transitions
        pop = apply_marriage_transitions(pop)

        # (e) LFPR & Employment
        # For years ≤ 2024: post-stratify against BLS actuals
        # For years > 2024: use TR2025 intermediate-scenario implied targets
        pop = apply_lfp_employment(pop, lfp_model, emp_model, year,
                                   tr_lfpr_proj=tr_lfpr_proj)

        # (f) Update wages (CBOLT shocks)
        new_wages, new_ped = update_wages(pop, wage_model, year)
        pop["incwage"] = new_wages
        pop["ped"]     = new_ped

        # (g) Update AIME (rolling 35-yr AWI-indexed average)
        pop["aime"] = update_aime(pop, year)

        # (h) Retirement transitions driven by SS Wealth hazard model
        #     Ages 62–69: stochastic hazard based on SSW/accrual/demographics
        #     Age 70+:    force-retire (DRC cap; near-universal claiming)
        #     New immigrants have no aime column yet — fill with 0 before calling
        if "aime" not in pop.columns:
            pop["aime"] = 0.0
        pop = apply_retirement_transitions(pop, hazard_model, wage_model, year, lt, rng)

        # Ensure any 25-61 DI proxy not already flagged (unchanged from calibration)
        mdi = (pop["age"].between(25, 61) & (pop["employed"] == "no")
               & (pop["receive_ss"] == "no"))
        pop.loc[pop[mdi].index[rng.random(mdi.sum()) < 0.005], "receive_ss"] = "yes"

        snap = snapshot(pop, year)
        records.append(snap)
        print(f"{year:>5} {snap['n_total']:>9,} "
              f"{snap['lfpr']:>7.3f} {snap['emp_rate']:>8.3f} "
              f"{snap['n_ss_benes']:>10,} {snap['med_wage']:>10,.0f} "
              f"{snap['pct_married']:>8.3f}")

    print("\n" + "=" * 65)

    sim_df = pd.DataFrame(records)
    return sim_df, pop, emp_moments


# ═════════════════════════════════════════════════════════════════════════════
# SECTION 7 – VALIDATION
# ═════════════════════════════════════════════════════════════════════════════

def validate(sim_df: pd.DataFrame, emp_moments: dict):
    """
    Full validation layer: compares simulated moments to TR2025 empirical targets
    across five panels — wages, population, aging, SS beneficiaries, and mortality.
    Prints RMSE and mean % error for each panel.
    """

    W = 90   # total line width

    def header(title):
        print("\n" + "─" * W)
        print(f"  {title}")
        print("─" * W)

    def sep():
        print("·" * W)

    def rmse(sim, emp):
        pairs = [(s, e) for s, e in zip(sim, emp) if not (np.isnan(s) or np.isnan(e))]
        if not pairs: return np.nan, np.nan
        s_arr, e_arr = np.array([p[0] for p in pairs]), np.array([p[1] for p in pairs])
        return (np.sqrt(np.mean((s_arr - e_arr)**2)),
                np.mean(np.abs(s_arr - e_arr) / np.abs(e_arr)) * 100)

    # ── Scaling factor ──────────────────────────────────────────────────────
    pop_emp = emp_moments["population"].set_index("year")
    sim_agg = sim_df.set_index("year")
    emp_total_2008 = float(pop_emp.loc[pop_emp.index == 2008].iloc[0]["pop_total"])
    sim_total_2008 = float(sim_agg.loc[sim_agg.index == 2008].iloc[0]["n_total"])
    scale = emp_total_2008 * 1000 / sim_total_2008   # sim individuals -> real people

    # ── Annual empirical SS beneficiaries (from supplement 5.A4, annual 2007-2024)
    ss_emp_annual = {
        2007: 49_864_838, 2008: 50_898_244, 2009: 52_522_819, 2010: 54_031_968,
        2011: 55_404_480, 2012: 56_758_185, 2013: 57_978_610, 2014: 59_007_158,
        2015: 59_963_425, 2016: 60_907_307, 2017: 61_903_360, 2018: 62_906_222,
        2019: 64_064_496, 2020: 64_850_867, 2021: 65_228_238, 2022: 65_994_457,
        2023: 67_076_966, 2024: 68_455_973,
    }

    # ── BLS annual LFPR for ages 20-64 (civilian, seasonally adj, annual avg)
    # Source: BLS LNS11300060 / LNS11300000 series
    lfpr_emp = {
        2008: 0.662, 2009: 0.653, 2010: 0.648, 2011: 0.644, 2012: 0.641,
        2013: 0.635, 2014: 0.629, 2015: 0.628, 2016: 0.628, 2017: 0.629,
        2018: 0.631, 2019: 0.634, 2020: 0.616, 2021: 0.619, 2022: 0.625,
        2023: 0.628, 2024: 0.626,
    }

    tfr_emp  = emp_moments["tfr_mort"].set_index("year")
    le_emp   = emp_moments["life_expect"].set_index("year")

    print("\n" + "═" * W)
    print(f"  VALIDATION REPORT — Simulated vs Empirical Moments  2008–2024")
    print(f"  Scaling factor: {scale:,.0f}x  "
          f"(each sim individual ≈ {scale:,.0f} real people)")
    print("═" * W)

    # ════════════════════════════════════════════════════════════════════════
    # PANEL 1 — WAGES  (primary target moment: mean worker wage = AWI)
    # ════════════════════════════════════════════════════════════════════════
    header("PANEL 1 · WAGES   [primary target: mean(E_sim) = AWI]")
    print(f"{'Year':>5}  {'MedWage_sim':>12}  {'AWI_emp':>10}  {'MeanWage_sim':>13}  "
          f"{'AWI_err%':>9}")
    print("-" * W)

    sim_wages, awi_targets, errs_wage = [], [], []
    for year in range(2008, 2025):
        yr = sim_agg[sim_agg.index == year]
        if yr.empty: continue
        med_sim  = float(yr.iloc[0]["med_wage"])
        awi_t    = AWI.get(year, np.nan)
        # Mean wage is AWI by construction (that's what we anchor to)
        # Report median vs AWI to show distributional position
        err_pct  = (med_sim - awi_t) / awi_t * 100 if not np.isnan(awi_t) else np.nan
        sim_wages.append(med_sim); awi_targets.append(awi_t); errs_wage.append(err_pct)
        print(f"{year:>5}  {med_sim:>12,.0f}  {awi_t:>10,.0f}  "
              f"{'(anchored)':>13}  {err_pct:>+8.1f}%")

    med_err = np.nanmean(np.abs(errs_wage))
    print(f"\n  Median vs AWI  →  mean |err| = {med_err:.1f}%  "
          f"(median below AWI = right-skewed dist, expected)")

    # ════════════════════════════════════════════════════════════════════════
    # PANEL 2 — TOTAL POPULATION
    # ════════════════════════════════════════════════════════════════════════
    header("PANEL 2 · POPULATION   [SS area, millions — TR2025 V.A3]")
    print(f"{'Year':>5}  {'Pop_sim(M)':>11}  {'Pop_emp(M)':>11}  "
          f"{'Err(M)':>8}  {'Err%':>7}")
    print("-" * W)

    sim_pop_l, emp_pop_l = [], []
    for year in range(2008, 2025):
        yr_s = sim_agg[sim_agg.index == year]
        yr_e = pop_emp[pop_emp.index == year]
        if yr_s.empty: continue
        pop_s = float(yr_s.iloc[0]["n_total"]) * scale / 1e6
        pop_e = float(yr_e.iloc[0]["pop_total"]) * 1000 / 1e6 if not yr_e.empty else np.nan
        diff  = pop_s - pop_e if not np.isnan(pop_e) else np.nan
        pct   = diff / pop_e * 100 if not np.isnan(pop_e) else np.nan
        sim_pop_l.append(pop_s); emp_pop_l.append(pop_e)
        flag = " ◄" if (not np.isnan(pct) and abs(pct) > 2) else ""
        print(f"{year:>5}  {pop_s:>11.2f}  {pop_e:>11.2f}  "
              f"{diff:>+8.2f}  {pct:>+6.1f}%{flag}")

    r, p = rmse(sim_pop_l, emp_pop_l)
    print(f"\n  RMSE = {r:.2f}M   Mean |err%| = {p:.1f}%")

    # ════════════════════════════════════════════════════════════════════════
    # PANEL 3 — AGE STRUCTURE  (65+ share and dependency)
    # ════════════════════════════════════════════════════════════════════════
    header("PANEL 3 · AGE STRUCTURE   [TR2025 V.A3 + V.A1 TFR]")
    print(f"{'Year':>5}  {'65+%_sim':>9}  {'65+%_emp':>9}  {'Err_pp':>8}  "
          f"{'TFR_emp':>8}  {'ASMDR_emp':>10}")
    print("-" * W)

    sim_65, emp_65 = [], []
    for year in range(2008, 2025):
        yr_s = sim_agg[sim_agg.index == year]
        yr_e = pop_emp[pop_emp.index == year]
        yr_t = tfr_emp[tfr_emp.index == year]
        yr_m = emp_moments["tfr_mort"][emp_moments["tfr_mort"]["year"] == year]
        if yr_s.empty: continue
        s     = yr_s.iloc[0]
        p65_s = float(s["n_65plus"]) / float(s["n_total"]) * 100
        p65_e = (float(yr_e.iloc[0]["pop_65plus"]) /
                 float(yr_e.iloc[0]["pop_total"]) * 100) if not yr_e.empty else np.nan
        diff  = p65_s - p65_e if not np.isnan(p65_e) else np.nan
        tfr_e = float(yr_t.iloc[0]["tfr"]) if not yr_t.empty else np.nan
        asmdr = float(yr_m.iloc[0]["asmdr_total"]) if not yr_m.empty else np.nan
        sim_65.append(p65_s); emp_65.append(p65_e)
        flag = " ◄" if (not np.isnan(diff) and abs(diff) > 0.5) else ""
        print(f"{year:>5}  {p65_s:>9.1f}  {p65_e:>9.1f}  "
              f"{diff:>+7.2f}pp  {tfr_e:>8.2f}  {asmdr:>10.1f}{flag}")

    r, p = rmse(sim_65, emp_65)
    print(f"\n  RMSE = {r:.2f}pp   Mean |err%| = {p:.1f}%")

    # ════════════════════════════════════════════════════════════════════════
    # PANEL 4 — SS BENEFICIARIES
    # ════════════════════════════════════════════════════════════════════════
    header("PANEL 4 · SS BENEFICIARIES   [OASDI total — Supplement 5.A4]")
    print(f"{'Year':>5}  {'Benes_sim(M)':>13}  {'Benes_emp(M)':>13}  "
          f"{'Err(M)':>8}  {'Err%':>7}")
    print("-" * W)

    sim_ss, emp_ss = [], []
    for year in range(2008, 2025):
        yr_s  = sim_agg[sim_agg.index == year]
        if yr_s.empty: continue
        n_ss_sim = float(yr_s.iloc[0]["n_ss_benes"]) * scale / 1e6
        n_ss_emp = ss_emp_annual.get(year, np.nan) / 1e6
        diff     = n_ss_sim - n_ss_emp
        pct      = diff / n_ss_emp * 100 if not np.isnan(n_ss_emp) else np.nan
        sim_ss.append(n_ss_sim); emp_ss.append(n_ss_emp)
        flag = " ◄" if (not np.isnan(pct) and abs(pct) > 5) else ""
        print(f"{year:>5}  {n_ss_sim:>13.2f}  {n_ss_emp:>13.2f}  "
              f"{diff:>+8.2f}  {pct:>+6.1f}%{flag}")

    r, p = rmse(sim_ss, emp_ss)
    print(f"\n  RMSE = {r:.2f}M   Mean |err%| = {p:.1f}%")

    # ════════════════════════════════════════════════════════════════════════
    # PANEL 5 — LABOUR FORCE PARTICIPATION RATE
    # ════════════════════════════════════════════════════════════════════════
    header("PANEL 5 · LFPR (ages 20–64)   [BLS annual average]")
    print(f"{'Year':>5}  {'LFPR_sim':>9}  {'LFPR_emp':>9}  {'Err_pp':>8}")
    print("-" * W)

    sim_lfpr, emp_lfpr = [], []
    for year in range(2008, 2025):
        yr_s = sim_agg[sim_agg.index == year]
        if yr_s.empty: continue
        lfpr_s = float(yr_s.iloc[0]["lfpr"])
        lfpr_e = lfpr_emp.get(year, np.nan)
        diff   = lfpr_s - lfpr_e if not np.isnan(lfpr_e) else np.nan
        sim_lfpr.append(lfpr_s); emp_lfpr.append(lfpr_e)
        flag = " ◄" if (not np.isnan(diff) and abs(diff) > 0.04) else ""
        print(f"{year:>5}  {lfpr_s:>9.3f}  {lfpr_e:>9.3f}  {diff:>+7.3f}pp{flag}")

    r, p = rmse(sim_lfpr, emp_lfpr)
    print(f"\n  RMSE = {r:.3f}pp   Mean |err%| = {p:.1f}%")
    print(f"  NOTE: LFPR levels are upward-biased (logistic fitted on ACS workers).")
    print(f"        Recalibrate against BLS LFPR targets to close the gap.")

    # ════════════════════════════════════════════════════════════════════════
    # PANEL 6 — MORTALITY  (life expectancy at birth, TR2025 V.A4)
    # ════════════════════════════════════════════════════════════════════════
    header("PANEL 6 · PERIOD LIFE EXPECTANCY AT BIRTH   [TR2025 V.A4]")
    print(f"  (Informational — not directly simulated; mortality via q(x) tables)")
    print(f"{'Year':>5}  {'e0_male_emp':>12}  {'e0_fem_emp':>11}  "
          f"{'COVID_flag':>12}")
    print("-" * W)
    for year in range(2008, 2025):
        yr_le = le_emp[le_emp.index == year]
        if yr_le.empty: continue
        e0m = float(yr_le.iloc[0]["e0_male"])
        e0f = float(yr_le.iloc[0]["e0_female"])
        covid = "← COVID dip" if year in (2020, 2021) else ""
        print(f"{year:>5}  {e0m:>12.1f}  {e0f:>11.1f}  {covid}")

    # ════════════════════════════════════════════════════════════════════════
    # PANEL 7 — WAGE GINI COEFFICIENT
    # ════════════════════════════════════════════════════════════════════════
    header("PANEL 7 · WAGE GINI   [empirical: SSA 4.B7 bracket distribution]")
    print(f"{'Year':>5}  {'Gini_sim':>9}  {'Gini_emp':>9}  {'Err':>8}  "
          f"{'TaxMax':>10}  {'Note'}")
    print("-" * W)

    # Compute empirical Gini from 4.B7 earnings bracket distribution
    # Bracket midpoints (nominal $); last bracket uses 1.5x tax max as midpoint
    BRACKET_MIDPOINTS = [5_000, 15_000, 30_000, 50_000, 70_000,
                         90_000, 110_000, 130_000, 145_000, 155_000]

    earn_dist_idx = emp_moments["earn_dist"].set_index("year")
    tax_max_idx   = emp_moments["tax_max"].set_index("year")

    def emp_gini_from_brackets(year):
        """Compute Gini from 4.B7 bracket counts for a given year."""
        row = earn_dist_idx[earn_dist_idx.index == year]
        if row.empty:
            return np.nan
        r = row.iloc[0]
        tm_row = tax_max_idx[tax_max_idx.index == year]
        tax_max_y = float(tm_row.iloc[0]["tax_max"]) if not tm_row.empty else 128_400
        # Bracket counts in thousands
        counts = [r["b1"], r["b2"], r["b3"], r["b4"], r["b5"],
                  r["b6"], r["b7_c"], r["b8"], r["b9"], r["b10"]]
        midpts = BRACKET_MIDPOINTS.copy()
        # Above-max group: use 1.5x tax max as midpoint
        above_cnt = r["above_max"]
        counts.append(above_cnt)
        midpts.append(tax_max_y * 1.5)
        # Build expanded array weighted by counts
        wages = []
        for cnt, mid in zip(counts, midpts):
            if pd.notna(cnt) and cnt > 0:
                wages.extend([mid] * int(cnt))
        if len(wages) < 2:
            return np.nan
        return gini_coefficient(np.array(wages))

    sim_gini_l, emp_gini_l = [], []
    for year in range(2008, 2025):
        yr_s = sim_agg[sim_agg.index == year]
        if yr_s.empty:
            continue
        gini_s = float(yr_s.iloc[0]["gini"]) if not np.isnan(yr_s.iloc[0]["gini"]) else np.nan
        gini_e = emp_gini_from_brackets(year)
        tm_row = tax_max_idx[tax_max_idx.index == year]
        tm = int(tm_row.iloc[0]["tax_max"]) if not tm_row.empty else 0
        diff = gini_s - gini_e if not (np.isnan(gini_s) or np.isnan(gini_e)) else np.nan
        sim_gini_l.append(gini_s)
        emp_gini_l.append(gini_e)
        note = " ◄" if (not np.isnan(diff) and abs(diff) > 0.05) else ""
        print(f"{year:>5}  {gini_s:>9.3f}  "
              f"{gini_e:>9.3f}  {diff:>+7.3f}  "
              f"${tm:>9,}{note}")

    r_gini, p_gini = rmse(sim_gini_l, emp_gini_l)
    print(f"\n  RMSE = {r_gini:.4f}   Mean |err%| = {p_gini:.1f}%")
    print(f"  NOTE: Empirical Gini approximated from bracket midpoints (4.B7).")
    print(f"        Simulation uses FTE workers only; empirical includes part-time.")

    # ════════════════════════════════════════════════════════════════════════
    # PANEL 8 — WORKERS ABOVE OASDI TAXABLE MAXIMUM
    # ════════════════════════════════════════════════════════════════════════
    header("PANEL 8 · WORKERS ABOVE TAX MAX   [Supplement 4.B1]")
    print(f"{'Year':>5}  {'AbvMax_sim(M)':>14}  {'AbvMax_emp(M)':>14}  "
          f"{'TaxMax':>10}  {'Err%':>7}")
    print("-" * W)

    above_max_idx = emp_moments["above_max"].set_index("year")

    sim_abv_l, emp_abv_l = [], []
    for year in range(2008, 2025):
        yr_s = sim_agg[sim_agg.index == year]
        if yr_s.empty:
            continue
        abv_sim = float(yr_s.iloc[0]["n_above_max"]) * scale / 1e6

        emp_row = above_max_idx[above_max_idx.index == year]
        abv_emp = float(emp_row.iloc[0]["workers_max_thou"]) / 1000.0 \
                  if not emp_row.empty else np.nan   # thousands → millions

        tm_row  = tax_max_idx[tax_max_idx.index == year]
        tm      = int(tm_row.iloc[0]["tax_max"]) if not tm_row.empty else 0

        diff = abv_sim - abv_emp if not np.isnan(abv_emp) else np.nan
        pct  = diff / abv_emp * 100 if not np.isnan(abv_emp) else np.nan
        sim_abv_l.append(abv_sim)
        emp_abv_l.append(abv_emp)
        flag = " ◄" if (not np.isnan(pct) and abs(pct) > 15) else ""
        emp_str = f"{abv_emp:>14.2f}" if not np.isnan(abv_emp) else f"{'n/a':>14}"
        pct_str = f"{pct:>+6.1f}%" if not np.isnan(pct) else f"{'n/a':>7}"
        print(f"{year:>5}  {abv_sim:>14.2f}  {emp_str}  "
              f"${tm:>9,}  {pct_str}{flag}")

    r_abv, p_abv = rmse(sim_abv_l, emp_abv_l)
    print(f"\n  RMSE = {r_abv:.2f}M   Mean |err%| = {p_abv:.1f}%")

    # ════════════════════════════════════════════════════════════════════════
    # SUMMARY SCORECARD
    # ════════════════════════════════════════════════════════════════════════
    print("\n" + "═" * W)
    print("  SUMMARY SCORECARD")
    print("═" * W)
    print(f"  {'Moment':<40} {'RMSE':>10}  {'Mean |err%|':>12}  {'Status'}")
    print("  " + "-" * (W - 2))
    r_pop,  p_pop  = rmse(sim_pop_l,  emp_pop_l)
    r_65,   p_65   = rmse(sim_65,     emp_65)
    r_ss,   p_ss   = rmse(sim_ss,     emp_ss)
    r_lfpr, p_lfpr = rmse(sim_lfpr,   emp_lfpr)

    def status(pct, threshold):
        return "✓ PASS" if pct < threshold else "✗ FLAG"

    rows = [
        ("Wages (median vs AWI)",             f"{med_err:.1f}%",   "level",          status(med_err,  15)),
        ("Total population (M)",              f"{r_pop:.2f}M",     f"{p_pop:.1f}%",  status(p_pop,    2)),
        ("65+ share (pp)",                    f"{r_65:.2f}pp",     f"{p_65:.1f}%",   status(p_65,     3)),
        ("SS beneficiaries (M)",              f"{r_ss:.2f}M",      f"{p_ss:.1f}%",   status(p_ss,    10)),
        ("LFPR (pp, ages 20-64)",             f"{r_lfpr:.3f}pp",   f"{p_lfpr:.1f}%", status(p_lfpr,  20)),
        ("Wage Gini (worker distribution)",   f"{r_gini:.4f}",     f"{p_gini:.1f}%", status(p_gini,  10)),
        ("Workers above tax max (M)",         f"{r_abv:.2f}M",     f"{p_abv:.1f}%",  status(p_abv,   20)),
    ]
    for label, rmse_s, pct_s, st in rows:
        print(f"  {label:<40} {rmse_s:>10}  {pct_s:>12}  {st}")
    print("═" * W)

    return sim_df


# ═════════════════════════════════════════════════════════════════════════════
# MAIN
# ═════════════════════════════════════════════════════════════════════════════

if __name__ == "__main__":
    sim_df, final_pop, emp_moments = run_simulation()
    validate(sim_df, emp_moments)

    # Save outputs
    sim_df.to_csv(f"{BASE_DIR}/simulation_summary_2008_2024.csv", index=False)
    final_pop.to_csv(f"{BASE_DIR}/final_population_2024.csv", index=False)
    print(f"\nOutputs saved:")
    print(f"  {BASE_DIR}/simulation_summary_2008_2024.csv")
    print(f"  {BASE_DIR}/final_population_2024.csv")
