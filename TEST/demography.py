"""
demography.py — fertility and mortality functions extracted from simulation.py
"""
import numpy as np
import pandas as pd
# ─────────────────────────────────────────────────────────────────────────────
# PATHS  (edit if running from a different directory)
# ─────────────────────────────────────────────────────────────────────────────
BASE_DIR  = r"C:\Users\kchanwong\Documents\GitHub\social_security_cato_model\TEST"
LT_M_HIST = f"{BASE_DIR}/PerLifeTables_M_Hist_TR2024.csv"
LT_F_HIST = f"{BASE_DIR}/PerLifeTables_F_Hist_TR2024.csv"
LT_M_ALT2 = f"{BASE_DIR}/PerLifeTables_M_Alt2_TR2024.csv"
LT_F_ALT2 = f"{BASE_DIR}/PerLifeTables_F_Alt2_TR2024.csv"
CHETTY_15 = f"{BASE_DIR}/health_ineq_online_table_15.csv"
RNG_SEED = 42
rng = np.random.default_rng(RNG_SEED)
# ─────────────────────────────────────────────────────────────────────────────
# FERTILITY CONSTANTS
# ─────────────────────────────────────────────────────────────────────────────
FERT_AGES  = [17, 22, 27, 32, 37, 42, 47]
FERT_SHAPE = np.array([0.08, 0.22, 0.28, 0.22, 0.12, 0.06, 0.02])
FERT_SHAPE /= FERT_SHAPE.sum()
# ─────────────────────────────────────────────────────────────────────────────
# MORTALITY
# ─────────────────────────────────────────────────────────────────────────────

def load_life_tables() -> pd.DataFrame:
    """
    Load period life tables (historical + Alt2 projection) for M and F.
    Returns DataFrame with columns: year, age, q, sex (1=M, 2=F).
    """
    dfs = []
    for path, sex in [(LT_M_HIST, 1), (LT_F_HIST, 2),
                      (LT_M_ALT2, 1), (LT_F_ALT2, 2)]:
        df = pd.read_csv(path, skiprows=4)
        df = df.rename(columns={"Year": "year", "x": "age", "q(x)": "q"})
        df["sex"] = sex
        dfs.append(df[["year", "age", "q", "sex"]])
    lt = pd.concat(dfs, ignore_index=True).drop_duplicates(["year", "age", "sex"])
    return lt.sort_values(["sex", "year", "age"]).reset_index(drop=True)


def load_income_mortality_gradient() -> pd.DataFrame:
    """
    Load Chetty et al. Table 15: mortality relative risk by income percentile.
    Returns DataFrame with columns: sex, age_at_d, pctile, mortrate, mean_rate, rho.
    rho = relative risk vs. mean; clipped to [0.3, 3.0].
    """
    t15 = pd.read_csv(CHETTY_15)
    t15["sex"] = t15["gnd"].map({"M": 1, "F": 2})
    grad = (t15.groupby(["sex", "age_at_d", "pctile"])["mortrate"]
               .mean().reset_index())
    mean_rate = (grad.groupby(["sex", "age_at_d"])["mortrate"]
                     .mean().reset_index()
                     .rename(columns={"mortrate": "mean_rate"}))
    grad = grad.merge(mean_rate, on=["sex", "age_at_d"])
    grad["rho"] = (grad["mortrate"] / grad["mean_rate"]).clip(0.3, 3.0)
    return grad


def apply_deaths(pop: pd.DataFrame, lt: pd.DataFrame, grad: pd.DataFrame,
                 year: int) -> pd.DataFrame:
    """
    Apply mortality using period life table q(x) by age and sex.
    For ages 40-76: scale q by income-percentile relative risk (Chetty gradient).
    Surviving spouses of deceased become 'widowed'.
    """
    avail_years = lt[lt["year"] <= year]["year"].unique()
    lt_sub = lt[lt["year"] == max(avail_years)]

    pop = pop.copy()

    # Build q lookup: q_cube[sex, age]
    q_cube = np.zeros((3, 121))
    lt_valid = lt_sub[lt_sub["age"].between(0, 120) & lt_sub["sex"].isin([1, 2])]
    q_cube[
        lt_valid["sex"].values.astype(int),
        lt_valid["age"].values.astype(int)
    ] = lt_valid["q"].values

    sex_v  = pop["sex"].values.astype(int)
    age_v  = pop["age"].values.astype(int).clip(0, 120)
    q_base = q_cube[sex_v, age_v]

    # Assign income percentile within (age, sex) group
    pop["inc_pctile"] = 0
    mask_worker = (pop["employed"] == "yes") & (pop["incwage"] > 0)
    for (s, a), grp in pop[mask_worker].groupby(["sex", "age"]):
        if len(grp) < 2:
            pop.loc[grp.index, "inc_pctile"] = 50
            continue
        pctile = pd.qcut(grp["incwage"], 100, labels=False, duplicates="drop")
        pop.loc[grp.index, "inc_pctile"] = (pctile + 1).fillna(50).astype(int)
    pop.loc[~mask_worker, "inc_pctile"] = 10

    # Build rho cube: rho_cube[sex, age, pctile]
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

    # Apply income gradient for ages 40-76
    rho_vec    = np.ones(len(pop))
    inside_pos = np.where(pop["age"].between(40, 76).values)[0]
    if len(inside_pos) > 0:
        rho_vec[inside_pos] = rho_cube[
            sex_v[inside_pos],
            age_v[inside_pos],
            np.clip(pop["inc_pctile"].values[inside_pos].astype(int), 1, 100)
        ]

    q_adj      = (q_base * rho_vec).clip(0, 1)
    pop["die"] = (rng.random(len(pop)) < q_adj).astype(int)

    # Surviving spouses become widowed
    dead_fams = set(pop.loc[pop["die"] == 1, "famunit"])
    pop.loc[
        pop["famunit"].isin(dead_fams) &
        (pop["relate"] == "spouse") &
        (pop["die"] == 0),
        ["marst", "receive_ss"]
    ] = ["widowed", "yes"]

    return pop[pop["die"] == 0].drop(columns=["die", "inc_pctile"]).reset_index(drop=True)


# ─────────────────────────────────────────────────────────────────────────────
# FERTILITY
# ─────────────────────────────────────────────────────────────────────────────

def asfr_from_tfr(tfr: float) -> dict:
    """
    Given a target TFR, compute ASFR for each 5-year age band.
    TFR = Σ ASFR_a * 5  =>  ASFR_a = tfr * shape_a / 5
    """
    return dict(zip(FERT_AGES, tfr * FERT_SHAPE / 5.0))


def make_babies(pop: pd.DataFrame, year: int, tfr: float) -> pd.DataFrame:
    """
    Apply fertility to women 15-49 based on ASFR derived from TFR.
    Newborns inherit family unit of mother; sex drawn 48/52 M/F.
    """
    asfr = asfr_from_tfr(tfr)

    age_to_mid = {}
    for lo, mid in zip([15, 20, 25, 30, 35, 40, 45], FERT_AGES):
        for a in range(lo, lo + 5):
            age_to_mid[a] = mid

    women = pop[(pop["sex"] == 2) & pop["age"].between(15, 49)].copy()
    women["asfr"] = women["age"].map(age_to_mid).map(asfr).fillna(0.0)
    mothers = women[rng.random(len(women)) < women["asfr"].values]

    if mothers.empty:
        return pop

    babies = pd.DataFrame({
        "famunit":     mothers["famunit"].values,
        "perwt":       mothers["perwt"].values,
        "hhwt":        mothers["hhwt"].values,
        "marst":       "single",
        "age":         0,
        "school":      "no",
        "sex":         rng.choice([1, 2], size=len(mothers), p=[0.48, 0.52]),
        "educ":        "hs",
        "labforce":    "no",
        "employed":    "no",
        "retired":     "no",
        "incwage":     0.0,
        "receive_ss":  "no",
        "cohort":      (year // 10) * 10,
        "relate":      "child",
        "year":        year,
        "predict_hat": np.nan,
        "ped":         0.0,
    })
    return pd.concat([pop, babies], ignore_index=True)
