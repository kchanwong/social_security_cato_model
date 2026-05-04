"""
demography.py — Cohort-component population projection, 2025–2099.
Replicates SSA Process 1.

Outputs
-------
run_demography() → dict with keys:
  pop_proj        : {year: ndarray shape (101,2)}  M/F by age 0-100 (thousands)
  dep_ratios      : DataFrame  (year, pop_under20, pop_20_64, pop_65p, ...)
  life_exp        : DataFrame  (year, e0_male, e0_female, e65_male, e65_female)
  working_age_pop : Series     ages 20-64 total (thousands)
"""

import os
import numpy as np
import pandas as pd
from scipy.interpolate import interp1d
from assumptions import (
    SCENARIO, ALL_YEARS, FIRST_PROJ_YEAR, FINAL_PROJ_YEAR, BASE_POP_YEAR,
    MORTALITY_ULT_REDUCTION, MORTALITY_ULTIMATE_YEAR,
    get_tfr_annual, get_lpr_annual, get_tup_annual,
)

# ── SSA TR2024 Alt2 period life tables ────────────────────────────────────────
_DATA_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', '..', 'data')

def _load_ssa_period_lt():
    """Load SSA TR2024 Alt2 q(x) arrays keyed by {year: {sex: array[101]}}."""
    out = {}
    for sex, fname in [(0, 'PerLifeTables_M_Alt2_TR2024.csv'),
                       (1, 'PerLifeTables_F_Alt2_TR2024.csv')]:
        path = os.path.join(_DATA_DIR, fname)
        df = pd.read_csv(path, skiprows=4)
        df = df[df['x'] <= 100].copy()
        for yr, grp in df.groupby('Year'):
            arr = grp.sort_values('x')['q(x)'].values[:101].copy()
            arr[100] = 1.0   # open age group — everyone dies at 100
            arr = np.minimum(arr, 0.9999)
            arr[100] = 1.0
            out.setdefault(int(yr), {})[sex] = arr
    return out

_SSA_QX = _load_ssa_period_lt()

AGES = np.arange(101)   # 0 … 100
SEX_M, SEX_F = 0, 1

# ── Age-group mapping for mortality reduction schedule ────────────────────────
def age_to_group(age):
    if   age <  1: return "lt1"
    elif age <  5: return "1to4"
    elif age < 15: return "5to14"
    elif age < 25: return "15to24"
    elif age < 35: return "25to34"
    elif age < 45: return "35to44"
    elif age < 55: return "45to54"
    elif age < 65: return "55to64"
    elif age < 75: return "65to74"
    elif age < 85: return "75to84"
    else:          return "85plus"

AGE_GROUP = np.array([age_to_group(a) for a in AGES])

# =============================================================================
# 1. STARTING POPULATION (Jan 1, 2025)
# =============================================================================
# Parametric graduation anchored to V.A3 2024 group totals (thousands):
#   0-19: 83,034  |  20-64: 199,564  |  65+: 62,835  |  Total: 345,433

def load_starting_population():
    grp = {"0_19": 83034, "20_64": 199564, "65p": 62835}

    # Male share by age
    male_share_x = [0,  5, 15, 25, 35, 45, 55, 65, 75, 85, 100]
    male_share_y = [0.512,0.513,0.510,0.504,0.499,0.494,0.487,0.478,0.466,0.453,0.440]
    ms = interp1d(male_share_x, male_share_y, kind='linear',
                  bounds_error=False, fill_value=(male_share_y[0], male_share_y[-1]))
    male_frac = ms(AGES)

    # Shape: smooth graduation
    shape_x = [0,5,10,15,19, 20,30,40,50,60,64, 65,70,75,80,85,90,95,100]
    shape_y  = [3700,4150,4350,4200,4100,
                4800,4500,4350,4400,4200,4050,
                3800,3200,2600,2000,1350,750,350,90]
    shp = interp1d(shape_x, shape_y, kind='linear',
                   bounds_error=False, fill_value=(shape_y[0], shape_y[-1]))
    raw = shp(AGES)

    # Scale each group to target
    pop = np.zeros(101)
    pop[0:20]   = raw[0:20]  * (grp["0_19"]  / raw[0:20].sum())
    pop[20:65]  = raw[20:65] * (grp["20_64"] / raw[20:65].sum())
    pop[65:101] = raw[65:101]* (grp["65p"]   / raw[65:101].sum())

    out = np.zeros((101, 2))
    out[:, SEX_M] = pop * male_frac
    out[:, SEX_F] = pop * (1 - male_frac)
    return out   # thousands

# =============================================================================
# 2. MORTALITY
# =============================================================================

def build_base_qx():
    """Makeham-Gompertz q(x) calibrated to SSA 2024 period life table."""
    a_m, b_m, c_m = 0.00030, 0.00003, 0.10
    a_f, b_f, c_f = 0.00015, 0.000015, 0.10
    mx_m = a_m + b_m * np.exp(c_m * AGES)
    mx_f = a_f + b_f * np.exp(c_f * AGES)
    qx_m = np.minimum(1 - np.exp(-mx_m), 0.9999)
    qx_f = np.minimum(1 - np.exp(-mx_f), 0.9999)
    infant_m = [0.0051,0.00033,0.00022,0.00016,0.00013,
                0.00010,0.00008,0.00008,0.00009,0.00011,
                0.00013,0.00015,0.00019,0.00028,0.00045,
                0.00083,0.00109,0.00130,0.00147,0.00156]
    infant_f = [0.0042,0.00027,0.00019,0.00014,0.00011,
                0.00009,0.00007,0.00008,0.00009,0.00011,
                0.00013,0.00016,0.00019,0.00023,0.00030,
                0.00041,0.00051,0.00059,0.00065,0.00069]
    qx_m[:20] = infant_m
    qx_f[:20] = infant_f
    return np.stack([qx_m, qx_f], axis=1)


def annual_mortality_reduction(year, scenario=SCENARIO):
    """Proportion reduction in m(x) for each age group this year (parametric fallback)."""
    ult  = MORTALITY_ULT_REDUCTION[scenario]
    ult_yr = MORTALITY_ULTIMATE_YEAR[scenario]
    frac = np.clip((year - BASE_POP_YEAR) / (ult_yr - BASE_POP_YEAR), 0, 1)
    start_factor = 0.5
    factor = start_factor + (1 - start_factor) * frac
    return {grp: ult[grp] / 100 * factor for grp in ult}


def step_qx(qx, year, scenario=SCENARIO):
    """Advance q(x) by one year of mortality improvement (parametric for all scenarios)."""
    reductions = annual_mortality_reduction(year, scenario)
    new_qx = np.zeros_like(qx)
    for sex in [SEX_M, SEX_F]:
        mx = -np.log(1 - np.minimum(qx[:, sex], 0.9999))
        for a in AGES:
            grp = AGE_GROUP[a]
            mx[a] *= (1 - reductions[grp])
        new_qx[:, sex] = np.minimum(1 - np.exp(-mx), 0.9999)
    return new_qx


def life_expectancy(qx):
    """Period life expectancy at birth and age 65 for each sex."""
    results = {}
    for sex, label in [(SEX_M, "male"), (SEX_F, "female")]:
        q = qx[:, sex]
        n = len(q)
        lx = np.ones(n + 1)
        for i in range(n):
            lx[i+1] = lx[i] * (1 - q[i])
        Lx = (lx[:n] + lx[1:]) / 2
        Lx[-1] = lx[-1] / q[-1]   # open interval
        Tx = np.cumsum(Lx[::-1])[::-1]
        ex = Tx / lx[:n]
        results[f"e0_{label}"]  = ex[0]
        results[f"e65_{label}"] = ex[65]
    return results

# =============================================================================
# 3. FERTILITY
# =============================================================================

ASFR_SHAPE = np.array([
    0.004,0.007,0.012,0.020,0.029,
    0.040,0.052,0.063,0.073,0.080,
    0.085,0.087,0.086,0.083,0.079,
    0.074,0.068,0.060,0.052,0.044,
    0.036,0.029,0.022,0.016,0.011,
    0.008,0.005,0.003,0.002,0.001,
    0.001,0.001,0.000,0.000,0.000,
])
ASFR_SHAPE /= ASFR_SHAPE.sum()
FERTILE_AGES = np.arange(15, 50)   # indices 15-49

# =============================================================================
# 4. IMMIGRATION
# =============================================================================

LPR_AGE_DIST = np.array([
    0.065,0.052,0.052,0.063,0.112,0.140,0.132,0.110,
    0.080,0.055,0.040,0.028,0.022,0.018,0.013,0.009,0.006,0.003
])
TUP_AGE_DIST = np.array([
    0.045,0.038,0.040,0.063,0.150,0.200,0.175,0.130,
    0.075,0.040,0.022,0.010,0.005,0.002,0.002,0.001,0.001,0.001
])
TUP_AGE_DIST /= TUP_AGE_DIST.sum()

AGE_GROUPS_5YR = [
    list(range(0,5)), list(range(5,10)), list(range(10,15)), list(range(15,20)),
    list(range(20,25)), list(range(25,30)), list(range(30,35)), list(range(35,40)),
    list(range(40,45)), list(range(45,50)), list(range(50,55)), list(range(55,60)),
    list(range(60,65)), list(range(65,70)), list(range(70,75)), list(range(75,80)),
    list(range(80,85)), list(range(85,101))
]
LPR_MALE_SHARE = 0.48
TUP_MALE_SHARE = 0.54

def distribute_immigration(lpr_k, tup_k):
    mat = np.zeros((101, 2))
    for i, ag in enumerate(AGE_GROUPS_5YR):
        w = len(ag)
        lpr = lpr_k * LPR_AGE_DIST[i] / w
        tup = tup_k * TUP_AGE_DIST[i] / w
        total_m = lpr * LPR_MALE_SHARE + tup * TUP_MALE_SHARE
        total_f = lpr * (1 - LPR_MALE_SHARE) + tup * (1 - TUP_MALE_SHARE)
        for a in ag:
            mat[a, SEX_M] += total_m
            mat[a, SEX_F] += total_f
    return mat

# =============================================================================
# 5. PROJECTION ENGINE
# =============================================================================

def project_population(scenario=SCENARIO):
    print(f"[demography.py] Running cohort-component projection | scenario: {scenario}")

    tfr_annual = get_tfr_annual(scenario)
    lpr_annual = get_lpr_annual(scenario)
    tup_annual = get_tup_annual(scenario)

    pop = load_starting_population()   # Jan 1, 2025
    qx  = build_base_qx()             # base 2024 q(x)

    n_years = len(ALL_YEARS)
    pop_proj = {}
    dep_rows = []
    le_rows  = []

    SRB = 1.048   # sex ratio at birth (male per female)

    for i, yr in enumerate(ALL_YEARS):
        # 4a. Advance q(x)
        qx = step_qx(qx, yr, scenario)

        # 4b. Survive population one year
        surv = pop[:100, :] * (1 - qx[:100, :])           # ages 0→1 … 99→100
        open_interval = (surv[99, :] +                     # survivors of age 99
                         pop[100, :] * (1 - qx[100, :]))   # survivors of age 100

        pop_new = np.zeros((101, 2))
        pop_new[1:100, :] = surv[:99, :]
        pop_new[100, :]   = open_interval

        # 4c. Births
        tfr  = tfr_annual[yr]
        asfr = ASFR_SHAPE * tfr
        fem_start = pop[FERTILE_AGES, SEX_F]
        fem_end   = pop_new[FERTILE_AGES, SEX_F]
        fem_mid   = (fem_start + fem_end) / 2
        total_births = float(np.dot(asfr, fem_mid))
        births_m = total_births * SRB / (1 + SRB)
        births_f = total_births * 1   / (1 + SRB)
        pop_new[0, SEX_M] = births_m * (1 - qx[0, SEX_M])
        pop_new[0, SEX_F] = births_f * (1 - qx[0, SEX_F])

        # 4d. Immigration
        pop_new += distribute_immigration(lpr_annual[yr], tup_annual[yr])
        pop_new  = np.maximum(pop_new, 0)

        # 4e. Store
        pop_proj[yr] = pop_new

        u20  = pop_new[:20,  :].sum()
        w    = pop_new[20:65, :].sum()
        e65p = pop_new[65:,  :].sum()
        tot  = u20 + w + e65p

        dep_rows.append({
            "year": yr,
            "pop_under20": u20, "pop_20_64": w, "pop_65p": e65p,
            "pop_total": tot,
            "aged_dep_ratio":  e65p / w,
            "total_dep_ratio": (u20 + e65p) / w,
        })

        le = life_expectancy(qx)
        le_rows.append({"year": yr, **le})

        if yr % 10 == 0:
            print(f"  {yr}: pop={tot:,.0f}k  aged_dep={e65p/w:.3f}  e0(M)={le['e0_male']:.1f}")

        pop = pop_new

    dep_df = pd.DataFrame(dep_rows).set_index("year")
    le_df  = pd.DataFrame(le_rows).set_index("year")
    wap    = dep_df["pop_20_64"]   # working-age pop (thousands)

    return {"pop_proj": pop_proj, "dep_ratios": dep_df,
            "life_exp": le_df, "working_age_pop": wap}

# =============================================================================
# 6. CALIBRATION CHECK vs. TR2025 V.A3
# =============================================================================

V_A3_BENCHMARK = pd.DataFrame([
    {"year":2025, "pop_total":348404, "aged_dep_ratio":0.322},
    {"year":2030, "pop_total":358152, "aged_dep_ratio":0.357},
    {"year":2035, "pop_total":366963, "aged_dep_ratio":0.374},
    {"year":2040, "pop_total":375094, "aged_dep_ratio":0.380},
    {"year":2050, "pop_total":388372, "aged_dep_ratio":0.394},
    {"year":2075, "pop_total":420329, "aged_dep_ratio":0.460},
]).set_index("year")

def calibrate(results):
    dr = results["dep_ratios"]
    rows = []
    for yr in V_A3_BENCHMARK.index:
        if yr not in dr.index:
            continue
        bench_pop = V_A3_BENCHMARK.loc[yr, "pop_total"]
        bench_dep = V_A3_BENCHMARK.loc[yr, "aged_dep_ratio"]
        mod_pop   = dr.loc[yr, "pop_total"]
        mod_dep   = dr.loc[yr, "aged_dep_ratio"]
        rows.append({
            "year":      yr,
            "TR_pop_k":  bench_pop,
            "Mod_pop_k": round(mod_pop),
            "pop_%diff": round(100*(mod_pop - bench_pop)/bench_pop, 2),
            "TR_dep":    bench_dep,
            "Mod_dep":   round(mod_dep, 3),
            "dep_diff":  round(mod_dep - bench_dep, 3),
        })
    print("\n[Calibration: V.A3 vs. Model]")
    print(pd.DataFrame(rows).to_string(index=False))
    max_err = max(abs(r["pop_%diff"]) for r in rows)
    if max_err > 3.0:
        print(f"  [!] Max pop deviation {max_err:.1f}% exceeds 3% -- recalibrate base pop.")
    else:
        print(f"  [ok] Max pop deviation: {max_err:.2f}%")

# =============================================================================
# ENTRY POINT
# =============================================================================

def run_demography(scenario=SCENARIO, calibrate_flag=True):
    results = project_population(scenario)
    if calibrate_flag and scenario == "intermediate":
        calibrate(results)
    dr = results["dep_ratios"]
    print(f"\n[demography.py] Done.")
    print(f"  2025 pop: {dr.loc[2025,'pop_total']:,.0f}k  "
          f"2050 pop: {dr.loc[2050,'pop_total']:,.0f}k  "
          f"2099 pop: {dr.loc[2099,'pop_total']:,.0f}k")
    return results

if __name__ == "__main__":
    demo_results = run_demography()
