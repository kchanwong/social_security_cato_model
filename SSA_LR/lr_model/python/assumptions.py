"""
assumptions.py — Single source of truth for all LR model parameters.
Sources: TR2025 Tables V.A1, V.A2, V.B1, V.C1
"""

import numpy as np
from scipy.interpolate import interp1d

# ── Projection horizon ────────────────────────────────────────────────────────
FIRST_PROJ_YEAR = 2025
FINAL_PROJ_YEAR = 2099
BASE_POP_YEAR   = 2024
VALUATION_YEAR  = 2025
ALL_YEARS       = list(range(FIRST_PROJ_YEAR, FINAL_PROJ_YEAR + 1))

# ── Scenario selector ─────────────────────────────────────────────────────────
SCENARIO = "intermediate"   # "intermediate" | "low_cost" | "high_cost" | "custom"

# =============================================================================
# PROCESS 1: DEMOGRAPHY
# =============================================================================

# ── 1.1 Fertility (TFR) — TR2025 Table V.A1 ──────────────────────────────────
TFR_NODES = {
    "intermediate": {2025:1.64, 2030:1.72, 2035:1.80, 2040:1.87,
                     2045:1.90, 2050:1.90, 2055:1.90, 2060:1.90,
                     2065:1.90, 2070:1.90, 2075:1.90, 2080:1.90,
                     2085:1.90, 2090:1.90, 2095:1.90, 2100:1.90},
    "low_cost":     {2025:1.67, 2030:1.84, 2035:1.97, 2040:2.07,
                     2045:2.10, 2050:2.10, 2055:2.10, 2060:2.10,
                     2065:2.10, 2070:2.10, 2075:2.10, 2080:2.10,
                     2085:2.10, 2090:2.10, 2095:2.10, 2100:2.10},
    "high_cost":    {2025:1.59, 2030:1.54, 2035:1.55, 2040:1.58,
                     2045:1.60, 2050:1.60, 2055:1.60, 2060:1.60,
                     2065:1.60, 2070:1.60, 2075:1.60, 2080:1.60,
                     2085:1.60, 2090:1.60, 2095:1.60, 2100:1.60},
}

# ── 1.2 Mortality — annual % reduction in m(x) by age group ──────────────────
MORTALITY_ULT_REDUCTION = {
    "intermediate": {"lt1":0.90,"1to4":1.00,"5to14":0.90,"15to24":0.75,
                     "25to34":0.75,"35to44":0.85,"45to54":1.00,"55to64":1.05,
                     "65to74":1.00,"75to84":0.85,"85plus":0.60},
    "low_cost":     {"lt1":0.50,"1to4":0.50,"5to14":0.50,"15to24":0.50,
                     "25to34":0.50,"35to44":0.50,"45to54":0.60,"55to64":0.65,
                     "65to74":0.65,"75to84":0.55,"85plus":0.40},
    "high_cost":    {"lt1":1.30,"1to4":1.40,"5to14":1.25,"15to24":1.10,
                     "25to34":1.10,"35to44":1.20,"45to54":1.40,"55to64":1.50,
                     "65to74":1.45,"75to84":1.25,"85plus":0.90},
}
MORTALITY_ULTIMATE_YEAR = {"intermediate":2044, "low_cost":2044, "high_cost":2044}

# ── 1.3 Immigration — net annual (thousands) — TR2025 Table V.A2 ─────────────
LPR_NET = {
    "intermediate": {2025:910,  2030:788,  2035:788,  2040:788,  2045:788,
                     2050:788,  2055:788,  2060:788,  2065:788,  2070:788,
                     2075:788,  2080:788,  2085:788,  2090:788,  2095:788,  2100:788},
    "low_cost":     {2025:1130, 2030:1000, 2035:1000, 2040:1000, 2045:1000,
                     2050:1000, 2055:1000, 2060:1000, 2065:1000, 2070:1000,
                     2075:1000, 2080:1000, 2085:1000, 2090:1000, 2095:1000, 2100:1000},
    "high_cost":    {2025:709,  2030:595,  2035:595,  2040:595,  2045:595,
                     2050:595,  2055:595,  2060:595,  2065:595,  2070:595,
                     2075:595,  2080:595,  2085:595,  2090:595,  2095:595,  2100:595},
}
TUP_NET = {
    "intermediate": {2025:1192, 2030:536,  2035:520,  2040:502,  2045:484,
                     2050:473,  2055:468,  2060:463,  2065:460,  2070:456,
                     2075:454,  2080:452,  2085:451,  2090:449,  2095:449,  2100:448},
    "low_cost":     {2025:1758, 2030:867,  2035:828,  2040:786,  2045:747,
                     2050:719,  2055:702,  2060:688,  2065:678,  2070:671,
                     2075:665,  2080:661,  2085:658,  2090:656,  2095:655,  2100:655},
    "high_cost":    {2025:626,  2030:204,  2035:213,  2040:218,  2045:221,
                     2050:227,  2055:234,  2060:239,  2065:243,  2070:245,
                     2075:246,  2080:247,  2085:247,  2090:247,  2095:247,  2100:247},
}

# =============================================================================
# PROCESS 2: ECONOMICS — TR2025 Table V.B1
# =============================================================================

AWI_NOMINAL_PCT = {
    "intermediate": {"transition": {2025:3.97,2026:4.13,2027:4.03,2028:4.11,2029:3.94,
                                    2030:3.88,2031:3.93,2032:3.95,2033:3.96,2034:3.85},
                     "ultimate": 3.56},
    "low_cost":     {"transition": {2025:5.10,2026:5.50,2027:5.28,2028:5.31,2029:5.38,
                                    2030:5.32,2031:5.23,2032:5.20,2033:5.23,2034:5.12},
                     "ultimate": 4.78},
    "high_cost":    {"transition": {2025:1.25,2026:2.22,2027:3.61,2028:3.49,2029:3.10,
                                    2030:2.81,2031:2.70,2032:2.72,2033:2.71,2034:2.59},
                     "ultimate": 2.34},
}
CPI_PCT = {
    "intermediate": {"transition": {y:2.40 for y in range(2025,2035)}, "ultimate": 2.40},
    "low_cost":     {"transition": {y:3.00 for y in range(2025,2035)}, "ultimate": 3.00},
    "high_cost":    {"transition": {2025:2.21,2026:1.85,**{y:1.80 for y in range(2027,2035)}},
                     "ultimate": 1.80},
}
# Override 2025 intermediate CPI
CPI_PCT["intermediate"]["transition"][2025] = 2.47
CPI_PCT["intermediate"]["transition"][2026] = 2.49

EARNINGS_PCT_COMPENSATION = {
    "intermediate": {"transition": {2025:-0.12,2026:-0.08,2027:-0.08,2028:-0.08,
                                    2029:-0.08,2030:-0.09,2031:-0.09,2032:-0.09,
                                    2033:-0.09,2034:-0.09}, "ultimate": -0.09},
    "low_cost":     {"transition": {y: 0.00 for y in range(2025,2035)}, "ultimate":  0.00},
    "high_cost":    {"transition": {2025:-0.12,2026:-0.07,2027:-0.10,2028:-0.12,
                                    2029:-0.13,2030:-0.14,2031:-0.15,2032:-0.16,
                                    2033:-0.17,2034:-0.18}, "ultimate": -0.17},
}
UNEMPLOYMENT_ULT = {"intermediate":4.50, "low_cost":3.50, "high_cost":5.50}

# =============================================================================
# PROCESS 4: TRUST FUND / INTEREST RATES
# =============================================================================

REAL_INTEREST_RATE_ULT = {"intermediate":2.70, "low_cost":3.30, "high_cost":2.20}

# New-issue bond yield path (nominal %)
YIELD_INTERMEDIATE = {**{y:4.1 for y in range(2025,2035)},
                      2025:4.2, 2035:4.3, 2036:4.4, 2037:4.5, 2038:4.5,
                      2039:4.6, 2040:4.6, 2041:4.7,
                      **{y:4.7 for y in range(2042, FINAL_PROJ_YEAR+1)}}
YIELD_LOW_COST  = {**{y:5.8 for y in range(2035,FINAL_PROJ_YEAR+1)},
                   2025:4.6,2026:4.6,2027:4.8,2028:4.8,2029:4.9,
                   2030:5.0,2031:5.1,2032:5.2,2033:5.3,2034:5.4}
YIELD_HIGH_COST = {**{y:4.0 for y in range(2035,FINAL_PROJ_YEAR+1)},
                   2025:4.0,2026:3.9,2027:3.8,2028:3.7,2029:3.6,
                   2030:3.6,2031:3.6,2032:3.6,2033:3.6,2034:3.6}
NEW_ISSUE_YIELD = {"intermediate": YIELD_INTERMEDIATE,
                   "low_cost":     YIELD_LOW_COST,
                   "high_cost":    YIELD_HIGH_COST}

# Payroll tax rates (employee share; employer matches)
OASI_EE_RATE  = 0.0530
DI_EE_RATE    = 0.0090
OASDI_EE_RATE = OASI_EE_RATE + DI_EE_RATE   # 6.20%

# Taxation of benefits factors (share of benefits returned as income tax)
TOB_FACTOR_OASI = {"intermediate":0.041, "low_cost":0.041, "high_cost":0.041}
TOB_FACTOR_DI   = {"intermediate":0.004, "low_cost":0.004, "high_cost":0.004}

# Admin productivity offset
PRODUCTIVITY_ADMIN = 0.016

# Railroad interchange (billions, 2025 base; grows with AWI)
RR_CASHFLOW_OASI_2025 = -5.2
RR_CASHFLOW_DI_2025   = -0.8

# =============================================================================
# HELPER: interpolate 5-year node dict to annual series
# =============================================================================

def interpolate_nodes(node_dict, years=None):
    """Linear interpolation from node dict {year: value} to annual array."""
    if years is None:
        years = ALL_YEARS
    xs = sorted(node_dict.keys())
    ys = [node_dict[x] for x in xs]
    f  = interp1d(xs, ys, kind='linear', bounds_error=False,
                  fill_value=(ys[0], ys[-1]))
    return {yr: float(f(yr)) for yr in years}

def get_tfr_annual(scenario=None):
    sc = scenario or SCENARIO
    return interpolate_nodes(TFR_NODES[sc])

def get_lpr_annual(scenario=None):
    sc = scenario or SCENARIO
    return interpolate_nodes(LPR_NET[sc])

def get_tup_annual(scenario=None):
    sc = scenario or SCENARIO
    return interpolate_nodes(TUP_NET[sc])

def get_awi_pct(year, scenario=None):
    sc = scenario or SCENARIO
    p  = AWI_NOMINAL_PCT[sc]
    return p["transition"].get(year, p["ultimate"])

def get_cpi_pct(year, scenario=None):
    sc = scenario or SCENARIO
    p  = CPI_PCT[sc]
    return p["transition"].get(year, p["ultimate"])

def get_earn_drift(year, scenario=None):
    sc = scenario or SCENARIO
    p  = EARNINGS_PCT_COMPENSATION[sc]
    return p["transition"].get(year, p["ultimate"])

def get_yield(year, scenario=None):
    sc = scenario or SCENARIO
    return NEW_ISSUE_YIELD[sc].get(year, NEW_ISSUE_YIELD[sc][FINAL_PROJ_YEAR]) / 100

if __name__ == "__main__":
    sc = SCENARIO
    tfr = get_tfr_annual(sc)
    print(f"[assumptions.py] Scenario: {sc}")
    print(f"  TFR 2025={tfr[2025]:.2f}, 2050={tfr[2050]:.2f}, 2099={tfr[2099]:.2f}")
    print(f"  Real rate: {REAL_INTEREST_RATE_ULT[sc]:.2f}%")
    print(f"  LPR ult: {list(get_lpr_annual(sc).values())[-1]:.0f}k")
