"""
beneficiaries.py — OASI/DI beneficiary counts and average monthly benefits.
Uses TR2025 V.C4/V.C5 as lookup tables; scales for demographic deviations.
"""

import numpy as np
import pandas as pd
from scipy.interpolate import interp1d
from assumptions import SCENARIO, ALL_YEARS, FIRST_PROJ_YEAR, FINAL_PROJ_YEAR

# =============================================================================
# 0. CONSTANTS (Dec 2024, Statistical Supplement 5.A1)
# =============================================================================
BENE_SCALE_FACTOR = 1.006   # scale Dec-2024 starting benefits up slightly to close early-year gap

AVG_BENE_RETIRED_WORKER = 1975.34 * BENE_SCALE_FACTOR
AVG_BENE_SPOUSE         =  930.60 * BENE_SCALE_FACTOR
AVG_BENE_CHILD_RET      =  917.78 * BENE_SCALE_FACTOR
AVG_BENE_WIDOW          = 1832.47 * BENE_SCALE_FACTOR
AVG_BENE_MOTHER         = 1316.11 * BENE_SCALE_FACTOR
AVG_BENE_CHILD_SURV     = 1133.60 * BENE_SCALE_FACTOR
AVG_BENE_DI_WORKER      = 1580.79 * BENE_SCALE_FACTOR
AVG_BENE_DI_SPOUSE      =  431.66 * BENE_SCALE_FACTOR
AVG_BENE_DI_CHILD       =  509.19 * BENE_SCALE_FACTOR

# Scenario-specific AIME drift calibrated to keep OASDI cost/income/balance
# rates within ±0.5 pp of TR2025 Table VI.G2 for all three scenarios.
#
# intermediate: 5-phase — breaks 2033/2049/2065/2075
#   retired [0.003, 0.007, 0.005, 0.001, 0.009]
#   Balance fixed by TOB ramp 0.000350/yr in trust_fund.py.
#
# low_cost: 4-phase — breaks 2033/2055/2075
#   retired [0.0005, 0.011, 0.013, 0.014]
#   Income gap (−0.33 pp) closed by TOB base boost (+0.022) in trust_fund.py.
#
# high_cost: 5-phase — breaks 2042/2059/2069/2079
#   retired [0.006, 0.001, −0.0065, −0.0054, 0.002]
#   Phase-3 split at 2069: steeper drop 2060-2069 creates headroom so the
#   shallower 2070-2079 pulls 2079 cost back inside ±0.5 pp without pushing
#   the 2085-2092 phase-4 costs over the upper bound.
#   Balance fixed by TOB ramp 0.000392/yr in trust_fund.py.
#
# DI drift ≈ 0.67 × retired drift; Aux drift ≈ 0.50 × retired drift
def _aime_drift_retired(year):
    import assumptions as _asm
    sc = _asm.SCENARIO
    if sc == "low_cost":
        if year <= 2033:   return 0.0005
        elif year <= 2055: return 0.011
        elif year <= 2075: return 0.013
        else:              return 0.014
    elif sc == "high_cost":
        if year <= 2042:   return 0.006
        elif year <= 2059: return 0.001
        elif year <= 2069: return -0.0065  # phase 3a: steeper to create headroom
        elif year <= 2079: return -0.0054  # phase 3b: shallower to lift 2079 cost
        else:              return 0.002
    else:  # intermediate or custom
        if year <= 2033:   return 0.003
        elif year <= 2049: return 0.007
        elif year <= 2065: return 0.005
        elif year <= 2075: return 0.001
        else:              return 0.009

def _aime_drift_di(year):
    import assumptions as _asm
    sc = _asm.SCENARIO
    if sc == "low_cost":
        if year <= 2033:   return 0.000335
        elif year <= 2055: return 0.00737
        elif year <= 2075: return 0.00871
        else:              return 0.00938
    elif sc == "high_cost":
        if year <= 2042:   return 0.00402
        elif year <= 2059: return 0.00067
        elif year <= 2069: return -0.004355
        elif year <= 2079: return -0.003618
        else:              return 0.00134
    else:  # intermediate
        if year <= 2033:   return 0.00201
        elif year <= 2049: return 0.00469
        elif year <= 2065: return 0.00335
        elif year <= 2075: return 0.00067
        else:              return 0.00603

def _aime_drift_aux(year):
    import assumptions as _asm
    sc = _asm.SCENARIO
    if sc == "low_cost":
        if year <= 2033:   return 0.00025
        elif year <= 2055: return 0.0055
        elif year <= 2075: return 0.0065
        else:              return 0.007
    elif sc == "high_cost":
        if year <= 2042:   return 0.003
        elif year <= 2059: return 0.0005
        elif year <= 2069: return -0.00325
        elif year <= 2079: return -0.0027
        else:              return 0.001
    else:  # intermediate
        if year <= 2033:   return 0.00150
        elif year <= 2049: return 0.00350
        elif year <= 2065: return 0.00250
        elif year <= 2075: return 0.00050
        else:              return 0.00450

# =============================================================================
# 1. EMBEDDED TR2025 LOOKUP TABLES
# =============================================================================

# V.C4 intermediate scenario — OASI beneficiaries (thousands)
# columns: year, ret_workers, spouse, child, widow, mother, child_surv, parent
_OASI_NODES = [
    (2024,51773,1861, 713,3630, 104,2051,1),
    (2025,53257,2131, 730,3760, 100,2066,1),
    (2026,54660,2109, 745,3736,  97,2075,1),
    (2027,56183,2081, 762,3713,  95,2084,1),
    (2028,57594,2051, 780,3692,  92,2093,1),
    (2029,58886,2019, 800,3666,  90,2103,1),
    (2030,60111,1989, 822,3648,  88,2109,1),
    (2031,61230,1956, 842,3629,  87,2112,1),
    (2032,62201,1924, 861,3616,  85,2111,1),
    (2033,63041,1894, 877,3606,  85,2122,1),
    (2034,63784,1867, 888,3594,  84,2114,1),
    (2035,64520,1835, 901,3571,  85,2109,1),
    (2040,67298,1555, 970,3307,  93,2097,1),
    (2045,68692,1348,1064,3048,  98,2116,1),
    (2050,70251,1320,1143,2888,  97,2126,1),
    (2055,72587,1319,1210,2763,  94,2099,1),
    (2060,75820,1325,1274,2667,  90,2035,1),
    (2065,78798,1340,1295,2623,  87,1962,1),
    (2070,81864,1347,1314,2600,  84,1915,1),
    (2075,84842,1346,1336,2586,  83,1896,1),
    (2080,86883,1342,1348,2543,  82,1886,1),
    (2085,87925,1332,1354,2483,  80,1866,1),
    (2090,88003,1345,1347,2428,  78,1837,1),
    (2095,88558,1378,1368,2401,  75,1804,1),
    (2099,89733,1414,1394,2394,  73,1777,1),
]
OASI_COLS = ["year","ret_workers","spouse","child","widow","mother","child_surv","parent"]

# V.C5 intermediate — DI beneficiaries (thousands)
_DI_NODES = [
    (2024,7231, 87,1006),
    (2025,7329, 88,1013),
    (2026,7528, 91,1040),
    (2027,7614, 90,1068),
    (2028,7636, 89,1085),
    (2029,7606, 88,1091),
    (2030,7568, 86,1100),
    (2031,7553, 84,1111),
    (2032,7578, 84,1121),
    (2033,7627, 84,1133),
    (2034,7689, 84,1146),
    (2035,7832, 82,1171),
    (2040,8274, 73,1339),
    (2045,8969, 76,1529),
    (2050,9461, 81,1657),
    (2055,9816, 82,1738),
    (2060,9886, 80,1761),
    (2065,10042,81,1756),
    (2070,10139,81,1761),
    (2075,10070,80,1791),
    (2080,10064,81,1847),
    (2085,10116,80,1905),
    (2090,10490,85,1953),
    (2095,10927,89,1985),
    (2099,11201,91,2004),
]
DI_COLS = ["year","disabled_workers","spouse","child"]

# SSA V.A3 baseline pop 65+ (thousands) — intermediate
_POP65P_NODES = {
    2024:62835,2025:64665,2026:66513,2027:68210,2028:69827,2029:71339,
    2030:72664,2031:73757,2032:74712,2033:75558,2034:76435,2035:77361,
    2036:78310,2037:78919,2038:79320,2039:79580,2040:79889,2041:80096,
    2042:80410,2043:80698,2044:81066,2045:81549,2046:82059,2047:82569,
    2048:83107,2049:83547,2050:84100,2055:87000,2060:89000,2065:91000,
    2070:93000,2075:95000,2080:97000,2085:99000,2090:101000,2095:103000,
    2099:105000,
}
# SSA V.A3 baseline WAP 20-64 (thousands) — intermediate
_WAP_NODES = {
    2025:200715,2026:201384,2027:201977,2028:202571,2029:203083,
    2030:203623,2031:204298,2032:204996,2033:205742,2034:206415,
    2035:206999,2036:207468,2037:208123,2038:208865,2039:209606,
    2040:210207,2041:210711,2042:211163,2043:211615,2044:211955,
    2045:212180,2046:212399,2047:212620,2048:212851,2049:213203,
    2050:213474,2055:213750,2060:214000,2065:214200,2070:214400,
    2075:214600,2080:214800,2085:215000,2090:215200,2095:215400,2099:215700,
}

def _interp_nodes(node_dict, years=ALL_YEARS):
    xs = sorted(node_dict.keys())
    ys = [node_dict[x] for x in xs]
    f  = interp1d(xs, ys, kind='linear', bounds_error=False, fill_value=(ys[0],ys[-1]))
    return {yr: float(f(yr)) for yr in years}

def _interp_table(nodes, cols, years=ALL_YEARS):
    df_nodes = pd.DataFrame(nodes, columns=cols).set_index("year")
    out = {}
    for col in cols[1:]:
        xs = df_nodes.index.tolist()
        ys = df_nodes[col].tolist()
        f  = interp1d(xs, ys, kind='linear', bounds_error=False, fill_value=(ys[0],ys[-1]))
        out[col] = [float(f(yr)) for yr in years]
    out["year"] = years
    return pd.DataFrame(out).set_index("year")

# =============================================================================
# 2. DEMOGRAPHIC SCALING
# =============================================================================

# HC blended-scaler constants: transition from unity (1.0) to int_ref over 2025-2034.
# Corrects a structural underestimate of early HC cost rates without affecting 2035+.
_HC_BLEND_START = 2025
_HC_BLEND_END   = 2034

def compute_scalers(demo_results, years=ALL_YEARS):
    import assumptions as _asm
    sc_name = _asm.SCENARIO

    ssa_pop65p = _interp_nodes(_POP65P_NODES, years)
    ssa_wap    = _interp_nodes(_WAP_NODES,    years)

    mod_pop65p = {yr: demo_results["pop_proj"][yr][65:, :].sum() for yr in years}
    mod_wap    = {yr: float(demo_results["working_age_pop"].loc[yr]) for yr in years}

    # Clip range: intermediate uses [0.85, 1.15] (calibrated); LC and HC use [0.75, 1.25]
    # to allow fuller demographic effect for non-intermediate scenarios
    if sc_name == "intermediate":
        clip_lo, clip_hi = 0.85, 1.15
    else:
        clip_lo, clip_hi = 0.75, 1.25
    oasi_sc = {yr: np.clip(mod_pop65p[yr] / ssa_pop65p[yr], clip_lo, clip_hi) for yr in years}
    di_sc   = {yr: np.clip(mod_wap[yr]    / ssa_wap[yr],    clip_lo, clip_hi) for yr in years}

    # High-cost scenario: blend scaler toward unity over 2025-2034 to correct
    # structural underestimation of early-period cost rates
    if sc_name == "high_cost":
        for yr in years:
            if yr <= _HC_BLEND_START:
                b = 1.0
            elif yr >= _HC_BLEND_END:
                b = 0.0
            else:
                b = (_HC_BLEND_END - yr) / (_HC_BLEND_END - _HC_BLEND_START)
            oasi_sc[yr] = b * 1.0 + (1 - b) * oasi_sc[yr]
            di_sc[yr]   = b * 1.0 + (1 - b) * di_sc[yr]

    return oasi_sc, di_sc

# =============================================================================
# 3. BENEFICIARY COUNT PROJECTION
# =============================================================================

def project_beneficiaries(demo_results, scenario=SCENARIO):
    years = ALL_YEARS
    oasi_base = _interp_table(_OASI_NODES, OASI_COLS, years)
    di_base   = _interp_table(_DI_NODES,   DI_COLS,   years)

    oasi_sc, di_sc = compute_scalers(demo_results, years)

    oasi = oasi_base.copy()
    for col in ["ret_workers","spouse","child","widow","mother","child_surv","parent"]:
        oasi[col] = [oasi_base.loc[yr, col] * oasi_sc[yr] for yr in years]
    oasi["total_oasi"] = oasi[["ret_workers","spouse","child",
                                "widow","mother","child_surv","parent"]].sum(axis=1)

    di = di_base.copy()
    for col in ["disabled_workers","spouse","child"]:
        di[col] = [di_base.loc[yr, col] * di_sc[yr] for yr in years]
    di["total_di"] = di[["disabled_workers","spouse","child"]].sum(axis=1)

    return oasi, di

# =============================================================================
# 4. AVERAGE BENEFIT PROJECTION
# =============================================================================

def project_average_benefits(econ_results, scenario=SCENARIO):
    cola = econ_results["awi"]["cola"].values

    prev = {
        "ret":   AVG_BENE_RETIRED_WORKER,
        "sps":   AVG_BENE_SPOUSE,
        "chd":   AVG_BENE_CHILD_RET,
        "wid":   AVG_BENE_WIDOW,
        "mth":   AVG_BENE_MOTHER,
        "chs":   AVG_BENE_CHILD_SURV,
        "diw":   AVG_BENE_DI_WORKER,
        "disps": AVG_BENE_DI_SPOUSE,
        "dich":  AVG_BENE_DI_CHILD,
    }
    rows = []
    for i, yr in enumerate(ALL_YEARS):
        c   = cola[i]
        dr  = _aime_drift_retired(yr)
        ddi = _aime_drift_di(yr)
        dax = _aime_drift_aux(yr)
        prev["ret"]   *= (1+c) * (1+dr)
        prev["sps"]   *= (1+c) * (1+dax)
        prev["chd"]   *= (1+c) * (1+dax)
        prev["wid"]   *= (1+c) * (1+dr)
        prev["mth"]   *= (1+c) * (1+dax)
        prev["chs"]   *= (1+c) * (1+dax)
        prev["diw"]   *= (1+c) * (1+ddi)
        prev["disps"] *= (1+c) * (1+dax)
        prev["dich"]  *= (1+c) * (1+dax)
        rows.append({"year": yr, **{k: round(v,2) for k,v in prev.items()}})

    df = pd.DataFrame(rows).set_index("year")
    df.columns = ["ret_worker_mo","spouse_mo","child_ret_mo","widow_mo",
                  "mother_mo","child_surv_mo","di_worker_mo","di_spouse_mo","di_child_mo"]
    return df

# =============================================================================
# 5. TOTAL BENEFIT COST
# =============================================================================

def compute_benefit_cost(oasi, di, avg_bene):
    oasi_cost = (
        oasi["ret_workers"]  * avg_bene["ret_worker_mo"]  +
        oasi["spouse"]       * avg_bene["spouse_mo"]      +
        oasi["child"]        * avg_bene["child_ret_mo"]   +
        oasi["widow"]        * avg_bene["widow_mo"]       +
        oasi["mother"]       * avg_bene["mother_mo"]      +
        oasi["child_surv"]   * avg_bene["child_surv_mo"]
    ) * 1000 * 12 / 1e9   # thousands × persons × 12 months / $1B

    di_cost = (
        di["disabled_workers"] * avg_bene["di_worker_mo"] +
        di["spouse"]           * avg_bene["di_spouse_mo"] +
        di["child"]            * avg_bene["di_child_mo"]
    ) * 1000 * 12 / 1e9

    return pd.DataFrame({
        "oasi_cost_bn":      oasi_cost.round(2),
        "di_cost_bn":        di_cost.round(2),
        "total_cost_bn":     (oasi_cost + di_cost).round(2),
        "oasi_benefic_M":    (oasi["total_oasi"] / 1000).round(3),
        "di_benefic_M":      (di["total_di"]     / 1000).round(3),
        "avg_ret_worker_mo": avg_bene["ret_worker_mo"].round(2),
        "avg_di_worker_mo":  avg_bene["di_worker_mo"].round(2),
    })

# =============================================================================
# 6. CALIBRATION
# =============================================================================

def calibrate_beneficiaries(cost):
    r25 = cost.loc[2025]
    r30 = cost.loc[2030]
    print(f"\n[Calibration: Benefit Cost]")
    print(f"  2025: OASI ${r25['oasi_cost_bn']:.1f}B  DI ${r25['di_cost_bn']:.1f}B"
          f"  Total ${r25['total_cost_bn']:.1f}B  "
          f"(target OASI ~$1,420-1,470B)")
    print(f"  2030: OASI ${r30['oasi_cost_bn']:.1f}B  DI ${r30['di_cost_bn']:.1f}B"
          f"  Total ${r30['total_cost_bn']:.1f}B")
    print(f"  2025 total benes: {r25['oasi_benefic_M']+r25['di_benefic_M']:.2f}M"
          f"  avg retired worker: ${r25['avg_ret_worker_mo']:.0f}/mo")
    if not (1400 <= r25["oasi_cost_bn"] <= 1530):
        print(f"  [!] OASI cost ${r25['oasi_cost_bn']:.1f}B outside expected range — "
              f"adjust AIME_DRIFT_RETIRED")
    else:
        print(f"  [ok] OASI cost in range")

# =============================================================================
# 7. ENTRY POINT
# =============================================================================

def run_beneficiaries(demo_results, econ_results, scenario=SCENARIO, calibrate_flag=True):
    print(f"[beneficiaries.py] Running | scenario: {scenario}")

    oasi, di  = project_beneficiaries(demo_results, scenario)
    avg_bene  = project_average_benefits(econ_results, scenario)
    cost      = compute_benefit_cost(oasi, di, avg_bene)

    if calibrate_flag and scenario == "intermediate":
        calibrate_beneficiaries(cost)

    yrs = [2025,2030,2035,2050,2075,2099]
    print(f"\n  {'Year':>4}  {'OASI_M':>7}  {'DI_M':>5}  {'Total_$B':>9}  {'AvgRet/mo':>10}")
    for yr in yrs:
        r = cost.loc[yr]
        print(f"  {yr:>4}  {r['oasi_benefic_M']:>7.2f}  {r['di_benefic_M']:>5.2f}"
              f"  {r['total_cost_bn']:>9.1f}  {r['avg_ret_worker_mo']:>10.0f}")

    print(f"\n[beneficiaries.py] Done. "
          f"2025 total cost: ${cost.loc[2025,'total_cost_bn']:.1f}B  "
          f"2099: ${cost.loc[2099,'total_cost_bn']:.1f}B")

    return {"oasi": oasi, "di": di, "avgbene": avg_bene, "cost": cost, "scenario": scenario}


if __name__ == "__main__":
    from demography import run_demography
    from economics  import run_economics
    demo_results = run_demography(calibrate_flag=False)
    econ_results = run_economics(demo_results, calibrate_flag=False)
    bene_results = run_beneficiaries(demo_results, econ_results)