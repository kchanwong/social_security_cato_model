"""
economics.py — AWI, taxable maximum, covered workers, taxable payroll, yield.
Replicates SSA Process 2. Calibration target: TR2025 Table V.B1.
"""

import numpy as np
import pandas as pd
from assumptions import (
    SCENARIO, ALL_YEARS, FIRST_PROJ_YEAR, FINAL_PROJ_YEAR,
    OASI_EE_RATE, DI_EE_RATE, OASDI_EE_RATE,
    get_awi_pct, get_cpi_pct, get_earn_drift, get_yield,
    AWI_NOMINAL_PCT,
)

# =============================================================================
# 0. HISTORICAL ANCHORS (2024)
# =============================================================================

AWI_2024              = 63795.13 * (1 + 4.21/100)   # ≈ $66,479
AWI_2023              = 63795.13
AWI_2022              = 60575.07
TAXMAX_2024           = 168600
CPI_2024              = 314.5

COVERED_WORKERS_M_K   = 162000    # thousands (~162M wage/salary)
COVERED_WORKERS_SE_K  =  15500    # thousands (~15.5M self-employed)
COVERED_WORKERS_TOT_K = COVERED_WORKERS_M_K + COVERED_WORKERS_SE_K
COVERED_EARNINGS_2024 = 10850e9   # dollars
TAXABLE_PAYROLL_2024  = 10430e9   # dollars — derived from TR2025 VI.A: OASI $1,105.6B + DI ~$187.7B = $1,293.3B ÷ 0.124

ACE_BASE_2024 = COVERED_EARNINGS_2024 / (COVERED_WORKERS_TOT_K * 1000)  # ≈$61,127

CWR_AGG_BASE  = COVERED_WORKERS_TOT_K / 199564   # ≈ 0.889
CWR_SE_SHARE  = COVERED_WORKERS_SE_K  / COVERED_WORKERS_TOT_K   # ≈ 0.087

# Transitional taxable ratio:
# 2024 actual: $10,430B / $10,850B = 0.961 (derived from TR2025 VI.A payroll taxes)
# 2034 Trustees' assumption: ETP/covered_earnings = 0.825 (SSA LR Doc section 2.4.b, terminal year)
# Linear interpolation 2024→2034, constant at 0.825 from 2034 onward
TAXRATIO_2024 = TAXABLE_PAYROLL_2024 / COVERED_EARNINGS_2024  # ≈ 0.961
TAXRATIO_TERM = 0.825   # Trustees' long-run assumption
TAXRATIO_TERM_YEAR = 2034

# =============================================================================
# 1. AWI
# =============================================================================

def build_awi_series(scenario=SCENARIO):
    awi, cpi_idx, cola_arr = [], [], []
    awi_prev  = AWI_2024
    cpi_prev  = CPI_2024

    for yr in ALL_YEARS:
        awi_pct_yr = get_awi_pct(yr, scenario)
        cpi_pct_yr = get_cpi_pct(yr, scenario)

        awi_prev  *= (1 + awi_pct_yr / 100)
        cpi_prev  *= (1 + cpi_pct_yr / 100)

        awi.append(awi_prev)
        cpi_idx.append(cpi_prev)
        cola_arr.append(cpi_pct_yr / 100)

    awi_arr = np.array(awi)
    awi_lag = np.concatenate([[AWI_2024], awi_arr[:-1]])
    cpi_arr = np.array(cpi_idx)
    cpi_lag = np.concatenate([[CPI_2024], cpi_arr[:-1]])

    nom_pct  = (awi_arr / awi_lag - 1) * 100
    real_pct = (awi_arr / awi_lag) / (cpi_arr / cpi_lag) - 1

    return pd.DataFrame({
        "year":              ALL_YEARS,
        "awi":               np.round(awi_arr, 2),
        "awi_nominal_pct":   np.round(nom_pct, 4),
        "real_awi_pct":      np.round(real_pct * 100, 4),
        "cpi_index":         np.round(cpi_arr, 2),
        "cola":              np.array(cola_arr),
    }).set_index("year")

# =============================================================================
# 2. TAXABLE MAXIMUM
# =============================================================================

def build_taxmax_series(awi_df, scenario=SCENARIO):
    awi_hist = {2022: AWI_2022, 2023: AWI_2023, 2024: AWI_2024}
    awi_proj = {yr: awi_df.loc[yr, "awi"] for yr in ALL_YEARS}
    awi_all  = {**awi_hist, **awi_proj}

    taxmax = []
    prev = TAXMAX_2024
    for i, yr in enumerate(ALL_YEARS):
        if i == 0:
            raw = TAXMAX_2024 * awi_all[2023] / awi_all[2022]
        else:
            raw = prev * awi_all[yr - 2] / awi_all[yr - 3]
        prev = round(raw / 300) * 300
        taxmax.append(prev)

    return pd.DataFrame({"year": ALL_YEARS,
                         "taxmax": taxmax}).set_index("year")

# =============================================================================
# 3. COVERED WORKERS
# =============================================================================

CWR_DRIFT = {"intermediate": -0.00050, "low_cost": +0.00030, "high_cost": -0.00120}

UNEMP_PATH = {
    "intermediate": {**{y: 0.045 for y in ALL_YEARS}, 2025: 0.044},
    "low_cost":     {**{y: 0.035 for y in ALL_YEARS}, 2025: 0.042, 2026: 0.038, 2027: 0.036},
    "high_cost":    {**{y: 0.055 for y in ALL_YEARS}, 2025: 0.048, 2026: 0.052, 2027: 0.054},
}

def build_covered_workers(wap_series, scenario=SCENARIO):
    drift = CWR_DRIFT[scenario]
    unemp = UNEMP_PATH[scenario]

    cw_wage, cw_se = [], []
    for i, yr in enumerate(ALL_YEARS):
        cwr = max(CWR_AGG_BASE + drift * (yr - 2024), 0.75)
        total_cw = wap_series.iloc[i] * cwr
        cw_se.append(total_cw * CWR_SE_SHARE)
        cw_wage.append(total_cw * (1 - CWR_SE_SHARE))

    cw_wage = np.array(cw_wage)
    cw_se   = np.array(cw_se)
    return pd.DataFrame({
        "year":            ALL_YEARS,
        "wap_k":           wap_series.values,
        "covered_wage_k":  np.round(cw_wage, 1),
        "covered_se_k":    np.round(cw_se, 1),
        "covered_total_k": np.round(cw_wage + cw_se, 1),
    }).set_index("year")

# =============================================================================
# 4. TAXABLE PAYROLL
# =============================================================================

def taxable_ratio(year):
    """Transitional taxable ratio: 0.961 in 2024 → 0.825 by 2034, constant thereafter.
    Anchored to TR2025 VI.A historical payroll taxes and SSA LR Doc 2.4.b terminal assumption."""
    if year <= 2024:
        return TAXRATIO_2024
    elif year >= TAXRATIO_TERM_YEAR:
        return TAXRATIO_TERM
    else:
        t = (year - 2024) / (TAXRATIO_TERM_YEAR - 2024)
        return TAXRATIO_2024 + t * (TAXRATIO_TERM - TAXRATIO_2024)

def build_taxable_payroll(cw_df, awi_df, taxmax_df, scenario=SCENARIO):
    ace_prev = ACE_BASE_2024

    ace_arr, txwage, txse, txpay, eff_pay = [], [], [], [], []

    tx_ratio_arr = []
    for i, yr in enumerate(ALL_YEARS):
        awi_pct = awi_df.loc[yr, "awi_nominal_pct"]
        if np.isnan(awi_pct):
            awi_pct = AWI_NOMINAL_PCT[scenario]["ultimate"]
        earn_drift = get_earn_drift(yr, scenario)

        ace = ace_prev * (1 + awi_pct / 100 + earn_drift / 100)
        ace_arr.append(ace)

        tx_ratio = taxable_ratio(yr)
        tx_ratio_arr.append(tx_ratio)

        # Wage workers taxable earnings (billions)
        cw_wage_n = cw_df.loc[yr, "covered_wage_k"] * 1000
        tw = ace * cw_wage_n * tx_ratio / 1e9

        # Self-employed (SE contributes on 92.35% of net income)
        cw_se_n = cw_df.loc[yr, "covered_se_k"] * 1000
        ts = ace * cw_se_n * tx_ratio * 0.920 / 1e9

        tp  = tw + ts
        eff = tp * 0.9997

        txwage.append(tw); txse.append(ts)
        txpay.append(tp);  eff_pay.append(eff)

        ace_prev = ace

    tm_awi_ratio = taxmax_df["taxmax"].values / awi_df["awi"].values

    return pd.DataFrame({
        "year":                    ALL_YEARS,
        "avg_covered_earn":        np.round(ace_arr, 2),
        "taxable_wages_bn":        np.round(txwage, 2),
        "taxable_se_bn":           np.round(txse, 2),
        "taxable_payroll_bn":      np.round(txpay, 2),
        "eff_taxable_payroll_bn":  np.round(eff_pay, 2),
        "taxmax_awi_ratio":        np.round(tm_awi_ratio, 4),
        "taxable_ratio":           np.round(tx_ratio_arr, 4),
    }).set_index("year")

# =============================================================================
# 5. YIELD
# =============================================================================

def build_yield_series(scenario=SCENARIO):
    yields = [get_yield(yr, scenario) for yr in ALL_YEARS]
    return pd.DataFrame({
        "year":             ALL_YEARS,
        "new_issue_yield":  yields,
    }).set_index("year")

# =============================================================================
# 6. CALIBRATION
# =============================================================================

def calibrate_economics(econ):
    sc = econ["scenario"]
    if sc != "intermediate":
        return

    # AWI % check vs. V.B1
    bench_awi = {2025:3.97,2026:4.13,2027:4.03,2028:4.11,2029:3.94,
                 2030:3.88,2031:3.93,2032:3.95,2033:3.96,2034:3.85}
    awi_df = econ["awi"]
    print("\n[Calibration: AWI % vs. V.B1]")
    print(f"  {'Year':>4}  {'V.B1':>6}  {'Model':>6}  {'Diff':>6}")
    max_diff = 0
    for yr, bench in bench_awi.items():
        mod = awi_df.loc[yr, "awi_nominal_pct"]
        diff = mod - bench
        max_diff = max(max_diff, abs(diff))
        print(f"  {yr:>4}  {bench:>6.2f}  {mod:>6.2f}  {diff:>+6.3f}")
    print(f"  Max AWI deviation: {max_diff:.4f} ppt")

    # Taxable payroll 2025
    pay_2025 = econ["payroll"].loc[2025, "taxable_payroll_bn"] / 1000
    print(f"\n  2025 taxable payroll (model): ${pay_2025:.2f}T  |  SR estimate: ~$9.85T"
          f"  |  diff: {100*(pay_2025-9.85)/9.85:+.1f}%")

# =============================================================================
# 7. ENTRY POINT
# =============================================================================

def run_economics(demo_results, scenario=SCENARIO, calibrate_flag=True):
    print(f"[economics.py] Running economics module | scenario: {scenario}")

    wap       = demo_results["working_age_pop"]
    awi_df    = build_awi_series(scenario)
    taxmax_df = build_taxmax_series(awi_df, scenario)
    cw_df     = build_covered_workers(wap, scenario)
    pay_df    = build_taxable_payroll(cw_df, awi_df, taxmax_df, scenario)
    yield_df  = build_yield_series(scenario)

    econ = {"awi": awi_df, "taxmax": taxmax_df, "workers": cw_df,
            "payroll": pay_df, "yield": yield_df, "scenario": scenario}

    if calibrate_flag:
        calibrate_economics(econ)

    yrs = [2025, 2035, 2050, 2075, 2099]
    print(f"\n[economics.py] Key series:")
    print(f"  {'Year':>4}  {'AWI':>8}  {'TaxMax':>8}  {'CovWkrs_M':>10}  {'TaxPay_T':>9}  {'Yield%':>7}")
    for yr in yrs:
        print(f"  {yr:>4}  "
              f"{awi_df.loc[yr,'awi']:>8,.0f}  "
              f"{taxmax_df.loc[yr,'taxmax']:>8,.0f}  "
              f"{cw_df.loc[yr,'covered_total_k']/1000:>10.2f}  "
              f"{pay_df.loc[yr,'taxable_payroll_bn']/1000:>9.2f}  "
              f"{yield_df.loc[yr,'new_issue_yield']*100:>7.2f}")

    print(f"\n[economics.py] Done. 2025 AWI: ${awi_df.loc[2025,'awi']:,.0f}"
          f"  |  2025 taxable payroll: ${pay_df.loc[2025,'taxable_payroll_bn']/1000:.2f}T")
    return econ

if __name__ == "__main__":
    from demography import run_demography
    demo_results = run_demography(calibrate_flag=False)
    econ_results = run_economics(demo_results)
