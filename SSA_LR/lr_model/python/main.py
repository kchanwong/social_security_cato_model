"""
main.py — Full SSA Long-Range OASDI projection pipeline.
Runs all four modules in sequence and prints a summary dashboard.

Usage:
    python3 main.py
    python3 main.py --scenario low_cost
    python3 main.py --scenario high_cost

    python3 main.py --tfr 1.5
        # flat TFR of 1.5 for all years (intermediate econ/mortality)

    python3 main.py --tfr-series "2025:1.4,2035:1.7,2050:1.9,2099:1.9"
        # custom node path; linearly interpolated between nodes.
        # values outside the node range are clamped to the nearest endpoint.

    python3 main.py --tfr-file my_tfr.csv
        # CSV with columns 'year' and 'tfr' (one row per node).
        # same interpolation as --tfr-series.
"""

import sys
import argparse
import numpy as np
import pandas as pd

# ── Parse CLI arguments ────────────────────────────────────────────────────────
parser = argparse.ArgumentParser(description="SSA LR OASDI Projection Model")
parser.add_argument("--scenario", default="intermediate",
                    choices=["intermediate","low_cost","high_cost","custom"],
                    help="Actuarial scenario (default: intermediate)")
parser.add_argument("--tfr", type=float, default=None,
                    help="Custom flat TFR for all years. Forces --scenario=custom.")
parser.add_argument("--tfr-series", default=None,
                    help='TFR node path as "year:value,year:value,..." '
                         'e.g. "2025:1.4,2040:1.7,2099:1.9". '
                         'Linearly interpolated between nodes. '
                         'Forces --scenario=custom.')
parser.add_argument("--tfr-file", default=None,
                    help="CSV file with columns [year, tfr] defining TFR nodes "
                         "(linearly interpolated). Forces --scenario=custom.")
parser.add_argument("--calibrate", action="store_true", default=True,
                    help="Print calibration diagnostics (default: True)")
parser.add_argument("--no-calibrate", dest="calibrate", action="store_false")
args = parser.parse_args()

# ── Validate: at most one TFR override ────────────────────────────────────────
tfr_flags = sum(x is not None for x in [args.tfr, args.tfr_series, args.tfr_file])
if tfr_flags > 1:
    parser.error("Specify at most one of --tfr, --tfr-series, --tfr-file.")

# ── Build custom TFR nodes dict (if any override supplied) ────────────────────
import assumptions as _asm

def _apply_tfr_nodes(nodes: dict):
    """Validate, print, and inject node dict into TFR_NODES['custom']."""
    if not nodes:
        parser.error("TFR node dict is empty — check your input.")
    print("[main.py] Custom TFR nodes: "
          + ", ".join(f"{y}:{v:.3f}" for y, v in sorted(nodes.items())))
    _asm.TFR_NODES["custom"] = nodes

if args.tfr is not None:
    # Flat path: same value at every standard node year
    nodes = {yr: args.tfr for yr in _asm.TFR_NODES["intermediate"]}
    _apply_tfr_nodes(nodes)
    args.scenario = "custom"
    print(f"[main.py] Mode: flat TFR = {args.tfr:.2f} (all years)")

elif args.tfr_series is not None:
    # Inline series: "2025:1.4,2035:1.7,2050:1.9"
    try:
        nodes = {}
        for token in args.tfr_series.split(","):
            token = token.strip()
            if not token:
                continue
            yr_str, val_str = token.split(":")
            nodes[int(yr_str.strip())] = float(val_str.strip())
    except ValueError as exc:
        parser.error(f"Could not parse --tfr-series: {exc}. "
                     'Expected format: "2025:1.4,2040:1.7,2099:1.9"')
    _apply_tfr_nodes(nodes)
    args.scenario = "custom"
    print("[main.py] Mode: TFR series from --tfr-series")

elif args.tfr_file is not None:
    # CSV file: must have columns 'year' and 'tfr'
    try:
        df_tfr = pd.read_csv(args.tfr_file)
        df_tfr.columns = [c.strip().lower() for c in df_tfr.columns]
        if "year" not in df_tfr.columns or "tfr" not in df_tfr.columns:
            parser.error(f"--tfr-file must have columns 'year' and 'tfr'. "
                         f"Found: {list(df_tfr.columns)}")
        nodes = {int(r["year"]): float(r["tfr"]) for _, r in df_tfr.iterrows()}
    except FileNotFoundError:
        parser.error(f"--tfr-file not found: {args.tfr_file}")
    except Exception as exc:
        parser.error(f"Could not read --tfr-file: {exc}")
    _apply_tfr_nodes(nodes)
    args.scenario = "custom"
    print(f"[main.py] Mode: TFR series from file '{args.tfr_file}'")

SCENARIO = args.scenario

# ── Patch scenario into assumptions so all modules pick it up ──────────────────
import assumptions as _asm
_asm.SCENARIO = SCENARIO

# For custom scenario: copy economic/mortality assumptions from intermediate
if SCENARIO == "custom":
    for key in ["AWI_NOMINAL_PCT","CPI_PCT","EARNINGS_PCT_COMPENSATION",
                "LPR_NET","TUP_NET"]:
        d = getattr(_asm, key)
        if "custom" not in d:
            d["custom"] = d["intermediate"]
    for key in ["MORTALITY_ULT_REDUCTION","MORTALITY_ULTIMATE_YEAR",
                "UNEMPLOYMENT_ULT","REAL_INTEREST_RATE_ULT",
                "TOB_FACTOR_OASI","TOB_FACTOR_DI"]:
        d = getattr(_asm, key)
        if "custom" not in d:
            d["custom"] = d["intermediate"]
    import economics as _ec
    _ec.CWR_DRIFT["custom"] = _ec.CWR_DRIFT["intermediate"]
    _ec.UNEMP_PATH["custom"] = _ec.UNEMP_PATH["intermediate"]
    import trust_fund as _tf
    # inherit yield from intermediate
    _asm.NEW_ISSUE_YIELD["custom"] = _asm.NEW_ISSUE_YIELD["intermediate"]

# ── Run pipeline ───────────────────────────────────────────────────────────────
from demography    import run_demography
from economics     import run_economics
from beneficiaries import run_beneficiaries
from trust_fund    import run_trust_fund

print(f"\n{'='*65}")
print(f"  SSA OASDI Long-Range Projection — scenario: {SCENARIO.upper()}")
print(f"{'='*65}\n")

demo_results = run_demography(scenario=SCENARIO, calibrate_flag=args.calibrate)
econ_results = run_economics(demo_results, scenario=SCENARIO,
                              calibrate_flag=args.calibrate)
bene_results = run_beneficiaries(demo_results, econ_results,
                                  scenario=SCENARIO,
                                  calibrate_flag=args.calibrate)
tf_results   = run_trust_fund(demo_results, econ_results, bene_results,
                               scenario=SCENARIO,
                               calibrate_flag=args.calibrate)

# ── Summary dashboard ─────────────────────────────────────────────────────────
tf   = tf_results["tf"]
exh  = tf_results["exhaustion"]
demo = demo_results
econ = econ_results
bene = bene_results

print(f"\n{'='*65}")
print(f"  SUMMARY DASHBOARD — {SCENARIO.upper()}")
print(f"{'='*65}")

# Demography
print(f"\n  DEMOGRAPHY")
yrs = [2025,2035,2050,2075,2099]
print(f"  {'Year':>4}  {'Pop_M':>7}  {'AgedDep':>8}  {'TFR':>5}  {'e0M':>5}")
for yr in yrs:
    pop   = demo["pop_proj"][yr].sum() / 1000
    adr   = demo["dep_ratios"].loc[yr,"aged_dep_ratio"]
    e0m   = demo["life_exp"].loc[yr,"e0_male"]
    tfr_d = _asm.get_tfr_annual(SCENARIO)
    print(f"  {yr:>4}  {pop:>7.1f}  {adr:>8.3f}  {tfr_d[yr]:>5.2f}  {e0m:>5.1f}")

# Economics
print(f"\n  ECONOMICS")
awi_df = econ["awi"]; pay_df = econ["payroll"]; cw_df = econ["workers"]
print(f"  {'Year':>4}  {'AWI':>8}  {'CovW_M':>7}  {'TaxPay_T':>9}")
for yr in yrs:
    print(f"  {yr:>4}  {awi_df.loc[yr,'awi']:>8,.0f}  "
          f"{cw_df.loc[yr,'covered_total_k']/1000:>7.2f}  "
          f"{pay_df.loc[yr,'taxable_payroll_bn']/1000:>9.2f}")

# Trust fund cash flows
print(f"\n  TRUST FUND CASH FLOWS ($B nominal)")
print(f"  {'Year':>4}  {'OASI_Inc':>9}  {'OASI_Cst':>9}  {'OASI_Net':>9}"
      f"  {'DI_Inc':>7}  {'DI_Cst':>7}  {'OASI_Assets':>12}  {'DI_Assets':>10}")
for yr in yrs:
    r = tf.loc[yr]
    print(f"  {yr:>4}  {r['oasi_income_bn']:>9.1f}  {r['oasi_cost_bn']:>9.1f}"
          f"  {r['oasi_net_bn']:>+9.1f}"
          f"  {r['di_income_bn']:>7.1f}  {r['di_cost_bn']:>7.1f}"
          f"  {r['oasi_assets_bn']:>12.1f}  {r['di_assets_bn']:>10.1f}")

# Summarized rates
print(f"\n  INCOME / COST RATES  (% of modeled ETP)")
print(f"  {'Year':>4}  {'OASDI_Inc%':>10}  {'OASDI_Cst%':>10}  {'Net%':>6}"
      f"  {'OASI_TF%':>8}  {'DI_TF%':>7}")
for yr in yrs:
    r = tf.loc[yr]
    print(f"  {yr:>4}  {r['oasdi_income_rate']:>10.3f}  "
          f"{r['oasdi_cost_rate']:>10.3f}  "
          f"{r['oasdi_net_rate']:>+6.3f}  "
          f"{r['oasi_tf_ratio']:>8.1f}  "
          f"{r['di_tf_ratio']:>7.1f}")

# Exhaustion
print(f"\n  TRUST FUND EXHAUSTION")
print(f"  OASI:  {exh['oasi']}")
print(f"  DI:    {exh['di']}")
print(f"  OASDI: {exh['oasdi']}")

print(f"\n{'='*65}")
print(f"  Pipeline complete — scenario: {SCENARIO}")
print(f"{'='*65}\n")

# ── Export rates CSV ──────────────────────────────────────────────────────────
import os, pathlib

if args.tfr_file is not None:
    # Strip directory and extension: "path/to/my_tfr.csv" → "my_tfr"
    csv_stem = pathlib.Path(args.tfr_file).stem
elif args.tfr_series is not None:
    csv_stem = "tfr_series"
elif args.tfr is not None:
    csv_stem = f"tfr_{args.tfr:.3f}".replace(".", "p")
else:
    csv_stem = SCENARIO

out_name = f"TFR_PROJ_{csv_stem}.csv"
rates_df = tf[["oasdi_income_rate", "oasdi_cost_rate", "oasdi_net_rate"]].copy()
rates_df.index.name = "year"
rates_df.columns = ["income_rate", "cost_rate", "balance_rate"]
rates_df.to_csv(out_name)
print(f"[main.py] Rates CSV written → {out_name}\n")

# ── Return results dict (for import usage) ────────────────────────────────────
results = {
    "scenario":    SCENARIO,
    "demo":        demo_results,
    "econ":        econ_results,
    "bene":        bene_results,
    "tf":          tf_results,
}