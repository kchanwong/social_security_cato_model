"""
project_one_year.py
Projects initial_simulation.csv forward by one year using:
  - Education transitions  (edu_transition_probs_cohort.csv)
  - Mortality              (death_and_births.py)
  - Fertility              (death_and_births.py)
  - Marriage / divorce     (household_formation.py)
  - Household formation    (household_formation.py)
"""

import sys
import numpy as np
import pandas as pd

sys.path.insert(0, r"C:\Users\kchanwong\Documents\GitHub\social_security_cato_model\TEST\Py_file")
from death_and_births   import load_life_tables, load_income_mortality_gradient, apply_deaths, make_babies, load_asfr
from household_formation import apply_marriages, apply_divorces, form_households

BASE_DIR = r"C:\Users\kchanwong\Documents\GitHub\social_security_cato_model\TEST"
RNG      = np.random.default_rng(20080101)

BASE_YEAR = 2008
PROJ_YEAR = BASE_YEAR + 1

EDUC_ORDER  = {"hs": 0, "some_college": 1, "ba_plus": 2}
EDUC_LEVELS = ["hs", "some_college", "ba_plus"]

# ─────────────────────────────────────────────────────────────────────────────
# 1. LOAD
# ─────────────────────────────────────────────────────────────────────────────
pop = pd.read_csv(f"{BASE_DIR}/Data_Output/initial_simulation.csv")

# Runtime columns not in the CSV
pop["year"]        = BASE_YEAR
pop["ped"]         = 0.0
pop["aime"]        = 0.0
pop["predict_hat"] = np.nan

edu_trans = pd.read_csv(f"{BASE_DIR}/Data_Output/edu_transition_probs_cohort.csv")

lt   = load_life_tables()
grad = load_income_mortality_gradient()

print(f"Loaded {len(pop):,} people.  Projecting {BASE_YEAR} -> {PROJ_YEAR}.")

# ─────────────────────────────────────────────────────────────────────────────
# 2. HELPERS
# ─────────────────────────────────────────────────────────────────────────────

def _cohort_grp(birth_year: pd.Series) -> pd.Series:
    """Map birth year to 5-year cohort label matching educ_transition output."""
    bins   = list(range(1895, 2011, 5))
    labels = [f"{lo}-{lo+4}" for lo in range(1895, 2006, 5)]
    return pd.cut(birth_year, bins=bins, labels=labels, right=True).astype(str)


def _age_grp(age: pd.Series) -> pd.Series:
    breaks = [18, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90]
    labels = ["19-25","26-30","31-35","36-40","41-45","46-50",
              "51-55","56-60","61-65","66-70","71-75","76-80","81-85","86-90"]
    return pd.cut(age, bins=breaks, labels=labels, right=True).astype(str)


# Pre-compute average prob_upgrade across years for stable lookup
edu_lookup = (
    edu_trans
    .groupby(["sex", "cohort_grp", "age_grp", "educ"], observed=True)["prob_upgrade"]
    .mean()
    .reset_index()
)

# ─────────────────────────────────────────────────────────────────────────────
# 3. AGE EVERYONE BY 1
# ─────────────────────────────────────────────────────────────────────────────
pop_prev = pop.copy()   # keep t-1 snapshot for household_formation
pop["age"]  = pop["age"] + 1
pop["year"] = PROJ_YEAR

# ─────────────────────────────────────────────────────────────────────────────
# 4. EDUCATION TRANSITIONS
#    For each person with educ in {hs, some_college} and age 18-65,
#    look up prob_upgrade and draw.
# ─────────────────────────────────────────────────────────────────────────────
upgradeable = pop[
    pop["educ"].isin(["hs", "some_college"]) &
    pop["age"].between(18, 65)
].copy()

orig_idx = upgradeable.index  # save before merge resets it to 0-based

upgradeable["cohort_grp"] = _cohort_grp(PROJ_YEAR - upgradeable["age"])
upgradeable["age_grp"]    = _age_grp(upgradeable["age"])

upgradeable = upgradeable.merge(
    edu_lookup,
    on=["sex", "cohort_grp", "age_grp", "educ"],
    how="left"
)
upgradeable["prob_upgrade"] = upgradeable["prob_upgrade"].fillna(0.0)

draws   = RNG.random(len(upgradeable))
upgrade = orig_idx[draws < upgradeable["prob_upgrade"].values]

def _next_educ(educ_series: pd.Series) -> pd.Series:
    return educ_series.map({"hs": "some_college", "some_college": "ba_plus"})

pop.loc[upgrade, "educ"] = _next_educ(pop.loc[upgrade, "educ"])

n_upgraded = len(upgrade)
print(f"  Education upgrades: {n_upgraded:,}")

# ─────────────────────────────────────────────────────────────────────────────
# 5. DEATHS
# ─────────────────────────────────────────────────────────────────────────────
n_before = len(pop)
pop = apply_deaths(pop, lt, grad, PROJ_YEAR)
print(f"  Deaths: {n_before - len(pop):,}  (pop {n_before:,} -> {len(pop):,})")

# ─────────────────────────────────────────────────────────────────────────────
# 6. BIRTHS
# ─────────────────────────────────────────────────────────────────────────────
n_before = len(pop)
pop = make_babies(pop, PROJ_YEAR, load_asfr(PROJ_YEAR))
print(f"  Births: {len(pop) - n_before:,}  (pop now {len(pop):,})")

# ─────────────────────────────────────────────────────────────────────────────
# 7. MARRIAGE + DIVORCE
# ─────────────────────────────────────────────────────────────────────────────
n_fam_before = pop["famunit"].nunique()
pop = apply_marriages(pop, RNG)
pop = apply_divorces(pop, RNG)
print(f"  Marriage/divorce: {pop['famunit'].nunique() - n_fam_before:+,} households")

# ─────────────────────────────────────────────────────────────────────────────
# 8. HOUSEHOLD FORMATION  (college departures + other-adult moves)
# ─────────────────────────────────────────────────────────────────────────────
# Align pop_prev index to current pop (deaths may have removed rows)
pop_prev_aligned = pop_prev.reindex(pop.index)
pop = form_households(pop, pop_prev_aligned, PROJ_YEAR, RNG)

# ─────────────────────────────────────────────────────────────────────────────
# 9. SAVE
# ─────────────────────────────────────────────────────────────────────────────
out_cols = ["famunit","perwt","hhwt","marst","age","school","sex","educ",
            "labforce","employed","retired","incwage","receive_ss","cohort",
            "relate","year","ped","aime","predict_hat"]
out_cols = [c for c in out_cols if c in pop.columns]

out_path = f"{BASE_DIR}/Data_Output/projected_{PROJ_YEAR}.csv"
pop[out_cols].to_csv(out_path, index=False)
print(f"\nSaved {len(pop):,} people -> {out_path}")

# ─────────────────────────────────────────────────────────────────────────────
# 10. VALIDATION vs SSA TARGETS
# ─────────────────────────────────────────────────────────────────────────────
ssa = pd.read_csv(f"{BASE_DIR}/Data_Input/SSPopJan_TR2023 (1).csv")
ssa.columns = ssa.columns.str.replace(" ", ".", regex=False)
ssa_yr = ssa[ssa["Year"] == PROJ_YEAR]
ssa_tot = ssa_yr["Total"].sum()

# SSA targets
ssa_cbr  = ssa_yr[ssa_yr["Age"] == 0]["Total"].values[0] / ssa_tot * 1000
ssa_mar  = (ssa_yr["M.Mar"].sum() + ssa_yr["F.Mar"].sum()) / ssa_tot
ssa_div  = (ssa_yr["M.Div"].sum() + ssa_yr["F.Div"].sum()) / ssa_tot
ssa_dep  = ((ssa_yr[ssa_yr["Age"] <  18]["Total"].sum() +
             ssa_yr[ssa_yr["Age"] >= 65]["Total"].sum()) /
             ssa_yr[ssa_yr["Age"].between(18, 64)]["Total"].sum())

# All simulation metrics use perwt throughout
wt      = pop["perwt"].values.astype(float)
sim_tot = wt.sum()

babies   = pop["age"].values == 0
sim_cbr  = (wt * babies).sum() / sim_tot * 1000

married  = pop["marst"].values == "married"
divorced = pop["marst"].values == "divorced"
sim_mar  = (wt * married).sum()  / sim_tot
sim_div  = (wt * divorced).sum() / sim_tot

young_w = (wt * (pop["age"].values <  18)).sum()
work_w  = (wt * ((pop["age"].values >= 18) & (pop["age"].values <= 64))).sum()
old_w   = (wt * (pop["age"].values >= 65)).sum()
sim_dep = (young_w + old_w) / work_w

def _fmt(sim, ssa, fmt=".4f"):
    diff = sim - ssa
    flag = "  <<<" if abs(diff / (ssa + 1e-9)) > 0.10 else ""
    return f"  sim={sim:{fmt}}  ssa={ssa:{fmt}}  diff={diff:+.4f}{flag}"

print(f"\n-- Validation vs SSA {PROJ_YEAR} (all weighted by perwt) --------")
print(f"  Crude birth rate (/1000) : {_fmt(sim_cbr, ssa_cbr, '.2f')}")
print(f"  Pct married              : {_fmt(sim_mar, ssa_mar)}")
print(f"  Pct divorced             : {_fmt(sim_div, ssa_div)}")
print(f"  Dependency ratio         : {_fmt(sim_dep, ssa_dep)}")
