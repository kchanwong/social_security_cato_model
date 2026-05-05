"""
household_formation.py
New households form via three triggers each simulation year:
  1. College departure  -- child aged 18-25 whose educ upgrades (hs->some_college
                          or some_college->ba_plus) moves out
  2. Marriage           -- two singles merge into one new household
  3. Other-adult move   -- relate=="other_adult" strikes out independently
"""

import numpy as np
import pandas as pd

# ─────────────────────────────────────────────────────────────────────────────
# LOAD MARRIAGE / DIVORCE RATES FROM SSA POPULATION FILE
# ─────────────────────────────────────────────────────────────────────────────
SSA_POP_PATH = (r"C:\Users\kritc\OneDrive\Documents\GitHub\social_security_cato_model"
                r"\TEST\Data_Input\SSPopJan_TR2023 (1).csv")

_BANDS     = ["15_24", "25_34", "35_44", "45_54", "55_64", "65_99"]
_BAND_CUTS = [14, 24, 34, 44, 54, 64, 120]


def _build_rates_by_year(path: str = SSA_POP_PATH) -> tuple[dict, dict]:
    """
    Returns (marriage_by_year, divorce_by_year).

    marriage_by_year[yr][(sex, band)] = target stock married fraction in year yr.
    divorce_by_year[yr][(sex, band)]  = annual P(married -> divorced) during
                                        the transition yr-1 -> yr.
    """
    df = pd.read_csv(path)
    df.columns = df.columns.str.replace(" ", ".", regex=False)
    df["m_mar_p"] = df["M.Mar"] / df["M.Tot"]
    df["f_mar_p"] = df["F.Mar"] / df["F.Tot"]
    df["m_div_p"] = df["M.Div"] / df["M.Tot"]
    df["f_div_p"] = df["F.Div"] / df["M.Tot"]
    df["band"]    = pd.cut(df["Age"], bins=_BAND_CUTS, labels=_BANDS)

    avail_years     = sorted(df["Year"].unique())
    marriage_by_year: dict = {}
    divorce_by_year:  dict = {}

    for yr in avail_years:
        yr_df = df[df["Year"] == yr]

        # ── Marriage target (stock fraction) ──────────────────────────────────
        mar_agg = yr_df.groupby("band", observed=True)[["m_mar_p", "f_mar_p"]].mean()
        mar: dict = {}
        for band in _BANDS:
            if band in mar_agg.index:
                mar[(1, band)] = float(mar_agg.loc[band, "m_mar_p"])
                mar[(2, band)] = float(mar_agg.loc[band, "f_mar_p"])
        marriage_by_year[yr] = mar

        # ── Divorce flow (transition yr-1 -> yr) ─────────────────────────────
        prev_yr = yr - 1
        if prev_yr in avail_years:
            prev_df = df[df["Year"] == prev_yr].copy()
            prev_df["Age"] += 1          # cohort shift: same people one year later
            merged = yr_df.merge(
                prev_df[["Age", "m_mar_p", "f_mar_p", "m_div_p", "f_div_p"]],
                on="Age", suffixes=("", "_prev")
            )
            merged["m_div_flow"] = (
                np.maximum(0, merged["m_div_p"] - merged["m_div_p_prev"])
                / np.maximum(merged["m_mar_p_prev"], 0.01)
            )
            merged["f_div_flow"] = (
                np.maximum(0, merged["f_div_p"] - merged["f_div_p_prev"])
                / np.maximum(merged["f_mar_p_prev"], 0.01)
            )
            merged["band"] = pd.cut(merged["Age"], bins=_BAND_CUTS, labels=_BANDS)
            div_agg = merged.groupby("band", observed=True)[
                ["m_div_flow", "f_div_flow"]
            ].mean()
            div: dict = {}
            for band in _BANDS:
                if band in div_agg.index:
                    div[(1, band)] = float(div_agg.loc[band, "m_div_flow"])
                    div[(2, band)] = float(div_agg.loc[band, "f_div_flow"])
            divorce_by_year[yr] = div
        else:
            divorce_by_year[yr] = {}

    return marriage_by_year, divorce_by_year


# Built once at import; per-year dicts cached here
_MARRIAGE_BY_YEAR, _DIVORCE_BY_YEAR = _build_rates_by_year()
_ALL_YEARS = sorted(_MARRIAGE_BY_YEAR.keys())

# Module-level fallback (average across all available years) for callers that
# don't pass a year — keeps backward compatibility with project_one_year.py.
def _avg(by_year: dict) -> dict:
    keys = set().union(*[d.keys() for d in by_year.values()])
    return {k: float(np.mean([d[k] for d in by_year.values() if k in d]))
            for k in keys}

_MAX_DIV_RATE = 0.020   # cap at 2% — 65_99 female averages 3.15% due to cohort artifact

MARRIAGE_RATES = _avg(_MARRIAGE_BY_YEAR)
DIVORCE_RATES  = {k: min(v, _MAX_DIV_RATE) for k, v in _avg(_DIVORCE_BY_YEAR).items()}


def get_rates_for_year(year: int) -> tuple[dict, dict]:
    """
    Return (marriage_rates, divorce_rates) for the given year.
    Falls back to the nearest available year if exact match is missing.
    """
    if year in _MARRIAGE_BY_YEAR:
        return _MARRIAGE_BY_YEAR[year], _DIVORCE_BY_YEAR.get(year, DIVORCE_RATES)

    nearest = min(_ALL_YEARS, key=lambda y: abs(y - year))
    return _MARRIAGE_BY_YEAR[nearest], _DIVORCE_BY_YEAR.get(nearest, DIVORCE_RATES)


P_OTHER_ADULT_MOVE = 0.15
MAX_AGE_GAP        = 10


# ─────────────────────────────────────────────────────────────────────────────
# HELPERS
# ─────────────────────────────────────────────────────────────────────────────

# Vectorized age-band lookup (avoids per-row Python calls)
_BAND_EDGES = np.array([0, 25, 35, 45, 55, 65, 1000], dtype=np.int32)
_BAND_NAMES = np.array(["15_24", "25_34", "35_44", "45_54", "55_64", "65_99"])


def _next_famunit_base(pop: pd.DataFrame) -> int:
    return int(pop["famunit"].max()) + 1



def _assign_new_famunits(pop: pd.DataFrame, idx, base: int) -> pd.DataFrame:
    """Give each member of idx their own new famunit, sequentially from base.
    idx may be a pandas Index or a numpy integer array of positional indices."""
    pop = pop.copy()
    n   = len(idx)
    fam_arr    = pop["famunit"].values.copy().astype(np.int64)
    relate_arr = pop["relate"].values.copy()
    fam_arr[idx]    = np.arange(base, base + n, dtype=np.int64)
    relate_arr[idx] = "head"
    pop["famunit"] = fam_arr
    pop["relate"]  = relate_arr
    return pop, base + n


# ─────────────────────────────────────────────────────────────────────────────
# TRIGGER 1 -- COLLEGE DEPARTURE
# ─────────────────────────────────────────────────────────────────────────────

EDUC_RANK = {"hs": 0, "some_college": 1, "ba_plus": 2}


def college_departures(pop: pd.DataFrame,
                       pop_prev: pd.DataFrame) -> pd.DataFrame:
    children = pop[
        (pop["relate"] == "child") &
        (pop["age"].between(18, 25)) &
        pop["educ"].isin(EDUC_RANK) &
        pop_prev["educ"].isin(EDUC_RANK)
    ].index

    if children.empty:
        return pop

    upgraded = children[
        pop.loc[children, "educ"].map(EDUC_RANK).values >
        pop_prev.loc[children, "educ"].map(EDUC_RANK).values
    ]

    if upgraded.empty:
        return pop

    base = _next_famunit_base(pop)
    pop, _ = _assign_new_famunits(pop, upgraded, base)
    return pop


# ─────────────────────────────────────────────────────────────────────────────
# TRIGGER 2 -- MARRIAGE
# ─────────────────────────────────────────────────────────────────────────────

def apply_marriages(pop: pd.DataFrame,
                    rng: np.random.Generator,
                    marriage_rates: dict | None = None) -> pd.DataFrame:
    """
    marriage_rates: {(sex, band): target_stock_married_fraction}.
    Defaults to module-level average if not supplied.

    Fully vectorized: bincount for current married %, np.isin for marital status
    filters, age-band batching for matching (O(n_unique_ages) vs O(n_men)).
    Assumes pop has a clean 0..N-1 RangeIndex (guaranteed by reset_index after deaths).
    """
    if marriage_rates is None:
        marriage_rates = MARRIAGE_RATES

    sex_v  = pop["sex"].values.astype(np.int8)
    age_v  = pop["age"].values.astype(np.int16)
    mar_v  = pop["marst"].values
    wt_v   = pop["perwt"].values.astype(np.float64)

    # ── Vectorized current married % by age for men (bincount) ───────────────
    men_mask = sex_v == 1
    m_ages   = age_v[men_mask]
    m_wt     = wt_v[men_mask]
    m_mar    = (mar_v[men_mask] == "married").astype(np.float64) * m_wt
    if m_ages.size == 0:
        return pop
    age_min = int(m_ages.min())
    age_max = int(m_ages.max())
    age_idx = (m_ages - age_min).astype(np.intp)
    n_bins  = age_max - age_min + 1
    wt_by_age  = np.bincount(age_idx, weights=m_wt,  minlength=n_bins)
    mar_by_age = np.bincount(age_idx, weights=m_mar, minlength=n_bins)
    pct_by_age = np.divide(mar_by_age, wt_by_age,
                           out=np.zeros_like(wt_by_age), where=wt_by_age > 0)

    # ── Eligible single men (np.isin avoids per-row Python calls) ────────────
    single_v = np.isin(mar_v, ["single", "divorced"])
    sm_mask  = men_mask & single_v & (age_v >= 18) & (age_v <= 70)
    sm_pos   = np.where(sm_mask)[0]
    if sm_pos.size == 0:
        return pop

    sm_ages    = age_v[sm_pos]
    band_idx_m = np.clip(np.searchsorted(_BAND_EDGES, sm_ages, side="right") - 1, 0, 5)
    target     = np.array([marriage_rates.get((1, _BAND_NAMES[bi]), 0.0)
                           for bi in band_idx_m])
    age_off    = np.clip(sm_ages - age_min, 0, n_bins - 1)
    cur_pct    = pct_by_age[age_off]
    marry_p    = np.clip(target - cur_pct, 0.0, None)

    will_pos  = sm_pos[rng.random(sm_pos.size) <= marry_p]
    if will_pos.size == 0:
        return pop

    # ── Eligible single women ─────────────────────────────────────────────────
    sw_mask = (sex_v == 2) & single_v & (age_v >= 18) & (age_v <= 70)
    sw_pos  = np.where(sw_mask)[0]
    if sw_pos.size == 0:
        pop = pop.copy()
        mar_v2 = mar_v.copy(); mar_v2[will_pos] = "married"
        pop["marst"] = mar_v2
        return pop

    # Shuffle women, sort by age for age-band range queries
    perm           = rng.permutation(sw_pos.size)
    sw_shuf        = sw_pos[perm]
    sw_ages_sorted = age_v[sw_shuf]           # pre-shuffled → sort once
    sord           = np.argsort(sw_ages_sorted, kind="stable")
    sw_sorted      = sw_shuf[sord]
    sw_ages_sorted = sw_ages_sorted[sord]
    taken          = np.zeros(sw_sorted.size, dtype=bool)

    # ── Match by unique age (O(n_unique_ages) ≈ 50, not O(n_men) ≈ 1000) ────
    will_ages  = age_v[will_pos]
    m_matched_list: list = []
    w_matched_list: list = []

    for uage in np.unique(will_ages):
        m_this = will_pos[will_ages == uage]
        lo = int(np.searchsorted(sw_ages_sorted, int(uage) - MAX_AGE_GAP, side="left"))
        hi = int(np.searchsorted(sw_ages_sorted, int(uage) + MAX_AGE_GAP, side="right"))
        avail = np.where(~taken[lo:hi])[0]
        if avail.size == 0:
            continue
        avail_global = avail + lo
        n_pairs = min(m_this.size, avail_global.size)
        rng.shuffle(m_this)
        chosen = rng.choice(avail_global, size=n_pairs, replace=False)
        taken[chosen] = True
        m_matched_list.append(m_this[:n_pairs])
        w_matched_list.append(sw_sorted[chosen])

    pop = pop.copy()
    mar_v2   = mar_v.copy()
    relate_v = pop["relate"].values.copy()
    fam_arr  = pop["famunit"].values.copy().astype(np.int64)

    if m_matched_list:
        m_matched = np.concatenate(m_matched_list)
        w_matched = np.concatenate(w_matched_list)

        # Vectorized famunit remap: woman's household → man's household
        old_fams = fam_arr[w_matched]
        new_fams = fam_arr[m_matched]
        sort_idx = np.argsort(old_fams)
        old_s    = old_fams[sort_idx]
        new_s    = new_fams[sort_idx]
        pos      = np.searchsorted(old_s, fam_arr, side="left")
        pc       = np.clip(pos, 0, old_s.size - 1)
        hit      = old_s[pc] == fam_arr
        fam_arr[hit] = new_s[pc[hit]]

        relate_v[w_matched] = "spouse"
        mar_v2[m_matched]   = "married"
        mar_v2[w_matched]   = "married"

        unmatched = np.setdiff1d(will_pos, m_matched, assume_unique=True)
        if unmatched.size:
            mar_v2[unmatched] = "married"
    else:
        mar_v2[will_pos] = "married"

    pop["famunit"] = fam_arr
    pop["relate"]  = relate_v
    pop["marst"]   = mar_v2
    return pop


# ─────────────────────────────────────────────────────────────────────────────
# DIVORCE
# ─────────────────────────────────────────────────────────────────────────────

def apply_divorces(pop: pd.DataFrame,
                   rng: np.random.Generator,
                   divorce_rates: dict | None = None) -> pd.DataFrame:
    """
    divorce_rates: {(sex, band): annual_P(married->divorced)}.
    Defaults to module-level average if not supplied.
    """
    if divorce_rates is None:
        divorce_rates = DIVORCE_RATES

    pop      = pop.copy()
    relate_v = pop["relate"].values
    marst_v  = pop["marst"].values
    age_v    = pop["age"].values.astype(np.int16)
    fam_v    = pop["famunit"].values.astype(np.int64)

    sp_pos = np.where((relate_v == "spouse") & (marst_v == "married"))[0]
    if sp_pos.size == 0:
        return pop

    sp_ages  = age_v[sp_pos]
    band_idx = np.clip(np.searchsorted(_BAND_EDGES, sp_ages, side="right") - 1, 0, 5)
    rates    = np.array([divorce_rates.get((2, _BAND_NAMES[bi]), 0.0)
                         for bi in band_idx])
    div_pos  = sp_pos[rng.random(sp_pos.size) < rates]
    if div_pos.size == 0:
        return pop

    div_fams  = set(fam_v[div_pos].tolist())
    head_pos  = np.where((relate_v == "head") &
                         np.isin(fam_v, list(div_fams)))[0]

    marst_v2 = marst_v.copy()
    marst_v2[head_pos] = "divorced"
    marst_v2[div_pos]  = "divorced"
    pop["marst"] = marst_v2

    base = _next_famunit_base(pop)
    pop, _ = _assign_new_famunits(pop, div_pos, base)
    return pop


# ─────────────────────────────────────────────────────────────────────────────
# TRIGGER 3 -- OTHER-ADULT MOVES OUT
# ─────────────────────────────────────────────────────────────────────────────

def other_adult_moves(pop: pd.DataFrame, rng: np.random.Generator) -> pd.DataFrame:
    eligible = pop[pop["relate"] == "other_adult"].index
    if eligible.empty:
        return pop

    movers = eligible[rng.random(len(eligible)) < P_OTHER_ADULT_MOVE]
    if movers.empty:
        return pop

    base = _next_famunit_base(pop)
    pop, _ = _assign_new_famunits(pop, movers, base)
    return pop


# ─────────────────────────────────────────────────────────────────────────────
# MAIN ENTRY POINT
# ─────────────────────────────────────────────────────────────────────────────

def form_households(pop: pd.DataFrame,
                    pop_prev: pd.DataFrame,
                    year: int,
                    rng: np.random.Generator,
                    marriage_rates: dict | None = None,
                    divorce_rates:  dict | None = None) -> pd.DataFrame:
    """
    Apply all three household-formation triggers in sequence.
    Pass marriage_rates / divorce_rates for year-specific calibration;
    omit to use module-level historical averages.
    """
    n_before = pop["famunit"].nunique()

    pop = college_departures(pop, pop_prev)
    pop = apply_marriages(pop, rng, marriage_rates=marriage_rates)
    pop = apply_divorces(pop,  rng, divorce_rates=divorce_rates)
    pop = other_adult_moves(pop, rng)

    n_after = pop["famunit"].nunique()
    print(f"[{year}] household formation: {n_after - n_before:+d} new households "
          f"({n_before} -> {n_after})")
    return pop
