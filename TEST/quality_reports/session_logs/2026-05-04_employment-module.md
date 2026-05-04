# Session Log — Employment Module Build
**Date:** 2026-05-04  
**Branch:** NEW_LT_MODEL  
**Goal:** Build LFPR transitions, McCall employment model, and BLS unemployment calibration for the Social Security microsimulation.

---

## Summary of Work

This session (and the prior session it continues from) built out the full economic module for the Social Security microsimulation. The work proceeded in three phases:

1. **LFPR transitions** — CPS-derived annual transition probabilities, base-year calibration to BLS Dec-2008
2. **McCall employment model** — Bellman solver calibrated to CPS unemployment/separation/wage data
3. **BLS unemployment calibration** — Post-McCall adjustment to match BLS education-specific unemployment rates

---

## Phase 1: LFPR Transitions

### Data
- CPS ASEC 2008–2025 pulled via IPUMS API (`ipumsr::define_extract_micro(collection='cps', ...)`)
- Adjacent-year linking via `CPSIDV`
- Produces `lf_transition_prob.csv`: P(LF_t+1 = yes | sex, age, lf_lag, year_t)
- File: [`R_file/lfpr_transition.r`](../../R_file/lfpr_transition.r)

### Base-year calibration
- Problem: `initial_simulation.csv` (raw CPS 2008 ASEC) had LFPR 25-54 = 86.9% vs BLS Dec-2008 = 82.8%
- Fix: Stochastically flip excess `labforce="yes"` → `"no"` in prime-age group until weighted LFPR hits target
- Location: [`Py_file/project_multi_year.py`](../../Py_file/project_multi_year.py) lines ~77–91

### BLS target
- Series: `LNS11300060` (prime-age 25-54 LFPR, seasonally adjusted)
- Use **December** values to match end-of-year population snapshot
- File: [`Data_Input/LNS11300060.csv`](../../Data_Input/LNS11300060.csv)

### Result
- LFPR MAE: **1.19pp** over 2009–2024

---

## Phase 2: McCall Employment Model

### Model
Bellman iteration for McCall job-search with separation at **monthly frequency**:

```
V(w) = u(w) + β_m [(1 - α_m) V(w) + α_m U]
U    = u(c) + β_m [(1 - γ_m) U  + γ_m Σ max{U, V(w_i)} p_i]
```

Reservation wage `w̄` determined endogenously. Monthly frequency required because at annual rates, λ = α(1-u*)/u* can exceed 1 for low-unemployment cells.

### Calibration
- **Cell definition:** sex × educ × age_band × year (648 cells)
- **Separation rate (α):** Year-specific from CPS adjacent-year links (P(employed→unemployed))
- **Offer arrival rate (γ):** Bisected to hit year-specific CPS unemployment rate in monthly steady state
- **Annual simulation probabilities** (via Poisson compounding):
  - `p_sep_annual = 1 - (1 - α_m)^12`
  - `p_find_annual = 1 - (1 - γ_m × p_accept)^12`
- **Steady-state employment rate:** `emp_rate = λ_m / (α_m + λ_m)` — used for new LF entrants

### Data pipeline
File: [`R_file/mccall_data.r`](../../R_file/mccall_data.r)  
Produces:
- `Data_Output/unemp_rates.csv` — unemployment rate by sex × educ × age_band × year
- `Data_Output/wage_distributions.csv` — 99-quantile wage CDF per cell
- `Data_Output/separation_rates.csv` — P(employed→unemployed) via CPSIDV self-join

### Wage growth channel
```python
wage_growth_factors(wage_csv) -> {(sex, educ, age_band, year): growth_factor}
# growth = median_wage(year) / median_wage(BASE_YEAR=2008)
```
At each projection year, scale offer distribution by growth factor. Workers endogenously respond: P(accept) = P(w × growth ≥ w̄).

### Implementation
File: [`Py_file/mccall_employment.py`](../../Py_file/mccall_employment.py)

Public API:
```python
calibrate_mccall(unemp_csv, sep_csv, wage_csv, beta=0.98) -> cell_params dict
wage_growth_factors(wage_csv) -> {(sex, educ, age_band, year): factor}
assign_new_lf_employment(pop, new_lf_mask, cell_params, proj_year, rng) -> pop
apply_employment(pop, proj_year, cell_params, wage_factors, rng) -> pop
```

---

## Bugs Fixed

### Bug 1: Windows multiprocessing crash
**Error:** `RuntimeError: An attempt has been made to start a new process before the current process has finished its bootstrapping phase.`  
**Cause:** `ProcessPoolExecutor` with `spawn` start method (Windows default) fails when called from a script entry point.  
**Fix:** Replaced `ProcessPoolExecutor` with a simple sequential for-loop. Runtime ~2 min for 648 cells.

### Bug 2: Annual frequency makes λ > 1
**Symptom:** Unemployment spiraling to 70%+ by 2024.  
**Cause:** At annual frequency, steady-state requires λ = α(1-u*)/u*. For cells with u* = 5%, α = 7%: λ = 1.33 — not a valid probability.  
**Fix:** All Bellman iteration runs at **monthly** frequency (α_m = α/12, β_m = β^(1/12)). Annual simulation probabilities recovered via compounding: p_sep = 1-(1-α_m)^12, p_find = 1-(1-γ_m×p_accept)^12.

### Bug 3: Double-acceptance condition
**Symptom:** Unemployment still drifting to 72% despite monthly fix.  
**Cause:** `p_find_annual` = P(receive acceptable offer in 12 months) already incorporates the acceptance condition P(w ≥ w̄). The code then drew from the full wage distribution and checked `w_draw >= w_bar` again, making effective job-finding rate = p_find_annual × p_accept (far too low).  
**Fix:** When `p_find_annual` draw passes, draw wage from the **truncated** distribution (w ≥ w̄ only). No second acceptance check.

```python
# After: correct — draw from acceptable wages only
accept_mask = wages >= w_bar
acc_wages = wages[accept_mask]
acc_probs = params["probs"][accept_mask] / params["probs"][accept_mask].sum()
cdf = np.cumsum(acc_probs)
w_idx = min(np.searchsorted(cdf, draws_wage[i]), len(acc_wages) - 1)
emp_arr[i] = "yes"
wage_arr[i] = float(acc_wages[w_idx])
```

### Bug 4: NILF→LF entrants all start as unemployed
**Symptom:** Unemployment stable but persistently 2-4pp above BLS target. Annual spike as LFPR transitions run.  
**Cause:** Workers transitioning from NILF to LF start with `employed="no"`. With ~4-5% of the LF being new entrants each year, this adds 4-5pp to the unemployment stock immediately (before McCall dynamics).  
**Fix:** `assign_new_lf_employment()` — after `_apply_lf_transitions()` identifies new entrants, draw their initial employment status from the cell steady-state employment rate (`emp_rate = 1 - u*`). Those who get employment draw wages from the acceptable distribution.

---

## Phase 3: BLS Unemployment Calibration

### Motivation
After fixes 1–4, unemployment tracked the right direction but ran ~2pp above BLS U-3. Residual sources:
- CPS-derived cell targets ≠ BLS U-3 exactly (universe/weighting differences)
- 16-24 and 55-64 age groups not anchored to BLS
- Annual model can't fully reproduce within-year recession spikes

### Solution
Post-McCall calibration: after `apply_employment()`, stochastically re-employ or un-employ prime-age (25-54) workers by education group until simulated unemployment hits BLS December targets.

### BLS targets by education
| Series | BLS definition | Our mapping |
|--------|---------------|-------------|
| `LNS14027659` | Less than HS diploma, 25+ | (not used — LNS14027660 is more representative for "hs") |
| `LNS14027660` | HS grad, no college, 25+ | `educ == "hs"` |
| `SCND2564` | Some college, no degree, 25-64 | `educ == "some_college"` |
| `LNS14027662` | BA+, 25+ | `educ == "ba_plus"` |

Files: [`Data_Input/fredgraph.csv`](../../Data_Input/fredgraph.csv), [`Data_Input/SCND2564.csv`](../../Data_Input/SCND2564.csv)  
All series: December (end-of-year) values.

### Mechanism
```python
def _calibrate_unemp_to_bls(pop, proj_year, cell_params, wage_factors):
    for educ_group in ["hs", "some_college", "ba_plus"]:
        target_u = _BLS_U_BY_EDUC[(educ_group, proj_year)] / 100.0
        sim_u = wt_unemp / wt_lf  # weighted among prime-age in LF
        if sim_u > target_u:
            # Stochastically re-employ: flip unemployed → employed
            # Draw wages from McCall acceptable distribution
        elif sim_u < target_u:
            # Stochastically un-employ: flip employed → unemployed
```

---

## Final Results (2009–2024)

Education-specific unemployment hits BLS targets exactly each year. Overall LF unemployment:

| Year | Sim overall | BLS (approx) |
|------|-------------|--------------|
| 2009 | 8.3%        | 9.3%         |
| 2010 | 8.6%        | 9.6%         |
| 2011 | 8.3%        | 8.9%         |
| 2012 | 8.0%        | 8.1%         |
| 2013 | 7.5%        | 7.4%         |
| 2014 | 6.7%        | 6.2%         |
| 2015 | 6.2%        | 5.3%         |
| 2016 | 5.5%        | 4.7%         |
| 2017 | 4.8%        | 4.4%         |
| 2018 | 4.4%        | 3.9%         |
| 2019 | 3.9%        | 3.7%         |
| 2020 | 6.4%        | 8.1%         |
| 2021 | 4.7%        | 5.4%         |
| 2022 | 4.1%        | 3.6%         |
| 2023 | 4.1%        | 3.7%         |
| 2024 | 4.3%        | 4.0%         |

Residual 0-1pp gap in overall rate: 16-24 and 55+ age groups not anchored to BLS (no education-specific BLS series available for those groups). Prime-age education groups hit exact BLS targets.

Other MAEs (2009–2024):
- Population: 0.1M
- CBR: 0.004 /1000
- LFPR 25-54: 1.19pp
- Dependency ratio: 0.0006

---

## Files Modified / Created

| File | Change |
|------|--------|
| [`Py_file/mccall_employment.py`](../../Py_file/mccall_employment.py) | **New file** — McCall Bellman solver, calibration, `assign_new_lf_employment`, `apply_employment` |
| [`R_file/mccall_data.r`](../../R_file/mccall_data.r) | **New file** — IPUMS extract for unemployment/wage/separation data |
| [`Py_file/project_multi_year.py`](../../Py_file/project_multi_year.py) | Added LFPR calibration, McCall integration, BLS unemployment post-calibration, `_calibrate_unemp_to_bls` |
| [`Data_Input/SCND2564.csv`](../../Data_Input/SCND2564.csv) | User-provided — some college unemployment rate (FRED) |

---

## Pending / Next Steps

1. **Income / wage module** — assign annual earnings for employed workers, accumulate AIME
   - Use `incwage` from McCall draw (already set in `apply_employment`)
   - Need to apply wage growth factors for projection years
   - Need AIME computation (35 highest earning years in SS-covered earnings)

2. **Retirement / benefit claiming module** — model retirement hazard and SS claiming age

3. **Validation against SSA targets** — compare simulated taxable payroll, beneficiary counts, trust fund projections
