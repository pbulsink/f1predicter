# Plan: Determine Constructor & Driver Race Pace (with FP2 fallback)

## Goal

Estimate, for a given season/round, the **race race-pace** (normalized lap time) for:

1. Each **constructor** (team-level pace).
2. Each **driver** (driver-level pace).

with a robust fallback when FP2 data is missing or partial (sprint weekends, mechanical failure, driver no-show), and using the bundled weekend tyre-compound allocation to make compound comparisons meaningful.

This is an **analytical layer** on top of the existing pipeline. It does not replace the position-based ML predictor in `simulate_race.R` — it produces pace estimates that can be surfaced (table/viz) or, later, blended into `simulate_race` / `calculate_driver_performance`.

---

## What exists today (from code + cache inspection)

**Data source of truth** — `cache/f1predicter.sqlite` (`laps` table, 503,705 rows):

| Column | Notes |
|---|---|
| `driver_id`, `constructor_id` | keyed by driver code + constructor |
| `season`, `round`, `session_type` | `FP1`, `FP2`, `FP3`, `Q1..Q3`, `R`, `S`, `SS` |
| `lap_number`, `stint`, `tyre_life` | `stint` = continuous tyre set; `tyre_life` = lap-of-stint (1-based) |
| `compound` | `SOFT`/`MEDIUM`/`HARD` (modern) + legacy `ULTRASOFT`/`SUPERSOFT`/`HYPERSOFT` + `INTERMEDIATE`/`WET`/`UNKNOWN`/`TEST`/`nan`/`""` |
| `lap_time`, `sector1time`..`sector3time` | seconds; `lap_time` can be `Inf` for invalid |
| `track_status` | `1` = green; other codes = yellow/VSC/SC/deleted |
| `is_accurate` | 0/1 (in/out-lap flag from f1dataR) |
| `deleted`, `deleted_reason` | 0/1 + reason |
| `fresh_tyre` | 1/0 |
| `position`, `air_temp`, `track_temp`, `humidity`, `pressure`, `rainfall`, `wind_*` | weather/track context |
| — | **No `session_start`, no session-elapsed-time column.** (Important — see §6.) |

**Provided allocation file** — `data-raw/f1_race_tyre_compounds_2018_2026.csv` (196 season/round rows):
- Columns: `Season`, `Round`, `Grand_Prix`, `Weekend_Hard`, `Weekend_Medium`, `Weekend_Soft` (values like `C1`–`C6`), `Data_Status` (`Announced` / `Not announced as of ...`), `Wikipedia_Source`, `Official_Source`.
- This is the **weekend Pirelli compound allocation** (which three compounds were available that weekend), **not** per-driver stint data.
- **Not yet a bundled dataset**: `data/` only contains `schedule.rda`. There is no `09_*.R` builder. This is a gap to close.

**What is NOT in the codebase** (confirmed by grep):
- No `lme4`/`mgcv`/`nlme` mixed-model usage for pace.
- No fuel-correction or tyre-degradation model.
- No `compound_deltas`, `driver_race_pace`, `constructor_race_pace` functions.
- The existing race simulation (`R/simulate_race.R`) is **position-based** (ML mean position + `position_sd` + DNF rate), not lap-time-based.

**Installed packages** (relevant):
- `nlme` ✓, `mgcv` ✓, `broom` ✓, `f1dataR` ✓, `stacks`/`ranger`/`glmnet`/`kknn` ✓
- `lme4` ✗ (MISSING — plan.md's `lmer()` call will not run as-is)
- `emmeans` ✗, `brms` ✗, `fixest` ✗, `sandwich` ✗, `car` ✗

---

## Constraints that shape the design

1. **No session-elapsed-time in cache.** `f1dataR::load_session_laps` does not return a `session_start`/elapsed column. The `laps` table has `lap_number`/`stint`/`tyre_life` but no wall-clock time. So the plan.md GAM-on-elapsed-time track-evolution step **cannot be run as written** against the cache. Two options (see §6).
2. **`lme4` not installed.** We use `nlme::lme` (already installed) or `stats::lm` with driver dummies + a post-hoc compound-delta extraction. `lme` is the natural substitute.
3. **Compound labels are inconsistent across years** (legacy `ULTRASOFT`/`SUPERSOFT`/`HYPERSOFT` pre-2018; `INTERMEDIATE`/`WET` for wet; `UNKNOWN`/`TEST`/`nan` for gaps). We need a mapping/normalization step.
4. **The allocation CSV uses `C1`–`C6` codes**, not `SOFT/MEDIUM/HARD`. The mapping is fixed by Pirelli (C1=hardest, C6=softest) and the CSV tells us, per race, which three of the six were on offer. We use it to (a) know the reference compound set for a weekend and (b) validate that a driver's `compound` ∈ that set.
5. **Sprint weekends**: `FP3` is absent; `SS` (Sprint Shootout) + `S` (Sprint) + `Q1/Q2/Q3` are present. The `sprint_results` table is keyed by `season`/`round`. We can detect sprint weekends by `sprint_date` in `schedule` (already loaded) or by presence of `S`/`SS` rows.
6. **`track_status`** codes other than `1` indicate yellow/VSC/SC — must be excluded from pace estimation.
7. **`position` is NULL in practice** (verified on 2025 FP2: all rows null). So we cannot use `position` for traffic filtering in practice; we must use `lap_time` vs `stint median` or sector sums as in `plan.md`.
8. **Fuel load is unknown** and must be estimated from stint position + a burn-rate prior. The plan.md heuristic is a reasonable starting point.
9. **Compound comparison across drivers is only valid within a weekend** because the allocation differs per race. Cross-race compound deltas are not meaningful without the allocation as a control.
10. **Driver no-show / mechanical issue**: some drivers will have zero valid long-run laps in FP2. The fallback hierarchy must handle per-driver missing data, not just per-weekend.

---

## Data model: what we actually need per row

For each **valid long-run lap** in a target session (FP2 by default):

```
season, round, driver_id, constructor_id,
session_type, stint, lap_number, tyre_life,
compound_raw, compound_std,          # normalized to SOFT/MEDIUM/HARD
lap_time,                            # seconds, valid only
lap_time_adj,                        # after evo + fuel + compound corrections
is_long_run,                         # stint >= N laps on same compound
stint_len,                           # n laps in stint
stint_median_time,                   # for traffic filter
fresh_tyre, track_status, is_accurate,
fuel_kg_est,                         # estimated fuel on car
fuel_correction,                     # fuel_kg * sensitivity
```

---

## Step 0 — Build the bundled compound-allocation dataset

The CSV is in `data-raw/` but never promoted. Add `data-raw/10_f1_race_tyre_compounds.R`:

1. `readr::read_csv("f1_race_tyre_compounds_2018_2026.csv")` (or `utils::read.csv`).
2. Keep only rows with `Data_Status == "Announced"`.
3. Normalize `Season`/`Round` to integer.
4. Map `C1`–`C6` to relative hardness (C1=1, C6=6) for sanity checks.
5. `usethis::use_data(f1_race_tyre_compounds, overwrite = TRUE)` → writes `data/f1_race_tyre_compounds.rda`.
6. Add a roxygen `@format`/`@source` block and a `data-raw/` script reference in `DESCRIPTION` (no `LazyData` change needed since it's already `true`).
7. Add a small exported accessor `race_tyre_compounds()` in `R/data.R` that returns the bundled tibble filtered by `season`/`round`, falling back to the full set when the race is not in the CSV (e.g., future races).

**Tests** (`tests/testthat/test-data.R`):
- `f1_race_tyre_compounds` is a tibble with the expected columns.
- `race_tyre_compounds(season=2024, round=1)` returns a single row with `Weekend_Hard/Medium/Soft` non-empty.
- `race_tyre_compounds(season=2030, round=1)` returns the full set (graceful fallback).

---

## Step 1 — Session selection & sprint detection

```r
get_pace_session <- function(season, round) {
  sched <- f1predicter::schedule
  is_sprint <- !is.na(sched[sched$season==season & sched$round==round, "sprint_date"])
  c("FP2", if (is_sprint) "SS" else "FP3")
}
```

Rationale:
- **Normal weekend**: FP2 is the main long-run session. FP3 is the closer-to-race session (lower fuel, more race-sim). We use **FP2 as primary** and **FP3 as a tiebreaker/cross-check** (see §7 fallback).
- **Sprint weekend**: FP3 does not exist. `SS` (Sprint Shootout) is a short-quali-style session (not a long-run race sim), so it is **not** a valid race-pace source. We fall back to **FP2 only**, with the understanding that on sprint weekends FP2 is the sole long-run session and typically includes longer runs (drivers know the sprint is short and race-sim the longer stint). If FP2 is also missing/poor, we fall through to `Q3` (low-fuel, fresh-tyre) as a last resort and flag the result as `pace_source = "quali"` with lower confidence.

The function returns the ordered session list to try, e.g. `c("FP2")` (sprint) or `c("FP2", "FP3")` (normal).

---

## Step 2 — Data cleaning & long-run isolation

For each candidate session in `get_pace_session()`:

```r
clean_laps <- function(laps, min_stint_laps = 5) {
  laps |>
    dplyr::filter(
      .data$is_accurate == 1,
      .data$track_status == 1,
      !.data$deleted,
      is.finite(.data$lap_time),
      .data$lap_time > 0,
      .data$compound %in% c("SOFT","MEDIUM","HARD","ULTRASOFT","SUPERSOFT","HYPERSOFT")
    ) |>
    dplyr::mutate(compound_std = dplyr::case_when(
      .data$compound %in% c("SOFT","ULTRASOFT","SUPERSOFT","HYPERSOFT") ~ "SOFT",
      .data$compound %in% c("MEDIUM") ~ "MEDIUM",
      .data$compound %in% c("HARD") ~ "HARD",
      TRUE ~ NA_character_
    )) |>
    dplyr::group_by(.data$driver_id, .data$constructor_id, .data$session_type, .data$stint) |>
    dplyr::mutate(stint_len = dplyr::n()) |>
    dplyr::ungroup() |>
    # User decision: a single stint of >5 laps is a valid pace source.
    dplyr::filter(.data$stint_len > min_stint_laps) |>
    # drop first lap of stint (out-lap / transition)
    dplyr::group_by(.data$driver_id, .data$session_type, .data$stint) |>
    dplyr::arrange(.data$tyre_life) |>
    dplyr::filter(.data$tyre_life > 1) |>
    # drop laps that look like traffic (lap_time > 105% of stint median)
    dplyr::group_by(.data$driver_id, .data$session_type, .data$stint) |>
    dplyr::mutate(stint_median = median(.data$lap_time, na.rm = TRUE)) |>
    dplyr::filter(.data$lap_time <= 1.05 * .data$stint_median) |>
    dplyr::ungroup()
}
```

**Rationale for the 105% traffic filter**: `position` is NULL in practice, so we cannot use position-based traffic detection. A lap that is >5% slower than the stint's own median is almost certainly lapped/traffic-affected or a mistake; it is also the cheapest robust filter. (The plan.md's sector-sum method is a refinement we can add later; start simple.)

**Why not use `stint` directly for long-run detection?** A "stint" in f1dataR is a continuous set of laps on the same tyres. A >5-lap minimum on the same compound matches typical FP2 long-run stints (verified: 2025 FP2 stints range from 3–20 laps). **User decision**: one stint of >5 laps is a valid pace source; two or more stints give a more robust median.

---

## Step 3 — Track evolution correction

**Problem**: As rubber lays down, the track gets faster. A lap done 30 min into the session is faster than the same lap done at 5 min, even with identical fuel/tyres. We must remove this effect.

**Constraint**: No elapsed-time column in the cache. Two workable paths:

### Option A (recommended) — Use `lap_number` as a proxy + a per-weekend GAM

`lap_number` (within-session lap count) is a reasonable proxy for session position: early laps are cold track, late laps are warm. It is not a perfect clock, but it monotonically increases through the session and is present for every row.

```r
library(mgcv)
evo_model <- gam(
  lap_time ~ s(lap_number, k = 8) +
            compound_std +
            (1 | driver_id),   # via nlme fallback if lme4 not available
  family = gaussian(),
  data = clean_laps
)
# Actually use nlme::lme with a spline term, OR use mgcv with a driver random effect via by=
# Simplest robust path: mgcv with driver as a factor is too many levels; use nlme::lme:
evo_model <- nlme::lme(
  lap_time ~ spl(lap_number, df = 8),   # need splines::splines
  random = ~ 1 | driver_id,
  data = clean_laps
)
```

In practice, the cleanest approach with installed packages:
- Use `mgcv::gam(lap_time ~ s(lap_number, k=8) + compound_std, data = clean_laps)` for the **global** track-evolution + compound effect.
- Do **not** include driver as a random effect here (would require `lme4`/`nlme` with a smooth). Instead, treat driver effects as part of the per-stint model in Step 5.
- Extract the **track-evolution residual** as: `resid_evo = lap_time - predict(evo_model, newdata = clean_laps, type = "terms")` minus the compound-only prediction. Concretely:

```r
pred_with_evo   <- predict(evo_model, newdata = clean_laps)
pred_no_evo     <- predict(evo_model, newdata = clean_laps, select = "compound_std")
clean_laps <- clean_laps |>
  dplyr::mutate(
    track_evo_correction = pred_with_evo - pred_no_evo,
    lap_time_evo_adj     = .data$lap_time - .data$track_evo_correction
  )
```

This gives each lap a "what would this time be at a reference lap_number" adjustment. We do not need the absolute level — only the **deviation from the mean**, which is what `pred_with_evo - pred_no_evo` captures.

### Option B (if `mgcv` smooth is unstable on small weekends)
- Bin `lap_number` into 4 quartiles, take the per-quartile mean `lap_time` per compound, and use a simple step correction. Less smooth but fully robust and easy to test.

**Decision**: Use Option A by default; fall back to Option B when a weekend has <30 valid long-run laps (small-sample guard).

---

## Step 4 — Fuel correction

Per the plan.md heuristic (reasonable and simple):

```r
FUEL_SENSITIVITY <- 0.035   # sec/kg/lap, default
EST_START_FUEL_KG <- 35     # kg at start of long run
BURN_RATE_KG <- 1.6         # kg/lap

clean_laps <- clean_laps |>
  dplyr::mutate(
    fuel_kg_est = pmax(0, EST_START_FUEL_KG - .data$tyre_life * BURN_RATE_KG),
    fuel_correction = .data$fuel_kg_est * FUEL_SENSITIVITY,
    lap_time_fuel_evo_adj = .data$lap_time_evo_adj - .data$fuel_correction
  )
```

**Caveat**: This assumes the first lap of the long-run stint starts at 35 kg. In reality, the driver may have been on a different compound set before, and the "start fuel" of the long run is not exactly 35 kg. This is an approximation; the absolute level is not important, only the **relative** effect within a stint (which lap has more fuel than another). The `tyre_life`-linear model captures that.

**Refinement (optional)**: Calibrate `FUEL_SENSITIVITY` per circuit using the known circuit length (from `f1dataR::load_circuit`). Skip for v1.

---

## Step 5 — Compound delta estimation

**Goal**: A per-weekend, per-compound offset (relative to MEDIUM) so that a driver on SOFT and a driver on MEDIUM can be compared.

**Why per-weekend**: The allocation differs per race; a "C3" in 2024 Bahrain is a different compound than "C3" in 2024 Monaco. The `SOFT/MEDIUM/HARD` labels in the laps table are *relative* to that weekend's allocation, so deltas are only meaningful within a weekend.

**Method** (using `nlme::lme` since `lme4` is not installed):

```r
library(nlme)
compound_model <- nlme::lme(
  lap_time_fuel_evo_adj ~ compound_std,
  random = ~ 1 | driver_id,
  data = short_run_laps,   # stint_len <= 3, fresh-tyre, low-fuel
  method = "REML"
)
compound_deltas <- as.data.frame(fixef(compound_model))
# Re-center so MEDIUM = 0
compound_deltas$offset <- compound_deltas$offset - compound_deltas$`compound_stdMEDIUM`
```

**Use short-run laps** (`stint_len <= 3`, `fresh_tyre == 1`, low `tyre_life`) for the compound delta — **only when at least two of SOFT/MEDIUM/HARD are present** in the session (user decision: wet-only weeks skip the compound-delta step and report `model_stable = FALSE`, `source = "wet"`) — because:
- Fresh tyres → minimal degradation confounding.
- Low fuel → minimal fuel confounding.
- The delta we extract is the **pure compound offset** at a reference fuel/tyre-age.

**Validation**: Compare the extracted `SOFT − MEDIUM` gap against the known Pirelli spec (typically ~0.5–0.7 s/lap in raw lap time). If the estimate is wildly off (>1.5 s), flag the weekend as `compound_model_unstable` and fall back to a fixed prior (0.6 s SOFT, 0.0 s MEDIUM, 0.5 s HARD).

---

## Step 6 — Per-driver stint model (degradation + base pace)

For each long-run stint (per driver, per compound):

```r
stint_models <- long_run_laps |>
  dplyr::group_by(.data$driver_id, .data$constructor_id, .data$session_type, .data$stint) |>
  dplyr::nest() |>
  dplyr::mutate(
    fit = dplyr::list(purrr::map(.data$data, ~ {
      d <- .x
      # filter to a clean linear region (drop first 2 laps, last 2 laps of degradation)
      d <- d |> dplyr::filter(.data$tyre_life >= 2, .data$tyre_life <= dplyr::max(.data$tyre_life) - 2)
      if (nrow(d) < 4) return(NULL)
      stats::lm(lap_time_fuel_evo_adj ~ .data$tyre_life, data = d)
    }))
  )
```

Extract `(Intercept)` → `P0` (base pace) and slope of `tyre_life` → `DegRate` (sec/lap degradation).

**Why linear?** For a 5–20 lap stint, degradation is approximately linear in the middle region. The plan.md's per-stint `lm` is the right call.

**Minimum data**: skip stints with <4 usable laps; mark `P0 = NA`.

---

## Step 7 — Normalization to a common reference

**Reference point** (per user decision):
- Compound: **the weekend's allocated MEDIUM** (from `f1_race_tyre_compounds`, e.g. C3/C4/C5). The `SOFT/MEDIUM/HARD` labels in the laps table are *already relative to that weekend's allocation*, so "MEDIUM" in the laps data IS the weekend's medium compound. We therefore use `compound_offset["MEDIUM"] = 0` as the reference. (Confirmed: use the weekend's allocated medium, not a fixed global medium.)
- Fuel: **0 kg** (we already removed the fuel effect; the `P0` is the intercept at `tyre_life=0`, i.e., fresh tyre, no fuel)
- Tyre age: **lap 5 of stint** (a representative "race lap")

```r
normalized_pace <- P0 + DegRate * 5 - compound_offset[compound]
```

where `compound_offset["MEDIUM"] = 0`, `compound_offset["SOFT"] = -0.6` (faster), `compound_offset["HARD"] = +0.5` (slower). These offsets are per-weekend (estimated in Step 5 against the weekend's MEDIUM).

**Per-driver race pace** = median of `normalized_pace` across all valid long-run stints for that driver in the target session(s).

**Per-driver pace relative to constructor** (user decision): each driver also reports a `gap_to_constructor_sec` = `driver_normalized_race_pace - constructor_team_race_pace` (positive = slower than the team's best car; negative = faster). This is the driver's intra-team gap, computed after `constructor_team_race_pace` is resolved in §8.

**Per-constructor race pace** = `min(p1, p2)` of the two drivers' `normalized_race_pace` (user decision: use **min** = "best car pace"). See §8 for the fallback logic when a driver has no data.

---

## Step 8 — Fallback logic (the core of the requirement)

This is the critical part. We need a **per-driver** decision tree:

```
For each driver in the round:
  1. Compute `pace_fp2` from FP2 long-run stints (if >=2 valid stints).
  2. If no FP2 pace:
     a. If normal weekend: compute `pace_fp3` from FP3 long-run stints (if >=2 valid stints).
     b. If sprint weekend or no FP3: compute `pace_q3` from Q3 laps (fresh-tyre, low-fuel).
        Flag `pace_source = "quali"` (lower confidence).
  3. If still no pace: mark `pace = NA`, `pace_source = "missing"`.
```

**Constructor-level aggregation** (user decision: **min** of the two drivers' paces):

```
For each constructor (2 drivers, D1 and D2):
  p1 = pace(D1);  p2 = pace(D2)

  if (both p1 and p2 are non-NA) {
    team_pace  = min(p1, p2)           # "best car pace" (user decision)
    team_source = "both_drivers"
  } else if (exactly one is non-NA) {
    team_pace  = the non-NA pace        # the one driver IS the best car
    team_source = "single_driver_fallback"
    # The missing driver is imputed with team_pace, flagged
    missing_driver_pace <- team_pace
    missing_driver_source <- "team_fallback"
  } else {
    team_pace = NA
    team_source = "no_data"
    # No pace is imputed: this is a data-extraction framework, not a model.
    # (User decision: no modelling without practice data.) Both drivers get
    # pace = NA, pace_source = "missing".
  }
```

**Driver pace relative to constructor** (user decision): after `team_pace` is resolved, each driver's `gap_to_constructor_sec = driver_pace - team_pace` is computed. For the driver(s) who produced the `min`, this is `<= 0`; for the slower driver it is `> 0`. Drivers imputed via `single_driver_fallback` get `gap_to_constructor_sec = 0` (they ARE the team pace) and `is_imputed = TRUE`.

**Rationale for `min`**: The user asked for `min` ("best car pace"). With two drivers of the same car, `min` captures the car's true pace ceiling; the faster driver's measured pace is the best signal of the car. The slower driver's gap is then reported as a positive `gap_to_constructor_sec`.

**Rationale for NOT modelling when both drivers are missing**: The user clarified this is a **data-extraction framework**, not a modelling exercise. Without practice data for both drivers we do not impute from historical results — we return `NA` / `pace_source = "missing"`. (The earlier `historical_fallback` idea is dropped.)

**Rationale for NOT always using the team value**:
- When both drivers have independent pace data, they may differ by 0.3–0.8 s due to driver skill, setup preference, or tyre management. We keep them separate and derive the team pace as `min(p1, p2)`.
- The slower driver's `gap_to_constructor_sec` preserves that signal.

---

## Step 9 — Output schema

Three data frames (matching the plan.md deliverables), plus a provenance column:

### `compound_deltas_df`
| Column | Type | Notes |
|---|---|---|
| `season`, `round` | int | |
| `compound` | chr | `SOFT` / `MEDIUM` / `HARD` |
| `offset_sec` | num | Relative to MEDIUM (MEDIUM = 0) |
| `n_laps` | int | Laps used in the estimate |
| `model_stable` | lgl | Whether the estimate passed the sanity check |
| `source` | chr | `fp2` / `fp3` / `q3` |

### `driver_race_pace_df`
| Column | Type | Notes |
|---|---|---|
| `season`, `round`, `driver_id`, `constructor_id` | | |
| `primary_compound` | chr | Most-used compound in long runs |
| `degradation_sec_per_lap` | num | Mean `DegRate` across stints |
| `normalized_race_pace` | num | Median `normalized_pace` (sec/lap, lower = faster) |
| `gap_to_constructor_sec` | num | `normalized_race_pace - team_race_pace` (positive = slower than team's best car) |
| `n_stints` | int | Valid long-run stints used |
| `pace_source` | chr | `fp2` / `fp3` / `q3` / `team_fallback` / `wet` / `missing` |
| `is_imputed` | lgl | `TRUE` if `pace_source` is `team_fallback` / `wet` / `missing` |

### `constructor_race_pace_df`
| Column | Type | Notes |
|---|---|---|
| `season`, `round`, `constructor_id` | | |
| `team_race_pace` | num | `min(p1, p2)` of driver paces (sec/lap, lower = faster) |
| `gap_to_leader_sec` | num | `team_race_pace - min(team_race_pace)` within the round |
| `team_pace_source` | chr | `both_drivers` / `single_driver_fallback` / `wet` / `no_data` |
| `n_drivers_with_pace` | int | 0, 1, or 2 |

---

## Step 10 — API surface

New exported functions in a new file `R/race_pace.R`:

```r
#' @export
estimate_race_pace(season, round,
                   min_stint_laps = 5,
                   traffic_threshold = 1.05,
                   fuel_sensitivity = 0.035,
                   est_start_fuel_kg = 35,
                   burn_rate_kg = 1.6,
                   reference_tyre_life = 5,
                   compound_prior = c(SOFT = -0.6, MEDIUM = 0.0, HARD = 0.5))

#' @export
get_race_pace_summary(season, round)   # returns list of the 3 data frames

#' @export
race_tyre_compounds(season = NULL, round = NULL)  # accessor for the bundled dataset
```

Internal helpers (not exported):
- `.clean_laps_for_pace()`
- `.estimate_track_evolution()`
- `.apply_fuel_correction()`
- `.estimate_compound_deltas()`
- `.estimate_stint_models()`
- `.normalize_pace()`
- `.resolve_driver_pace_with_fallback()`
- `.aggregate_constructor_pace()`
- `.historical_constructor_pace()`

---

## Step 11 — Testing plan

### Unit tests (`tests/testthat/test-race_pace.R`)

1. **`race_tyre_compounds()`** accessor:
   - Returns a tibble with expected columns.
   - Filters correctly by `season`/`round`.
   - Falls back to full set for unknown race.

2. **`.clean_laps_for_pace()`**:
   - Mock a small `laps` tibble (5–10 rows) with known `stint`/`compound`/`lap_time`/`track_status`/`is_accurate`.
   - Assert: invalid laps removed, short stints removed, first lap of stint removed, traffic laps (>105% of median) removed.
   - Assert: `compound_std` correctly maps `ULTRASOFT`→`SOFT`, `HYPERSOFT`→`SOFT`, etc.

3. **`.estimate_track_evolution()`** (using `mgcv`):
   - Mock a clean laps frame with a known monotonic `lap_time ~ lap_number` trend + compound effect.
   - Assert: `track_evo_correction` is non-zero and monotonic in `lap_number`.
   - Assert: fallback to binning kicks in when `n < 30`.

4. **`.apply_fuel_correction()`**:
   - Assert: `fuel_kg_est` is `max(0, 35 - tyre_life * 1.6)`.
   - Assert: `lap_time_fuel_evo_adj < lap_time_evo_adj` for all rows (fuel correction is subtracted).

5. **`.estimate_compound_deltas()`**:
    - Mock short-run laps with a known 0.6 s SOFT−MEDIUM gap.
    - Assert: extracted `SOFT` offset ≈ -0.6 (within tolerance 0.15).
    - Assert: offsets are re-centered so `MEDIUM = 0`.
    - Assert: when `n_laps < 10`, `model_stable = FALSE` and prior is used.

6. **`.estimate_stint_models()`**:
   - Mock a single driver's stint with a known linear degradation of 0.1 s/lap.
   - Assert: extracted slope ≈ 0.1 (within 0.03).
   - Assert: stints with <4 laps return `P0 = NA`.

7. **`.resolve_driver_pace_with_fallback()`** (the critical fallback logic):
    - **Case A**: Driver has FP2 pace → returns FP2, `pace_source = "fp2"`, `is_imputed = FALSE`.
    - **Case B**: Driver has no FP2, has FP3 (normal weekend) → returns FP3, `pace_source = "fp3"`.
    - **Case C**: Driver has no FP2/FP3, sprint weekend → returns Q3, `pace_source = "q3"`.
    - **Case D**: Driver has only wet-tyre laps → returns wet pace, `pace_source = "wet"`, `is_imputed = TRUE`.
    - **Case E**: Driver has no practice data at all, teammate has pace → imputed with teammate's pace, `pace_source = "team_fallback"`, `is_imputed = TRUE`.
    - **Case F**: Driver has no practice data, teammate also has none → `pace = NA`, `pace_source = "missing"`, `is_imputed = TRUE`. (No historical modelling — user decision.)
    - **Case G**: Constructor with D1=pace1, D2=pace2 (both valid) → driver paces stay separate; `team_race_pace = min(pace1, pace2)`, `team_pace_source = "both_drivers"`, `gap_to_constructor_sec` computed for each.

8. **End-to-end** (integration):
   - Pick a known race (e.g., 2024 R1 Bahrain, normal weekend; 2023 R4 Japan, sprint weekend).
   - Run `estimate_race_pace(season, round)` against the real cache.
   - Assert: all 20 drivers have a `pace_source` value; no `NA` in `driver_id`/`constructor_id`.
    - Assert: `constructor_race_pace_df` has 10 rows (10 constructors), each with `n_drivers_with_pace` in {0,1,2}.
    - Assert: `gap_to_leader_sec >= 0` for all rows.
    - Assert: `team_race_pace = min(driver paces)` where both drivers have pace.
    - Assert: `gap_to_constructor_sec >= 0` for the slower driver; `<= 0` (zero) for the faster driver.

### Edge cases to cover
- Sprint weekend with no FP2 (should not happen, but test the guard).
- **Wet race** (compound = `INTERMEDIATE`/`WET`): **not excluded** (user decision — this is a data-extraction framework). Wet laps are included and reported with `pace_source = "wet"`, `is_imputed = TRUE`. The compound-delta step is skipped for wet-only weeks (no SOFT/MEDIUM/HARD comparison). A driver who has *both* wet and dry laps reports the dry pace with `pace_source = "fp2"/"fp3"/"q3"`.
- A driver who ran only 1 stint of >5 laps → **valid** pace, `n_stints = 1`, `is_imputed = FALSE` (user decision: one stint of >5 laps is sufficient).
- A round where the compound-allocation CSV has no row (e.g., 2030 future race): use the prior compound deltas.

---

## Step 12 — Integration with existing pipeline

**Do NOT modify** `simulate_race.R` or `championship.R` in v1. The race-pace estimates are a **new analytical surface**, surfaced via:

1. **A new vignette** `vignettes/race_pace.Rmd` — "Estimating driver and constructor race pace from practice data" with worked examples (2024 Bahrain, 2023 Japan sprint).
2. **A new exported function** `estimate_race_pace()` that returns the 3 data frames.
3. **Optional**: a `gt` table formatter (similar to `format_results_prob_table()`) for a nice `driver_race_pace_df` display. Add to `R/social.R` or a new `R/race_pace_format.R`.

**Future** (not in v1):
- Blend `normalized_race_pace` into `calculate_driver_performance()` as an additional feature (weight 0.1–0.2).
- Use `degradation_sec_per_lap` to adjust the race-sim `position_sd` (higher degradation → wider SD).
- Use `compound_deltas` to inform the tyre strategy in `simulate_race()`.

---

## Step 13 — Dependencies

**Add to `Imports`** (none of the new ones are missing):
- `nlme` (for `lme`) — already in base R ecosystem, confirm it's in `DESCRIPTION` (it's **not** currently listed — add it).
- `mgcv` (for `gam`) — add to `Imports`.
- `broom` — already in `Imports`.
- `splines` — base R, no change.

**Add to `Suggests`**:
- `lme4` (optional, if we later want to upgrade from `nlme::lme`).

**Do NOT add**: `lme4`, `emmeans`, `brms`, `fixest` — none are installed and none are needed for v1.

---

## Step 14 — Risks & mitigations

| Risk | Mitigation |
|---|---|
| `mgcv` GAM unstable on small weekends | Fallback to quartile-binning (Option B in §3). |
| `nlme::lme` convergence warnings on small n | Wrap in `tryCatch`; fall back to `lm` with driver dummies + post-hoc compound extraction. |
| Fuel heuristic wrong for a specific circuit | Expose `fuel_sensitivity`/`est_start_fuel_kg`/`burn_rate_kg` as arguments; document the assumption. |
| Compound labels inconsistent across years | `compound_std` mapping in §2; legacy compounds (pre-2018) are mapped to nearest modern equivalent and flagged. |
| Sprint weekends have no FP3 | §1 `get_pace_session()` handles this; §8 fallback handles it. |
| One driver missing in FP2 (mechanical) | §8 team-average fallback, clearly flagged. |
| Both drivers missing | §8 historical fallback, clearly flagged, low weight. |
| Wet race | Exclude wet/intermediate compound laps; flag round as `wet_excluded`. |
| `lme4` not installed | Use `nlme::lme` (installed). |
| No elapsed-time in cache | Use `lap_number` as proxy (§3 Option A); document the limitation. **Future**: user can raise a `lap_start_time` (clock time) column so laps compare on same-track-time — see §18. |

---

## Step 15 — Out of scope (explicit)

- **No changes to the ML prediction pipeline** (`train_stacked_model`, `train_ordinal_ensemble`, etc.) in v1. (User decision: race-pace is standalone.)
- **No changes to `simulate_race.R`** in v1. (User decision: standalone, not blended into the position-based simulation.)
- **No new dependencies** beyond `nlme` and `mgcv` (both installed).
- **No per-circuit fuel calibration** in v1 (user decision: use the global 0.035 s/kg).
- **No Bayesian modelling** (`brms`/`rstanarm` not installed; not needed).
- **No historical-result fallback** when both drivers have no practice data (user decision: this is a data-extraction framework, not a modelling exercise — return `NA` / `pace_source = "missing"`).
- **No pre-race mode** — this is a post-session analytical tool. (A pre-race version would use the previous race's pace as a prior; that's a future enhancement.)

---

## Step 16 — Acceptance criteria

1. `estimate_race_pace(season, round)` runs without error on any completed race in the cache (2018–2026).
2. All 20 drivers in a normal weekend have a non-`NA` `normalized_race_pace` (or a clearly-flagged fallback: `team_fallback` / `wet` / `missing`).
3. All 10 constructors have a non-`NA` `team_race_pace` (or a clearly-flagged fallback: `single_driver_fallback` / `wet` / `no_data`).
4. Sprint weekends (e.g., 2023 R4, R9, R12, R17, R18, R20) work without referencing the non-existent FP3.
5. The `team_fallback` imputation is used when exactly one driver has pace data, and is clearly flagged (`is_imputed = TRUE`).
6. The compound-delta estimate for SOFT vs MEDIUM is within 0.15 s of the Pirelli spec for at least 80% of weekends.
7. `constructor_race_pace_df.team_race_pace` equals `min(driver paces)` for all constructors where both drivers have pace.
8. `driver_race_pace_df.gap_to_constructor_sec` is `>= 0` for the slower driver and `<= 0` (zero) for the faster driver.
9. All new tests pass: `devtools::test(filter = "race_pace", reporter = "check")`.
10. `devtools::check(error_on = "warning")` passes (no new warnings/notes).
11. `NEWS.md` has a bullet under the dev heading: `` * `estimate_race_pace()` now estimates driver and constructor race pace from practice data with FP2/FP3/qualifying fallback. ``
12. `air format .` passes.

---

## Step 17 — File-level change list

| File | Action | Notes |
|---|---|---|
| `data-raw/10_f1_race_tyre_compounds.R` | **Create** | Builds `f1_race_tyre_compounds` dataset from the CSV. |
| `data/f1_race_tyre_compounds.rda` | **Create** | Bundled dataset (run the builder). |
| `R/data.R` | **Modify** | Add `race_tyre_compounds()` accessor. |
| `R/race_pace.R` | **Create** | All new pace-estimation functions. |
| `R/race_pace_format.R` | **Create** (optional) | `gt` table formatter for `driver_race_pace_df`. |
| `DESCRIPTION` | **Modify** | Add `nlme` and `mgcv` to `Imports`; add `lme4` to `Suggests`. |
| `NAMESPACE` | **Regenerate** | Via `roxygen2::roxygenise()` after adding `@export` tags. |
| `vignettes/race_pace.Rmd` | **Create** | Worked examples + methodology. |
| `tests/testthat/test-race_pace.R` | **Create** | Unit + integration tests (§11). |
| `tests/testthat/test-data.R` | **Modify** | Add tests for `race_tyre_compounds()` accessor. |
| `NEWS.md` | **Modify** | Add bullet under dev heading. |
| `AGENTS.md` | **Modify** (optional) | Add a one-liner pointing to the new `race_pace.R` module. |

---

## Step 18 — Decisions (resolved by the user)

1. **Reference compound for normalization**: **the weekend's allocated MEDIUM** (from `f1_race_tyre_compounds`). The laps-table `SOFT/MEDIUM/HARD` labels are already relative to that weekend's allocation, so `MEDIUM = 0` in the laps data is the correct reference. (§7)
2. **Team pace aggregation**: **`min`** of the two drivers' paces ("best car pace"). Each driver additionally reports `gap_to_constructor_sec = driver_pace - team_pace`. (§7, §8, §9)
3. **Fuel sensitivity**: **global** 0.035 s/kg/lap for v1 (no per-circuit calibration). (§4, §15)
4. **Minimum stints for a "valid" pace estimate**: **one stint of >5 laps is sufficient** (i.e., `n_stints >= 1` with `stint_len > 5` yields a non-fallback pace). Drivers with two or more stints get a more robust estimate (median across stints), but a single qualifying stint is still a "measured" pace, not an imputation. (§2, §8, §11)
5. **Wet races**: **do not exclude** (this is a data-extraction framework, not a modelling exercise). Wet laps are included and reported with `pace_source = "wet"`, `is_imputed = TRUE`. The compound-delta step is skipped for wet-only weeks. (§8, §11 edge cases)
6. **Race-pace integration**: **standalone** in v1. No changes to `simulate_race.R` or `calculate_driver_performance()`. (§12, §15)
7. **Pre-race mode**: **out of scope** for v1 (this is a data-extraction framework that requires practice data to exist). A future enhancement could add a `pre_race = TRUE` mode that uses the previous race's pace as a prior. (§15)
8. **`lap_start_time` (clock time)**: The user noted they can raise a `lap_start_time` column so laps can be compared on same-track-time. When available, this supersedes `lap_number` as the track-evolution proxy (§3 Option A becomes: `s(lap_start_time)` instead of `s(lap_number)`). The current plan uses `lap_number` as the v1 proxy; `lap_start_time` is the intended v2 improvement. (§3, §14)
