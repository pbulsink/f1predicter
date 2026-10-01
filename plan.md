Here is a complete, structured specification designed for an R-based data science agent. This plan uses `fastf1` (via `reticulate`) or processed CSV outputs from FastF1 to execute statistical modeling for fuel corrections, tire degradation curves, compound deltas, and driver/constructor race pace rankings.

---

# Execution Plan: F1 Race Pace & Compound Delta Analysis (R)

### Objective

Process raw Formula 1 Free Practice 2 (FP2) timing data to estimate:

1. Circuit track evolution rate.
2. Compound pace deltas ($\Delta C_{\text{compound}}$).
3. Driver-level and Constructor-level normalized long-run race paces.

---

## Workflow Overview

```
[Raw FP2 Laps] ──> [1. Data Cleaning] ──> [2. Track Evolution] ──> [3. Fuel Normalization] ──> [4. Compound Deltas] ──> [5. Final Pace Model]

```

---

## 1. Environment Setup & Data Ingestion

### R Dependencies

* Data Wrangling: `tidyverse` (`dplyr`, `tidyr`, `stringr`, `purrr`)
* Statistical Modeling: `stats`, `mgcv` (GAMs for non-linear track evolution), `lme4` (Mixed-Effects models)
* Output Visualizations: `ggplot2`

### Data Ingestion Guidelines

* Import FP2 timing data containing: `Driver`, `Team`, `LapNumber`, `LapTime` (converted to seconds), `Sector1Time`, `Sector2Time`, `Sector3Time`, `Compound`, `TyreLife`, `Stint`, `TrackStatus`, `IsAccurate`, and session elapsed time (`Time` in seconds).

---

## 2. Step 1: Data Cleaning & Stint Isolation

### Filtering Rules

1. **Remove Invalid Laps:**
* Keep only rows where `IsAccurate == TRUE` (filters out in-laps, out-laps, and timing glitches).
* Filter out laps where `TrackStatus != "1"` (removes Yellow Flags, VSC, Safety Car periods).


2. **Isolate Long Runs (High Fuel):**
* Filter for continuous stints with $\ge 5$ timed laps on the same set of tires (`Stint`).
* Drop the first lap of a stint (often a push/out-lap transition) and any lap where sector times indicate clear traffic (e.g., lap time exceeds $105\%$ of the stint median).



---

## 3. Step 2: Track Evolution Modeling

As rubber lays down, the track speeds up over the 60-minute session. We need to isolate session elapsed time from tire degradation.

### Methodology

1. Select all clean laps from short and long runs across all drivers.
2. Fit a Generalized Additive Model (GAM) or smooth spline to track global lap time decay against session elapsed time (`Time`):

```r
library(mgcv)
# Track evolution model across all valid laps
track_model <- gam(LapTime ~ s(SessionTimeSecs) + Driver + Compound, data = clean_laps)

# Extract track evolution effect relative to session mid-point (t0)
t0 <- median(clean_laps$SessionTimeSecs)
clean_laps <- clean_laps %>%
  mutate(TrackEvoCorrection = predict(track_model, newdata = .) - 
                              predict(track_model, newdata = transform(., SessionTimeSecs = t0))) %>%
  mutate(LapTime_EvoAdj = LapTime - TrackEvoCorrection)

```

---

## 4. Step 3: Fuel Load Correction

Because exact fuel loads are unpublished, apply a standardized FP2 fuel consumption heuristic for long runs.

### Constants to Define

* `FUEL_SENSITIVITY`: Track-specific factor (default to `0.035` seconds per kg per lap, or adjust based on track length: $\approx 0.03 \text{s/kg}$ for shorter tracks, $\approx 0.04 \text{s/kg}$ for high-power tracks like Spa/Monza).
* `EST_START_FUEL_KG`: Estimated starting fuel for long runs in FP2 (typically `35` kg).
* `BURN_RATE_KG`: Fuel burn per lap (typically `1.6` kg/lap).

### Calculation

For each long-run stint:


$$\text{Fuel}_{\text{lap}} = \max(0, \text{EST\_START\_FUEL\_KG} - (\text{StintLapNumber} \times \text{BURN\_RATE\_KG}))$$

$$\text{FuelCorrection} = \text{Fuel}_{\text{lap}} \times \text{FUEL\_SENSITIVITY}$$

$$\text{LapTime}_{\text{FuelEvoAdj}} = \text{LapTime}_{\text{EvoAdj}} - \text{FuelCorrection}$$

---

## 5. Step 4: Compound Delta Estimation

To compare a driver on Softs to a driver on Mediums, calculate global compound offsets using short-run qualifying simulations and long-run intercepts.

### Methodology

1. **Short-Run Peak Deltas:** Extract minimum `LapTime_EvoAdj` per driver per compound during low-fuel runs ($\text{Stint length} \le 3$ laps).
2. **Linear Regression with Mixed Effects:** Fit a linear mixed-effects model (`lmer`) to isolate compound differences while controlling for driver baseline speed:

```r
library(lme4)
# Fit mixed-effects model on short-run clean laps
compound_model <- lmer(LapTime_EvoAdj ~ Compound + (1 | Driver), data = short_run_laps)

# Extract fixed effects for compounds relative to Medium (baseline)
compound_deltas <- fixef(compound_model)

```

3. Output a lookup table of relative deltas (e.g., `SOFT_DELTA = -0.65s`, `MEDIUM_DELTA = 0.00s`, `HARD_DELTA = +0.50s`).

---

## 6. Step 5: Tire Degradation & Base Race Pace Modeling

Now model the degradation slope ($D_{\text{tire}}$) and theoretical initial race pace ($P_{0}$) per driver stint.

### Linear Regression per Stint

For each long-run stint ($s$), fit:


$$\text{LapTime}_{\text{FuelEvoAdj}} = P_{0} + (D_{\text{tire}} \times \text{TyreLife})$$

```r
stint_models <- long_run_laps %>%
  group_by(Driver, Team, Compound, Stint) %>%
  nest() %>%
  mutate(
    fit = map(data, ~ lm(LapTime_FuelEvoAdj ~ TyreLife, data = .x)),
    tidied = map(fit, broom::tidy)
  ) %>%
  unnest(tidied)

# Extract Intercept (P0 = Baseline Pace) and Slope (Tire Deg Rate)
stint_summary <- stint_models %>%
  select(Driver, Team, Compound, Stint, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate) %>%
  rename(P0 = `(Intercept)`, DegRate = TyreLife)

```

---

## 7. Step 6: Normalization & Aggregation

Convert all stints to a standardized benchmark to generate final rankings:

* Benchmark Compound: **Medium**
* Benchmark Fuel Load: **0 kg (or standardized 30 kg baseline)**
* Benchmark Tire Life: **Lap 5 of stint**

### Normalization Formula

$$\text{NormalizedPace} = P_{0} + (D_{\text{tire}} \times 5) - \Delta C_{\text{compound}}$$

### Final Aggregations

1. **Driver Expected Race Pace:** Median of `NormalizedPace` across all long-run stints for each driver.
2. **Constructor Expected Race Pace:** Mean or minimum of the two drivers' median `NormalizedPace` values for each team.

---

## Expected Outputs & Deliverables

The executing R script should output three clean summary data frames:

1. `compound_deltas_df`: Single-lap and long-run pace offsets between Soft, Medium, and Hard compounds.
2. `driver_race_pace_df`: Ranked list of drivers with columns: `Driver`, `Team`, `PrimaryCompound`, `DegradationRate_sec_per_lap`, `NormalizedRacePace`.
3. `constructor_race_pace_df`: Ranked list of teams with columns: `Team`, `TeamRacePace`, `GapToLeader_sec`.
