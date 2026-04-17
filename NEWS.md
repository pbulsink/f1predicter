# f1predicter 0.1.0 (development)

## New Features

* `process_quali_times()`, `create_constructor_features()`, `create_circuit_features()`, and `combine_and_finalize_features()` now apply exponential season decay weighting to historical averages. This prioritizes 2026 data (weight 1.0) while exponentially decaying prior seasons (0.5 per year), allowing the model to adapt to F1 rule changes while preserving institutional knowledge for new drivers and constructors (#noissue).

* Added utility functions for season-based exponential decay:
  - `season_decay_weights()` — Computes exponential weights for historical seasons
  - `s_lagged_cumwmean_expanded_with_season_decay()` — Applies combined in-season and season-decay weighting
  - `make_season_weighted_slider()` — Factory for creating season-aware sliding window functions

## Testing

* Added 6 new unit tests for season decay functionality in `test-utils.R` to prevent regressions.
