# f1predicter 0.1.0.9000

* `get_weekend_data()`, `get_season_data()`, `load_all_data()`, and `clean_data()` now use a single `f1predicter.sqlite` cache with legacy RDS/CSV migration support (@pbulsink, #9).
* `clean_data()` gains a `params` argument (a named list of processing priors and defaults) and a `cache_processed` argument that saves/reloads the cleaned data frame to `"processed_data.rds"` in the cache directory to avoid expensive reprocessing (#8).
* `get_weekend_data()`, `get_season_data()`, and `load_all_data()` now store and read cache files as `.rds` instead of `.csv` for faster I/O and lossless type preservation. Existing `.csv` files are read transparently as a fallback so caches built with earlier versions continue to work (#8).
* `save_gt_as_png_ragg()` now auto-detects optimal PNG `width` and `height` from the table's natural content dimensions when not explicitly provided, replacing fixed 1400×800 defaults. New `dpi` and `padding` parameters allow tuning the auto-detection (@pbulsink, #10).
* All internal functions now consistently use `@noRd`, all exported functions have `@examples` wrapped in `\dontrun{}`, and complex utility functions have inline documentation comments (#6).
