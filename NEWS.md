# f1predicter 0.1.0.9000

* `get_processing_params()` is a new exported function that returns the full set of tunable parameters (grid/position priors, failure rates, pit-stop defaults, etc.) used during data cleaning. Pass a modified copy to `clean_data(params = ...)` to tune the processing pipeline without touching source code (#8).
* `clean_data()` gains a `params` argument (default `get_processing_params()`) and a `cache_processed` argument that saves/reloads the cleaned data frame to `"processed_data.rds"` in the cache directory to avoid expensive reprocessing (#8).
* `get_weekend_data()`, `get_season_data()`, and `load_all_data()` now store and read cache files as `.rds` instead of `.csv` for faster I/O and lossless type preservation. Existing `.csv` files are read transparently as a fallback so caches built with earlier versions continue to work (#8).
* `migrate_cache_to_rds()` is a new exported maintenance utility that converts legacy season-level `.csv` cache files to `.rds` format (#8).
* All internal functions now consistently use `@noRd`, all exported functions have `@examples` wrapped in `\dontrun{}`, and complex utility functions have inline documentation comments (#6).
