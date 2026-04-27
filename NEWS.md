# f1predicter 0.1.0.9000

* `save_gt_as_png_ragg()` now auto-detects optimal PNG `width` and `height` from the table's natural content dimensions when not explicitly provided, replacing fixed 1400×800 defaults. New `dpi` and `padding` parameters allow tuning the auto-detection (@pbulsink, #10).
* All internal functions now consistently use `@noRd`, all exported functions have `@examples` wrapped in `\dontrun{}`, and complex utility functions have inline documentation comments (#6).
