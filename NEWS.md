# f1predicter 0.1.0.9000

* `save_gt_as_png_ragg()` now auto-detects optimal PNG `width` and `height` from the table's natural content dimensions when not explicitly provided, replacing fixed 1400×800 defaults. New `dpi` and `padding` parameters allow tuning the auto-detection (@pbulsink, #10).
* `train_ordinal_ensemble()` now trains a stacked ensemble of ordinal classification engines (polr, ordinalNet, ordinalForest, rpartScore) using `ordered` + `tidymodels`, replacing the previous direct `MASS::polr` approach (#noissue).
* `predict_quali_pos_class()` and `predict_position_class()` now use the standard tidymodels/stacks predict interface, supporting both `last_fit` and `model_stack` objects (#noissue).
* All internal functions now consistently use `@noRd`, all exported functions have `@examples` wrapped in `\dontrun{}`, and complex utility functions have inline documentation comments (#6).
