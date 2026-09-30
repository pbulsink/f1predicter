# Regenerate all lightweight position-only ensemble models and save them to
# the model store.
#
# Trains the "ensemble_light" engine variant (regression + ordinal position
# models only, skipping pole/win/podium/t10) for every qualifying/results
# model timing and butchers + saves each to the path configured in
# `options('f1predicter.models')` (see `construct_model_path()` /
# `save_position_models()` in R/models_position_light.R). Run this manually
# whenever the underlying data or ensemble hyperparameters change and the
# saved lightweight models need to be refreshed.
#
# Usage:
#   Rscript data-raw/regenerate_position_ensembles.R

#devtools::load_all()

data <- clean_data()

cli::cli_h1("Regenerating lightweight qualifying position ensembles")
model_quali_position_early(data = data)
gc()
model_quali_position_late(data = data)
gc()

cli::cli_h1("Regenerating lightweight results position ensembles")
model_results_position_early(data = data)
gc()
model_results_position_late(data = data)
gc()
model_results_position_after_quali(data = data)
gc()

cli::cli_alert_success(
  "All lightweight position ensembles regenerated and saved."
)
