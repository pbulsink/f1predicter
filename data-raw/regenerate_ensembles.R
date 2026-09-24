# Regenerate all ensemble models and save them to the model store.
#
# Trains the "ensemble" engine variant for every qualifying/results model
# timing and butchers + saves each to the path configured in
# `options('f1predicter.models')` (see `construct_model_path()` /
# `save_models()` in R/models_reg.R). Run this manually whenever the
# underlying data or ensemble hyperparameters change and the saved models
# need to be refreshed.
#
# Usage:
#   Rscript data-raw/regenerate_ensembles.R

#devtools::load_all()

data <- clean_data()

cli::cli_h1("Regenerating qualifying ensembles")
model_quali_early(data = data, engine = "ensemble")
gc()
model_quali_late(data = data, engine = "ensemble")
gc()

cli::cli_h1("Regenerating results ensembles")
model_results_early(data = data, engine = "ensemble")
gc()
model_results_late(data = data, engine = "ensemble")
gc()
model_results_after_quali(data = data, engine = "ensemble")
gc()

cli::cli_alert_success("All ensemble models regenerated and saved.")
