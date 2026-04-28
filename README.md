<!-- badges: start -->
[![R-CMD-check](https://github.com/pbulsink/f1predicter/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pbulsink/f1predicter/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/pbulsink/f1predicter/graph/badge.svg)](https://app.codecov.io/gh/pbulsink/f1predicter)
<!-- badges: end -->

# f1predicter

**f1predicter** is an R package for predicting Formula 1 race and qualifying results, designed for robust, automated, and social-media-ready output. It supports modern F1 grid sizes (including 22 drivers) and is optimized for headless environments (e.g., Raspberry Pi).

## Features
- Predicts race and qualifying results, podium, top 10, and more
- Handles any number of drivers per race (dynamic grid)
- Produces beautiful probability/odds tables as PNG images (no browser required)
- Designed for automated posting to social media
- Comprehensive test suite for reliability

## Installation

```r
# Install from GitHub (requires devtools)
devtools::install_github("pbulsink/f1predicter")
```

## Usage

```r
library(f1predicter)

# Load and clean data
data <- clean_data()

# Generate predictions for the next race
df <- generate_new_data(season = 2026, round = 1, historical_data = data)
preds <- predict_round(df)

# Format and save a probability table as PNG
result <- format_results_prob_table(preds, save_image = TRUE)
cat("Table image saved to:", result$filename)
```

## Table Image Export (No Browser Needed)
All table image exports use the `ragg` package for PNG creation. No webshot, Chrome, or chromote required. This works reliably on headless/ARM systems (e.g., Raspberry Pi).

## Testing
Run the full test suite with:

```r
devtools::test()
```

## Contributing
Pull requests and issues are welcome! See [AGENTS.md](AGENTS.md) for workflow details.

## License
GPL-3
