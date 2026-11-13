# Predicting Stock Returns with Gradient Boosting

This is the repository for my Blog Post 9 project. I first revisit the stock-selection experiment from class and use the class results to show why stock returns are difficult to predict. I then reproduce the original XGBoost model, build an improved version, and compare both models with simple benchmarks using out-of-sample data.

The improved model found a stronger relationship between predicted and realized returns, but it did not beat the simple historical-mean benchmark on prediction error or produce reliably higher monthly investment returns.

## Repository guide

```text
Stock_Returns/
├── code/
│   ├── original/
│   ├── 01_class_experiment.R
│   ├── 02_reproduce_original_xgboost.R
│   ├── 03_improved_xgboost.R
│   └── 04_fair_model_comparison.R
├── data/
│   ├── raw/
│   └── processed/
├── models/
├── notes/
├── results/
│   ├── figures/
│   ├── predictions/
│   └── tables/
└── blogpost9_Stock_Returns.Rproj
```

### Code

The numbered scripts contain the full analysis and should be run in order.

- `01_class_experiment.R` analyzes the two class portfolios and my five-stock portfolio.
- `02_reproduce_original_xgboost.R` reproduces the original model and evaluates it on out-of-sample data.
- `03_improved_xgboost.R` creates new predictors, selects the model settings with an earlier validation period, and evaluates the improved model.
- `04_fair_model_comparison.R` compares both models using the same test period, top-10 portfolio rule, equal weights, and transaction-cost assumption.
- `code/original/` contains the original classroom scripts for reference.

### Data

- `data/raw/Full_Results.xlsx` contains the final portfolio values from the class experiment.
- `data/raw/my_own_stock.png` records the result of my five-stock portfolio.
- The `.rds` files contain the daily stock and SPY prices used in the analysis.
- `data/processed/` contains the datasets created by the analysis scripts.

### Models and results

- `models/` contains the fitted original and improved XGBoost models.
- `results/figures/` contains the figures created for the project.
- `results/tables/` contains the model metrics, portfolio results, and statistical comparisons.
- `results/predictions/` contains the out-of-sample predictions from both models.
- `notes/` contains short summaries of each part of the analysis.

## Data sources

The class portfolio results come from the course stock-market experiment. Each portfolio began with $100,000 on August 18, 2026, and remained unchanged through November 12, 2026.

Daily adjusted stock and SPY prices were obtained from Yahoo Finance using `tidyquant`. Cached copies are included in `data/raw/`, so the analysis can be reproduced without downloading the data again.

## Reproducing the analysis

Open `blogpost9_Stock_Returns.Rproj` in RStudio. The scripts use the `here` package, so they should be run from the project root.

Install the required packages if needed:

```r
install.packages(
  c(
    "tidyverse",
    "readxl",
    "tidyquant",
    "tidymodels",
    "lubridate",
    "slider",
    "xgboost",
    "here",
    "scales"
  )
)
```

Run the scripts in order:

```r
source("code/01_class_experiment.R")
source("code/02_reproduce_original_xgboost.R")
source("code/03_improved_xgboost.R")
source("code/04_fair_model_comparison.R")
```

Each script saves its outputs automatically. Running all four scripts recreates the processed data, fitted models, predictions, tables, figures, and result notes used in the project.