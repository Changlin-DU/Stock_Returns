# ============================================================
# Blog Post 9
# Part 3: Improved XGBoost Model
# ============================================================

# ============================================================
# 1. PACKAGES
# ============================================================

required_packages <- c(
  "tidyverse",
  "tidymodels",
  "lubridate",
  "slider",
  "xgboost",
  "here",
  "scales"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Install these packages before continuing: ",
      paste(missing_packages, collapse = ", ")
    )
  )
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(tidymodels)
  library(lubridate)
  library(slider)
  library(xgboost)
  library(here)
  library(scales)
})

tidymodels_prefer()

set.seed(123)
options(scipen = 999)


# ============================================================
# 2. SETTINGS
# ============================================================

data_start <- as.Date("2022-01-01")
data_end <- as.Date("2026-11-12")

validation_start <- as.Date("2023-07-01")
test_start <- as.Date("2024-01-01")

prediction_horizon <- 20
portfolio_size <- 10

# Approximate one-way trading cost:
# 10 basis points = 0.10%.
transaction_cost_bps <- 10
transaction_cost_rate <- transaction_cost_bps / 10000


# ============================================================
# 3. COLORS
# ============================================================

pink_fill <- "#F5B2C8"
pink_dark <- "#C85F87"

blue_fill <- "#A9D6EE"
blue_dark <- "#4F91B8"

grey_medium <- "#8A9199"
grey_dark <- "#67717E"
black_color <- "#111111"

text_color <- "#263238"
grid_color <- "#E7E9ED"


theme_blog <- function() {
  theme_minimal(base_size = 12) +
    theme(
      text = element_text(
        family = "sans",
        color = text_color
      ),
      
      plot.title = element_text(
        size = 16,
        face = "bold",
        color = text_color,
        margin = margin(b = 6)
      ),
      
      plot.subtitle = element_text(
        size = 11,
        color = "#56616A",
        margin = margin(b = 12)
      ),
      
      plot.caption = element_text(
        size = 9,
        color = "#6B7280",
        hjust = 0,
        margin = margin(t = 12)
      ),
      
      axis.title = element_text(
        size = 11,
        color = text_color
      ),
      
      axis.text = element_text(
        size = 10,
        color = text_color
      ),
      
      panel.grid.minor = element_blank(),
      
      panel.grid.major = element_line(
        color = grid_color,
        linewidth = 0.4
      ),
      
      legend.position = "top",
      
      legend.justification = "left",
      
      legend.title = element_blank(),
      
      plot.margin = margin(
        t = 14,
        r = 18,
        b = 14,
        l = 14
      )
    )
}


# ============================================================
# 4. OUTPUT FOLDERS
# ============================================================

folders <- c(
  here("data", "processed"),
  here("models"),
  here("results", "tables"),
  here("results", "figures"),
  here("results", "predictions"),
  here("notes")
)

invisible(
  lapply(
    folders,
    dir.create,
    recursive = TRUE,
    showWarnings = FALSE
  )
)


# ============================================================
# 5. LOAD CACHED MARKET DATA
# ============================================================

stock_cache <- here(
  "data",
  "raw",
  "stock_prices_2022-01-01_to_2026-11-12.rds"
)

spy_cache <- here(
  "data",
  "raw",
  "spy_prices_2022-01-01_to_2026-11-12.rds"
)


if (!file.exists(stock_cache)) {
  stop(
    "Cached stock data were not found. ",
    "Run code/02_reproduce_original_xgboost.R first."
  )
}

if (!file.exists(spy_cache)) {
  stop(
    "Cached SPY data were not found. ",
    "Run code/02_reproduce_original_xgboost.R first."
  )
}


prices <- readRDS(stock_cache)

spy_prices <- readRDS(spy_cache)


prices <- prices |>
  dplyr::filter(
    date >= data_start,
    date <= data_end,
    !is.na(adjusted)
  ) |>
  dplyr::distinct(
    symbol,
    date,
    .keep_all = TRUE
  ) |>
  dplyr::arrange(
    symbol,
    date
  )


spy_prices <- spy_prices |>
  dplyr::filter(
    date >= data_start,
    date <= data_end,
    !is.na(adjusted)
  ) |>
  dplyr::distinct(
    date,
    .keep_all = TRUE
  ) |>
  dplyr::arrange(date)


# ============================================================
# 6. SPY FEATURES
# ============================================================

spy_features <- spy_prices |>
  dplyr::transmute(
    date,
    spy_price = adjusted
  ) |>
  dplyr::arrange(date) |>
  dplyr::mutate(
    spy_return =
      spy_price /
      dplyr::lag(
        spy_price,
        n = 1
      ) - 1,
    
    spy_mom_1m =
      dplyr::lag(
        spy_price,
        n = 1
      ) /
      dplyr::lag(
        spy_price,
        n = 21
      ) - 1,
    
    spy_mom_3m =
      dplyr::lag(
        spy_price,
        n = 1
      ) /
      dplyr::lag(
        spy_price,
        n = 63
      ) - 1,
    
    spy_mom_6m =
      dplyr::lag(
        spy_price,
        n = 1
      ) /
      dplyr::lag(
        spy_price,
        n = 126
      ) - 1,
    
    spy_target =
      dplyr::lead(
        spy_price,
        n = prediction_horizon
      ) /
      dplyr::lead(
        spy_price,
        n = 1
      ) - 1
  )


# ============================================================
# 7. HELPER FUNCTIONS FOR ROLLING FEATURES
# ============================================================

safe_beta <- function(
    stock_returns,
    market_returns
) {
  
  valid <- complete.cases(
    stock_returns,
    market_returns
  )
  
  stock_returns <- stock_returns[valid]
  market_returns <- market_returns[valid]
  
  if (
    length(stock_returns) < 20 ||
    stats::var(market_returns) == 0
  ) {
    return(NA_real_)
  }
  
  stats::cov(
    stock_returns,
    market_returns
  ) /
    stats::var(
      market_returns
    )
}


# ============================================================
# 8. IMPROVED FEATURE ENGINEERING
# ============================================================

improved_data <- prices |>
  dplyr::group_by(symbol) |>
  dplyr::arrange(
    date,
    .by_group = TRUE
  ) |>
  dplyr::mutate(
    stock_return =
      adjusted /
      dplyr::lag(
        adjusted,
        n = 1
      ) - 1
  ) |>
  dplyr::ungroup() |>
  dplyr::left_join(
    spy_features,
    by = "date"
  ) |>
  dplyr::group_by(symbol) |>
  dplyr::arrange(
    date,
    .by_group = TRUE
  ) |>
  dplyr::mutate(
    # --------------------------------------------------------
    # Correctly directed momentum features
    # --------------------------------------------------------
    
    mom_1m =
      dplyr::lag(
        adjusted,
        n = 1
      ) /
      dplyr::lag(
        adjusted,
        n = 21
      ) - 1,
    
    mom_3m =
      dplyr::lag(
        adjusted,
        n = 1
      ) /
      dplyr::lag(
        adjusted,
        n = 63
      ) - 1,
    
    mom_6m =
      dplyr::lag(
        adjusted,
        n = 1
      ) /
      dplyr::lag(
        adjusted,
        n = 126
      ) - 1,
    
    mom_12m =
      dplyr::lag(
        adjusted,
        n = 1
      ) /
      dplyr::lag(
        adjusted,
        n = 252
      ) - 1,
    
    # Correctly directed five-day return
    ret_5d =
      dplyr::lag(
        adjusted,
        n = 1
      ) /
      dplyr::lag(
        adjusted,
        n = 6
      ) - 1,
    
    # --------------------------------------------------------
    # Relative momentum
    # --------------------------------------------------------
    
    excess_mom_1m =
      mom_1m - spy_mom_1m,
    
    excess_mom_3m =
      mom_3m - spy_mom_3m,
    
    excess_mom_6m =
      mom_6m - spy_mom_6m,
    
    # --------------------------------------------------------
    # Lagged return and market-adjusted return
    # --------------------------------------------------------
    
    lagged_return_1d =
      dplyr::lag(
        stock_return,
        n = 1
      ),
    
    lagged_excess_return_1d =
      dplyr::lag(
        stock_return - spy_return,
        n = 1
      ),
    
    # --------------------------------------------------------
    # Volatility
    # --------------------------------------------------------
    
    vol_1m = slider::slide_dbl(
      dplyr::lag(
        stock_return,
        n = 1
      ),
      stats::sd,
      .before = 20,
      .complete = TRUE
    ),
    
    vol_3m = slider::slide_dbl(
      dplyr::lag(
        stock_return,
        n = 1
      ),
      stats::sd,
      .before = 62,
      .complete = TRUE
    ),
    
    idiosyncratic_vol_3m =
      slider::slide_dbl(
        dplyr::lag(
          stock_return - spy_return,
          n = 1
        ),
        stats::sd,
        .before = 62,
        .complete = TRUE
      ),
    
    volatility_ratio =
      vol_1m / vol_3m,
    
    # --------------------------------------------------------
    # Rolling market beta
    # --------------------------------------------------------
    
    beta_3m = slider::slide2_dbl(
      dplyr::lag(
        stock_return,
        n = 1
      ),
      
      dplyr::lag(
        spy_return,
        n = 1
      ),
      
      safe_beta,
      
      .before = 62,
      .complete = TRUE
    ),
    
    # --------------------------------------------------------
    # Drawdown from the preceding three-month high
    # --------------------------------------------------------
    
    lagged_price =
      dplyr::lag(
        adjusted,
        n = 1
      ),
    
    rolling_high_3m =
      slider::slide_dbl(
        lagged_price,
        ~ max(
          .x,
          na.rm = TRUE
        ),
        .before = 62,
        .complete = TRUE
      ),
    
    drawdown_3m =
      lagged_price /
      rolling_high_3m - 1,
    
    # --------------------------------------------------------
    # Trading-volume signal
    # --------------------------------------------------------
    
    lagged_volume =
      dplyr::lag(
        volume,
        n = 1
      ),
    
    average_volume_1m =
      slider::slide_dbl(
        lagged_volume,
        ~ mean(
          .x,
          na.rm = TRUE
        ),
        .before = 20,
        .complete = TRUE
      ),
    
    volume_ratio_1m =
      lagged_volume /
      average_volume_1m - 1,
    
    # --------------------------------------------------------
    # Target and target end date
    # --------------------------------------------------------
    
    target =
      dplyr::lead(
        adjusted,
        n = prediction_horizon
      ) /
      dplyr::lead(
        adjusted,
        n = 1
      ) - 1,
    
    target_end_date =
      dplyr::lead(
        date,
        n = prediction_horizon
      )
  ) |>
  dplyr::ungroup() |>
  dplyr::select(
    date,
    symbol,
    target_end_date,
    target,
    
    mom_1m,
    mom_3m,
    mom_6m,
    mom_12m,
    
    ret_5d,
    
    excess_mom_1m,
    excess_mom_3m,
    excess_mom_6m,
    
    lagged_return_1d,
    lagged_excess_return_1d,
    
    vol_1m,
    vol_3m,
    idiosyncratic_vol_3m,
    volatility_ratio,
    
    beta_3m,
    drawdown_3m,
    volume_ratio_1m
  ) |>
  tidyr::drop_na()


if (nrow(improved_data) == 0) {
  stop(
    "Improved feature engineering produced no observations."
  )
}


write_csv(
  improved_data,
  here(
    "data",
    "processed",
    "improved_model_data.csv"
  )
)


# ============================================================
# 9. PURGED TRAIN, VALIDATION, AND TEST SPLITS
# ============================================================

# Training labels must end before validation begins.
tuning_train <- improved_data |>
  dplyr::filter(
    target_end_date < validation_start
  )

# Validation labels must end before the test period begins.
validation <- improved_data |>
  dplyr::filter(
    date >= validation_start,
    target_end_date < test_start
  )

# Final training labels must end before the test begins.
final_train <- improved_data |>
  dplyr::filter(
    target_end_date < test_start
  )

test <- improved_data |>
  dplyr::filter(
    date >= test_start
  )


if (
  nrow(tuning_train) == 0 ||
  nrow(validation) == 0 ||
  nrow(final_train) == 0 ||
  nrow(test) == 0
) {
  stop(
    "One or more time-based samples are empty."
  )
}


split_summary <- tibble(
  sample = c(
    "Tuning train",
    "Validation",
    "Final purged train",
    "Out-of-sample test"
  ),
  
  first_date = c(
    min(tuning_train$date),
    min(validation$date),
    min(final_train$date),
    min(test$date)
  ),
  
  last_date = c(
    max(tuning_train$date),
    max(validation$date),
    max(final_train$date),
    max(test$date)
  ),
  
  observations = c(
    nrow(tuning_train),
    nrow(validation),
    nrow(final_train),
    nrow(test)
  ),
  
  tickers = c(
    n_distinct(tuning_train$symbol),
    n_distinct(validation$symbol),
    n_distinct(final_train$symbol),
    n_distinct(test$symbol)
  )
)


write_csv(
  split_summary,
  here(
    "results",
    "tables",
    "improved_data_split_summary.csv"
  )
)


# ============================================================
# 10. NUMBER OF PROCESSOR THREADS
# ============================================================

available_cores <- parallel::detectCores(
  logical = FALSE
)

if (
  is.na(available_cores) ||
  available_cores < 1
) {
  available_cores <- 1
}

xgb_threads <- max(
  1,
  min(
    4,
    available_cores
  )
)


# ============================================================
# 11. MODEL-SPECIFICATION FUNCTION
# ============================================================

make_xgb_spec <- function(
    trees,
    tree_depth,
    learn_rate,
    min_n,
    loss_reduction,
    sample_size,
    mtry_proportion
) {
  
  parsnip::boost_tree(
    trees = as.integer(trees),
    tree_depth = as.integer(tree_depth),
    learn_rate = learn_rate,
    min_n = as.integer(min_n),
    loss_reduction = loss_reduction,
    sample_size = sample_size,
    mtry = mtry_proportion
  ) |>
    parsnip::set_engine(
      "xgboost",
      counts = FALSE,
      nthread = xgb_threads,
      verbosity = 0
    ) |>
    parsnip::set_mode(
      "regression"
    )
}


# ============================================================
# 12. VALIDATION RANK-CORRELATION FUNCTION
# ============================================================

calculate_rank_summary <- function(
    prediction_data
) {
  
  daily_values <- prediction_data |>
    dplyr::group_by(date) |>
    dplyr::summarise(
      rank_ic = if (
        dplyr::n_distinct(prediction) > 1 &&
        dplyr::n_distinct(target) > 1
      ) {
        stats::cor(
          prediction,
          target,
          method = "spearman",
          use = "complete.obs"
        )
      } else {
        NA_real_
      },
      
      .groups = "drop"
    )
  
  tibble(
    mean_rank_ic =
      mean(
        daily_values$rank_ic,
        na.rm = TRUE
      ),
    
    median_rank_ic =
      median(
        daily_values$rank_ic,
        na.rm = TRUE
      ),
    
    positive_rank_ic_share =
      mean(
        daily_values$rank_ic > 0,
        na.rm = TRUE
      )
  )
}


# ============================================================
# 13. CANDIDATE HYPERPARAMETERS
# ============================================================

candidate_grid <- tribble(
  ~candidate, ~trees, ~tree_depth, ~learn_rate,
  ~min_n, ~loss_reduction, ~sample_size,
  ~mtry_proportion,
  
  1, 300, 2, 0.050, 30, 0.00, 0.80, 0.80,
  2, 500, 2, 0.030, 30, 0.01, 0.80, 0.80,
  3, 400, 3, 0.030, 30, 0.01, 0.80, 0.80,
  4, 300, 3, 0.050, 20, 0.00, 0.80, 0.80,
  5, 500, 3, 0.030, 50, 0.05, 0.80, 0.80,
  6, 400, 4, 0.030, 50, 0.05, 0.75, 0.70,
  7, 600, 2, 0.020, 40, 0.01, 0.85, 0.90,
  8, 500, 4, 0.020, 60, 0.10, 0.75, 0.70
)


write_csv(
  candidate_grid,
  here(
    "results",
    "tables",
    "improved_candidate_grid.csv"
  )
)


# ============================================================
# 14. VALIDATION RECIPE
# ============================================================

tuning_recipe <- recipes::recipe(
  target ~ .,
  data = tuning_train
) |>
  recipes::update_role(
    date,
    symbol,
    target_end_date,
    new_role = "id"
  )


# ============================================================
# 15. VALIDATE CANDIDATE MODELS
# ============================================================

tuning_results_list <- vector(
  mode = "list",
  length = nrow(candidate_grid)
)


for (i in seq_len(nrow(candidate_grid))) {
  
  candidate_row <- candidate_grid[i, ]
  
  message(
    paste0(
      "Validating candidate ",
      candidate_row$candidate,
      " of ",
      nrow(candidate_grid),
      "..."
    )
  )
  
  tuning_results_list[[i]] <- tryCatch(
    {
      
      candidate_spec <- make_xgb_spec(
        trees =
          candidate_row$trees,
        
        tree_depth =
          candidate_row$tree_depth,
        
        learn_rate =
          candidate_row$learn_rate,
        
        min_n =
          candidate_row$min_n,
        
        loss_reduction =
          candidate_row$loss_reduction,
        
        sample_size =
          candidate_row$sample_size,
        
        mtry_proportion =
          candidate_row$mtry_proportion
      )
      
      
      candidate_workflow <- workflows::workflow() |>
        workflows::add_recipe(
          tuning_recipe
        ) |>
        workflows::add_model(
          candidate_spec
        )
      
      
      candidate_fit <- generics::fit(
        candidate_workflow,
        data = tuning_train
      )
      
      
      candidate_predictions <- stats::predict(
        candidate_fit,
        new_data = validation
      ) |>
        dplyr::bind_cols(
          validation |>
            dplyr::select(
              date,
              symbol,
              target
            )
        ) |>
        dplyr::transmute(
          date,
          symbol,
          target,
          prediction = .pred
        )
      
      
      validation_rank <- calculate_rank_summary(
        candidate_predictions
      )
      
      
      validation_rmse <- sqrt(
        mean(
          (
            candidate_predictions$target -
              candidate_predictions$prediction
          )^2
        )
      )
      
      
      validation_mae <- mean(
        abs(
          candidate_predictions$target -
            candidate_predictions$prediction
        )
      )
      
      
      dplyr::bind_cols(
        candidate_row,
        validation_rank
      ) |>
        dplyr::mutate(
          validation_rmse =
            validation_rmse,
          
          validation_mae =
            validation_mae,
          
          status =
            "success"
        )
    },
    error = function(e) {
      
      candidate_row |>
        dplyr::mutate(
          mean_rank_ic = NA_real_,
          median_rank_ic = NA_real_,
          positive_rank_ic_share = NA_real_,
          validation_rmse = NA_real_,
          validation_mae = NA_real_,
          status = paste0(
            "error: ",
            conditionMessage(e)
          )
        )
    }
  )
}


tuning_results <- dplyr::bind_rows(
  tuning_results_list
)


write_csv(
  tuning_results,
  here(
    "results",
    "tables",
    "improved_tuning_results.csv"
  )
)


successful_candidates <- tuning_results |>
  dplyr::filter(
    status == "success",
    !is.na(mean_rank_ic)
  )


if (nrow(successful_candidates) == 0) {
  stop(
    "All improved-model candidates failed."
  )
}


# Select by validation rank correlation, then RMSE.
best_parameters <- successful_candidates |>
  dplyr::arrange(
    dplyr::desc(mean_rank_ic),
    validation_rmse
  ) |>
  dplyr::slice_head(n = 1)


write_csv(
  best_parameters,
  here(
    "results",
    "tables",
    "improved_best_parameters.csv"
  )
)


# ============================================================
# 16. FINAL IMPROVED MODEL
# ============================================================

final_recipe <- recipes::recipe(
  target ~ .,
  data = final_train
) |>
  recipes::update_role(
    date,
    symbol,
    target_end_date,
    new_role = "id"
  )


final_spec <- make_xgb_spec(
  trees =
    best_parameters$trees,
  
  tree_depth =
    best_parameters$tree_depth,
  
  learn_rate =
    best_parameters$learn_rate,
  
  min_n =
    best_parameters$min_n,
  
  loss_reduction =
    best_parameters$loss_reduction,
  
  sample_size =
    best_parameters$sample_size,
  
  mtry_proportion =
    best_parameters$mtry_proportion
)


improved_workflow <- workflows::workflow() |>
  workflows::add_recipe(
    final_recipe
  ) |>
  workflows::add_model(
    final_spec
  )


message("Fitting final improved XGBoost model...")

fit_start_time <- Sys.time()

improved_fit <- generics::fit(
  improved_workflow,
  data = final_train
)

fit_end_time <- Sys.time()


improved_fit_minutes <- as.numeric(
  difftime(
    fit_end_time,
    fit_start_time,
    units = "mins"
  )
)


saveRDS(
  improved_fit,
  here(
    "models",
    "improved_xgboost_fit.rds"
  )
)


# ============================================================
# 17. OUT-OF-SAMPLE PREDICTIONS
# ============================================================

improved_predictions <- stats::predict(
  improved_fit,
  new_data = test
) |>
  dplyr::bind_cols(
    test |>
      dplyr::select(
        date,
        symbol,
        target
      )
  ) |>
  dplyr::arrange(
    date,
    symbol
  )


training_mean_target <- mean(
  final_train$target,
  na.rm = TRUE
)


improved_predictions <- improved_predictions |>
  dplyr::mutate(
    historical_mean_prediction =
      training_mean_target,
    
    zero_prediction =
      0
  )


write_csv(
  improved_predictions,
  here(
    "results",
    "predictions",
    "improved_xgboost_predictions.csv"
  )
)


# ============================================================
# 18. PREDICTIVE PERFORMANCE
# ============================================================

safe_correlation <- function(
    actual,
    predicted
) {
  
  if (
    stats::sd(actual) == 0 ||
    stats::sd(predicted) == 0
  ) {
    return(NA_real_)
  }
  
  stats::cor(
    actual,
    predicted,
    use = "complete.obs"
  )
}


prediction_metrics <- function(
    actual,
    predicted,
    model_name
) {
  
  residuals <- actual - predicted
  
  tibble(
    model = model_name,
    
    observations =
      length(actual),
    
    rmse =
      sqrt(
        mean(
          residuals^2
        )
      ),
    
    mae =
      mean(
        abs(residuals)
      ),
    
    r_squared =
      1 -
      sum(
        residuals^2
      ) /
      sum(
        (
          actual - mean(actual)
        )^2
      ),
    
    pearson_correlation =
      safe_correlation(
        actual,
        predicted
      ),
    
    directional_accuracy =
      mean(
        sign(predicted) ==
          sign(actual)
      ),
    
    mean_prediction =
      mean(predicted),
    
    mean_actual_return =
      mean(actual)
  )
}


improved_predictive_performance <- bind_rows(
  prediction_metrics(
    actual =
      improved_predictions$target,
    
    predicted =
      improved_predictions$.pred,
    
    model_name =
      "Improved XGBoost"
  ),
  
  prediction_metrics(
    actual =
      improved_predictions$target,
    
    predicted =
      improved_predictions$
      historical_mean_prediction,
    
    model_name =
      "Historical mean prediction"
  ),
  
  prediction_metrics(
    actual =
      improved_predictions$target,
    
    predicted =
      improved_predictions$
      zero_prediction,
    
    model_name =
      "Zero-return prediction"
  )
)


write_csv(
  improved_predictive_performance,
  here(
    "results",
    "tables",
    "improved_predictive_performance.csv"
  )
)


# ============================================================
# 19. TEST-PERIOD RANK CORRELATION
# ============================================================

improved_daily_rank_ic <- improved_predictions |>
  dplyr::transmute(
    date,
    target,
    prediction = .pred
  ) |>
  dplyr::group_by(date) |>
  dplyr::summarise(
    rank_ic = if (
      dplyr::n_distinct(prediction) > 1 &&
      dplyr::n_distinct(target) > 1
    ) {
      stats::cor(
        prediction,
        target,
        method = "spearman",
        use = "complete.obs"
      )
    } else {
      NA_real_
    },
    
    .groups = "drop"
  )


improved_rank_summary <- improved_daily_rank_ic |>
  dplyr::summarise(
    evaluation_dates =
      sum(!is.na(rank_ic)),
    
    mean_rank_ic =
      mean(
        rank_ic,
        na.rm = TRUE
      ),
    
    median_rank_ic =
      median(
        rank_ic,
        na.rm = TRUE
      ),
    
    sd_rank_ic =
      stats::sd(
        rank_ic,
        na.rm = TRUE
      ),
    
    positive_rank_ic_share =
      mean(
        rank_ic > 0,
        na.rm = TRUE
      )
  )


write_csv(
  improved_daily_rank_ic,
  here(
    "results",
    "tables",
    "improved_daily_rank_ic.csv"
  )
)

write_csv(
  improved_rank_summary,
  here(
    "results",
    "tables",
    "improved_rank_ic_summary.csv"
  )
)


# ============================================================
# 20. MONTHLY PORTFOLIO FORMATION
# ============================================================

formation_dates <- improved_predictions |>
  dplyr::mutate(
    month = lubridate::floor_date(
      date,
      unit = "month"
    )
  ) |>
  dplyr::group_by(month) |>
  dplyr::summarise(
    date = min(date),
    .groups = "drop"
  )


formation_universe <- improved_predictions |>
  dplyr::mutate(
    month = lubridate::floor_date(
      date,
      unit = "month"
    )
  ) |>
  dplyr::inner_join(
    formation_dates,
    by = c(
      "month",
      "date"
    )
  )


# ============================================================
# 21. IMPROVED TOP-10 EQUAL-WEIGHT PORTFOLIO
# ============================================================

improved_holdings <- formation_universe |>
  dplyr::group_by(
    month,
    date
  ) |>
  dplyr::slice_max(
    order_by = .pred,
    n = portfolio_size,
    with_ties = FALSE
  ) |>
  dplyr::arrange(
    month,
    dplyr::desc(.pred)
  ) |>
  dplyr::mutate(
    rank =
      dplyr::row_number(),
    
    weight =
      1 / dplyr::n()
  ) |>
  dplyr::ungroup()


write_csv(
  improved_holdings,
  here(
    "results",
    "tables",
    "improved_monthly_holdings.csv"
  )
)


# ============================================================
# 22. TURNOVER
# ============================================================

calculate_turnover <- function(
    holdings
) {
  
  portfolio_dates <- sort(
    unique(
      holdings$date
    )
  )
  
  turnover_values <- numeric(
    length(portfolio_dates)
  )
  
  turnover_values[[1]] <- 1
  
  if (length(portfolio_dates) > 1) {
    
    for (i in 2:length(portfolio_dates)) {
      
      previous_weights <- holdings |>
        dplyr::filter(
          date == portfolio_dates[[i - 1]]
        ) |>
        dplyr::select(
          symbol,
          previous_weight = weight
        )
      
      current_weights <- holdings |>
        dplyr::filter(
          date == portfolio_dates[[i]]
        ) |>
        dplyr::select(
          symbol,
          current_weight = weight
        )
      
      combined_weights <- dplyr::full_join(
        previous_weights,
        current_weights,
        by = "symbol"
      ) |>
        dplyr::mutate(
          previous_weight =
            tidyr::replace_na(
              previous_weight,
              0
            ),
          
          current_weight =
            tidyr::replace_na(
              current_weight,
              0
            )
        )
      
      turnover_values[[i]] <-
        0.5 *
        sum(
          abs(
            combined_weights$current_weight -
              combined_weights$previous_weight
          )
        )
    }
  }
  
  tibble(
    date =
      portfolio_dates,
    
    turnover =
      turnover_values
  )
}


improved_turnover <- calculate_turnover(
  improved_holdings
)


write_csv(
  improved_turnover,
  here(
    "results",
    "tables",
    "improved_turnover.csv"
  )
)


# ============================================================
# 23. MONTHLY RETURNS AND BENCHMARKS
# ============================================================

improved_monthly_returns <- improved_holdings |>
  dplyr::group_by(
    month,
    date
  ) |>
  dplyr::summarise(
    improved_gross_return =
      sum(
        weight * target,
        na.rm = TRUE
      ),
    
    selected_stocks =
      dplyr::n(),
    
    .groups = "drop"
  ) |>
  dplyr::left_join(
    improved_turnover,
    by = "date"
  ) |>
  dplyr::mutate(
    transaction_cost =
      turnover *
      transaction_cost_rate,
    
    improved_net_return =
      improved_gross_return -
      transaction_cost
  )


equal_weight_returns <- formation_universe |>
  dplyr::group_by(
    month,
    date
  ) |>
  dplyr::summarise(
    equal_weight_return =
      mean(
        target,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )


spy_forward_returns <- spy_features |>
  dplyr::filter(
    !is.na(spy_target)
  ) |>
  dplyr::transmute(
    date,
    spy_return = spy_target
  )


improved_backtest <- improved_monthly_returns |>
  dplyr::inner_join(
    equal_weight_returns,
    by = c(
      "month",
      "date"
    )
  ) |>
  dplyr::inner_join(
    spy_forward_returns,
    by = "date"
  ) |>
  dplyr::arrange(date) |>
  dplyr::mutate(
    improved_gross_growth =
      cumprod(
        1 + improved_gross_return
      ),
    
    improved_net_growth =
      cumprod(
        1 + improved_net_return
      ),
    
    equal_weight_growth =
      cumprod(
        1 + equal_weight_return
      ),
    
    spy_growth =
      cumprod(
        1 + spy_return
      )
  )


write_csv(
  improved_backtest,
  here(
    "results",
    "tables",
    "improved_monthly_backtest.csv"
  )
)


# ============================================================
# 24. INVESTMENT METRICS
# ============================================================

investment_metrics <- function(
    returns,
    strategy_name
) {
  
  returns <- returns[
    !is.na(returns)
  ]
  
  number_of_months <- length(returns)
  
  wealth <- cumprod(
    1 + returns
  )
  
  drawdown <-
    wealth /
    cummax(wealth) - 1
  
  tibble(
    strategy =
      strategy_name,
    
    months =
      number_of_months,
    
    total_return =
      tail(
        wealth,
        1
      ) - 1,
    
    annualized_return =
      prod(
        1 + returns
      )^(
        12 /
          number_of_months
      ) - 1,
    
    annualized_volatility =
      stats::sd(
        returns
      ) *
      sqrt(12),
    
    annualized_sharpe_rf_zero =
      mean(returns) /
      stats::sd(returns) *
      sqrt(12),
    
    maximum_drawdown =
      min(drawdown),
    
    positive_month_share =
      mean(
        returns > 0
      ),
    
    mean_monthly_return =
      mean(returns)
  )
}


improved_investment_performance <- bind_rows(
  investment_metrics(
    improved_backtest$improved_gross_return,
    "Improved XGBoost gross"
  ),
  
  investment_metrics(
    improved_backtest$improved_net_return,
    "Improved XGBoost net"
  ),
  
  investment_metrics(
    improved_backtest$equal_weight_return,
    "Equal-weight stock universe"
  ),
  
  investment_metrics(
    improved_backtest$spy_return,
    "SPY"
  )
)


write_csv(
  improved_investment_performance,
  here(
    "results",
    "tables",
    "improved_investment_performance.csv"
  )
)


# ============================================================
# 25. CAPM RESULTS
# ============================================================

calculate_capm <- function(
    strategy_returns,
    market_returns,
    strategy_name
) {
  
  capm_data <- tibble(
    strategy_return =
      strategy_returns,
    
    market_return =
      market_returns
  ) |>
    tidyr::drop_na()
  
  capm_fit <- stats::lm(
    strategy_return ~ market_return,
    data = capm_data
  )
  
  coefficients <- stats::coef(
    capm_fit
  )
  
  tibble(
    strategy =
      strategy_name,
    
    monthly_alpha =
      unname(
        coefficients[[1]]
      ),
    
    annualized_alpha =
      unname(
        coefficients[[1]]
      ) * 12,
    
    beta =
      unname(
        coefficients[[2]]
      ),
    
    capm_r_squared =
      summary(
        capm_fit
      )$r.squared
  )
}


improved_capm_results <- bind_rows(
  calculate_capm(
    improved_backtest$improved_gross_return,
    improved_backtest$spy_return,
    "Improved XGBoost gross"
  ),
  
  calculate_capm(
    improved_backtest$improved_net_return,
    improved_backtest$spy_return,
    "Improved XGBoost net"
  )
)


write_csv(
  improved_capm_results,
  here(
    "results",
    "tables",
    "improved_capm_results.csv"
  )
)


# ============================================================
# 26. FEATURE IMPORTANCE
# ============================================================

improved_engine <- workflows::extract_fit_engine(
  improved_fit
)


improved_feature_importance <- tryCatch(
  {
    xgboost::xgb.importance(
      model = improved_engine
    ) |>
      tibble::as_tibble()
  },
  error = function(e) {
    
    warning(
      paste0(
        "Feature importance could not be extracted: ",
        conditionMessage(e)
      )
    )
    
    tibble()
  }
)


if (nrow(improved_feature_importance) > 0) {
  
  write_csv(
    improved_feature_importance,
    here(
      "results",
      "tables",
      "improved_feature_importance.csv"
    )
  )
}


# ============================================================
# 27. FIGURE: IMPROVED CUMULATIVE RETURNS
# ============================================================

improved_growth_plot_data <- improved_backtest |>
  dplyr::select(
    date,
    improved_net_growth,
    equal_weight_growth,
    spy_growth
  ) |>
  tidyr::pivot_longer(
    cols = c(
      improved_net_growth,
      equal_weight_growth,
      spy_growth
    ),
    names_to = "strategy",
    values_to = "growth"
  ) |>
  dplyr::mutate(
    strategy = dplyr::recode(
      strategy,
      
      improved_net_growth =
        "Improved XGBoost, net",
      
      equal_weight_growth =
        "Equal-weight universe",
      
      spy_growth =
        "SPY"
    ),
    
    strategy = factor(
      strategy,
      levels = c(
        "Improved XGBoost, net",
        "Equal-weight universe",
        "SPY"
      )
    )
  )


improved_cumulative_plot <- ggplot(
  improved_growth_plot_data,
  aes(
    x = date,
    y = growth,
    color = strategy,
    linetype = strategy,
    linewidth = strategy
  )
) +
  geom_hline(
    yintercept = 1,
    color = "#B2B7BF",
    linetype = "dashed",
    linewidth = 0.6
  ) +
  geom_line(
    lineend = "round"
  ) +
  scale_color_manual(
    values = c(
      "Improved XGBoost, net" =
        pink_dark,
      
      "Equal-weight universe" =
        grey_medium,
      
      "SPY" =
        black_color
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Improved XGBoost, net" =
        "solid",
      
      "Equal-weight universe" =
        "dashed",
      
      "SPY" =
        "dotdash"
    )
  ) +
  scale_linewidth_manual(
    values = c(
      "Improved XGBoost, net" = 1.25,
      "Equal-weight universe" = 1,
      "SPY" = 1
    )
  ) +
  scale_y_continuous(
    labels = label_number(
      accuracy = 0.01,
      suffix = "×"
    )
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b\n%Y"
  ) +
  guides(
    linewidth = "none"
  ) +
  labs(
    title = "Improved XGBoost portfolio performance",
    
    subtitle = paste0(
      "Out-of-sample results after estimated ",
      transaction_cost_bps,
      " bps trading costs"
    ),
    
    x = NULL,
    
    y = "Growth of $1",
    
    color = NULL,
    
    linetype = NULL,
    
    caption = paste0(
      "The improved portfolio holds the 10 ",
      "highest-ranked stocks with equal weights."
    )
  ) +
  theme_blog() +
  theme(
    panel.grid.major.x =
      element_blank(),
    
    legend.key.width =
      grid::unit(
        1.25,
        "cm"
      )
  )


ggsave(
  filename = here(
    "results",
    "figures",
    "improved_cumulative_returns.png"
  ),
  plot = improved_cumulative_plot,
  width = 8,
  height = 5.4,
  dpi = 300,
  bg = "white"
)


# ============================================================
# FIGURE: IMPROVED PREDICTION DENSITY
# ============================================================

prediction_bounds <- stats::quantile(
  c(
    improved_predictions$.pred,
    improved_predictions$target
  ),
  probs = c(
    0.01,
    0.99
  ),
  na.rm = TRUE,
  names = FALSE
)


improved_prediction_plot_data <- improved_predictions |>
  dplyr::filter(
    dplyr::between(
      .pred,
      prediction_bounds[[1]],
      prediction_bounds[[2]]
    ),
    
    dplyr::between(
      target,
      prediction_bounds[[1]],
      prediction_bounds[[2]]
    )
  )


improved_prediction_plot <- ggplot(
  improved_prediction_plot_data,
  aes(
    x = .pred,
    y = target
  )
) +
  geom_bin_2d(
    aes(
      fill = after_stat(count)
    ),
    bins = 48,
    show.legend = TRUE
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = grey_dark,
    linewidth = 0.8
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    color = pink_dark,
    linewidth = 1.1
  ) +
  scale_fill_gradientn(
    colors = c(
      "#F7FBFD",
      blue_fill,
      blue_dark,
      "#275D7A"
    ),
    trans = "sqrt",
    labels = label_comma(),
    name = "Observations per bin"
  ) +
  scale_x_continuous(
    labels = label_percent(
      accuracy = 1
    ),
    expand = expansion(
      mult = c(0.01, 0.01)
    )
  ) +
  scale_y_continuous(
    labels = label_percent(
      accuracy = 1
    ),
    expand = expansion(
      mult = c(0.01, 0.01)
    )
  ) +
  coord_cartesian(
    xlim = prediction_bounds,
    ylim = prediction_bounds,
    expand = FALSE,
    ratio = 1,
    clip = "on"
  ) +
  guides(
    fill = guide_colorbar(
      title.position = "top",
      title.hjust = 0,
      barwidth = grid::unit(
        7,
        "cm"
      ),
      barheight = grid::unit(
        0.32,
        "cm"
      )
    )
  ) +
  labs(
    title = "Improved predictions and realized returns",
    
    subtitle = paste0(
      "Density of ",
      comma(
        nrow(improved_predictions)
      ),
      " out-of-sample observations"
    ),
    
    x = "Predicted forward return",
    
    y = "Realized forward return",
    
    caption = paste0(
      "Pink line: fitted relationship; ",
      "dashed line: perfect predictions.\n",
      "The outer 1% of values is omitted from the display."
    )
  ) +
  theme_blog() +
  theme(
    legend.position = "top",
    
    legend.justification = "left",
    
    legend.title = element_text(
      size = 9,
      color = text_color,
      face = "plain"
    ),
    
    legend.text = element_text(
      size = 8
    ),
    
    plot.caption = element_text(
      size = 9,
      color = "#6B7280",
      hjust = 0,
      lineheight = 1.15,
      margin = margin(
        t = 14
      )
    ),
    
    plot.margin = margin(
      t = 16,
      r = 22,
      b = 24,
      l = 18
    ),
    
    panel.grid.minor = element_blank()
  )


ggsave(
  filename = here(
    "results",
    "figures",
    "improved_prediction_scatter.png"
  ),
  plot = improved_prediction_plot,
  width = 8.2,
  height = 6.8,
  dpi = 300,
  bg = "white"
)

# ============================================================
# 29. FIGURE: IMPROVED FEATURE IMPORTANCE
# ============================================================

if (nrow(improved_feature_importance) > 0) {
  
  improved_importance_plot_data <-
    improved_feature_importance |>
    dplyr::slice_max(
      order_by = Gain,
      n = 15,
      with_ties = FALSE
    ) |>
    dplyr::arrange(Gain) |>
    dplyr::mutate(
      Feature = factor(
        Feature,
        levels = Feature
      )
    )
  
  
  improved_feature_importance_plot <- ggplot(
    improved_importance_plot_data,
    aes(
      x = Feature,
      y = Gain
    )
  ) +
    geom_col(
      fill = pink_fill,
      color = pink_dark,
      linewidth = 0.6,
      width = 0.68
    ) +
    coord_flip() +
    scale_y_continuous(
      labels = label_percent(
        accuracy = 1
      )
    ) +
    labs(
      title = "Improved model feature importance",
      
      subtitle = "Top predictors ranked by XGBoost gain",
      
      x = NULL,
      
      y = "Share of model gain",
      
      caption = paste0(
        "Feature importance reflects model usage, ",
        "not causality."
      )
    ) +
    theme_blog() +
    theme(
      legend.position = "none"
    )
  
  
  ggsave(
    filename = here(
      "results",
      "figures",
      "improved_feature_importance.png"
    ),
    plot = improved_feature_importance_plot,
    width = 7.5,
    height = 5.8,
    dpi = 300,
    bg = "white"
  )
}


# ============================================================
# 30. SAVE NOTES
# ============================================================

best_model_row <- improved_predictive_performance |>
  dplyr::filter(
    model == "Improved XGBoost"
  )

net_performance_row <- improved_investment_performance |>
  dplyr::filter(
    strategy == "Improved XGBoost net"
  )


improved_notes <- c(
  "# Improved XGBoost model",
  "",
  "## Improvements",
  "",
  "- Corrected momentum and short-term return direction.",
  "- Removed raw price levels from the predictor set.",
  "- Added relative momentum, volatility, beta, drawdown, and volume features.",
  "- Used purged time splits to prevent target leakage.",
  "- Selected hyperparameters using a separate validation period.",
  "- Used equal-weight Top 10 portfolio construction.",
  paste0(
    "- Applied estimated transaction costs of ",
    transaction_cost_bps,
    " basis points per unit of turnover."
  ),
  "",
  "## Selected parameters",
  "",
  paste0(
    "The selected candidate was number ",
    best_parameters$candidate,
    ", with ",
    best_parameters$trees,
    " trees, tree depth ",
    best_parameters$tree_depth,
    ", and learning rate ",
    best_parameters$learn_rate,
    "."
  ),
  "",
  "## Predictive performance",
  "",
  paste0(
    "Out-of-sample RMSE was ",
    percent(
      best_model_row$rmse,
      accuracy = 0.01
    ),
    ", prediction correlation was ",
    round(
      best_model_row$pearson_correlation,
      4
    ),
    ", and mean rank correlation was ",
    round(
      improved_rank_summary$mean_rank_ic,
      4
    ),
    "."
  ),
  "",
  "## Investment performance",
  "",
  paste0(
    "After estimated trading costs, total return was ",
    percent(
      net_performance_row$total_return,
      accuracy = 0.1
    ),
    ", annualized Sharpe ratio was ",
    round(
      net_performance_row$
        annualized_sharpe_rf_zero,
      3
    ),
    ", and maximum drawdown was ",
    percent(
      net_performance_row$maximum_drawdown,
      accuracy = 0.1
    ),
    "."
  )
)


writeLines(
  improved_notes,
  here(
    "notes",
    "03_improved_xgboost_results.md"
  )
)


# ============================================================
# 31. SESSION INFORMATION
# ============================================================

capture.output(
  sessionInfo(),
  file = here(
    "results",
    "session_info_improved_xgboost.txt"
  )
)


# ============================================================
# 32. PRINT RESULTS
# ============================================================

cat(
  "\n========================================\n",
  "IMPROVED DATA SPLITS\n",
  "========================================\n"
)

print(
  split_summary,
  width = Inf
)


cat(
  "\n========================================\n",
  "VALIDATION RESULTS\n",
  "========================================\n"
)

print(
  tuning_results,
  n = Inf,
  width = Inf
)


cat(
  "\n========================================\n",
  "SELECTED HYPERPARAMETERS\n",
  "========================================\n"
)

print(
  best_parameters,
  width = Inf
)


cat(
  "\n========================================\n",
  "IMPROVED PREDICTIVE PERFORMANCE\n",
  "========================================\n"
)

print(
  improved_predictive_performance,
  width = Inf
)


cat(
  "\n========================================\n",
  "IMPROVED RANK CORRELATION\n",
  "========================================\n"
)

print(
  improved_rank_summary,
  width = Inf
)


cat(
  "\n========================================\n",
  "IMPROVED INVESTMENT PERFORMANCE\n",
  "========================================\n"
)

print(
  improved_investment_performance,
  width = Inf
)


cat(
  "\n========================================\n",
  "IMPROVED CAPM RESULTS\n",
  "========================================\n"
)

print(
  improved_capm_results,
  width = Inf
)


cat(
  "\n========================================\n",
  "TURNOVER SUMMARY\n",
  "========================================\n"
)

print(
  improved_turnover |>
    dplyr::summarise(
      months =
        dplyr::n(),
      
      mean_turnover =
        mean(turnover),
      
      median_turnover =
        median(turnover),
      
      maximum_turnover =
        max(turnover)
    ),
  width = Inf
)


cat(
  "\nImproved XGBoost analysis completed successfully.\n",
  "Review results/tables, results/figures, ",
  "results/predictions, models, and notes.\n"
)