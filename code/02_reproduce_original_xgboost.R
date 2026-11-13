# ============================================================
# Blog Post 9
# Part 2: Reproduce the Original XGBoost Model
# ============================================================

# ============================================================
# 1. PACKAGES
# ============================================================

required_packages <- c(
  "tidyverse",
  "tidyquant",
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
  library(tidyquant)
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
# 2. FIXED DATES AND MODEL SETTINGS
# ============================================================

data_start <- as.Date("2022-01-01")
data_end <- as.Date("2026-11-12")

# Add one day to help include observations through data_end.
download_end <- data_end + lubridate::days(1)

train_test_split_date <- as.Date("2024-01-01")

prediction_horizon <- 20
portfolio_size <- 10


# ============================================================
# 3. COLOR PALETTE
# ============================================================

pink_fill <- "#F5B2C8"
pink_dark <- "#C85F87"

blue_fill <- "#A9D6EE"
blue_dark <- "#4F91B8"

grey_dark <- "#67717E"
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
      
      legend.text = element_text(
        size = 10,
        color = text_color
      ),
      
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
  here("data", "raw"),
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
# 5. ORIGINAL STOCK UNIVERSE
# ============================================================

tickers <- c(
  # Technology
  "AAPL", "MSFT", "NVDA", "GOOG", "META",
  "AMZN", "AVGO", "ORCL", "ADBE", "CRM",
  "CSCO", "AMD", "INTC", "QCOM", "TXN",
  "IBM", "NOW", "INTU", "AMAT", "MU",
  
  # Financials
  "JPM", "BAC", "WFC", "GS", "MS",
  "BLK", "C", "SCHW", "SPGI", "AXP",
  "USB", "PNC", "TFC", "COF",
  
  # Healthcare
  "UNH", "JNJ", "LLY", "PFE", "MRK",
  "ABBV", "TMO", "DHR", "ABT", "BMY",
  "AMGN", "GILD", "CVS", "ISRG", "MDT",
  
  # Consumer discretionary
  "TSLA", "HD", "MCD", "NKE", "SBUX",
  "LOW", "TJX", "BKNG", "CMG", "ORLY",
  
  # Consumer staples
  "WMT", "COST", "PG", "KO", "PEP",
  "PM", "MO", "CL", "EL",
  
  # Industrials
  "CAT", "GE", "HON", "UPS", "BA",
  "RTX", "DE", "LMT", "MMM", "ETN",
  
  # Energy
  "XOM", "CVX", "COP", "SLB", "EOG",
  
  # Communication and media
  "NFLX", "DIS", "CMCSA", "TMUS", "VZ",
  
  # Real estate, utilities, and materials
  "AMT", "PLD", "NEE", "DUK", "SO",
  "LIN", "APD"
)

write_csv(
  tibble(symbol = tickers),
  here(
    "results",
    "tables",
    "original_stock_universe.csv"
  )
)


# ============================================================
# 6. DOWNLOAD OR LOAD CACHED STOCK DATA
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


if (file.exists(stock_cache)) {
  
  message("Loading cached stock-price data...")
  
  prices <- readRDS(stock_cache)
  
} else {
  
  message(
    "Downloading stock prices. ",
    "This may take several minutes..."
  )
  
  prices <- tryCatch(
    {
      tidyquant::tq_get(
        tickers,
        get = "stock.prices",
        from = as.character(data_start),
        to = as.character(download_end),
        complete_cases = TRUE
      )
    },
    error = function(e) {
      stop(
        paste0(
          "Stock-price download failed:\n",
          conditionMessage(e)
        )
      )
    }
  )
  
  saveRDS(
    prices,
    stock_cache
  )
}


if (file.exists(spy_cache)) {
  
  message("Loading cached SPY data...")
  
  spy_prices <- readRDS(spy_cache)
  
} else {
  
  message("Downloading SPY benchmark data...")
  
  spy_prices <- tryCatch(
    {
      tidyquant::tq_get(
        "SPY",
        get = "stock.prices",
        from = as.character(data_start),
        to = as.character(download_end),
        complete_cases = TRUE
      )
    },
    error = function(e) {
      stop(
        paste0(
          "SPY download failed:\n",
          conditionMessage(e)
        )
      )
    }
  )
  
  saveRDS(
    spy_prices,
    spy_cache
  )
}


# ============================================================
# 7. VALIDATE MARKET DATA
# ============================================================

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


available_tickers <- sort(
  unique(prices$symbol)
)

missing_tickers <- setdiff(
  tickers,
  available_tickers
)


ticker_download_summary <- tibble(
  requested_tickers =
    length(tickers),
  
  downloaded_tickers =
    length(available_tickers),
  
  missing_tickers =
    paste(
      missing_tickers,
      collapse = ", "
    ),
  
  first_date =
    min(prices$date),
  
  last_date =
    max(prices$date),
  
  observations =
    nrow(prices)
)


write_csv(
  ticker_download_summary,
  here(
    "results",
    "tables",
    "ticker_download_summary.csv"
  )
)


if (length(available_tickers) < 70) {
  stop(
    paste0(
      "Only ",
      length(available_tickers),
      " tickers downloaded successfully. ",
      "Do not continue with an incomplete universe."
    )
  )
}


if (length(missing_tickers) > 0) {
  warning(
    paste0(
      "Unavailable tickers: ",
      paste(
        missing_tickers,
        collapse = ", "
      )
    )
  )
}


# ============================================================
# 8. PREPARE SPY DATA
# ============================================================

spy <- spy_prices |>
  dplyr::transmute(
    date,
    spy_price = adjusted
  ) |>
  dplyr::arrange(date) |>
  dplyr::mutate(
    # Explicitly use dplyr::lag to avoid package conflicts.
    spy_ret =
      spy_price /
      dplyr::lag(
        spy_price,
        n = 1
      ) - 1,
    
    # Benchmark return from t+1 through t+20.
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
# 9. ORIGINAL FEATURE ENGINEERING
# ============================================================

# These formulas intentionally reproduce the original model.
# The reversed momentum definitions will be changed only in
# the improved model.

model_data <- prices |>
  dplyr::group_by(symbol) |>
  dplyr::arrange(
    date,
    .by_group = TRUE
  ) |>
  dplyr::mutate(
    return =
      adjusted /
      dplyr::lag(
        adjusted,
        n = 1
      ) - 1
  ) |>
  dplyr::ungroup() |>
  dplyr::left_join(
    spy |>
      dplyr::select(
        date,
        spy_ret
      ),
    by = "date"
  ) |>
  dplyr::group_by(symbol) |>
  dplyr::arrange(
    date,
    .by_group = TRUE
  ) |>
  dplyr::mutate(
    mom_1m =
      dplyr::lag(
        adjusted,
        n = 21
      ) /
      dplyr::lag(
        adjusted,
        n = 1
      ) - 1,
    
    mom_3m =
      dplyr::lag(
        adjusted,
        n = 63
      ) /
      dplyr::lag(
        adjusted,
        n = 1
      ) - 1,
    
    mom_6m =
      dplyr::lag(
        adjusted,
        n = 126
      ) /
      dplyr::lag(
        adjusted,
        n = 1
      ) - 1,
    
    vol_1m = slider::slide_dbl(
      dplyr::lag(
        return,
        n = 1
      ),
      stats::sd,
      .before = 21,
      .complete = TRUE
    ),
    
    ret_5d =
      dplyr::lag(
        adjusted,
        n = 5
      ) /
      dplyr::lag(
        adjusted,
        n = 1
      ) - 1,
    
    excess_ret =
      return - spy_ret
  ) |>
  dplyr::ungroup() |>
  tidyr::drop_na() |>
  dplyr::group_by(symbol) |>
  dplyr::arrange(
    date,
    .by_group = TRUE
  ) |>
  dplyr::mutate(
    target =
      dplyr::lead(
        adjusted,
        n = prediction_horizon
      ) /
      dplyr::lead(
        adjusted,
        n = 1
      ) - 1
  ) |>
  dplyr::ungroup() |>
  tidyr::drop_na(target)


if (nrow(model_data) == 0) {
  stop(
    "Feature engineering produced no usable observations."
  )
}


write_csv(
  model_data,
  here(
    "data",
    "processed",
    "original_model_data.csv"
  )
)


# ============================================================
# 10. TRAIN AND TEST SPLIT
# ============================================================

train <- model_data |>
  dplyr::filter(
    date < train_test_split_date
  )

test <- model_data |>
  dplyr::filter(
    date >= train_test_split_date
  )


if (nrow(train) == 0 || nrow(test) == 0) {
  stop(
    "The time split produced an empty train or test set."
  )
}


data_split_summary <- tibble(
  sample = c(
    "Training",
    "Out-of-sample test"
  ),
  
  first_date = c(
    min(train$date),
    min(test$date)
  ),
  
  last_date = c(
    max(train$date),
    max(test$date)
  ),
  
  observations = c(
    nrow(train),
    nrow(test)
  ),
  
  tickers = c(
    n_distinct(train$symbol),
    n_distinct(test$symbol)
  )
)


write_csv(
  data_split_summary,
  here(
    "results",
    "tables",
    "original_data_split_summary.csv"
  )
)


# ============================================================
# 11. ORIGINAL MODEL RECIPE
# ============================================================

original_recipe <- recipes::recipe(
  target ~ .,
  data = train
) |>
  recipes::update_role(
    date,
    symbol,
    new_role = "id"
  )


# ============================================================
# 12. ORIGINAL XGBOOST SPECIFICATION
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


original_xgb_spec <- parsnip::boost_tree(
  trees = 300,
  tree_depth = 5,
  learn_rate = 0.05,
  loss_reduction = 0,
  min_n = 10
) |>
  parsnip::set_engine(
    "xgboost",
    nthread = xgb_threads,
    verbosity = 0
  ) |>
  parsnip::set_mode("regression")


# ============================================================
# 13. FIT ORIGINAL MODEL
# ============================================================

original_workflow <- workflows::workflow() |>
  workflows::add_recipe(
    original_recipe
  ) |>
  workflows::add_model(
    original_xgb_spec
  )


message("Fitting the original XGBoost model...")

fit_start_time <- Sys.time()

original_fit <- generics::fit(
  original_workflow,
  data = train
)

fit_end_time <- Sys.time()

fit_minutes <- as.numeric(
  difftime(
    fit_end_time,
    fit_start_time,
    units = "mins"
  )
)


saveRDS(
  original_fit,
  here(
    "models",
    "original_xgboost_fit.rds"
  )
)


fit_summary <- tibble(
  model = "Original XGBoost",
  trees = 300,
  tree_depth = 5,
  learn_rate = 0.05,
  loss_reduction = 0,
  min_n = 10,
  training_observations = nrow(train),
  test_observations = nrow(test),
  fit_minutes = fit_minutes
)


write_csv(
  fit_summary,
  here(
    "results",
    "tables",
    "original_model_fit_summary.csv"
  )
)


# ============================================================
# 14. OUT-OF-SAMPLE PREDICTIONS
# ============================================================

message("Generating out-of-sample predictions...")

original_predictions <- stats::predict(
  original_fit,
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
  train$target,
  na.rm = TRUE
)


original_predictions <- original_predictions |>
  dplyr::mutate(
    zero_prediction = 0,
    
    historical_mean_prediction =
      training_mean_target
  )


write_csv(
  original_predictions,
  here(
    "results",
    "predictions",
    "original_xgboost_predictions.csv"
  )
)


# ============================================================
# 15. PREDICTIVE METRICS
# ============================================================

safe_correlation <- function(
    actual,
    predicted
) {
  
  if (
    stats::sd(
      actual,
      na.rm = TRUE
    ) == 0 ||
    stats::sd(
      predicted,
      na.rm = TRUE
    ) == 0
  ) {
    return(NA_real_)
  }
  
  stats::cor(
    actual,
    predicted,
    use = "complete.obs"
  )
}


calculate_prediction_metrics <- function(
    actual,
    predicted,
    model_name
) {
  
  valid <- complete.cases(
    actual,
    predicted
  )
  
  actual <- actual[valid]
  predicted <- predicted[valid]
  
  residuals <- actual - predicted
  
  total_sum_squares <- sum(
    (
      actual - mean(actual)
    )^2
  )
  
  residual_sum_squares <- sum(
    residuals^2
  )
  
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
      residual_sum_squares /
      total_sum_squares,
    
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


predictive_performance <- bind_rows(
  calculate_prediction_metrics(
    actual =
      original_predictions$target,
    
    predicted =
      original_predictions$.pred,
    
    model_name =
      "Original XGBoost"
  ),
  
  calculate_prediction_metrics(
    actual =
      original_predictions$target,
    
    predicted =
      original_predictions$
      historical_mean_prediction,
    
    model_name =
      "Historical mean prediction"
  ),
  
  calculate_prediction_metrics(
    actual =
      original_predictions$target,
    
    predicted =
      original_predictions$
      zero_prediction,
    
    model_name =
      "Zero-return prediction"
  )
)


write_csv(
  predictive_performance,
  here(
    "results",
    "tables",
    "original_predictive_performance.csv"
  )
)


# ============================================================
# 16. CROSS-SECTIONAL RANK CORRELATION
# ============================================================

daily_rank_ic <- original_predictions |>
  dplyr::group_by(date) |>
  dplyr::summarise(
    stocks = dplyr::n(),
    
    rank_ic = if (
      dplyr::n_distinct(.pred) > 1 &&
      dplyr::n_distinct(target) > 1
    ) {
      stats::cor(
        .pred,
        target,
        method = "spearman",
        use = "complete.obs"
      )
    } else {
      NA_real_
    },
    
    .groups = "drop"
  )


rank_ic_summary <- daily_rank_ic |>
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
  daily_rank_ic,
  here(
    "results",
    "tables",
    "original_daily_rank_ic.csv"
  )
)

write_csv(
  rank_ic_summary,
  here(
    "results",
    "tables",
    "original_rank_ic_summary.csv"
  )
)


# ============================================================
# 17. MONTHLY FORMATION DATES
# ============================================================

formation_dates <- original_predictions |>
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


formation_universe <- original_predictions |>
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
# 18. ORIGINAL TOP-10 PORTFOLIO
# ============================================================

original_holdings <- formation_universe |>
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
    
    positive_prediction =
      pmax(
        .pred,
        0
      ),
    
    positive_prediction_total =
      sum(
        positive_prediction
      ),
    
    weight = dplyr::case_when(
      positive_prediction_total > 0 ~
        positive_prediction /
        positive_prediction_total,
      
      TRUE ~
        1 / dplyr::n()
    )
  ) |>
  dplyr::ungroup()


write_csv(
  original_holdings,
  here(
    "results",
    "tables",
    "original_monthly_holdings.csv"
  )
)


# ============================================================
# 19. MONTHLY MODEL RETURNS
# ============================================================

model_monthly_returns <- original_holdings |>
  dplyr::group_by(
    month,
    date
  ) |>
  dplyr::summarise(
    model_return =
      sum(
        weight * target,
        na.rm = TRUE
      ),
    
    selected_stocks =
      dplyr::n(),
    
    .groups = "drop"
  )


# ============================================================
# 20. EQUAL-WEIGHT BENCHMARK
# ============================================================

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
    
    universe_stocks =
      dplyr::n(),
    
    .groups = "drop"
  )


# ============================================================
# 21. ALIGNED SPY BENCHMARK
# ============================================================

spy_forward_returns <- spy |>
  dplyr::filter(
    !is.na(spy_target)
  ) |>
  dplyr::transmute(
    date,
    spy_return = spy_target
  )


# ============================================================
# 22. COMBINE MONTHLY RETURNS
# ============================================================

backtest <- model_monthly_returns |>
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
    model_growth =
      cumprod(
        1 + model_return
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
  backtest,
  here(
    "results",
    "tables",
    "original_monthly_backtest.csv"
  )
)


# ============================================================
# 23. INVESTMENT PERFORMANCE
# ============================================================

calculate_investment_metrics <- function(
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
  
  running_peak <- cummax(
    wealth
  )
  
  drawdown <-
    wealth /
    running_peak - 1
  
  annualized_volatility <- if (
    number_of_months > 1
  ) {
    stats::sd(returns) * sqrt(12)
  } else {
    NA_real_
  }
  
  annualized_sharpe <- if (
    number_of_months > 1 &&
    stats::sd(returns) > 0
  ) {
    mean(returns) /
      stats::sd(returns) *
      sqrt(12)
  } else {
    NA_real_
  }
  
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
      annualized_volatility,
    
    annualized_sharpe_rf_zero =
      annualized_sharpe,
    
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


investment_performance <- bind_rows(
  calculate_investment_metrics(
    backtest$model_return,
    "Original XGBoost portfolio"
  ),
  
  calculate_investment_metrics(
    backtest$equal_weight_return,
    "Equal-weight stock universe"
  ),
  
  calculate_investment_metrics(
    backtest$spy_return,
    "SPY"
  )
)


write_csv(
  investment_performance,
  here(
    "results",
    "tables",
    "original_investment_performance.csv"
  )
)


# ============================================================
# 24. CAPM ALPHA AND BETA
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
  
  capm_coefficients <- stats::coef(
    capm_fit
  )
  
  tibble(
    strategy =
      strategy_name,
    
    monthly_alpha =
      unname(
        capm_coefficients[[1]]
      ),
    
    annualized_alpha =
      unname(
        capm_coefficients[[1]]
      ) * 12,
    
    beta =
      unname(
        capm_coefficients[[2]]
      ),
    
    capm_r_squared =
      summary(
        capm_fit
      )$r.squared
  )
}


capm_results <- bind_rows(
  calculate_capm(
    strategy_returns =
      backtest$model_return,
    
    market_returns =
      backtest$spy_return,
    
    strategy_name =
      "Original XGBoost portfolio"
  ),
  
  calculate_capm(
    strategy_returns =
      backtest$equal_weight_return,
    
    market_returns =
      backtest$spy_return,
    
    strategy_name =
      "Equal-weight stock universe"
  )
)


write_csv(
  capm_results,
  here(
    "results",
    "tables",
    "original_capm_results.csv"
  )
)


# ============================================================
# 25. LATEST EVALUABLE RANKING
# ============================================================

latest_evaluable_date <- max(
  original_predictions$date
)


latest_evaluable_ranking <- original_predictions |>
  dplyr::filter(
    date == latest_evaluable_date
  ) |>
  dplyr::arrange(
    dplyr::desc(.pred)
  ) |>
  dplyr::mutate(
    rank =
      dplyr::row_number()
  ) |>
  dplyr::select(
    rank,
    date,
    symbol,
    predicted_return = .pred,
    realized_return = target
  )


write_csv(
  latest_evaluable_ranking,
  here(
    "results",
    "tables",
    "original_latest_evaluable_ranking.csv"
  )
)


# ============================================================
# 26. FEATURE IMPORTANCE
# ============================================================

xgb_engine <- workflows::extract_fit_engine(
  original_fit
)


feature_importance <- tryCatch(
  {
    xgboost::xgb.importance(
      model = xgb_engine
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


if (nrow(feature_importance) > 0) {
  write_csv(
    feature_importance,
    here(
      "results",
      "tables",
      "original_feature_importance.csv"
    )
  )
}


# ============================================================
# FIGURE: CUMULATIVE RETURNS
# ============================================================

growth_plot_data <- backtest |>
  dplyr::select(
    date,
    model_growth,
    equal_weight_growth,
    spy_growth
  ) |>
  tidyr::pivot_longer(
    cols = c(
      model_growth,
      equal_weight_growth,
      spy_growth
    ),
    names_to = "strategy",
    values_to = "growth"
  ) |>
  dplyr::mutate(
    strategy = dplyr::recode(
      strategy,
      
      model_growth =
        "Original XGBoost",
      
      equal_weight_growth =
        "Equal-weight universe",
      
      spy_growth =
        "SPY"
    ),
    
    strategy = factor(
      strategy,
      levels = c(
        "Original XGBoost",
        "Equal-weight universe",
        "SPY"
      )
    )
  )


cumulative_return_plot <- ggplot(
  growth_plot_data,
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
      "Original XGBoost" =
        pink_dark,
      
      "Equal-weight universe" =
        "#8A9199",
      
      "SPY" =
        "#111111"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Original XGBoost" =
        "solid",
      
      "Equal-weight universe" =
        "dashed",
      
      "SPY" =
        "dotdash"
    )
  ) +
  scale_linewidth_manual(
    values = c(
      "Original XGBoost" = 1.25,
      "Equal-weight universe" = 1.0,
      "SPY" = 1.0
    )
  ) +
  scale_y_continuous(
    labels = label_number(
      accuracy = 0.01,
      suffix = "×"
    ),
    expand = expansion(
      mult = c(0.03, 0.08)
    )
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b\n%Y"
  ) +
  guides(
    color = guide_legend(
      override.aes = list(
        linewidth = c(
          1.25,
          1,
          1
        )
      )
    ),
    
    linetype = guide_legend(),
    
    linewidth = "none"
  ) +
  labs(
    title = "Original XGBoost portfolio performance",
    
    subtitle = paste0(
      "Monthly formation dates · ",
      "Out-of-sample period beginning in 2024"
    ),
    
    x = NULL,
    
    y = "Growth of $1",
    
    color = NULL,
    
    linetype = NULL,
    
    caption = paste0(
      "Returns exclude transaction costs. ",
      "All three strategies use aligned forward periods."
    )
  ) +
  theme_blog() +
  theme(
    panel.grid.major.x = element_blank(),
    
    legend.position = "top",
    
    legend.direction = "horizontal",
    
    legend.key.width = grid::unit(
      1.25,
      "cm"
    )
  )


ggsave(
  filename = here(
    "results",
    "figures",
    "original_cumulative_returns.png"
  ),
  plot = cumulative_return_plot,
  width = 8,
  height = 5.4,
  dpi = 300,
  bg = "white"
)


# ============================================================
# 28. FIGURE: PREDICTED VS. REALIZED RETURNS
# ============================================================

prediction_bounds <- stats::quantile(
  c(
    original_predictions$.pred,
    original_predictions$target
  ),
  probs = c(
    0.01,
    0.99
  ),
  na.rm = TRUE,
  names = FALSE
)


prediction_plot_data <- original_predictions |>
  dplyr::filter(
    .pred >= prediction_bounds[[1]],
    .pred <= prediction_bounds[[2]],
    target >= prediction_bounds[[1]],
    target <= prediction_bounds[[2]]
  )


prediction_scatter_plot <- ggplot(
  prediction_plot_data,
  aes(
    x = .pred,
    y = target
  )
) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    linewidth = 0.7,
    color = grey_dark
  ) +
  geom_point(
    shape = 21,
    fill = blue_fill,
    color = blue_dark,
    alpha = 0.20,
    size = 1.3,
    stroke = 0.4
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    color = pink_dark,
    fill = pink_fill,
    linewidth = 0.9,
    alpha = 0.25
  ) +
  scale_x_continuous(
    labels = label_percent(
      accuracy = 1
    )
  ) +
  scale_y_continuous(
    labels = label_percent(
      accuracy = 1
    )
  ) +
  labs(
    title = "Predicted and realized stock returns",
    
    subtitle = paste0(
      "Original XGBoost model · ",
      "Out-of-sample observations"
    ),
    
    x = "Predicted forward return",
    
    y = "Realized forward return",
    
    caption = paste0(
      "The display excludes the outer 1% of values for ",
      "readability; all observations remain in the metrics."
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
    "original_prediction_scatter.png"
  ),
  plot = prediction_scatter_plot,
  width = 7,
  height = 5.6,
  dpi = 300,
  bg = "white"
)


# ============================================================
# 29. FIGURE: FEATURE IMPORTANCE
# ============================================================

if (nrow(feature_importance) > 0) {
  
  feature_importance_plot_data <-
    feature_importance |>
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
  
  
  feature_importance_plot <- ggplot(
    feature_importance_plot_data,
    aes(
      x = Feature,
      y = Gain
    )
  ) +
    geom_col(
      fill = blue_fill,
      color = blue_dark,
      linewidth = 0.6,
      width = 0.68
    ) +
    coord_flip() +
    scale_y_continuous(
      labels = label_percent(
        accuracy = 1
      ),
      expand = expansion(
        mult = c(0, 0.08)
      )
    ) +
    labs(
      title = "Original model feature importance",
      
      subtitle = "Top 15 predictors ranked by XGBoost gain",
      
      x = NULL,
      
      y = "Share of model gain",
      
      caption = paste0(
        "Feature importance describes model usage, ",
        "not a causal relationship."
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
      "original_feature_importance.png"
    ),
    plot = feature_importance_plot,
    width = 7.5,
    height = 5.8,
    dpi = 300,
    bg = "white"
  )
}


# ============================================================
# 30. SAVE AUTOMATIC NOTES
# ============================================================

model_predictive_row <- predictive_performance |>
  dplyr::filter(
    model == "Original XGBoost"
  )

model_investment_row <- investment_performance |>
  dplyr::filter(
    strategy == "Original XGBoost portfolio"
  )

equal_weight_row <- investment_performance |>
  dplyr::filter(
    strategy == "Equal-weight stock universe"
  )

spy_row <- investment_performance |>
  dplyr::filter(
    strategy == "SPY"
  )

model_capm_row <- capm_results |>
  dplyr::filter(
    strategy == "Original XGBoost portfolio"
  )


baseline_notes <- c(
  "# Original XGBoost baseline",
  "",
  "## Model design",
  "",
  paste0(
    "The model was trained on observations before ",
    format(
      train_test_split_date,
      "%B %d, %Y"
    ),
    " and evaluated on later observations."
  ),
  "",
  paste0(
    "It used 300 trees, a tree depth of 5, a learning ",
    "rate of 0.05, and a minimum node size of 10."
  ),
  "",
  "## Predictive performance",
  "",
  paste0(
    "Out-of-sample RMSE was ",
    percent(
      model_predictive_row$rmse,
      accuracy = 0.01
    ),
    ", while MAE was ",
    percent(
      model_predictive_row$mae,
      accuracy = 0.01
    ),
    "."
  ),
  "",
  paste0(
    "Prediction correlation was ",
    round(
      model_predictive_row$pearson_correlation,
      4
    ),
    ", and directional accuracy was ",
    percent(
      model_predictive_row$directional_accuracy,
      accuracy = 0.1
    ),
    "."
  ),
  "",
  paste0(
    "Mean cross-sectional rank correlation was ",
    round(
      rank_ic_summary$mean_rank_ic,
      4
    ),
    "."
  ),
  "",
  "## Investment performance",
  "",
  paste0(
    "The model portfolio produced a total return of ",
    percent(
      model_investment_row$total_return,
      accuracy = 0.1
    ),
    ", compared with ",
    percent(
      equal_weight_row$total_return,
      accuracy = 0.1
    ),
    " for the equal-weight universe and ",
    percent(
      spy_row$total_return,
      accuracy = 0.1
    ),
    " for SPY."
  ),
  "",
  paste0(
    "Its annualized Sharpe ratio was ",
    round(
      model_investment_row$
        annualized_sharpe_rf_zero,
      3
    ),
    ", and maximum drawdown was ",
    percent(
      model_investment_row$maximum_drawdown,
      accuracy = 0.1
    ),
    "."
  ),
  "",
  paste0(
    "Estimated annualized CAPM alpha was ",
    percent(
      model_capm_row$annualized_alpha,
      accuracy = 0.01
    ),
    ", and beta was ",
    round(
      model_capm_row$beta,
      3
    ),
    "."
  ),
  "",
  "## Baseline limitations",
  "",
  "- Original momentum and reversal ratios are reversed.",
  "- Forward target periods overlap.",
  "- The original split does not purge targets crossing 2024.",
  "- Backtest results exclude transaction costs and taxes.",
  "- The final evaluable ranking is historical."
)


writeLines(
  baseline_notes,
  here(
    "notes",
    "02_original_xgboost_results.md"
  )
)


# ============================================================
# 31. SESSION INFORMATION
# ============================================================

capture.output(
  sessionInfo(),
  file = here(
    "results",
    "session_info_original_xgboost.txt"
  )
)


# ============================================================
# 32. PRINT PRINCIPAL RESULTS
# ============================================================

cat(
  "\n========================================\n",
  "DATA DOWNLOAD SUMMARY\n",
  "========================================\n"
)

print(
  ticker_download_summary,
  width = Inf
)


cat(
  "\n========================================\n",
  "TRAIN AND TEST SPLIT\n",
  "========================================\n"
)

print(
  data_split_summary,
  width = Inf
)


cat(
  "\n========================================\n",
  "MODEL FIT SUMMARY\n",
  "========================================\n"
)

print(
  fit_summary,
  width = Inf
)


cat(
  "\n========================================\n",
  "PREDICTIVE PERFORMANCE\n",
  "========================================\n"
)

print(
  predictive_performance,
  width = Inf
)


cat(
  "\n========================================\n",
  "RANK CORRELATION\n",
  "========================================\n"
)

print(
  rank_ic_summary,
  width = Inf
)


cat(
  "\n========================================\n",
  "INVESTMENT PERFORMANCE\n",
  "========================================\n"
)

print(
  investment_performance,
  width = Inf
)


cat(
  "\n========================================\n",
  "CAPM RESULTS\n",
  "========================================\n"
)

print(
  capm_results,
  width = Inf
)


cat(
  "\n========================================\n",
  "LAST EVALUABLE TOP 10\n",
  "========================================\n"
)

print(
  latest_evaluable_ranking |>
    dplyr::slice_head(n = 10),
  n = 10,
  width = Inf
)


cat(
  "\nOriginal XGBoost baseline completed successfully.\n",
  "Review results/tables, results/figures, ",
  "results/predictions, models, and notes.\n"
)