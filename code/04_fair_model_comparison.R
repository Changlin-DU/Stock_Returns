# ============================================================
# Blog Post 9
# Part 4: Fair Out-of-Sample Model Comparison
# ============================================================


# ============================================================
# 1. PACKAGES
# ============================================================

required_packages <- c(
  "tidyverse",
  "lubridate",
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
  library(lubridate)
  library(here)
  library(scales)
})

set.seed(123)
options(scipen = 999)


# ============================================================
# 2. SETTINGS
# ============================================================

portfolio_size <- 10

transaction_cost_bps <- 10
transaction_cost_rate <- transaction_cost_bps / 10000

bootstrap_repetitions <- 10000


# ============================================================
# 3. COLORS AND THEME
# ============================================================

pink_fill <- "#F5B2C8"
pink_dark <- "#C85F87"

blue_fill <- "#A9D6EE"
blue_dark <- "#4F91B8"

grey_medium <- "#9299A1"
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
  here("results", "tables"),
  here("results", "figures"),
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
# 5. LOAD SAVED OUT-OF-SAMPLE PREDICTIONS
# ============================================================

original_prediction_path <- here(
  "results",
  "predictions",
  "original_xgboost_predictions.csv"
)

improved_prediction_path <- here(
  "results",
  "predictions",
  "improved_xgboost_predictions.csv"
)

improved_backtest_path <- here(
  "results",
  "tables",
  "improved_monthly_backtest.csv"
)


required_files <- c(
  original_prediction_path,
  improved_prediction_path,
  improved_backtest_path
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  stop(
    paste0(
      "These required files are missing:\n",
      paste(missing_files, collapse = "\n")
    )
  )
}


original_predictions <- readr::read_csv(
  original_prediction_path,
  show_col_types = FALSE
) |>
  dplyr::transmute(
    date = as.Date(date),
    symbol,
    original_prediction = .pred,
    original_target = target
  )


improved_predictions <- readr::read_csv(
  improved_prediction_path,
  show_col_types = FALSE
) |>
  dplyr::transmute(
    date = as.Date(date),
    symbol,
    improved_prediction = .pred,
    improved_target = target,
    historical_mean_prediction,
    zero_prediction
  )


saved_benchmarks <- readr::read_csv(
  improved_backtest_path,
  show_col_types = FALSE
) |>
  dplyr::transmute(
    month = as.Date(month),
    spy_return
  )


# ============================================================
# 6. CREATE A COMMON TEST SAMPLE
# ============================================================

comparison_predictions <- original_predictions |>
  dplyr::inner_join(
    improved_predictions,
    by = c("date", "symbol")
  ) |>
  dplyr::mutate(
    target_difference =
      abs(original_target - improved_target)
  )


maximum_target_difference <- max(
  comparison_predictions$target_difference,
  na.rm = TRUE
)

if (
  !is.finite(maximum_target_difference) ||
  maximum_target_difference > 1e-10
) {
  stop(
    "The original and improved prediction files do not use ",
    "identical realized targets."
  )
}


comparison_predictions <- comparison_predictions |>
  dplyr::transmute(
    date,
    symbol,
    target = original_target,
    original_prediction,
    improved_prediction,
    historical_mean_prediction,
    zero_prediction
  ) |>
  tidyr::drop_na()


if (nrow(comparison_predictions) == 0) {
  stop("The common out-of-sample prediction sample is empty.")
}


# ============================================================
# 7. PREDICTIVE PERFORMANCE FUNCTIONS
# ============================================================

calculate_predictive_metrics <- function(
    data,
    prediction_column,
    model_name
) {
  
  actual <- data$target
  prediction <- data[[prediction_column]]
  
  errors <- actual - prediction
  
  tibble(
    model = model_name,
    
    observations = length(actual),
    
    rmse = sqrt(
      mean(errors^2)
    ),
    
    mae = mean(
      abs(errors)
    ),
    
    r_squared =
      1 -
      sum(errors^2) /
      sum(
        (actual - mean(actual))^2
      ),
    
    pearson_correlation = if (
      stats::sd(prediction) > 0
    ) {
      stats::cor(
        prediction,
        actual,
        method = "pearson"
      )
    } else {
      NA_real_
    },
    
    directional_accuracy = mean(
      sign(prediction) == sign(actual)
    ),
    
    mean_prediction = mean(prediction),
    
    mean_actual_return = mean(actual)
  )
}


predictive_comparison <- dplyr::bind_rows(
  calculate_predictive_metrics(
    comparison_predictions,
    "original_prediction",
    "Original XGBoost"
  ),
  
  calculate_predictive_metrics(
    comparison_predictions,
    "improved_prediction",
    "Improved XGBoost"
  ),
  
  calculate_predictive_metrics(
    comparison_predictions,
    "historical_mean_prediction",
    "Historical mean prediction"
  ),
  
  calculate_predictive_metrics(
    comparison_predictions,
    "zero_prediction",
    "Zero-return prediction"
  )
)


write_csv(
  predictive_comparison,
  here(
    "results",
    "tables",
    "fair_predictive_comparison.csv"
  )
)


# ============================================================
# 8. DAILY CROSS-SECTIONAL RANK CORRELATION
# ============================================================

daily_rank_comparison <- comparison_predictions |>
  dplyr::group_by(date) |>
  dplyr::summarise(
    original_rank_ic = if (
      dplyr::n_distinct(original_prediction) > 1 &&
      dplyr::n_distinct(target) > 1
    ) {
      stats::cor(
        original_prediction,
        target,
        method = "spearman",
        use = "complete.obs"
      )
    } else {
      NA_real_
    },
    
    improved_rank_ic = if (
      dplyr::n_distinct(improved_prediction) > 1 &&
      dplyr::n_distinct(target) > 1
    ) {
      stats::cor(
        improved_prediction,
        target,
        method = "spearman",
        use = "complete.obs"
      )
    } else {
      NA_real_
    },
    
    .groups = "drop"
  )


rank_ic_comparison <- tibble(
  model = c(
    "Original XGBoost",
    "Improved XGBoost"
  ),
  
  evaluation_dates = c(
    sum(!is.na(daily_rank_comparison$original_rank_ic)),
    sum(!is.na(daily_rank_comparison$improved_rank_ic))
  ),
  
  mean_rank_ic = c(
    mean(
      daily_rank_comparison$original_rank_ic,
      na.rm = TRUE
    ),
    
    mean(
      daily_rank_comparison$improved_rank_ic,
      na.rm = TRUE
    )
  ),
  
  median_rank_ic = c(
    median(
      daily_rank_comparison$original_rank_ic,
      na.rm = TRUE
    ),
    
    median(
      daily_rank_comparison$improved_rank_ic,
      na.rm = TRUE
    )
  ),
  
  sd_rank_ic = c(
    sd(
      daily_rank_comparison$original_rank_ic,
      na.rm = TRUE
    ),
    
    sd(
      daily_rank_comparison$improved_rank_ic,
      na.rm = TRUE
    )
  ),
  
  positive_rank_ic_share = c(
    mean(
      daily_rank_comparison$original_rank_ic > 0,
      na.rm = TRUE
    ),
    
    mean(
      daily_rank_comparison$improved_rank_ic > 0,
      na.rm = TRUE
    )
  )
)


write_csv(
  daily_rank_comparison,
  here(
    "results",
    "tables",
    "fair_daily_rank_ic.csv"
  )
)

write_csv(
  rank_ic_comparison,
  here(
    "results",
    "tables",
    "fair_rank_ic_comparison.csv"
  )
)


# ============================================================
# 9. MONTHLY FORMATION DATES
# ============================================================

monthly_prediction_sample <- comparison_predictions |>
  dplyr::mutate(
    month = lubridate::floor_date(
      date,
      unit = "month"
    )
  ) |>
  dplyr::group_by(month) |>
  dplyr::filter(
    date == min(date)
  ) |>
  dplyr::ungroup()


# ============================================================
# 10. APPLY IDENTICAL PORTFOLIO RULES TO BOTH MODELS
# ============================================================

original_holdings <- monthly_prediction_sample |>
  dplyr::group_by(month) |>
  dplyr::slice_max(
    order_by = original_prediction,
    n = portfolio_size,
    with_ties = FALSE
  ) |>
  dplyr::arrange(
    month,
    dplyr::desc(original_prediction)
  ) |>
  dplyr::mutate(
    rank = dplyr::row_number(),
    weight = 1 / portfolio_size,
    model = "Original XGBoost"
  ) |>
  dplyr::ungroup()


improved_holdings <- monthly_prediction_sample |>
  dplyr::group_by(month) |>
  dplyr::slice_max(
    order_by = improved_prediction,
    n = portfolio_size,
    with_ties = FALSE
  ) |>
  dplyr::arrange(
    month,
    dplyr::desc(improved_prediction)
  ) |>
  dplyr::mutate(
    rank = dplyr::row_number(),
    weight = 1 / portfolio_size,
    model = "Improved XGBoost"
  ) |>
  dplyr::ungroup()


all_holdings <- dplyr::bind_rows(
  original_holdings,
  improved_holdings
)


write_csv(
  all_holdings,
  here(
    "results",
    "tables",
    "fair_monthly_holdings.csv"
  )
)


# ============================================================
# 11. TURNOVER FUNCTION
# ============================================================

calculate_turnover <- function(
    holdings_data,
    model_label
) {
  
  model_holdings <- holdings_data |>
    dplyr::filter(
      model == model_label
    ) |>
    dplyr::arrange(
      month,
      rank
    )
  
  formation_months <- sort(
    unique(model_holdings$month)
  )
  
  turnover_values <- numeric(
    length(formation_months)
  )
  
  previous_symbols <- character(0)
  
  for (i in seq_along(formation_months)) {
    
    current_symbols <- model_holdings |>
      dplyr::filter(
        month == formation_months[i]
      ) |>
      dplyr::pull(symbol)
    
    if (i == 1) {
      
      # Initial purchase from cash into a fully invested portfolio.
      turnover_values[i] <- 1
      
    } else {
      
      overlap_count <- length(
        intersect(
          previous_symbols,
          current_symbols
        )
      )
      
      turnover_values[i] <-
        1 - overlap_count / portfolio_size
    }
    
    previous_symbols <- current_symbols
  }
  
  tibble(
    model = model_label,
    month = formation_months,
    turnover = turnover_values,
    transaction_cost =
      turnover_values * transaction_cost_rate
  )
}


original_turnover <- calculate_turnover(
  all_holdings,
  "Original XGBoost"
)

improved_turnover <- calculate_turnover(
  all_holdings,
  "Improved XGBoost"
)

turnover_comparison <- dplyr::bind_rows(
  original_turnover,
  improved_turnover
)


write_csv(
  turnover_comparison,
  here(
    "results",
    "tables",
    "fair_turnover_comparison.csv"
  )
)


# ============================================================
# 12. MONTHLY MODEL RETURNS
# ============================================================

original_monthly_returns <- original_holdings |>
  dplyr::group_by(month) |>
  dplyr::summarise(
    original_gross_return =
      weighted.mean(
        target,
        weight
      ),
    
    original_selected_stocks = dplyr::n(),
    
    .groups = "drop"
  ) |>
  dplyr::left_join(
    original_turnover |>
      dplyr::select(
        month,
        original_turnover = turnover,
        original_transaction_cost =
          transaction_cost
      ),
    
    by = "month"
  ) |>
  dplyr::mutate(
    original_net_return =
      original_gross_return -
      original_transaction_cost
  )


improved_monthly_returns <- improved_holdings |>
  dplyr::group_by(month) |>
  dplyr::summarise(
    improved_gross_return =
      weighted.mean(
        target,
        weight
      ),
    
    improved_selected_stocks = dplyr::n(),
    
    .groups = "drop"
  ) |>
  dplyr::left_join(
    improved_turnover |>
      dplyr::select(
        month,
        improved_turnover = turnover,
        improved_transaction_cost =
          transaction_cost
      ),
    
    by = "month"
  ) |>
  dplyr::mutate(
    improved_net_return =
      improved_gross_return -
      improved_transaction_cost
  )


# ============================================================
# 13. SIMPLE BENCHMARK RETURNS
# ============================================================

benchmark_monthly_returns <- monthly_prediction_sample |>
  dplyr::group_by(month) |>
  dplyr::summarise(
    equal_weight_return =
      mean(
        target
      ),
    
    universe_stocks =
      dplyr::n_distinct(symbol),
    
    .groups = "drop"
  ) |>
  dplyr::left_join(
    saved_benchmarks,
    by = "month"
  )


# ============================================================
# 14. COMBINE MONTHLY RESULTS
# ============================================================

fair_monthly_comparison <- original_monthly_returns |>
  dplyr::inner_join(
    improved_monthly_returns,
    by = "month"
  ) |>
  dplyr::inner_join(
    benchmark_monthly_returns,
    by = "month"
  ) |>
  dplyr::arrange(month) |>
  tidyr::drop_na(
    original_net_return,
    improved_net_return,
    equal_weight_return,
    spy_return
  ) |>
  dplyr::mutate(
    original_net_growth =
      cumprod(
        1 + original_net_return
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
  fair_monthly_comparison,
  here(
    "results",
    "tables",
    "fair_monthly_model_comparison.csv"
  )
)


# ============================================================
# 15. INVESTMENT-PERFORMANCE FUNCTION
# ============================================================

calculate_investment_performance <- function(
    return_vector,
    strategy_name
) {
  
  return_vector <- return_vector[
    is.finite(return_vector)
  ]
  
  number_of_months <- length(
    return_vector
  )
  
  growth <- cumprod(
    1 + return_vector
  )
  
  running_maximum <- cummax(
    c(
      1,
      growth
    )
  )[-1]
  
  drawdowns <-
    growth / running_maximum - 1
  
  monthly_sd <- stats::sd(
    return_vector
  )
  
  tibble(
    strategy = strategy_name,
    
    months = number_of_months,
    
    total_return =
      prod(
        1 + return_vector
      ) - 1,
    
    annualized_return =
      prod(
        1 + return_vector
      )^(12 / number_of_months) - 1,
    
    annualized_volatility =
      monthly_sd * sqrt(12),
    
    annualized_sharpe_rf_zero = if (
      is.finite(monthly_sd) &&
      monthly_sd > 0
    ) {
      mean(return_vector) /
        monthly_sd *
        sqrt(12)
    } else {
      NA_real_
    },
    
    maximum_drawdown =
      min(
        drawdowns,
        na.rm = TRUE
      ),
    
    positive_month_share =
      mean(
        return_vector > 0
      ),
    
    mean_monthly_return =
      mean(return_vector)
  )
}


fair_investment_performance <- dplyr::bind_rows(
  calculate_investment_performance(
    fair_monthly_comparison$original_net_return,
    "Original XGBoost, equal-weight top 10, net"
  ),
  
  calculate_investment_performance(
    fair_monthly_comparison$improved_net_return,
    "Improved XGBoost, equal-weight top 10, net"
  ),
  
  calculate_investment_performance(
    fair_monthly_comparison$equal_weight_return,
    "Equal-weight stock universe"
  ),
  
  calculate_investment_performance(
    fair_monthly_comparison$spy_return,
    "SPY"
  )
)


write_csv(
  fair_investment_performance,
  here(
    "results",
    "tables",
    "fair_investment_performance.csv"
  )
)


# ============================================================
# 16. PAIRED MONTHLY RETURN TESTS
# ============================================================

calculate_paired_month_test <- function(
    primary_return,
    comparison_return,
    comparison_name
) {
  
  return_difference <-
    primary_return - comparison_return
  
  t_test_result <- stats::t.test(
    return_difference,
    mu = 0
  )
  
  bootstrap_means <- replicate(
    bootstrap_repetitions,
    mean(
      base::sample(
        return_difference,
        size = length(return_difference),
        replace = TRUE
      )
    )
  )
  
  bootstrap_interval <- stats::quantile(
    bootstrap_means,
    probs = c(
      0.025,
      0.975
    ),
    names = FALSE
  )
  
  tibble(
    comparison =
      paste(
        "Improved XGBoost minus",
        comparison_name
      ),
    
    months =
      length(return_difference),
    
    mean_monthly_difference =
      mean(return_difference),
    
    median_monthly_difference =
      median(return_difference),
    
    improved_outperformance_share =
      mean(return_difference > 0),
    
    paired_t_statistic =
      unname(t_test_result$statistic),
    
    paired_t_p_value =
      t_test_result$p.value,
    
    paired_t_ci_lower =
      t_test_result$conf.int[1],
    
    paired_t_ci_upper =
      t_test_result$conf.int[2],
    
    bootstrap_ci_lower =
      bootstrap_interval[1],
    
    bootstrap_ci_upper =
      bootstrap_interval[2]
  )
}


set.seed(123)

paired_monthly_tests <- dplyr::bind_rows(
  calculate_paired_month_test(
    fair_monthly_comparison$improved_net_return,
    fair_monthly_comparison$original_net_return,
    "Original XGBoost"
  ),
  
  calculate_paired_month_test(
    fair_monthly_comparison$improved_net_return,
    fair_monthly_comparison$equal_weight_return,
    "equal-weight universe"
  ),
  
  calculate_paired_month_test(
    fair_monthly_comparison$improved_net_return,
    fair_monthly_comparison$spy_return,
    "SPY"
  )
)


write_csv(
  paired_monthly_tests,
  here(
    "results",
    "tables",
    "fair_paired_monthly_tests.csv"
  )
)


# ============================================================
# 17. TURNOVER SUMMARY
# ============================================================

turnover_summary <- turnover_comparison |>
  dplyr::group_by(model) |>
  dplyr::summarise(
    months = dplyr::n(),
    
    mean_turnover =
      mean(turnover),
    
    median_turnover =
      median(turnover),
    
    maximum_turnover =
      max(turnover),
    
    total_estimated_cost =
      sum(transaction_cost),
    
    .groups = "drop"
  )


write_csv(
  turnover_summary,
  here(
    "results",
    "tables",
    "fair_turnover_summary.csv"
  )
)


# ============================================================
# 18. FAIR CUMULATIVE-RETURN FIGURE
# ============================================================

cumulative_plot_data <- fair_monthly_comparison |>
  dplyr::select(
    month,
    `Original XGBoost, net` =
      original_net_growth,
    
    `Improved XGBoost, net` =
      improved_net_growth,
    
    `Equal-weight universe` =
      equal_weight_growth,
    
    SPY =
      spy_growth
  ) |>
  tidyr::pivot_longer(
    cols = -month,
    names_to = "strategy",
    values_to = "growth"
  ) |>
  dplyr::mutate(
    strategy = factor(
      strategy,
      levels = c(
        "Improved XGBoost, net",
        "Original XGBoost, net",
        "Equal-weight universe",
        "SPY"
      )
    )
  )


fair_cumulative_plot <- ggplot(
  cumulative_plot_data,
  aes(
    x = month,
    y = growth,
    color = strategy,
    linetype = strategy,
    linewidth = strategy
  )
) +
  geom_hline(
    yintercept = 1,
    color = "#AEB4BD",
    linewidth = 0.7,
    linetype = "dashed"
  ) +
  geom_line(
    lineend = "round"
  ) +
  scale_color_manual(
    values = c(
      "Improved XGBoost, net" =
        pink_dark,
      
      "Original XGBoost, net" =
        blue_dark,
      
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
      
      "Original XGBoost, net" =
        "solid",
      
      "Equal-weight universe" =
        "longdash",
      
      "SPY" =
        "dotdash"
    )
  ) +
  scale_linewidth_manual(
    values = c(
      "Improved XGBoost, net" = 1.35,
      "Original XGBoost, net" = 1.15,
      "Equal-weight universe" = 1.05,
      "SPY" = 1.05
    )
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b\n%Y",
    expand = expansion(
      mult = c(0.01, 0.02)
    )
  ) +
  scale_y_continuous(
    labels = label_number(
      accuracy = 0.01,
      suffix = "\u00d7"
    ),
    expand = expansion(
      mult = c(0.03, 0.08)
    )
  ) +
  labs(
    title =
      "Fair out-of-sample investment comparison",
    
    subtitle =
      paste0(
        "All model portfolios hold the 10 highest-ranked stocks ",
        "with equal weights and deduct ",
        transaction_cost_bps,
        " bps per unit of turnover"
      ),
    
    x = NULL,
    y = "Growth of $1",
    
    caption =
      paste0(
        "The original and improved models use identical formation dates, ",
        "portfolio rules, return targets, and transaction-cost assumptions."
      )
  ) +
  guides(
    color = guide_legend(
      nrow = 2,
      byrow = TRUE
    ),
    
    linetype = guide_legend(
      nrow = 2,
      byrow = TRUE
    ),
    
    linewidth = guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  ) +
  theme_blog()


ggsave(
  filename = here(
    "results",
    "figures",
    "fair_cumulative_returns.png"
  ),
  
  plot = fair_cumulative_plot,
  width = 10,
  height = 7,
  dpi = 320,
  bg = "white"
)


# ============================================================
# 19. RISK-RETURN FIGURE
# ============================================================

risk_return_plot_data <- fair_investment_performance |>
  dplyr::mutate(
    short_strategy = dplyr::case_when(
      stringr::str_detect(
        strategy,
        "^Original"
      ) ~ "Original XGBoost",
      
      stringr::str_detect(
        strategy,
        "^Improved"
      ) ~ "Improved XGBoost",
      
      stringr::str_detect(
        strategy,
        "^Equal"
      ) ~ "Equal-weight universe",
      
      TRUE ~ "SPY"
    ),
    
    short_strategy = factor(
      short_strategy,
      levels = c(
        "Improved XGBoost",
        "Original XGBoost",
        "Equal-weight universe",
        "SPY"
      )
    )
  )


risk_return_plot <- ggplot(
  risk_return_plot_data,
  aes(
    x = annualized_volatility,
    y = annualized_return,
    fill = short_strategy,
    color = short_strategy
  )
) +
  geom_point(
    shape = 21,
    size = 5,
    stroke = 1.2
  ) +
  geom_text(
    aes(
      label = short_strategy
    ),
    nudge_y = 0.022,
    size = 3.6,
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values = c(
      "Improved XGBoost" =
        pink_fill,
      
      "Original XGBoost" =
        blue_fill,
      
      "Equal-weight universe" =
        "#D9DDE2",
      
      "SPY" =
        "#B8BDC4"
    )
  ) +
  scale_color_manual(
    values = c(
      "Improved XGBoost" =
        pink_dark,
      
      "Original XGBoost" =
        blue_dark,
      
      "Equal-weight universe" =
        grey_dark,
      
      "SPY" =
        black_color
    )
  ) +
  scale_x_continuous(
    labels = label_percent(
      accuracy = 1
    ),
    expand = expansion(
      mult = c(0.15, 0.15)
    )
  ) +
  scale_y_continuous(
    labels = label_percent(
      accuracy = 1
    ),
    expand = expansion(
      mult = c(0.20, 0.25)
    )
  ) +
  labs(
    title =
      "Return and risk under a common portfolio rule",
    
    subtitle =
      "Annualized out-of-sample performance after model transaction costs",
    
    x = "Annualized volatility",
    y = "Annualized return",
    
    caption =
      "Higher return and lower volatility are preferable; Sharpe ratios and drawdowns are reported in the saved table."
  ) +
  guides(
    fill = "none",
    color = "none"
  ) +
  theme_blog()


ggsave(
  filename = here(
    "results",
    "figures",
    "fair_risk_return_comparison.png"
  ),
  
  plot = risk_return_plot,
  width = 9,
  height = 6.5,
  dpi = 320,
  bg = "white"
)


# ============================================================
# 20. SAVE A REPRODUCIBLE TEXT SUMMARY
# ============================================================

original_performance <- fair_investment_performance |>
  dplyr::filter(
    stringr::str_detect(
      strategy,
      "^Original"
    )
  )

improved_performance <- fair_investment_performance |>
  dplyr::filter(
    stringr::str_detect(
      strategy,
      "^Improved"
    )
  )

original_predictive <- predictive_comparison |>
  dplyr::filter(
    model == "Original XGBoost"
  )

improved_predictive <- predictive_comparison |>
  dplyr::filter(
    model == "Improved XGBoost"
  )

historical_benchmark_predictive <- predictive_comparison |>
  dplyr::filter(
    model == "Historical mean prediction"
  )


comparison_note <- c(
  "# Fair Original-versus-Improved Comparison",
  "",
  paste0(
    "- Common out-of-sample observations: ",
    format(
      nrow(comparison_predictions),
      big.mark = ","
    ),
    "."
  ),
  
  paste0(
    "- Evaluation months: ",
    nrow(fair_monthly_comparison),
    "."
  ),
  
  paste0(
    "- Original-model RMSE: ",
    round(original_predictive$rmse, 4),
    "; improved-model RMSE: ",
    round(improved_predictive$rmse, 4),
    "; historical-mean RMSE: ",
    round(historical_benchmark_predictive$rmse, 4),
    "."
  ),
  
  paste0(
    "- Original-model Pearson correlation: ",
    round(
      original_predictive$pearson_correlation,
      4
    ),
    "; improved-model correlation: ",
    round(
      improved_predictive$pearson_correlation,
      4
    ),
    "."
  ),
  
  paste0(
    "- Original standardized net total return: ",
    percent(
      original_performance$total_return,
      accuracy = 0.1
    ),
    "."
  ),
  
  paste0(
    "- Improved standardized net total return: ",
    percent(
      improved_performance$total_return,
      accuracy = 0.1
    ),
    "."
  ),
  
  paste0(
    "- Original standardized Sharpe ratio: ",
    round(
      original_performance$annualized_sharpe_rf_zero,
      2
    ),
    "; improved standardized Sharpe ratio: ",
    round(
      improved_performance$annualized_sharpe_rf_zero,
      2
    ),
    "."
  ),
  
  paste0(
    "- Original standardized maximum drawdown: ",
    percent(
      original_performance$maximum_drawdown,
      accuracy = 0.1
    ),
    "; improved standardized maximum drawdown: ",
    percent(
      improved_performance$maximum_drawdown,
      accuracy = 0.1
    ),
    "."
  ),
  
  "",
  "Interpretation should distinguish predictive accuracy from investment performance. A model can have weak point predictions yet still rank some stocks usefully, while strong backtest returns can also reflect concentration, market exposure, or sampling luck."
)


writeLines(
  comparison_note,
  here(
    "notes",
    "04_fair_model_comparison.md"
  )
)


# ============================================================
# 21. PRINT MAIN RESULTS
# ============================================================

cat(
  "\n",
  paste(
    rep("=", 58),
    collapse = ""
  ),
  "\nFAIR PREDICTIVE COMPARISON\n",
  paste(
    rep("=", 58),
    collapse = ""
  ),
  "\n",
  sep = ""
)

print(
  predictive_comparison
)


cat(
  "\n",
  paste(
    rep("=", 58),
    collapse = ""
  ),
  "\nFAIR RANK-CORRELATION COMPARISON\n",
  paste(
    rep("=", 58),
    collapse = ""
  ),
  "\n",
  sep = ""
)

print(
  rank_ic_comparison
)


cat(
  "\n",
  paste(
    rep("=", 58),
    collapse = ""
  ),
  "\nFAIR INVESTMENT PERFORMANCE\n",
  paste(
    rep("=", 58),
    collapse = ""
  ),
  "\n",
  sep = ""
)

print(
  fair_investment_performance
)


cat(
  "\n",
  paste(
    rep("=", 58),
    collapse = ""
  ),
  "\nPAIRED MONTHLY RETURN TESTS\n",
  paste(
    rep("=", 58),
    collapse = ""
  ),
  "\n",
  sep = ""
)

print(
  paired_monthly_tests
)


cat(
  "\n",
  paste(
    rep("=", 58),
    collapse = ""
  ),
  "\nTURNOVER COMPARISON\n",
  paste(
    rep("=", 58),
    collapse = ""
  ),
  "\n",
  sep = ""
)

print(
  turnover_summary
)


cat(
  "\nFair model comparison completed successfully.\n",
  "Review results/tables, results/figures, and notes/04_fair_model_comparison.md.\n",
  sep = ""
)


# ============================================================
# 22. SESSION INFORMATION
# ============================================================

capture.output(
  sessionInfo(),
  file = here(
    "results",
    "session_info_fair_comparison.txt"
  )
)
