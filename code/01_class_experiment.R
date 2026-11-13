# ============================================================
# Blog Post 9
# Part 1: Revisiting the Class Stock-Prediction Experiment
# ============================================================

library(tidyverse)
library(readxl)
library(here)
library(scales)

set.seed(123)
options(scipen = 999)

# ============================================================
# 1. SETTINGS
# ============================================================

initial_capital <- 100000

experiment_start <- as.Date("2026-08-18")
experiment_end   <- as.Date("2026-11-12")

# Color palette
pink_fill  <- "#F5B2C8"
pink_dark  <- "#C85F87"
blue_fill  <- "#A9D6EE"
blue_dark  <- "#4F91B8"
text_color <- "#263238"
grid_color <- "#E7E9ED"
line_color <- "#6B7280"

# Shared theme for all figures
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
        margin = margin(b = 14)
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
      
      panel.grid.major.x = element_blank(),
      
      panel.grid.major.y = element_line(
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
# 2. OUTPUT FOLDERS
# ============================================================

folders <- c(
  here("data", "processed"),
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
# 3. IMPORT CLASS RESULTS
# ============================================================

input_file <- here(
  "data",
  "raw",
  "Full_Results.xlsx"
)

if (!file.exists(input_file)) {
  stop(
    "Full_Results.xlsx was not found in data/raw/."
  )
}

class_results_raw <- read_excel(input_file)

required_columns <- c(
  "portfolio1",
  "portfolio2"
)

if (!all(required_columns %in% names(class_results_raw))) {
  stop(
    "The spreadsheet must contain portfolio1 and portfolio2."
  )
}

class_results <- class_results_raw |>
  transmute(
    team = row_number(),
    
    portfolio1 = parse_number(
      as.character(portfolio1)
    ),
    
    portfolio2 = parse_number(
      as.character(portfolio2)
    )
  )

if (anyNA(class_results)) {
  stop(
    "Missing or nonnumeric portfolio values were found."
  )
}

class_results <- class_results |>
  mutate(
    portfolio1_return =
      portfolio1 / initial_capital - 1,
    
    portfolio2_return =
      portfolio2 / initial_capital - 1,
    
    difference =
      portfolio2 - portfolio1,
    
    return_difference =
      portfolio2_return - portfolio1_return,
    
    comparison = if_else(
      difference > 0,
      "Portfolio 2 higher",
      "Portfolio 1 higher"
    ),
    
    comparison = factor(
      comparison,
      levels = c(
        "Portfolio 1 higher",
        "Portfolio 2 higher"
      )
    )
  )

write_csv(
  class_results,
  here(
    "data",
    "processed",
    "class_portfolio_results.csv"
  )
)

# ============================================================
# 4. DESCRIPTIVE STATISTICS
# ============================================================

class_long <- class_results |>
  select(
    team,
    portfolio1,
    portfolio2
  ) |>
  pivot_longer(
    cols = c(
      portfolio1,
      portfolio2
    ),
    names_to = "portfolio",
    values_to = "final_value"
  ) |>
  mutate(
    portfolio = recode(
      portfolio,
      portfolio1 = "Portfolio 1\nInterest-based",
      portfolio2 = "Portfolio 2\nIndicator-based"
    ),
    
    portfolio = factor(
      portfolio,
      levels = c(
        "Portfolio 1\nInterest-based",
        "Portfolio 2\nIndicator-based"
      )
    ),
    
    return =
      final_value / initial_capital - 1
  )

portfolio_summary <- class_long |>
  group_by(portfolio) |>
  summarise(
    teams = n(),
    
    mean_final_value =
      mean(final_value),
    
    median_final_value =
      median(final_value),
    
    standard_deviation =
      sd(final_value),
    
    minimum =
      min(final_value),
    
    maximum =
      max(final_value),
    
    mean_return =
      mean(return),
    
    teams_above_initial =
      sum(final_value > initial_capital),
    
    share_above_initial =
      mean(final_value > initial_capital),
    
    .groups = "drop"
  )

write_csv(
  portfolio_summary,
  here(
    "results",
    "tables",
    "class_portfolio_summary.csv"
  )
)

# ============================================================
# 5. PAIRED T-TEST
# ============================================================

paired_test <- t.test(
  class_results$portfolio2,
  class_results$portfolio1,
  paired = TRUE,
  alternative = "two.sided",
  conf.level = 0.95
)

paired_test_table <- tibble(
  mean_difference =
    unname(paired_test$estimate),
  
  confidence_interval_low =
    paired_test$conf.int[[1]],
  
  confidence_interval_high =
    paired_test$conf.int[[2]],
  
  t_statistic =
    unname(paired_test$statistic),
  
  degrees_freedom =
    unname(paired_test$parameter),
  
  two_sided_p_value =
    paired_test$p.value
)

write_csv(
  paired_test_table,
  here(
    "results",
    "tables",
    "class_paired_t_test.csv"
  )
)

# ============================================================
# 6. PAIRED BOOTSTRAP
# ============================================================

paired_differences <- class_results$difference

B <- 10000

bootstrap_means <- replicate(
  B,
  mean(
    sample(
      paired_differences,
      size = length(paired_differences),
      replace = TRUE
    )
  )
)

bootstrap_ci <- quantile(
  bootstrap_means,
  probs = c(0.025, 0.975),
  names = FALSE
)

# ============================================================
# 7. SIGN-FLIP RANDOMIZATION TEST
# ============================================================

observed_mean_difference <-
  mean(paired_differences)

null_mean_differences <- replicate(
  B,
  mean(
    paired_differences *
      sample(
        c(-1, 1),
        size = length(paired_differences),
        replace = TRUE
      )
  )
)

randomization_p_value <- (
  sum(
    abs(null_mean_differences) >=
      abs(observed_mean_difference)
  ) + 1
) / (B + 1)

# ============================================================
# 8. CLASS COMPARISON SUMMARY
# ============================================================

class_comparison_summary <- tibble(
  metric = c(
    "Number of paired teams",
    "Mean Portfolio 1 final value",
    "Mean Portfolio 2 final value",
    "Mean paired difference",
    "Median paired difference",
    "SD of paired differences",
    "Portfolio 2 wins",
    "Portfolio 1 wins",
    "Correlation",
    "Two-sided paired t-test p-value",
    "Paired t-test CI lower",
    "Paired t-test CI upper",
    "Bootstrap CI lower",
    "Bootstrap CI upper",
    "Randomization p-value"
  ),
  
  value = c(
    nrow(class_results),
    
    mean(class_results$portfolio1),
    
    mean(class_results$portfolio2),
    
    mean(class_results$difference),
    
    median(class_results$difference),
    
    sd(class_results$difference),
    
    sum(class_results$difference > 0),
    
    sum(class_results$difference < 0),
    
    cor(
      class_results$portfolio1,
      class_results$portfolio2
    ),
    
    paired_test$p.value,
    
    paired_test$conf.int[[1]],
    
    paired_test$conf.int[[2]],
    
    bootstrap_ci[[1]],
    
    bootstrap_ci[[2]],
    
    randomization_p_value
  )
)

write_csv(
  class_comparison_summary,
  here(
    "results",
    "tables",
    "class_comparison_summary.csv"
  )
)

# ============================================================
# 9. FIGURE 1: CLASS PORTFOLIO DISTRIBUTIONS
# ============================================================

class_distribution_plot <- ggplot(
  class_long,
  aes(
    x = portfolio,
    y = final_value
  )
) +
  geom_hline(
    yintercept = initial_capital,
    linetype = "dashed",
    linewidth = 0.7,
    color = line_color
  ) +
  geom_boxplot(
    aes(
      fill = portfolio,
      color = portfolio
    ),
    width = 0.52,
    alpha = 0.55,
    linewidth = 0.8,
    outlier.shape = NA
  ) +
  geom_jitter(
    aes(
      fill = portfolio,
      color = portfolio
    ),
    shape = 21,
    width = 0.10,
    height = 0,
    size = 2.4,
    stroke = 0.7,
    alpha = 0.78
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 23,
    size = 3.5,
    stroke = 0.9,
    fill = "white",
    color = text_color
  ) +
  scale_fill_manual(
    values = c(
      "Portfolio 1\nInterest-based" =
        pink_fill,
      
      "Portfolio 2\nIndicator-based" =
        blue_fill
    )
  ) +
  scale_color_manual(
    values = c(
      "Portfolio 1\nInterest-based" =
        pink_dark,
      
      "Portfolio 2\nIndicator-based" =
        blue_dark
    )
  ) +
  scale_y_continuous(
    labels = label_dollar(),
    breaks = seq(
      90000,
      120000,
      by = 10000
    ),
    expand = expansion(
      mult = c(0.04, 0.07)
    )
  ) +
  guides(
    fill = "none",
    color = "none"
  ) +
  labs(
    title = "Final values of the two class portfolios",
    
    subtitle = paste0(
      "August 18–November 12, 2026 · ",
      "Diamonds mark group means"
    ),
    
    x = NULL,
    
    y = "Final portfolio value",
    
    caption = paste0(
      "Dashed line: $100,000 initial capital. ",
      "The exact Portfolio 2 indicator was not documented."
    )
  ) +
  theme_blog() +
  theme(
    axis.text.x = element_text(
      size = 11,
      face = "bold",
      lineheight = 1.05
    )
  )

ggsave(
  filename = here(
    "results",
    "figures",
    "class_portfolio_distributions.png"
  ),
  plot = class_distribution_plot,
  width = 7.6,
  height = 5.4,
  dpi = 300,
  bg = "white"
)

# ============================================================
# 10. FIGURE 2: PAIRED PORTFOLIO COMPARISON
# ============================================================

all_portfolio_values <- c(
  class_results$portfolio1,
  class_results$portfolio2
)

axis_min <- floor(
  min(all_portfolio_values) / 5000
) * 5000

axis_max <- ceiling(
  max(all_portfolio_values) / 5000
) * 5000

axis_breaks <- seq(
  axis_min,
  axis_max,
  by = 10000
)

class_paired_plot <- ggplot(
  class_results,
  aes(
    x = portfolio1,
    y = portfolio2
  )
) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    linewidth = 0.7,
    color = line_color
  ) +
  geom_point(
    aes(
      fill = comparison,
      color = comparison
    ),
    shape = 21,
    size = 3.2,
    stroke = 0.9,
    alpha = 0.82
  ) +
  scale_fill_manual(
    values = c(
      "Portfolio 1 higher" =
        pink_fill,
      
      "Portfolio 2 higher" =
        blue_fill
    )
  ) +
  scale_color_manual(
    values = c(
      "Portfolio 1 higher" =
        pink_dark,
      
      "Portfolio 2 higher" =
        blue_dark
    )
  ) +
  scale_x_continuous(
    limits = c(
      axis_min,
      axis_max
    ),
    breaks = axis_breaks,
    labels = label_dollar(),
    expand = expansion(mult = 0.015)
  ) +
  scale_y_continuous(
    limits = c(
      axis_min,
      axis_max
    ),
    breaks = axis_breaks,
    labels = label_dollar(),
    expand = expansion(mult = 0.015)
  ) +
  coord_fixed(
    ratio = 1,
    clip = "off"
  ) +
  labs(
    title = "Paired outcomes by student team",
    
    subtitle = paste0(
      "Portfolio 2 finished higher for ",
      sum(class_results$difference > 0),
      " of ",
      nrow(class_results),
      " teams"
    ),
    
    x = "Portfolio 1 final value",
    
    y = "Portfolio 2 final value",
    
    caption = paste0(
      "Points above the dashed 45-degree line ",
      "favor Portfolio 2."
    )
  ) +
  theme_blog() +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    aspect.ratio = 1
  )

ggsave(
  filename = here(
    "results",
    "figures",
    "class_paired_comparison.png"
  ),
  plot = class_paired_plot,
  width = 7,
  height = 6.4,
  dpi = 300,
  bg = "white"
)

# ============================================================
# 11. PERSONAL FIVE-STOCK PORTFOLIO
# ============================================================

# Values manually transcribed from my_own_stock.png.
# Reported gain/loss is retained because displayed prices
# have been rounded.

personal_holdings <- tribble(
  ~symbol, ~company, ~purchase_price_display,
  ~current_price_display, ~quantity,
  ~current_value, ~reported_gain_loss,
  ~reported_return,
  
  "AAPL", "Apple Inc.",
  314.35, 344.63, 50,
  17231.50, 1514.00, 0.0963,
  
  "LLY", "Eli Lilly & Co.",
  1193.46, 1417.54, 20,
  28350.80, 4481.60, 0.1878,
  
  "MRNA", "Moderna Inc.",
  146.49, 221.72, 200,
  44344.00, 15046.65, 0.5136,
  
  "NVDA", "NVIDIA Corp.",
  210.65, 204.64, 100,
  20464.00, -600.50, -0.0285,
  
  "NVO", "Novo Nordisk ADR",
  47.13, 40.00, 200,
  8000.00, -1425.00, -0.1512
) |>
  mutate(
    implied_cost_basis =
      current_value - reported_gain_loss,
    
    initial_weight =
      implied_cost_basis /
      sum(implied_cost_basis),
    
    ending_weight =
      current_value /
      sum(current_value),
    
    contribution_to_total_gain =
      reported_gain_loss /
      sum(reported_gain_loss),
    
    result = if_else(
      reported_gain_loss >= 0,
      "Gain",
      "Loss"
    ),
    
    result = factor(
      result,
      levels = c(
        "Gain",
        "Loss"
      )
    )
  )

write_csv(
  personal_holdings,
  here(
    "data",
    "processed",
    "personal_stock_holdings.csv"
  )
)

# ============================================================
# 12. PERSONAL PORTFOLIO SUMMARY
# ============================================================

reported_total_value <- 118390.30
reported_total_gain  <- 19016.75
reported_return      <- 0.1914

implied_invested_basis <-
  reported_total_value - reported_total_gain

capital_reconciliation_gap <-
  initial_capital - implied_invested_basis

personal_portfolio_summary <- tibble(
  metric = c(
    "Experiment start date",
    "Experiment end date",
    "Initial capital",
    "Number of stocks",
    "Reported final value",
    "Reported gain",
    "Reported return on invested basis",
    "Return relative to $100,000",
    "Implied invested cost basis",
    "Difference from $100,000"
  ),
  
  value = c(
    as.character(experiment_start),
    
    as.character(experiment_end),
    
    as.character(initial_capital),
    
    as.character(nrow(personal_holdings)),
    
    as.character(reported_total_value),
    
    as.character(reported_total_gain),
    
    as.character(reported_return),
    
    as.character(
      reported_total_value /
        initial_capital - 1
    ),
    
    as.character(implied_invested_basis),
    
    as.character(
      round(
        capital_reconciliation_gap,
        2
      )
    )
  )
)

write_csv(
  personal_portfolio_summary,
  here(
    "results",
    "tables",
    "personal_portfolio_summary.csv"
  )
)

# ============================================================
# 13. FIGURE 3: PERSONAL STOCK GAINS AND LOSSES
# ============================================================

personal_plot_data <- personal_holdings |>
  arrange(reported_gain_loss) |>
  mutate(
    symbol = factor(
      symbol,
      levels = symbol
    ),
    
    value_label = dollar(
      reported_gain_loss,
      accuracy = 1
    )
  )

personal_gain_plot <- ggplot(
  personal_plot_data,
  aes(
    x = symbol,
    y = reported_gain_loss,
    fill = result,
    color = result
  )
) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.7,
    color = line_color
  ) +
  geom_col(
    width = 0.64,
    linewidth = 0.7
  ) +
  geom_text(
    aes(
      label = value_label,
      hjust = if_else(
        reported_gain_loss >= 0,
        -0.12,
        1.12
      )
    ),
    size = 3.8,
    fontface = "bold",
    color = text_color
  ) +
  coord_flip(
    clip = "off"
  ) +
  scale_fill_manual(
    values = c(
      "Gain" = blue_fill,
      "Loss" = pink_fill
    )
  ) +
  scale_color_manual(
    values = c(
      "Gain" = blue_dark,
      "Loss" = pink_dark
    )
  ) +
  scale_y_continuous(
    labels = label_dollar(),
    breaks = seq(
      0,
      15000,
      by = 5000
    ),
    expand = expansion(
      mult = c(0.16, 0.16)
    )
  ) +
  labs(
    title = "Five-stock portfolio: gains and losses",
    
    subtitle = paste0(
      "Student-selected portfolio · ",
      "August 18–November 12, 2026"
    ),
    
    x = NULL,
    
    y = "Reported gain or loss",
    
    caption = paste0(
      "MRNA generated most of the portfolio's total gain."
    )
  ) +
  theme_blog() +
  theme(
    legend.position = "top",
    axis.text.y = element_text(
      size = 11,
      face = "bold"
    )
  )

ggsave(
  filename = here(
    "results",
    "figures",
    "personal_stock_gain_loss.png"
  ),
  plot = personal_gain_plot,
  width = 7.6,
  height = 5.2,
  dpi = 300,
  bg = "white"
)

# ============================================================
# 14. SAVE ANALYSIS NOTES
# ============================================================

note_text <- c(
  "# Class stock-prediction experiment",
  "",
  paste0(
    "The experiment ran from ",
    format(experiment_start, "%B %d, %Y"),
    " to ",
    format(experiment_end, "%B %d, %Y"),
    ". Each portfolio started with $100,000, and no ",
    "rebalancing occurred during the experiment."
  ),
  "",
  paste0(
    "Portfolio 1 was selected according to students' ",
    "interests. The classroom code describes Portfolio 2 ",
    "as indicator-based, but the exact indicator was not ",
    "documented in the available files."
  ),
  "",
  paste0(
    "Across ",
    nrow(class_results),
    " paired teams, the mean final value was ",
    dollar(mean(class_results$portfolio1)),
    " for Portfolio 1 and ",
    dollar(mean(class_results$portfolio2)),
    " for Portfolio 2."
  ),
  "",
  paste0(
    "Portfolio 2 exceeded Portfolio 1 for ",
    sum(class_results$difference > 0),
    " of ",
    nrow(class_results),
    " teams. Its estimated mean advantage was ",
    dollar(mean(class_results$difference)),
    ". The 95% confidence interval was ",
    dollar(paired_test$conf.int[[1]]),
    " to ",
    dollar(paired_test$conf.int[[2]]),
    ", with a two-sided p-value of ",
    round(paired_test$p.value, 4),
    "."
  ),
  "",
  paste0(
    "The personal five-stock portfolio ended at ",
    dollar(reported_total_value),
    ". MRNA generated most of the positive gain, while ",
    "NVDA and NVO lost value. This concentration shows ",
    "how a strong portfolio outcome can depend heavily on ",
    "one unusually successful stock."
  ),
  "",
  "## Interpretation",
  "",
  paste0(
    "Portfolio 2 had a higher average outcome, but its ",
    "dispersion was also larger, and the uncertainty ",
    "interval for its mean advantage included zero. A ",
    "short-period winner may reflect chance, company-specific ",
    "news, or a favorable market environment rather than a ",
    "persistent predictive relationship."
  ),
  "",
  paste0(
    "Stock prediction is difficult because prices respond ",
    "quickly to new information, relationships change across ",
    "market regimes, and flexible models can fit historical ",
    "noise. Reliable evidence therefore requires evaluation ",
    "on unseen dates and comparison with simple benchmarks."
  )
)

writeLines(
  note_text,
  here(
    "notes",
    "01_class_experiment_results.md"
  )
)

# ============================================================
# 15. REPRODUCIBILITY INFORMATION
# ============================================================

capture.output(
  sessionInfo(),
  file = here(
    "results",
    "session_info.txt"
  )
)

# ============================================================
# 16. PRINT RESULTS
# ============================================================

cat(
  "\n========================================\n",
  "CLASS PORTFOLIO SUMMARY\n",
  "========================================\n"
)

print(
  portfolio_summary,
  width = Inf
)

cat(
  "\n========================================\n",
  "PAIRED COMPARISON\n",
  "========================================\n"
)

print(
  paired_test_table,
  width = Inf
)

print(
  class_comparison_summary,
  n = Inf
)

cat(
  "\n========================================\n",
  "PERSONAL FIVE-STOCK PORTFOLIO\n",
  "========================================\n"
)

print(
  personal_holdings |>
    select(
      symbol,
      company,
      current_value,
      reported_gain_loss,
      reported_return,
      initial_weight,
      ending_weight
    ),
  width = Inf
)

print(
  personal_portfolio_summary,
  n = Inf
)

cat(
  "\nAnalysis completed successfully.\n",
  "Updated figures use the pink and light-blue palette.\n"
)