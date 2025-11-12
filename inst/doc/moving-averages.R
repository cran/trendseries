## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 5,
  warning = FALSE,
  message = FALSE
)

## ----setup--------------------------------------------------------------------
library(trendseries)
library(dplyr)
library(ggplot2)
library(tidyr)

# Load data
data("vehicles", "ibcbr", "electric", package = "trendseries")

## ----ma-basic-----------------------------------------------------------------
# Use recent data (last 5 years)
vehicles_recent <- vehicles |>
  slice_tail(n = 60)

# Apply 12-month moving average
vehicles_ma <- vehicles_recent |>
  augment_trends(
    value_col = "production",
    methods = "ma",
    window = 12
  )

# View results
head(vehicles_ma)

## ----ma-plot------------------------------------------------------------------
# Prepare plot data
plot_data <- vehicles_ma |>
  select(date, production, trend_ma) |>
  pivot_longer(
    cols = c(production, trend_ma),
    names_to = "series",
    values_to = "value"
  ) |>
  mutate(
    series = ifelse(series == "production", "Original Data", "12-Month MA")
  )

# Plot
ggplot(plot_data, aes(x = date, y = value, color = series)) +
  geom_line(linewidth = 0.9) +
  labs(
    title = "Vehicle Production: Simple Moving Average",
    subtitle = "12-month window smooths out month-to-month variation",
    x = "Date",
    y = "Production (thousands of units)",
    color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

## ----window-comparison--------------------------------------------------------
# Apply different window sizes
windows_to_test <- c(3, 6, 12, 24)

# Start with original data
vehicles_windows <- vehicles_recent

# Add each window size
for (w in windows_to_test) {
  temp <- vehicles_recent |>
    augment_trends(value_col = "production", methods = "ma", window = w) |>
    select(trend_ma)

  names(temp) <- paste0("ma_", w, "m")
  vehicles_windows <- bind_cols(vehicles_windows, temp)
}

# Prepare for plotting
plot_data <- vehicles_windows |>
  select(date, production, starts_with("ma_")) |>
  pivot_longer(
    cols = c(production, starts_with("ma_")),
    names_to = "method",
    values_to = "value"
  ) |>
  mutate(
    method = case_when(
      method == "production" ~ "Original",
      method == "ma_3m" ~ "3-month MA",
      method == "ma_6m" ~ "6-month MA",
      method == "ma_12m" ~ "12-month MA",
      method == "ma_24m" ~ "24-month MA"
    ),
    method = factor(method, levels = c("Original", "3-month MA", "6-month MA",
                                       "12-month MA", "24-month MA"))
  )

# Plot
ggplot(plot_data, aes(x = date, y = value, color = method)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Effect of Window Size on Moving Average",
    subtitle = "Larger windows = smoother trends, but slower to react",
    x = "Date",
    y = "Production (thousands of units)",
    color = "Method"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

## ----align-comparison---------------------------------------------------------
# Apply 12-month moving average with different alignments
vehicles_align <- vehicles_recent |>
  augment_trends(
    value_col = "production",
    methods = "ma",
    window = 12,
    align = "center"
  ) |>
  rename(trend_center = trend_ma)

# Add right alignment
vehicles_align <- vehicles_align |>
  augment_trends(
    value_col = "production",
    methods = "ma",
    window = 12,
    align = "right"
  ) |>
  rename(trend_right = trend_ma)

# Add left alignment
vehicles_align <- vehicles_align |>
  augment_trends(
    value_col = "production",
    methods = "ma",
    window = 12,
    align = "left"
  ) |>
  rename(trend_left = trend_ma)

# Prepare for plotting
plot_data <- vehicles_align |>
  select(date, production, starts_with("trend_")) |>
  pivot_longer(
    cols = starts_with("trend_"),
    names_to = "alignment",
    values_to = "value"
  ) |>
  mutate(
    alignment = case_when(
      alignment == "trend_center" ~ "Center (default)",
      alignment == "trend_right" ~ "Right (causal)",
      alignment == "trend_left" ~ "Left (anti-causal)"
    ),
    alignment = factor(
      alignment,
      levels = c("Center (default)", "Right (causal)", "Left (anti-causal)")
    )
  )

# Plot
ggplot(plot_data, aes(x = date, y = value, color = alignment)) +
  geom_line(linewidth = 0.9, alpha = 0.8) +
  labs(
    title = "Moving Average Alignment Comparison",
    subtitle = "12-month window with different alignments",
    x = "Date",
    y = "Production (thousands of units)",
    color = "Alignment"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

## ----realtime-example---------------------------------------------------------
# Simulate real-time analysis: what would we see in Dec 2022?
cutoff_date <- as.Date("2022-12-31")

# Data available up to cutoff
historical_data <- vehicles |>
  filter(date <= cutoff_date)

# Apply right-aligned MA (what we could compute in real-time)
realtime_ma <- historical_data |>
  augment_trends(
    value_col = "production",
    methods = "ma",
    window = 12,
    align = "right"
  )

# Show last 6 months of trend
realtime_ma |>
  slice_tail(n = 6) |>
  select(date, production, trend_ma)

## ----na-pattern---------------------------------------------------------------
# Check NA pattern for each alignment
na_summary <- vehicles_align |>
  summarise(
    center_nas = sum(is.na(trend_center)),
    right_nas = sum(is.na(trend_right)),
    left_nas = sum(is.na(trend_left))
  )

na_summary

## ----ewma-comparison----------------------------------------------------------
# Apply both methods separately (EWMA cannot use both window and smoothing)
# First: MA with window parameter
vehicles_ma <- vehicles_recent |>
  augment_trends(
    value_col = "production",
    methods = "ma",
    window = 12
  )

# Second: EWMA with smoothing (alpha) parameter
vehicles_ewma <- vehicles_recent |>
  augment_trends(
    value_col = "production",
    methods = "ewma",
    smoothing = 0.3
  )

# Combine the results
vehicles_ma_ewma <- vehicles_recent |>
  left_join(
    select(vehicles_ma, date, trend_ma),
    by = "date"
  ) |>
  left_join(
    select(vehicles_ewma, date, trend_ewma),
    by = "date"
  )

# Prepare for plotting
plot_data <- vehicles_ma_ewma |>
  select(date, production, trend_ma, trend_ewma) |>
  pivot_longer(
    cols = c(production, trend_ma, trend_ewma),
    names_to = "method",
    values_to = "value"
  ) |>
  mutate(
    method = case_when(
      method == "production" ~ "Original",
      method == "trend_ma" ~ "12-month MA",
      method == "trend_ewma" ~ "EWMA (α=0.3)"
    )
  )

# Plot
ggplot(plot_data, aes(x = date, y = value, color = method)) +
  geom_line(linewidth = 0.9) +
  labs(
    title = "Simple MA vs EWMA",
    subtitle = "EWMA emphasizes recent observations more than simple MA",
    x = "Date",
    y = "Production (thousands of units)",
    color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

## ----ewma-alpha---------------------------------------------------------------
# Test different alpha values
alphas <- c(0.1, 0.3, 0.5, 0.8)

vehicles_alphas <- vehicles_recent

for (a in alphas) {
  temp <- vehicles_recent |>
    augment_trends(value_col = "production", methods = "ewma", smoothing = a) |>
    select(trend_ewma)

  names(temp) <- paste0("ewma_", a)
  vehicles_alphas <- bind_cols(vehicles_alphas, temp)
}

# Plot
plot_data <- vehicles_alphas |>
  select(date, production, starts_with("ewma_")) |>
  pivot_longer(
    cols = c(production, starts_with("ewma_")),
    names_to = "method",
    values_to = "value"
  ) |>
  mutate(
    method = case_when(
      method == "production" ~ "Original",
      method == "ewma_0.1" ~ "α = 0.1 (smooth)",
      method == "ewma_0.3" ~ "α = 0.3",
      method == "ewma_0.5" ~ "α = 0.5",
      method == "ewma_0.8" ~ "α = 0.8 (responsive)"
    )
  )

ggplot(plot_data, aes(x = date, y = value, color = method)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "EWMA with Different Alpha Values",
    subtitle = "Higher alpha = more weight on recent data",
    x = "Date",
    y = "Production (thousands of units)",
    color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

## ----advanced-ma--------------------------------------------------------------
# Apply multiple advanced MA methods
# Note: EWMA uses smoothing, other methods use window
# Apply window-based methods
vehicles_window_methods <- vehicles_recent |>
  augment_trends(
    value_col = "production",
    methods = c("ma", "wma"),
    window = 12
  )

# Apply EWMA with smoothing parameter
vehicles_ewma_method <- vehicles_recent |>
  augment_trends(
    value_col = "production",
    methods = "ewma",
    smoothing = 0.3
  )

# Combine results
vehicles_advanced <- vehicles_recent |>
  left_join(
    select(vehicles_window_methods, date, starts_with("trend_")),
    by = "date"
  ) |>
  left_join(
    select(vehicles_ewma_method, date, trend_ewma),
    by = "date"
  )

# Prepare for plotting
plot_data <- vehicles_advanced |>
  select(date, production, starts_with("trend_")) |>
  pivot_longer(
    cols = c(production, starts_with("trend_")),
    names_to = "method",
    values_to = "value"
  ) |>
  mutate(
    method = case_when(
      method == "production" ~ "Original",
      method == "trend_ma" ~ "Simple MA",
      method == "trend_ewma" ~ "EWMA",
      method == "trend_wma" ~ "Weighted MA"
    )
  )

# Plot
ggplot(plot_data, aes(x = date, y = value, color = method)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Advanced Moving Average Methods",
    subtitle = "Weighted MA and EWMA reduce lag compared to simple MA",
    x = "Date",
    y = "Production (thousands of units)",
    color = "Method"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

## ----trend-changes------------------------------------------------------------
# Get recent IBC-Br data
ibcbr_recent <- ibcbr |>
  slice_tail(n = 72)

# Apply EWMA for responsiveness
ibcbr_trend <- ibcbr_recent |>
  augment_trends(
    value_col = "index",
    methods = "ewma",
    smoothing = 0.25
  )

# Prepare plot
plot_data <- ibcbr_trend |>
  select(date, index, trend_ewma) |>
  pivot_longer(
    cols = c(index, trend_ewma),
    names_to = "series",
    values_to = "value"
  ) |>
  mutate(
    series = ifelse(series == "index", "Original", "EWMA Trend")
  )

# Plot
ggplot(plot_data, aes(x = date, y = value, color = series)) +
  geom_line(linewidth = 0.9) +
  labs(
    title = "IBC-Br Economic Activity Index",
    subtitle = "EWMA trend helps identify economic turning points",
    x = "Date",
    y = "Index Value",
    color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

## ----seasonal-comparison------------------------------------------------------
# Get recent electricity data (seasonal)
electric_recent <- electric |>
  slice_tail(n = 60)

# Apply same 12-month MA to both series
electric_ma <- electric_recent |>
  augment_trends(value_col = "consumption", methods = "ma", window = 12)

vehicles_ma_comp <- vehicles_recent |>
  augment_trends(value_col = "production", methods = "ma", window = 12)

# Create plots
p1 <- electric_ma |>
  select(date, consumption, trend_ma) |>
  pivot_longer(cols = c(consumption, trend_ma), names_to = "series") |>
  mutate(series = ifelse(series == "consumption", "Original", "12-month MA")) |>
  ggplot(aes(x = date, y = value, color = series)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Electricity (Seasonal)",
    x = NULL,
    y = "GWh",
    color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

p2 <- vehicles_ma_comp |>
  select(date, production, trend_ma) |>
  pivot_longer(cols = c(production, trend_ma), names_to = "series") |>
  mutate(series = ifelse(series == "production", "Original", "12-month MA")) |>
  ggplot(aes(x = date, y = value, color = series)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Vehicles (Less Seasonal)",
    x = NULL,
    y = "Thousands",
    color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Display plots
print(p1)
print(p2)

## ----cross-series-------------------------------------------------------------
# Prepare data for three indicators
multi_series <- bind_rows(
  ibcbr_recent |>
    select(date, value = index) |>
    mutate(indicator = "Economic Activity"),

  vehicles_recent |>
    select(date, value = production) |>
    mutate(indicator = "Vehicle Production"),

  electric_recent |>
    select(date, value = consumption) |>
    mutate(indicator = "Electricity")
)

# Apply EWMA to all series
multi_trends <- multi_series |>
  group_by(indicator) |>
  augment_trends(
    value_col = "value",
    methods = "ewma",
    frequency = 12,
    smoothing = 0.2
  ) |>
  ungroup()

# Normalize trends to first observation = 100
multi_normalized <- multi_trends |>
  group_by(indicator) |>
  mutate(
    trend_normalized = (trend_ewma / first(trend_ewma)) * 100
  ) |>
  ungroup()

# Plot normalized trends
ggplot(multi_normalized, aes(x = date, y = trend_normalized, color = indicator)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Comparing Economic Indicators: EWMA Trends",
    subtitle = "Normalized to first observation = 100",
    x = "Date",
    y = "Index (normalized)",
    color = "Indicator"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

## ----eval=FALSE---------------------------------------------------------------
# # Conservative (smooth)
# data |> augment_trends(value_col = "value", methods = "ma", window = 24)
# data |> augment_trends(value_col = "value", methods = "ewma", smoothing = 0.15)
# 
# # Balanced (recommended starting point)
# data |> augment_trends(value_col = "value", methods = "ma", window = 12)
# data |> augment_trends(value_col = "value", methods = "ewma", smoothing = 0.3)
# 
# # Responsive (catches changes quickly)
# data |> augment_trends(value_col = "value", methods = "ma", window = 6)
# data |> augment_trends(value_col = "value", methods = "ewma", smoothing = 0.6)

## ----eval=FALSE---------------------------------------------------------------
# # Conservative
# data |> augment_trends(value_col = "value", methods = "ma", window = 8)
# 
# # Balanced
# data |> augment_trends(value_col = "value", methods = "ma", window = 4)
# 
# # Responsive
# data |> augment_trends(value_col = "value", methods = "ewma", smoothing = 0.5)

