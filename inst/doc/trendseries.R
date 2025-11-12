## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4.5
)

## ----setup, message=FALSE-----------------------------------------------------
library(trendseries)
library(dplyr)
library(ggplot2)

theme_series <- theme_minimal(paper = "#fefefe") +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    # Use colors
    palette.colour.discrete = c(
      "#024873FF",
      "#BF4F26FF",
      "#D98825FF",
      "#D9AA1EFF",
      "#A2A637FF"
    )
  )

## ----first-trend--------------------------------------------------------------
# Load the data
data("gdp_construction", package = "trendseries")

# Take a quick look
head(gdp_construction)

## ----hp-basic-----------------------------------------------------------------
# Extract trend using HP filter
gdp_with_trend <- augment_trends(
  gdp_construction,
  value_col = "index",
  methods = "hp"
)

# View the result
head(gdp_with_trend)

## ----plot-first-trend---------------------------------------------------------
# Prepare data for plotting
plot_data <- gdp_with_trend |>
  select(date, index, trend_hp) |>
  tidyr::pivot_longer(
    cols = c(index, trend_hp),
    names_to = "series",
    values_to = "value"
  ) |>
  mutate(
    series = case_when(
      series == "index" ~ "Data (original)",
      series == "trend_hp" ~ "HP Filter Trend"
    )
  )

# Create the plot
ggplot(plot_data, aes(x = date, y = value, color = series)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Brazil GDP Construction: Original vs Trend",
    x = "Date",
    y = "Construction Index",
    color = NULL
  ) +
  theme_series

## -----------------------------------------------------------------------------
gdp <- ts(
  gdp_construction$index,
  frequency = 4,
  start = c(1996, 1)
)

gdp_trend_hp <- extract_trends(gdp, "hp")

## ----compare-methods----------------------------------------------------------
# Extract multiple trends at once
gdp_comparison <- gdp_construction |>
  augment_trends(
    value_col = "index",
    methods = c("hp", "loess", "ma")
  )

# View the first few rows
gdp_comparison |>
  select(date, index, starts_with("trend_")) |>
  head()

## ----plot-comparison----------------------------------------------------------
# Prepare data for plotting
comparison_plot <- gdp_comparison |>
  select(date, index, starts_with("trend_")) |>
  tidyr::pivot_longer(
    cols = c(index, starts_with("trend_")),
    names_to = "method",
    values_to = "value"
  ) |>
  mutate(
    method = case_when(
      method == "index" ~ "Data (original)",
      method == "trend_hp" ~ "HP Filter",
      method == "trend_loess" ~ "LOESS",
      method == "trend_ma" ~ "Moving Average"
    )
  )

# Plot
ggplot(comparison_plot, aes(x = date, y = value, color = method)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Comparing Different Trend Extraction Methods",
    subtitle = "Same data, different methods",
    x = "Date",
    y = "Construction Index",
    color = "Method"
  ) +
  theme_series

## ----monthly-data-------------------------------------------------------------
# Load monthly vehicle production data
data("vehicles", package = "trendseries")

# Look at recent data (last 4 years)
recent_vehicles <- vehicles |>
  slice_tail(n = 48)

head(recent_vehicles)

## ----monthly-trend------------------------------------------------------------
# Extract trend from monthly data
vehicles_with_trend <- vehicles |>
  augment_trends(
    value_col = "production",
    methods = "hp"
  )

vehicles_with_trend <- vehicles_with_trend |>
  tidyr::pivot_longer(
    cols = c(production, trend_hp),
    names_to = "series",
    values_to = "value"
  ) |>
  mutate(
    series = ifelse(series == "production", "Original", "HP Trend"),
    # To make sure the trend is plotted on top of the original series
    # configure levels accordingly
    series = factor(series, levels = c("Original", "HP Trend"))
  )

ggplot(vehicles_with_trend, aes(x = date, y = value, color = series)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Brazil Vehicle Production: Monthly Data",
    subtitle = "Last 4 years of data",
    x = "Date",
    y = "Production (thousands of units)",
    color = NULL
  ) +
  theme_series

## ----window-param-------------------------------------------------------------
# Try different window sizes
vehicles_windows <- recent_vehicles |>
  augment_trends(
    value_col = "production",
    methods = "ma",
    window = 6
  ) |>
  rename(trend_ma_6m = trend_ma)

# Add 12-month window
vehicles_windows <- vehicles_windows |>
  augment_trends(
    value_col = "production",
    methods = "ma",
    window = 12
  )

# Visualize
vehicles_windows <- vehicles_windows |>
  select(date, production, trend_ma_6m, trend_ma) |>
  tidyr::pivot_longer(
    cols = c(production, trend_ma_6m, trend_ma),
    names_to = "method",
    values_to = "value"
  ) |>
  mutate(
    method = case_when(
      method == "production" ~ "Data (original)",
      method == "trend_ma_6m" ~ "MA (6-month)",
      method == "trend_ma" ~ "MA (12-month)"
    )
  )

ggplot(vehicles_windows, aes(x = date, y = value, color = method)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Window Size Comparison",
    subtitle = "Larger windows = smoother trends",
    x = "Date",
    y = "Production (thousands)",
    color = NULL
  ) +
  theme_series

## -----------------------------------------------------------------------------
data("ibcbr", package = "trendseries")

series <- full_join(ibcbr, vehicles, by = "date")

series <- series |>
  filter(date >= as.Date("2010-01-01")) |>
  tidyr::pivot_longer(
    cols = c(index, production),
    names_to = "indicator",
    values_to = "value"
  ) |>
  # Normalize to Jan 2010 = 100 for comparison
  mutate(
    norm_index = value / first(value) * 100,
    .by = indicator
  )

series <- augment_trends(
  series,
  value_col = "norm_index",
  methods = "hp",
  group_vars = "indicator"
)

## ----multi-series-------------------------------------------------------------
# Plot trends only
series |>
  ggplot(aes(x = date, color = indicator)) +
  geom_line(aes(y = norm_index), alpha = 0.4) +
  geom_line(aes(y = trend_hp), linewidth = 1) +
  labs(
    title = "Economic Indicators: HP Filter Trends",
    subtitle = "Normalized to first observation = 100",
    x = "Date",
    y = "Index (normalized)",
    color = "Indicator"
  ) +
  theme_series

## ----eval=FALSE---------------------------------------------------------------
# # Single method, quarterly data
# data |>
#   augment_trends(value_col = "your_column", methods = "hp")
# 
# # Single series, monthly data
# extract_trends(your_ts_data, method = "loess")

## ----eval=FALSE---------------------------------------------------------------
# data |>
#   augment_trends(
#     value_col = "your_column",
#     methods = c("hp", "loess", "ma")
#   )

## ----eval=FALSE---------------------------------------------------------------
# # Smoother HP filter (higher lambda)
# data |>
#   augment_trends(
#     value_col = "your_column",
#     methods = "hp",
#     smoothing = 3200  # vs default 1600 for quarterly
#   )
# 
# # Longer moving average window
# data |>
#   augment_trends(
#     value_col = "your_column",
#     methods = "ma",
#     window = 24  # 2-year window for monthly data
#   )

## ----eval=FALSE---------------------------------------------------------------
# # Apply trend to multiple series at once
# multi_series_data |>
#   group_by(country) |>
#   augment_trends(value_col = "gdp", methods = "hp") |>
#   ungroup()
# 
# # Or using group_vars argument
# multi_series_data |>
#   augment_trends(
#     value_col = "gdp",
#     methods = "hp",
#     group_vars = "country"
#   )

## -----------------------------------------------------------------------------
gdp_cons <- ts(
  gdp_construction$index,
  frequency = 4,
  start = c(1996, 1)
)

# Or, using lubridate to extract year and month
gdp_cons <- ts(
  gdp_construction$index,
  frequency = 4,
  start = c(lubridate::year(min(gdp_construction$date)),
            lubridate::quarter(min(gdp_construction$date)))
)

## -----------------------------------------------------------------------------
gdp_trend_hp <- mFilter::hpfilter(gdp_cons, 1600)

## -----------------------------------------------------------------------------
# Convert back to data frame using tsbox
trend_df <- tsbox::ts_df(gdp_trend_hp$trend)
names(trend_df) <- c("date", "trend_hp")

# Join with original data
gdp_manual <- left_join(gdp_construction, trend_df, by = "date")

