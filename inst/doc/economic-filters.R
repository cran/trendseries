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
data("gdp_construction", "ibcbr", "vehicles", package = "trendseries")

## ----hp-quarterly-------------------------------------------------------------
# Apply HP filter to quarterly data
gdp_hp <- gdp_construction |>
  augment_trends(
    value_col = "index",
    methods = "hp"
  )

# View results
head(gdp_hp)

## ----hp-plot------------------------------------------------------------------
# Calculate the cycle (deviation from trend)
gdp_hp <- gdp_hp |>
  mutate(cycle = index - trend_hp)

# Plot 1: Trend vs Original
p1 <- gdp_hp |>
  select(date, index, trend_hp) |>
  pivot_longer(cols = c(index, trend_hp), names_to = "series") |>
  mutate(series = ifelse(series == "index", "Original", "HP Trend")) |>
  ggplot(aes(x = date, y = value, color = series)) +
  geom_line(linewidth = 0.9) +
  labs(
    title = "GDP Construction: Original vs HP Trend",
    subtitle = "Quarterly data with standard HP filter (λ=1600)",
    x = NULL,
    y = "Index",
    color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot 2: Cyclical component
p2 <- gdp_hp |>
  ggplot(aes(x = date, y = cycle)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 0.9, color = "#0072B2") +
  labs(
    title = "Cyclical Component (Output Gap)",
    subtitle = "Deviations from HP trend",
    x = "Date",
    y = "Gap (index points)",
    color = NULL
  ) +
  theme_minimal()

print(p1)
print(p2)

## ----hp-lambda----------------------------------------------------------------
# Test different lambda values on quarterly data
lambdas <- c(400, 1600, 6400)

gdp_lambdas <- gdp_construction

for (lambda in lambdas) {
  temp <- gdp_construction |>
    augment_trends(
      value_col = "index",
      methods = "hp",
      smoothing = lambda
    ) |>
    select(trend_hp)

  names(temp) <- paste0("hp_", lambda)
  gdp_lambdas <- bind_cols(gdp_lambdas, temp)
}

# Plot
gdp_lambdas |>
  select(date, index, starts_with("hp_")) |>
  pivot_longer(
    cols = c(index, starts_with("hp_")),
    names_to = "method",
    values_to = "value"
  ) |>
  mutate(
    method = case_when(
      method == "index" ~ "Original",
      method == "hp_400" ~ "λ = 400 (flexible)",
      method == "hp_1600" ~ "λ = 1600 (standard)",
      method == "hp_6400" ~ "λ = 6400 (very smooth)"
    )
  ) |>
  ggplot(aes(x = date, y = value, color = method)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "HP Filter with Different Lambda Values",
    subtitle = "Quarterly GDP construction data",
    x = "Date",
    y = "Index",
    color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

## ----hp-monthly---------------------------------------------------------------
# Apply HP filter to monthly IBC-Br data
ibcbr_hp <- ibcbr |>
  slice_tail(n = 120) |>  # Last 10 years
  augment_trends(
    value_col = "index",
    methods = "hp"
  ) |>
  mutate(cycle = index - trend_hp)

# Plot trend and cycle
p1 <- ibcbr_hp |>
  select(date, index, trend_hp) |>
  pivot_longer(cols = c(index, trend_hp), names_to = "series") |>
  mutate(series = ifelse(series == "index", "Original", "HP Trend")) |>
  ggplot(aes(x = date, y = value, color = series)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "IBC-Br Economic Activity: HP Trend",
    subtitle = "Monthly data with λ=14400",
    x = NULL,
    y = "Index",
    color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

p2 <- ibcbr_hp |>
  ggplot(aes(x = date, y = cycle)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 0.8, color = "#0072B2") +
  geom_ribbon(aes(ymin = pmin(cycle, 0), ymax = 0), alpha = 0.3, fill = "#D55E00") +
  geom_ribbon(aes(ymin = 0, ymax = pmax(cycle, 0)), alpha = 0.3, fill = "#009E73") +
  labs(
    title = "Business Cycle Component",
    subtitle = "Green = above trend (expansion), Red = below trend (contraction)",
    x = "Date",
    y = "Gap (index points)"
  ) +
  theme_minimal()

print(p1)
print(p2)

## ----bk-filter----------------------------------------------------------------
# Apply BK filter to quarterly GDP data
# Isolate cycles between 6 and 32 quarters (standard business cycle range)
gdp_bk <- gdp_construction |>
  augment_trends(
    value_col = "index",
    methods = "bk",
    band = c(6, 32)  # Business cycle frequencies
  )

# The BK filter returns the cycle, not the trend
# So we need to calculate the trend as: trend = original - cycle
gdp_bk <- gdp_bk |>
  mutate(
    cycle_bk = trend_bk,  # Rename for clarity
    trend_bk_calc = index - cycle_bk
  )

# Plot the cycle
gdp_bk |>
  filter(!is.na(cycle_bk)) |>  # BK loses observations at edges
  ggplot(aes(x = date, y = cycle_bk)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 0.9, color = "#0072B2") +
  geom_ribbon(aes(ymin = pmin(cycle_bk, 0), ymax = 0), alpha = 0.3, fill = "#D55E00") +
  geom_ribbon(aes(ymin = 0, ymax = pmax(cycle_bk, 0)), alpha = 0.3, fill = "#009E73") +
  labs(
    title = "Baxter-King Business Cycle",
    subtitle = "Isolates fluctuations between 6-32 quarters",
    x = "Date",
    y = "Cycle (index points)"
  ) +
  theme_minimal()

## ----cf-filter----------------------------------------------------------------
# Apply CF filter
gdp_cf <- gdp_construction |>
  augment_trends(
    value_col = "index",
    methods = "cf",
    band = c(6, 32)
  ) |>
  mutate(
    cycle_cf = trend_cf,
    trend_cf_calc = index - cycle_cf
  )

# Compare BK and CF cycles
comparison <- bind_rows(
  gdp_bk |>
    select(date, cycle = cycle_bk) |>
    mutate(method = "Baxter-King"),

  gdp_cf |>
    select(date, cycle = cycle_cf) |>
    mutate(method = "Christiano-Fitzgerald")
)

comparison |>
  filter(!is.na(cycle)) |>
  ggplot(aes(x = date, y = cycle, color = method)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 0.9) +
  labs(
    title = "Comparing BK and CF Filters",
    subtitle = "Both isolate 6-32 quarter business cycles",
    x = "Date",
    y = "Cycle (index points)",
    color = "Method"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

## ----band-comparison----------------------------------------------------------
# Try different frequency bands on monthly vehicle data
vehicles_recent <- vehicles |>
  slice_tail(n = 120)

# Short cycles (1-2 years)
vehicles_short <- vehicles_recent |>
  augment_trends(value_col = "production", methods = "cf", band = c(12, 24)) |>
  select(date, production, cycle_short = trend_cf)

# Medium cycles (1.5-4 years)
vehicles_medium <- vehicles_recent |>
  augment_trends(value_col = "production", methods = "cf", band = c(18, 48)) |>
  select(cycle_medium = trend_cf)

# Combine
vehicles_bands <- bind_cols(vehicles_short, vehicles_medium) |>
  select(date, cycle_short, cycle_medium) |>
  pivot_longer(cols = starts_with("cycle"), names_to = "band", values_to = "cycle") |>
  mutate(
    band = case_when(
      band == "cycle_short" ~ "Short (12-24 months)",
      band == "cycle_medium" ~ "Medium (18-48 months)"
    )
  )

vehicles_bands |>
  filter(!is.na(cycle)) |>
  ggplot(aes(x = date, y = cycle, color = band)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 0.9) +
  labs(
    title = "CF Filter with Different Frequency Bands",
    subtitle = "Vehicle production - different cycle definitions",
    x = "Date",
    y = "Cycle",
    color = "Frequency Band"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

## ----hamilton-----------------------------------------------------------------
# Apply Hamilton filter to quarterly GDP
gdp_hamilton <- gdp_construction |>
  augment_trends(
    value_col = "index",
    methods = "hamilton"
  ) |>
  mutate(cycle_hamilton = index - trend_hamilton)

# Compare HP and Hamilton
comparison_hp_ham <- bind_rows(
  gdp_hp |>
    select(date, cycle) |>
    mutate(method = "HP Filter"),

  gdp_hamilton |>
    select(date, cycle = cycle_hamilton) |>
    mutate(method = "Hamilton Filter")
)

comparison_hp_ham |>
  filter(!is.na(cycle)) |>
  ggplot(aes(x = date, y = cycle, color = method)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 0.9) +
  labs(
    title = "HP Filter vs Hamilton Filter",
    subtitle = "Both estimate business cycle deviations",
    x = "Date",
    y = "Cycle (output gap)",
    color = "Method"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

## ----recession-dating---------------------------------------------------------
# Use HP filter cycle to identify recession periods
ibcbr_cycles <- ibcbr |>
  slice_tail(n = 120) |>
  augment_trends(value_col = "index", methods = "hp") |>
  mutate(
    cycle = index - trend_hp,
    is_recession = cycle < 0  # Below trend
  )

# Plot with recession shading
ibcbr_cycles |>
  ggplot(aes(x = date, y = cycle)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_ribbon(
    data = filter(ibcbr_cycles, is_recession),
    aes(ymin = -Inf, ymax = Inf),
    alpha = 0.2,
    fill = "gray70"
  ) +
  geom_line(linewidth = 0.9, color = "#0072B2") +
  labs(
    title = "IBC-Br Business Cycles and Recessions",
    subtitle = "Gray shading indicates periods below trend",
    x = "Date",
    y = "Output Gap (index points)"
  ) +
  theme_minimal()

## ----output-gap---------------------------------------------------------------
# Calculate output gap as percentage of trend
ibcbr_gap <- ibcbr |>
  slice_tail(n = 120) |>
  augment_trends(value_col = "index", methods = "hp") |>
  mutate(
    gap_pct = ((index - trend_hp) / trend_hp) * 100
  )

# Plot output gap
ibcbr_gap |>
  ggplot(aes(x = date, y = gap_pct)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = c(-2, 2), linetype = "dotted", color = "gray60", alpha = 0.7) +
  geom_line(linewidth = 0.9, color = "#0072B2") +
  geom_ribbon(aes(ymin = pmin(gap_pct, 0), ymax = 0), alpha = 0.3, fill = "#D55E00") +
  geom_ribbon(aes(ymin = 0, ymax = pmax(gap_pct, 0)), alpha = 0.3, fill = "#009E73") +
  labs(
    title = "Output Gap: Economic Activity vs Trend",
    subtitle = "Gap as percentage of trend (dotted lines = ±2%)",
    x = "Date",
    y = "Gap (%)"
  ) +
  theme_minimal()

## ----multi-sector-------------------------------------------------------------
# Get recent data for multiple series
recent_gdp <- gdp_construction |>
  slice_tail(n = 40) |>
  augment_trends(value_col = "index", methods = "hp") |>
  mutate(
    cycle = ((index - trend_hp) / trend_hp) * 100,
    indicator = "GDP Construction"
  ) |>
  select(date, indicator, cycle)

recent_vehicles <- vehicles |>
  slice_tail(n = 120) |>
  augment_trends(value_col = "production", methods = "hp") |>
  mutate(
    cycle = ((production - trend_hp) / trend_hp) * 100,
    indicator = "Vehicle Production"
  ) |>
  select(date, indicator, cycle)

recent_activity <- ibcbr |>
  slice_tail(n = 120) |>
  augment_trends(value_col = "index", methods = "hp") |>
  mutate(
    cycle = ((index - trend_hp) / trend_hp) * 100,
    indicator = "Economic Activity"
  ) |>
  select(date, indicator, cycle)

# Combine and plot
bind_rows(recent_gdp, recent_vehicles, recent_activity) |>
  ggplot(aes(x = date, y = cycle, color = indicator)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 0.9) +
  labs(
    title = "Business Cycles Across Economic Sectors",
    subtitle = "All series show output gaps as % of HP trend",
    x = "Date",
    y = "Output Gap (%)",
    color = "Indicator"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

## ----eval=FALSE---------------------------------------------------------------
# # Quarterly data (standard)
# data |> augment_trends(value_col = "value", methods = "hp", smoothing = 1600)
# 
# # Monthly data (standard)
# data |> augment_trends(value_col = "value", methods = "hp", smoothing = 14400)
# 
# # More flexible trend (lower lambda)
# data |> augment_trends(value_col = "value", methods = "hp", smoothing = 400)
# 
# # Smoother trend (higher lambda)
# data |> augment_trends(value_col = "value", methods = "hp", smoothing = 6400)

## ----eval=FALSE---------------------------------------------------------------
# # Quarterly: standard business cycles
# data |> augment_trends(value_col = "value", methods = "bk", band = c(6, 32))
# data |> augment_trends(value_col = "value", methods = "cf", band = c(6, 32))
# 
# # Monthly: standard business cycles
# data |> augment_trends(value_col = "value", methods = "cf", band = c(18, 96))

## ----eval=FALSE---------------------------------------------------------------
# # Default parameters work well for most cases
# data |> augment_trends(value_col = "value", methods = "hamilton")

