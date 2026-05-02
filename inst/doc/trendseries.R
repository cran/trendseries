## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4.5,
  message = FALSE,
  warning = FALSE
)

## -----------------------------------------------------------------------------
library(trendseries)
library(dplyr)
library(ggplot2)

theme_series <- theme_minimal(paper = "#fefefe") +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    # Use colors
    palette.colour.discrete = c(
        "#2c3e50",
        "#e74c3c",
        "#f39c12",
        "#1abc9c",
        "#9b59b6"
    )
  )

## -----------------------------------------------------------------------------
head(electric)

ggplot(electric, aes(date, consumption)) +
  geom_line() +
  theme_series

## -----------------------------------------------------------------------------
elec_trend <- augment_trends(
  electric,
  value_col = "consumption",
  methods = "stl"
)

head(elec_trend)

## ----eval = FALSE-------------------------------------------------------------
# elec_trend <- augment_trends(
#   electric,
#   date_col = "date",
#   value_col = "consumption",
#   methods = "stl",
#   frequency = 12
# )

## -----------------------------------------------------------------------------
# Prepare data for plotting
plot_data <- elec_trend |>
  tidyr::pivot_longer(
    cols = -date,
    names_to = "series",
    values_to = "value"
  ) |>
  mutate(
    series = case_when(
      series == "consumption" ~ "Data (original)",
      series == "trend_stl" ~ "Trend (STL)"
    )
  )

# Create the plot
ggplot(plot_data, aes(x = date, y = value, color = series)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Residential Electricity Consumption",
    x = NULL,
    y = "Electric Consumption (GWh)",
    color = NULL
  ) +
  theme_series

## -----------------------------------------------------------------------------
ggplot(elec_trend, aes(x = date)) +
  geom_line(
    aes(y = consumption),
    linewidth = 0.8,
    alpha = 0.5,
    color = "#024873FF") +
  geom_line(
    aes(y = trend_stl),
    linewidth = 1,
    color = "#024873FF") +
  labs(
    title = "Residential Electricity Consumption",
    subtitle = "Decomposition using an STL trend",
    x = NULL,
    y = "Electric Consumption (GWh)",
    color = NULL
  ) +
  theme_series

## -----------------------------------------------------------------------------
cities <- c("Houston", "San Antonio", "Dallas", "Austin")

txtrend <- txhousing |>
  filter(city %in% cities, year >= 2010) |>
  mutate(date = lubridate::make_date(year, month, 1)) |>
  augment_trends(
    value_col = "median",
    group_cols = "city"
  )

ggplot(txtrend, aes(date)) +
  geom_line(aes(y = median), alpha = 0.5, color = "#024873FF") +
  geom_line(aes(y = trend_stl), color = "#024873FF") +
  facet_wrap(vars(city)) +
  theme_series

## -----------------------------------------------------------------------------
ggplot(retail_autofuel, aes(date, value)) +
  geom_line(lwd = 0.8, color = "#024873FF") +
  theme_series

## ----compare-methods----------------------------------------------------------
fuel_trends <- retail_autofuel |>
  filter(date >= as.Date("2012-01-01")) |>
  augment_trends(
    methods = c("stl", "hp", "loess")
  )

comparison_plot <- fuel_trends |>
  tidyr::pivot_longer(
    cols = c(value, starts_with("trend_")),
    names_to = "method",
  ) |>
  mutate(
    method = case_when(
      method == "value" ~ "Data (original)",
      method == "trend_hp" ~ "HP Filter",
      method == "trend_stl" ~ "STL",
      method == "trend_loess" ~ "LOESS"
    )
  )

ggplot(comparison_plot, aes(x = date, y = value, color = method)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Comparing Different Trend Extraction Methods",
    subtitle = "Same data, different methods",
    x = "Date",
    y = "Retail Sales Index",
    color = "Method"
  ) +
  theme_series

## -----------------------------------------------------------------------------
elec_trends <- electric |>
  rename(value = consumption) |>
  # window controls the s.window argument by default
  augment_trends(methods = "stl", window = 17) |>
  # Creates a 11-month moving median
  augment_trends(methods = "median", window = 11) |>
  # Creates a (centered) 5-month moving average
  augment_trends(methods = "ma", window = 5) |>
  # Creates a (centered) 2x12 moving average
  augment_trends(methods = "ma", window = 12)

## ----echo = FALSE-------------------------------------------------------------
comparison_plot <- elec_trends |>
  tidyr::pivot_longer(
    cols = c(value, starts_with("trend_")),
    names_to = "method",
  ) |>
  mutate(
    method = case_when(
      method == "value" ~ "Data (original)",
      method == "trend_median" ~ "Median",
      method == "trend_stl" ~ "STL",
      method == "trend_ma" ~ "MA (5)",
      method == "trend_ma_1" ~ "MA (2x12)"
    )
  ) |>
  filter(date >= as.Date("2018-01-01"))

ggplot(comparison_plot, aes(x = date, y = value, color = method)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Comparing Different Trend Extraction Methods",
    subtitle = "Same data, different methods",
    x = "Date",
    y = "Retail Sales Index",
    color = "Method"
  ) +
  theme_series

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

