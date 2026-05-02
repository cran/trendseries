## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4.5,
  message = FALSE,
  warning = FALSE
)

## ----setup--------------------------------------------------------------------
library(trendseries)
library(dplyr)
library(ggplot2)
library(tidyr)

## -----------------------------------------------------------------------------
library(trendseries)
library(dplyr)
library(ggplot2)

## -----------------------------------------------------------------------------
theme_series <- theme_minimal(paper = "#fefefe") +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#2c3e50"),
    strip.text = element_text(color = "#fefefe"),
    axis.ticks.x = element_line(color = "gray40", linewidth = 0.5),
    axis.line.x = element_line(color = "gray40", linewidth = 0.5),
    # Use colors
    palette.colour.discrete = c(
      "#2c3e50",
      "#e74c3c",
      "#f39c12",
      "#1abc9c",
      "#9b59b6"
    )
  )

## ----vehicles-plot------------------------------------------------------------
# Using the 'vehicles' dataset (ships with trendseries)
vehicles_recent <- vehicles |>
  # Only use data after 2018
  filter(date >= as.Date("2018-01-01"))

ggplot(vehicles_recent, aes(date, production)) +
  geom_line(lwd = 0.7) +
  theme_series

## ----ma-basic-----------------------------------------------------------------
# Apply a moving average trend
vehicles_trend <- augment_trends(
  vehicles_recent,
  value_col = "production",
  methods = "ma",
  window = 12
)


vehicles_trend

## -----------------------------------------------------------------------------
ggplot(vehicles_trend, aes(date)) +
  geom_line(aes(y = production, color = "Original"), lwd = 0.6, alpha = 0.8) +
  geom_line(aes(y = trend_ma, color = "Trend: 12-month MA"), lwd = 0.8) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(x = NULL, y = NULL, title = "Vehicle Production: Simple Moving Average") +
  theme_series

## ----window-comparison--------------------------------------------------------
# Apply different window sizes
windows_to_test <- c(3, 6, 12, 24)

vehicles_trend <- vehicles_recent |>
  augment_trends(
    value_col = "production",
    methods = "ma",
    window = windows_to_test
  )

vehicles_trend

## -----------------------------------------------------------------------------
# Prepare for plotting
plot_data <- vehicles_trend |>
  pivot_longer(
    cols = c(production, starts_with("trend_ma")),
    names_to = "method",
    values_to = "value"
  ) |>
  mutate(
    method = factor(
      method,
      levels = c("production", paste0("trend_ma_", c(3, 6, 12, 24))),
      labels = c(
        "Original",
        "3-month MA",
        "6-month MA",
        "12-month MA",
        "24-month MA"
      )
    )
  )


# Plot
ggplot(plot_data, aes(date, value, color = method)) +
  geom_line() +
  labs(
    title = "Effect of Window Size on Moving Average",
    subtitle = "Larger windows = smoother trends, but slower to react",
    x = NULL,
    y = NULL,
    color = NULL
  ) +
  theme_series

## -----------------------------------------------------------------------------
vehicles_trend <- augment_trends(
  vehicles_recent,
  value_col = "production",
  methods = "ma",
  window = 12,
  align = "right"
)

ggplot(vehicles_trend, aes(date)) +
  geom_line(aes(y = production, color = "Original"), lwd = 0.6, alpha = 0.8) +
  geom_line(aes(y = trend_ma, color = "Trend: 12-month MA"), lwd = 0.8) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(x = NULL, y = NULL, title = "Vehicle Production: Simple Moving Average") +
  theme_series

## -----------------------------------------------------------------------------
transit <- transit_london_monthly

ggplot(transit, aes(date_month, journey_monthly, color = transit_mode)) +
  geom_line(lwd = 0.7) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_comma(scale = 1e-6)) +
  labs(
    x = NULL,
    y = "Journeys (million)",
    title = "Transit ridership in London",
    subtitle = "Monthly journey counts averaged across London's transit systems",
    color = NULL
  ) +
  theme_series

## -----------------------------------------------------------------------------
transit_trends <- augment_trends(
  transit,
  date_col = "date_month",
  value_col = "journey_monthly",
  group_cols = "transit_mode",
  methods = "ma",
  window = 12
)

ggplot(transit_trends, aes(date_month, color = transit_mode)) +
  geom_line(aes(y = journey_monthly), lwd = 0.7, alpha = 0.8) +
  geom_line(aes(y = trend_ma), lwd = 0.8) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_comma(scale = 1e-6)) +
  labs(
    x = NULL,
    y = "Journeys (million)",
    title = "Grouped series trends",
    subtitle = "Monthly journey counts averaged across London's transit systems",
    color = NULL
  ) +
  theme_series

## -----------------------------------------------------------------------------
transit_trends <- augment_trends(
  transit,
  date_col = "date_month",
  value_col = "journey_monthly",
  group_cols = "transit_mode",
  methods = c("ma", "median", "spencer")
)

## -----------------------------------------------------------------------------
transit_trends

## -----------------------------------------------------------------------------
transit_trends <- augment_trends(
  transit,
  date_col = "date_month",
  value_col = "journey_monthly",
  group_cols = "transit_mode",
  methods = c("ma", "median", "spencer")
)

transit_trends_long <- transit_trends |>
  pivot_longer(
    cols = c(starts_with("trend_")),
    names_to = "method",
    names_repair = "unique"
  ) |>
  mutate(
    method = factor(
      method,
      levels = c(
        "trend_ma",
        "trend_median",
        "trend_spencer"
      ),
      labels = c(
        "12-month MA",
        "5-month median",
        "15-term Spencer"
      )
    )
  )

ggplot() +
  geom_line(
    data = transit_trends,
    aes(date_month, journey_monthly),
    lwd = 0.5,
    alpha = 0.8
  ) +
  geom_line(
    data = transit_trends_long,
    aes(date_month, value, color = method),
    lwd = 0.8
  ) +
  facet_wrap(vars(transit_mode, method), ncol = 3) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_comma(scale = 1e-6)) +
  labs(
    x = NULL,
    y = "Journeys (million)",
    title = "Grouped series trends",
    subtitle = "Monthly journey counts averaged across London's transit systems",
    color = NULL
  ) +
  theme_series +
  theme(
    axis.text.x = element_text(angle = 90)
  )

