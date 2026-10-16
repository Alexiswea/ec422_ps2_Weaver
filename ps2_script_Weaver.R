library(tidyquant)
library(tidyverse)

#unemp_raw <- tq_get(
#  c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
#  get  = "economic.data",
#  from = "1976-01-01"
#)

#head(unemp_raw)

#dir.create("data", showWarnings = FALSE)

#write_rds(unemp_raw, "data/unemp_raw.rds")

unemp_raw = read_rds("data/unemp_raw.rds")

unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(
    region = case_when(
      symbol == "UNRATE" ~ "United States",
      symbol == "ORUNRATE" ~ "Oregon",
      symbol == "CAREUNRATE" ~ "California",
      symbol == "TXUNRATE" ~ "Texas",
      symbol == "NYUNRATE" ~ "New York",
      TRUE ~ NA_character_
    )
  )

#4.2
unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate = replace(
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    )
  ) |>
  ungroup()

unemp |>
  filter(date >= as.Date("2025-08-01"), date <= as.Date("2025-11-01")) |>
  arrange(region, date)

unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(unemployment_rate_change = unemployment_rate - lag(unemployment_rate)) |>
  ungroup()

unemp_recent <- unemp |>
  filter(date >= as.Date("2015-01-01"))

unemp_summary <- unemp |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )

unemp_recent_summary <- unemp_recent |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )

unemp_summary
unemp_recent_summary

#4.3
unemp_top_increases <- unemp |>
  slice_max(unemployment_rate_change, n = 5)

unemp_top_decreases <- unemp |>
  slice_min(unemployment_rate_change, n = 5)

unemp_top_increases
unemp_top_decreases

#4.4
unemp_no_covid <- unemp |>
  filter(year(date) != 2020)

unemp_top_increases_no_covid <- unemp_no_covid |>
  slice_max(unemployment_rate_change, n = 5)

unemp_top_decreases_no_covid <- unemp_no_covid |>
  slice_min(unemployment_rate_change, n = 5)

unemp_top_increases_no_covid
unemp_top_decreases_no_covid

#5.1
ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  labs(
    title = "Monthly Unemployment Rates: U.S. and Selected States",
    x = "Date",
    y = "Unemployment Rate (%)",
    color = "Region"
  ) +
  theme_minimal()


#5.2
ggplot(unemp, aes(x = date, y = unemployment_rate)) +
  geom_line() +
  facet_wrap(~ region) +
  labs(
    title = "Monthly Unemployment Rates by Region",
    x = "Date",
    y = "Unemployment Rate (%)"
  ) +
  theme_minimal()

#5.3
ggplot(unemp_summary, aes(x = fct_reorder(region, mean_rate), y = mean_rate)) +
  geom_col() +
  labs(
    title = "Average Unemployment Rate by Region (1976 to Present)",
    x = "Region",
    y = "Mean Unemployment Rate (%)"
  ) +
  theme_minimal()

#5.4
oregon_gap <- unemp |>
  filter(symbol %in% c("ORUR", "UNRATE")) |>
  dplyr::select(date, symbol, unemployment_rate) |>
  pivot_wider(names_from = symbol, values_from = unemployment_rate) |>
  mutate(gap = ORUR - UNRATE)

ggplot(oregon_gap, aes(x = date, y = gap)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Oregon Unemployment Rate Minus U.S. Rate",
    x = "Date",
    y = "Percentage Point Difference"
  ) +
  theme_minimal()
#This plot shows the difference between Oregon’s unemployment rate and the U.S. unemployment rate over time, with a dashed line at zero marking when the two are equal. I chose it to see how Oregon’s labor market compares to the national one, and whether the gap changes during recessions.
