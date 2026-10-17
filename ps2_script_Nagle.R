library(tidyquant)
library(tidyverse)

# unemp_raw <- tq_get(
#   c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
#   get  = "economic.data",
#   from = "1976-01-01"
# )
# 
# head(unemp_raw)
# 
# dir.create("data", showWarnings = FALSE)
# 
# write_rds(unemp_raw, "data/unemp_raw.rds")

unemp_raw = read_rds("data/unemp_raw.rds")

# Part 4.1: clean up
unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(region = case_when(
    symbol == "UNRATE" ~ "United States",
    symbol == "ORUR"   ~ "Oregon",
    symbol == "CAUR"   ~ "California",
    symbol == "TXUR"   ~ "Texas",
    symbol == "NYUR"   ~ "New York"
  ))

# Part 4.2: fill missing Oct 2025 value
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

# Part 4.2: month-over-month change
unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(unemployment_rate_change = unemployment_rate - lag(unemployment_rate)) |>
  ungroup()

# Part 4.2: recent data and summaries
unemp_recent <- unemp |> filter(date >= as.Date("2015-01-01"))

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

# Part 4.3: biggest monthly increases/decreases
unemp_top_increases <- unemp |> slice_max(unemployment_rate_change, n = 5, na_rm = TRUE)
unemp_top_decreases <- unemp |> slice_min(unemployment_rate_change, n = 5, na_rm = TRUE)

# Part 4.4: same, excluding 2020
unemp_no_covid <- unemp |> filter(year(date) != 2020)

unemp_top_increases_no_covid <- unemp_no_covid |> slice_max(unemployment_rate_change, n = 5, na_rm = TRUE)
unemp_top_decreases_no_covid <- unemp_no_covid |> slice_min(unemployment_rate_change, n = 5, na_rm = TRUE)

unemp_summary
unemp_top_increases_no_covid

# Part 5.1: all regions on one plot
ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  labs(title = "Monthly Unemployment Rate, 1976–Present",
       x = "Date", y = "Unemployment rate (%)", color = "Region") +
  theme_minimal()

# Part 5.2: one panel per region
ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  facet_wrap(~ region) +
  labs(title = "Monthly Unemployment Rate by Region, 1976–Present",
       x = "Date", y = "Unemployment rate (%)") +
  theme_minimal() +
  theme(legend.position = "none")

# Part 5.3: average rate by region
ggplot(unemp_summary, aes(x = fct_reorder(region, mean_rate), y = mean_rate)) +
  geom_col() +
  labs(title = "Average Unemployment Rate by Region, 1976–Present",
       x = "Region", y = "Mean unemployment rate (%)") +
  theme_minimal()

# Part 5.4: Oregon vs. U.S. gap
or_us_gap <- unemp |>
  filter(region %in% c("Oregon", "United States")) |>
  select(date, region, unemployment_rate) |>
  pivot_wider(names_from = region, values_from = unemployment_rate) |>
  mutate(gap = Oregon - `United States`)

ggplot(or_us_gap, aes(x = date, y = gap)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Oregon Unemployment Rate Minus U.S. Rate",
       x = "Date", y = "Percentage points") +
  theme_minimal()