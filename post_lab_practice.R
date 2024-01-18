library(tidyverse)

co2 <- read_csv(here::here('data/co2_mm_mlo.csv'))

co2_date <- co2 %>%
  mutate(date = paste(month.name[as.numeric(month)], year, sep = ' ')) %>%
  select(date, co2_mean = average, co2_sd = sdev) %>%
  mutate(co2_sd = ifelse(co2_sd == -9.99, NA, co2_sd))

write_csv(co2_date, here('data/co2_mauna_loa.csv'))

co2_ts <- read_csv(here('data/co2_mauna_loa.csv')) %>%
  mutate(date = my(date)) %>%
  as_tsibble(index = date) %>%
  mutate(date = yearmonth(date))

### hint: lubridate::my() is not the same as tsibble::yearmonth()... use the
### first to convert text to date, use the second to convert date (or well-
### formatted text, which is not what we have!) to tsibble

### Consider additive vs multiplicative seasonality and trend...
co2_ets <- co2_ts %>%
  model(
    ets = ETS(co2_mean ~ season("M") + trend('M'))
  )

co2_forecast <- co2_ets %>% 
  forecast(h = "20 years")

# Plot the 3 forecasts
co2_forecast %>% 
  autoplot(co2_ts) 
