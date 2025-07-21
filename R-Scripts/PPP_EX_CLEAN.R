# PPP_EX (indicadores para transformación de igov_rppp a MN con el fin de
# derivar gov_expenditure_current)

library(readr)
library(dplyr)
library(tidyr)

# Importar desde fila 5
ppp_fx_raw <- read_csv("PPP_EX.csv")

# Ver estructura inicial
glimpse(ppp_fx_raw)
# Formato Long
ppp_fx_long <- ppp_fx_raw %>%
  pivot_longer(
    cols = matches("^\\d{4}"),  # columnas con años
    names_to = "year_raw",
    values_to = "value"
  ) %>%
  mutate(
    year = as.integer(str_extract(year_raw, "^\\d{4}")),
    value = na_if(value, ".."),
    value = as.numeric(value)
  ) %>%
  select(country = `Country Name`,
         ISO = `Country Code`,
         indicator = `Series Code`,
         year,
         value)

# Separar indicadores
# Ratio PPP a tipo de cambio
ppp_ratio <- ppp_fx_long %>%
  filter(indicator == "PA.NUS.PPPC.RF") %>%
  rename(ppp_to_fx = value) %>%
  select(ISO, year, ppp_to_fx)

# Tipo de cambio nominal
fx_rate <- ppp_fx_long %>%
  filter(indicator == "PA.NUS.FCRF") %>%
  rename(fx = value) %>%
  select(ISO, year, fx)

# combine
exchange_factors <- fx_rate %>%
  left_join(ppp_ratio, by = c("ISO", "year"))
saveRDS(exchange_factors, "data/clean/exchange_factors.rds")

