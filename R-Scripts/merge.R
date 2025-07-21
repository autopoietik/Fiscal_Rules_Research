# MERGE SCRIPT---------------------------------------
# 📦 Cargar librerías necesarias
library(dplyr)
library(readr)

# Base principal: WEO_Clean

# Merge W/ Investment Clean

# 📂 Leer bases limpias desde data/clean/
weo_panel <- read_rds("data/clean/weo_panel.rds")
investment_clean <- read_rds("data/clean/investment_clean.rds")

# 🔗 Merge por ISO y year
merged_panel <- weo_panel %>%
  left_join(investment_clean, by = c("ISO", "year", "country"))

# Limpiar filas no deseadas
# 🧼 Eliminar filas no válidas en ISO
merged_panel <- merged_panel %>%
  filter(!is.na(ISO),
         !grepl("^International Monetary Fund", ISO))


# 💾 Guardar base combinada
write_rds(merged_panel, "data/clean/merged_panel.rds")

# Merge W/ Kaopen------------------

# Cargar bases limpias
merged_panel <- readRDS("data/clean/merged_panel.rds")
kaopen_clean <- readRDS("data/clean/kaopen_clean.rds")

# Realizar el merge por ISO y year
merged_panel <- merged_panel %>%
  left_join(kaopen_clean, by = c("ISO", "year"))

merged_panel$cn <- NULL
merged_panel$country_name <- NULL

# Guardar nueva base con KAOPEN
saveRDS(merged_panel, file = "data/clean/merged_panel.rds")

# Merge W/ Credit Clean
merged_panel <- read_rds("data/clean/merged_panel.rds")
credit_clean <- read_rds("data/clean/credit_clean.rds")

merged_panel <- merged_panel %>%
  left_join(credit_clean %>% select(ISO, year, credit_private_gdp),
            by = c("ISO", "year"))

write_rds(merged_panel, "data/clean/merged_panel.rds")

# Merge W/ WGI
merged_panel <- readRDS("data/clean/merged_panel.rds")
wgi_clean <- readRDS("data/clean/wgi_clean.rds")

# Hacer el merge por ISO y year
merged_panel <- merged_panel %>%
  left_join(wgi_clean, by = c("ISO", "year"))

merged_panel <- merged_panel %>%
  rename(country = country.x) %>%
  select(-country.y)


# Guardar versión actualizada del panel
saveRDS(merged_panel, "data/clean/merged_panel.rds")

# merge W/ Fiscal Rules (dummy data)
fiscal_rules<- read_dta("data/raw/dummy_data_fiscal_rules1.dta")
merged_panel <- readRDS("data/clean/merged_panel.rds")

# Renombrar las columnas
fiscal_rules <- fiscal_rules %>%
  rename(n_country = Country, country = Country_name, year = Year)

merged_panel <- merged_panel %>%
  left_join(fiscal_rules, by = c("country", "year"))


# Verificar paises
# Ver todos los países únicos
unique_countries2 <- fiscal_rules %>%
  distinct(country) %>%
  arrange(country)

print(unique_countries)

# Ver todos los países únicos
unique_countries <- merged_panel %>%
  distinct(country) %>%
  arrange(country)

print(unique_countries)

library(dplyr)

# Filtrar por año
merged_panel <- merged_panel %>%
  filter(year >= 1985)

# Vector de países con nombre NA

paises_n_country_na_todo_t <- merged_panel %>%
  group_by(country) %>%
  summarise(todos_na = all(is.na(n_country)), .groups = "drop") %>%
  filter(todos_na) %>%
  pull(country)


# Paso 3: Eliminar las filas de esos países
merged_panel <- merged_panel %>%
  filter(!country %in% paises_n_country_na_todo_t)


saveRDS(merged_panel, "data/clean/merged_panel.rds")
merged_panel <- readRDS("data/clean/merged_panel.rds")
# Exportar el data frame a un archivo CSV
write.csv(merged_panel, "merged_panel.csv", row.names = FALSE)

merged_panel$DESC <- NULL
write.dta(merged_panel, "merged_panel.dta")
