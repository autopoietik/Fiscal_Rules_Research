# ========================================
# 01_clean_weo.R
# Limpieza de datos IMF WEO

# 📦 Cargar paquetes necesarios
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

weo_raw <- read_excel("data/raw/WEO_Data.xlsx")

# Exploración -------
# Ver estructura general
str(weo_raw)
class(weo_raw)
# Ver las primeras filas
head(weo_raw, 10)

# Ver nombres de columnas
names(weo_raw)

# Dimensiones
dim(weo_raw)

# Revisar valores únicos en la columna de descriptor de variable
unique(weo_raw$`Subject Descriptor`)

# Revisar años (si están como columnas)
names(weo_raw)[grepl("^\\d{4}$", names(weo_raw))]  # Extrae nombres de columnas que parecen años

# Ver cuántos países hay
length(unique(weo_raw$Country))

# Revisar un resumen general por variable
table(weo_raw$`Subject Descriptor`)

# Limpieza ---------------
#1.1 Eliminar Columnas Irrelevantes
weo_trim <- weo_raw %>%
  select(ISO, Country, `Subject Descriptor`, `1980`:`2024`)

# 1.2 Transformar a formato Long (pivotar años)
weo_long <- weo_trim %>%
  pivot_longer(cols = `1980`:`2024`,
               names_to = "year",
               values_to = "value") %>%
  mutate(year = as.integer(year))

#1.3 Limpiar valores
weo_long <- weo_long %>%
  mutate(value = na_if(value, "n/a"),
         value = as.numeric(value))

#1.4 Pivotear variable a columna
weo_wide <- weo_long %>%
  pivot_wider(names_from = `Subject Descriptor`, values_from = value)

weo_wide$`NA` <- NULL

#1.5 Rename!
weo_panel <- weo_wide %>%
  rename(
    country = Country,
    gdp_real = `Gross domestic product, constant prices`,
    gdp_nominal = `Gross domestic product, current prices`,
    gdp_deflator = `Gross domestic product, deflator`,
    gov_expenditure = `General government total expenditure`
  )
# 1.6
saveRDS(weo_panel, file = "data/clean/weo_panel.rds")

