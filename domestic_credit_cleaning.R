# Domestic Credit to private sector %GPD ----
# Cargar librerías necesarias
library(readr)
library(dplyr)
library(tidyr)
# Importar con skip de 4 filas
credit_raw <- read_csv("data/raw/API_FS.AST.PRVT.GD.ZS_DS2_en_csv_v2_37872.csv", skip = 4)

# Explorar estructura de la base
glimpse(credit_raw)

#Limpiar 1.

credit_clean <- credit_raw %>%
  # Renombrar columna
  rename(ISO = `Country Code`) %>%
  # Seleccionar columnas necesarias
  select(ISO, `Country Name`, matches("^\\d{4}$")) %>%
  # Pivotar a largo
  pivot_longer(cols = matches("^\\d{4}$"),
               names_to = "year", 
               values_to = "credit_private_gdp") %>%
  # Convertir año a numérico
  mutate(
    year = as.integer(year),
    credit_private_gdp = as.numeric(credit_private_gdp)
  ) %>%
  # Eliminar valores NA o países agregados
  filter(!is.na(credit_private_gdp)) %>%
  filter(nchar(ISO) == 3)  # Mantener solo códigos ISO-3 válidos

# verificar 2
glimpse(credit_clean)

saveRDS(credit_clean, file = "data/clean/credit_clean.rds")

