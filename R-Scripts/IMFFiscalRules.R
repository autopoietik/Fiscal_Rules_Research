# Cargar librerías necesarias
library(readxl)
library(dplyr)
library(janitor)
# Paso 1: Importar desde fila 4 con nombres reales
fiscal_rules_raw <- read_excel(
  path = "data/raw/IMF Fiscal Rules Dataset 1985 - 2021 - January 2022 rev Dec 21.xlsx",
  sheet = "Rules",
  skip = 1
)

# Verificar estructura inicial
glimpse(fiscal_rules_raw)

# Paso 1: Remover primeras 2 filas no informativas
fiscal_rules_step1 <- fiscal_rules_raw %>%
  filter(!is.na(year)) %>%
  clean_names()

# Chequeo inicial post limpieza
glimpse(fiscal_rules_step1)


#----------------------------
library(haven)
library(dplyr)

# Importar las bases
cleaning_data <- read_dta("data/raw/cleaning_data_fiscal_rules.dta")
dummy_data <- read_dta("data/raw/dummy_data_fiscal_rules1.dta")

# Glimpse para ver estructura
glimpse(cleaning_data)
glimpse(dummy_data)

