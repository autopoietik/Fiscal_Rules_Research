# 00_setup.R -------------------------------------------------------------
# Carga de paquetes esenciales
library(tidyverse)    # dplyr, tidyr, ggplot2, etc.
library(readxl)       # leer .xlsx
library(readr)        # leer .csv /.txt
library(haven)        # leer .dta /.sav
library(janitor)      # limpiar nombres de variables
library(countrycode)  # estandarizar códigos de país
library(lubridate)    # manejo cómodo de fechas
library(plm)          # utilidades panel (más adelante)

# Opciones globales
options(
  scipen = 999,         # evita notación científica
  dplyr.summarise.inform = FALSE
)
# Función auxiliar para mantener estilo tidyverse en merges repetidos
safe_left_join <- function(x, y, by) {
  dplyr::left_join(x, y, by = by, keep = FALSE)
}
