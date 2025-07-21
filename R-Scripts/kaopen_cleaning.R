# KAOPEN CLEANING
library(readxl)
library(dplyr)

# Importación desde la hoja activa (única hoja esperada)
kaopen_raw <- read_excel("data/raw/kaopen_2022.xlsx")

# Exploración preliminar
glimpse(kaopen_raw)

kaopen_clean <- kaopen_raw %>%
  rename(ISO = ccode)

saveRDS(kaopen_clean, file = "data/clean/kaopen_clean.rds")
