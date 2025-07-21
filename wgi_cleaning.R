# WGI Cleaning
# 📦 Cargar librerías necesarias
library(readxl)
library(dplyr)

# 📥 Importar la hoja principal del archivo WGI
wgi_raw <- read_excel("data/raw/wgidataset_with_sourcedata.xlsx")

# 👁️ Visualizar estructura de los datos
glimpse(wgi_raw)

#Limpieza 

#1.1
wgi_step1 <- wgi_raw %>%
  filter(indicator %in% c("cc", "ge", "rl", "rq", "va", "pv")) %>%
  select(ISO = code, country = countryname, year, indicator, estimate, stddev, pctrank)

glimpse(wgi_step1)

#1.2
wgi_step2 <- wgi_step1 %>%
  mutate(
    estimate = na_if(estimate, "..") %>% as.numeric(),
    stddev   = na_if(stddev, "..") %>% as.numeric(),
    pctrank  = na_if(pctrank, "..") %>% as.numeric()
  )

glimpse(wgi_step2)

#1.3
wgi_clean <- wgi_step2 %>%
  pivot_wider(
    names_from = indicator,
    values_from = c(estimate, stddev, pctrank),
    names_glue = "wgi_{.value}_{indicator}"
  )
#1.4
write_rds(wgi_clean, "data/clean/wgi_clean.rds")

