# Clean IMF Investment
# 📦 Paquete necesario
library(readxl)
library(dplyr)

# 📁 Importar hoja "Dataset"
investment_raw <- read_excel("IMFInvestmentandCapitalStockDataset2021.xlsx", sheet = "Dataset")


# Explor-----

# 👁️ Ver estructura general
str(investment_raw)

# 🧪 Ver primeras filas
head(investment_raw, 10)

# 📊 Ver nombres de columnas
names(investment_raw)

# 📅 Ver años disponibles si están en columnas
names(investment_raw)[grepl("^\\d{4}$", names(investment_raw))]

# 🔍 Ver países disponibles
unique(investment_raw$Country)

# 🧮 Revisar valores únicos de la variable que nos interesa
summary(investment_raw$igov_rppp)

# 1 Limpieza-----
# 1.1 
investment_clean <- investment_raw %>%
select(ISO = isocode, country, year, igov_rppp)

#1.2
glimpse(investment_clean)
summary(investment_clean$igov_rppp)

#1.3
# Como RDS
saveRDS(investment_clean, "data/clean/investment_clean.rds")

# AQUI SE UTILIZA la base exchange_factors para igov_mn !
###
###
# 🔁 Cargar factores de conversión (de script PPP_EX.R)

# exchange_factors <- read_rds("exchange_factors.rds")

# 🧬 Hacer merge para traer fx y ppp_to_fx
investment_clean <- investment_clean %>%
  left_join(exchange_factors, by = c("ISO", "year"))

# 💰 Calcular inversión pública en moneda nacional
investment_clean <- investment_clean %>%
  mutate(
    igov_mn = if_else(
      !is.na(igov_rppp) & !is.na(fx) & !is.na(ppp_to_fx),
      igov_rppp * fx / ppp_to_fx,
      NA_real_
    )
  )

# 🧼 Verifica resultado (opcional)
summary(investment_clean$igov_mn)

# 💾 Guardar base actualizada
write_rds(investment_clean, "investment_clean.rds")


