
#--------------------------intro-----------------
merged_panel <- readRDS("data/clean/merged_panel.rds")

library(VIM)
library(mice)
library(naniar)
library(Hmisc)
library(corrplot)
library(pheatmap)
library(plotly)
library(DT)
library(kableExtra)
library(gridExtra)
library(viridis)
library(RColorBrewer)
library(reshape2)
library(tidyverse)    # Para manejo de datos y piping
library(readr)        # Para lectura rápida de CSV
library(lubridate)    # Para manejo de fechas si se requiere
library(janitor)   

# Fase 1.1: Resumen general de valores faltantes. -----

# Total de observaciones
total_obs <- nrow(merged_panel)

# 1. Tabla: Conteo y porcentaje de NA por variable en orden descendente
na_summary <- merged_panel |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(cols = everything(), names_to = "variable", values_to = "n_missing") |>
  mutate(
    pct_missing = round((n_missing / total_obs) * 100, 2)
  ) |>
  arrange(desc(pct_missing))

# 2. Mostrar primeras variables más afectadas
na_summary |> 
  head(20) |>
  kableExtra::kable(format = "html", caption = "Top 20 variables con más valores faltantes") |>
  kableExtra::kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

# 3. Filas completas vs incompletas
row_summary <- merged_panel |>
  mutate(row_has_na = if_any(everything(), is.na)) |>
  count(row_has_na, name = "n_rows") |>
  mutate(pct = round(100 * n_rows / sum(n_rows), 2)) #Toda fila tiene al menos un NA!

# Fase 1.2 Distribución temporal y país-----
# Detectar nombres de variables numéricas
numeric_vars <- merged_panel |>
  select(where(is.numeric)) |>
  names()

# Pivot solo las columnas numéricas
na_by_year <- merged_panel |>
  select(year, all_of(numeric_vars)) |>
  pivot_longer(cols = -year, names_to = "variable", values_to = "value") |>
  group_by(year) |>
  summarise(
    pct_missing = mean(is.na(value)) * 100
  )

# Gráfico corregido
ggplot(na_by_year, aes(x = year, y = pct_missing)) +
  geom_line(color = "#2c7fb8", linewidth = 1) +
  geom_point(color = "#2c7fb8") +
  labs(
    title = "Porcentaje de valores faltantes por año (solo variables numéricas)",
    x = "Año",
    y = "% de NA"
  ) +
  theme_minimal()
###
###
# Calcular % de NA por país

na_by_country <- merged_panel |>
  select(country, all_of(numeric_vars)) |>
  pivot_longer(cols = -country, names_to = "variable", values_to = "value") |>
  group_by(country) |>
  summarise(
    pct_missing = mean(is.na(value)) * 100
  ) |> arrange(desc(pct_missing))

# Gráfico de NA por país (top 10 con más NA)
na_by_country |>
  slice_max(pct_missing, n = 10) |>
  ggplot(aes(x = reorder(country, pct_missing), y = pct_missing)) +
  geom_col(fill = "#d7301f") +
  coord_flip() +
  labs(
    title = "Top 10 países con mayor porcentaje de valores faltantes",
    x = "País",
    y = "% de NA"
  ) +
  theme_minimal()

###
# 1. Detectar variables numéricas (excluyendo identificadores)
numeric_vars <- merged_panel |>
  select(where(is.numeric)) |>
  select(-year) |>  # excluimos year explícitamente
  names()

# 2. Calcular porcentaje de NA por fila país-año (solo numéricas)
na_matrix <- merged_panel |>
  mutate(
    row_na_pct = rowMeans(is.na(across(all_of(numeric_vars)))) * 100
  ) |>
  select(ISO, year, row_na_pct)

# 3. Graficar el mapa de calor país-año
ggplot(na_matrix, aes(x = year, y = reorder(ISO, -row_na_pct), fill = row_na_pct)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "C", name = "% NA", direction = -1) +
  labs(
    title = "Mapa de calor: % de valores faltantes por país y año",
    x = "Año",
    y = "País (ISO)"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6),
    plot.title = element_text(face = "bold")
  )




###
# Fase 2
# Categorización de variables manual por patrones en nombres
categorias <- tibble(
  categoria = c(
    "actividad_economica", "finanzas", "institucional", 
    "reglas_fiscales", "otros"
  ),
  patron = list(
    c("gdp", "gov", "igov"),
    c("kaopen", "credit", "ka_open"),
    c("wgi_"),
    c("BBR", "DR", "RR", "ER", "NR", "SR", "YI", "MCOG", "FEPN", "FEPS", "ECS", "AEC", "SFR", "YMASC", "LBNR", "LBSR", "COV"),
    c("region", "TOE", "MYEC", "SOC", "IB", "FRL", "DESC", "IERL")
  )
)

# Función que asigna categoría en base a patrón
categorizar_variable <- function(var, categorias_df) {
  for (i in seq_len(nrow(categorias_df))) {
    if (any(str_detect(var, categorias_df$patron[[i]]))) {
      return(categorias_df$categoria[i])
    }
  }
  return("sin_categoria")
}

# Crear tabla con % NA + categoría
na_categoria_summary <- merged_panel |>
  summarise(across(where(is.numeric), ~ mean(is.na(.)) * 100)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "pct_na") |>
  mutate(
    categoria = map_chr(variable, ~ categorizar_variable(.x, categorias))
  ) |>
  arrange(desc(pct_na))
##
# Boxplot de % NA por categoría
na_categoria_summary |>
  ggplot(aes(x = categoria, y = pct_na)) +
  geom_boxplot(fill = "skyblue") +
  geom_jitter(width = 0.2, alpha = 0.5, size = 1.5) +
  labs(
    title = "Distribución de % de NA por categoría temática",
    x = "Categoría",
    y = "% de valores faltantes"
  ) +
  theme_minimal()
##
##

# Fase 3: Estrategias de imputación de NAs
# LOCF ; mice; descartar variable.

# Subconjuntos por estrategia
variables_completas <- na_categoria_summary |> filter(calidad == "completa") |> pull(variable)
variables_parciales <- na_categoria_summary |> filter(calidad == "parcial") |> pull(variable)
variables_incompletas <- na_categoria_summary |> filter(calidad == "muy_incompleta") |> pull(variable)
# Construcción de tabla resumen final
tabla_resumen_final <- na_categoria_summary |>
  mutate(
    decision = "-"  # Placeholder para completar manualmente
  ) |>
  select(variable, categoria, pct_na, calidad, decision)

# Mostrar primeras filas como preview
tabla_resumen_final |>
  slice_head(n = 20) |>
  kableExtra::kable(format = "html", caption = "Resumen final de variables con valores faltantes") |>
  kableExtra::kable_styling(full_width = FALSE)

