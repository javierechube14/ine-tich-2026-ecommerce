# ==============================================================================
# FASE 0 (DEFINITIVA): AUDITORÍA INTEGRAL Y VALIDACIÓN - INE TIC-H 2026
# ==============================================================================

library(tidyverse)
library(readxl)
library(janitor)
library(survey)

# 1. Importar microdatos como texto para evitar parsing issues y conservar ceros
ruta_cuest <- "data-raw/CSV/TICHcuestionario_2026.tab"
cuest <- read_delim(
  ruta_cuest,
  delim = "\t",
  col_types = cols(.default = col_character()),
  show_col_types = FALSE,
  trim_ws = TRUE
)

cat("=== 01. PROBLEMAS DE IMPORTACIÓN Y ESTRUCTURA ===\n")
cat("Problemas de lectura detectados:", nrow(problems(cuest)), "\n")
cat("Registros totales:", nrow(cuest), "| Columnas:", ncol(cuest), "\n")

# 2. Filtrar población objetivo oficial (16 a 74 años) y convertir FACTOR_P
cuest_16_74 <- cuest %>%
  mutate(
    EDAD_NUM = as.numeric(EDAD),
    FACTOR_P_NUM = as.numeric(gsub(",", ".", FACTOR_P))
  ) %>%
  filter(!is.na(EDAD_NUM) & EDAD_NUM >= 16 & EDAD_NUM <= 74)

cat("Registros 16-74 años:", nrow(cuest_16_74), 
    "| Población estimada:", round(sum(cuest_16_74$FACTOR_P_NUM)), "\n")

# 3. Auditoría de saltos estructurales: ULT_COM vs FREC_INT y patrones de ausencia
cat("\n=== 02. TABLA CRUZADA ULT_COM vs FREC_INT (16-74 AÑOS) ===\n")
print(table(ULT_COM = cuest_16_74$ULT_COM, FREC_INT = cuest_16_74$FREC_INT, useNA = "ifany"))

cat("\n=== 03. PATRONES DE AUSENCIA EN BLOQUES DE FLUJO ===\n")
vars_flujo <- c("ULT_COM", "FREC_INT", "PREOPUB", "GESINT1", "IAG", "IAG_PRIV", "IAG_NO4")
print(cuest_16_74 %>% summarise(across(all_of(vars_flujo), ~ sum(is.na(.x)))))

# 4. Auditoría de variables socioeconómicas reales (ING_HOG, TRAB, NIVELEST, SEXO)
cat("\n=== 04. VARIABLES SOCIOECONÓMICAS (16-74 AÑOS) ===\n")
vars_socio <- c("ING_HOG", "TRAB", "NIVELEST", "SEXO", "HABITAT_ESTRATO")
for (v in vars_socio) {
  cat("\nVariable:", v, "| NAs:", sum(is.na(cuest_16_74[[v]])), "\n")
  print(table(cuest_16_74[[v]], useNA = "ifany"))
}

# 5. Cálculo de prevalencia ponderada con denominador total 16-74 años
cuest_16_74 <- cuest_16_74 %>%
  mutate(
    COMPRA_3M = case_when(
      ULT_COM == "1" ~ 1L,
      ULT_COM %in% c("2", "3", "4") ~ 0L,
      is.na(ULT_COM) & is.na(FREC_INT) ~ 0L,
      TRUE ~ NA_integer_
    )
  )

cat("\nNAs residuales en COMPRA_3M:", sum(is.na(cuest_16_74$COMPRA_3M)), "\n")

diseno_16_74 <- svydesign(ids = ~1, weights = ~FACTOR_P_NUM, data = cuest_16_74)
prev_oficial <- svymean(~COMPRA_3M, diseno_16_74, na.rm = TRUE) * 100
cat("\n=== 05. VALIDACIÓN EXTERNA: TASA OFICIAL COMPRA ONLINE 3M (16-74 AÑOS) ===\n")
print(prev_oficial)

# 6. Extraer etiquetas exactas del Excel DR para las variables de ambos modelos
ruta_dr <- "data-raw/dr_TICHcuestionario_2026.xlsx"
dr_vars <- read_excel(ruta_dr, sheet = 1) %>% clean_names()
col_var <- names(dr_vars)[1]

vars_diccionario <- c(
  "ULT_COM", "FREC_INT", "PREOPUB", paste0("GESINT", 1:6),
  "SEXO", "EDAD", "NIVELEST", "ING_HOG", "TRAB", "HABITAT_ESTRATO",
  "IAG", "IAG_NO4", "FACTOR_P"
)

cat("\n=== 06. ETIQUETAS OFICIALES DEL DISEÑO DE REGISTRO ===\n")
print(dr_vars %>% filter(toupper(.data[[col_var]]) %in% vars_diccionario), n = 30, width = Inf)