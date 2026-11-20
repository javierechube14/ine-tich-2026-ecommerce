# ==============================================================================
# FASE 1 Y 2 (DEFINITIVAS): DEPURACIÓN OFICIAL, MODELOS A/B Y BARRERAS (TIC-H 2026)
# ==============================================================================

library(tidyverse)
library(survey)

# 1. Importar microdatos como texto para preservar códigos originales
cuest <- read_delim(
  "data-raw/CSV/TICHcuestionario_2026.tab",
  delim = "\t",
  col_types = cols(.default = col_character()),
  show_col_types = FALSE,
  trim_ws = TRUE
)

# 2. Filtrar población 16-74 años y verificar flujo exacto con ULT_INT
datos <- cuest %>%
  mutate(
    EDAD_NUM = as.numeric(EDAD),
    FACTOR_P_NUM = as.numeric(gsub(",", ".", FACTOR_P))
  ) %>%
  filter(!is.na(EDAD_NUM) & EDAD_NUM >= 16 & EDAD_NUM <= 74)

cat("=== 01. VERIFICACIÓN DE FLUJO: ULT_INT vs ULT_COM ===\n")
print(table(ULT_INT = datos$ULT_INT, ULT_COM = datos$ULT_COM, useNA = "ifany"))

# 3. Construcción limpia de variables según diccionarios oficiales (Tablas1 y Tablas3)
datos <- datos %>%
  mutate(
    # Variable dependiente: Compra online en los últimos 3 meses (1 = Sí, 0 = No)
    COMPRA_3M = case_when(
      ULT_COM == "1" ~ 1L,
      ULT_COM %in% c("2", "3", "4") ~ 0L,
      is.na(ULT_COM) & ULT_INT == "4" ~ 0L,
      TRUE ~ NA_integer_
    ),
    # Universo Modelo B: Usuarios de Internet en los últimos 3 meses (ULT_INT == "1")
    USUARIO_INT_3M = if_else(ULT_INT == "1", 1L, 0L),
    
    # Variables sociodemográficas
    SEXO_F = factor(SEXO, levels = c("1", "6"), labels = c("Hombre", "Mujer")),
    TRAB_F = factor(TRAB, levels = c("1", "6"), labels = c("Ocupado/a", "No ocupado/a")),
    GRUPO_EDAD = cut(
      EDAD_NUM,
      breaks = c(15, 24, 34, 44, 54, 64, 74),
      labels = c("16-24", "25-34", "35-44", "45-54", "55-64", "65-74")
    ),
    ESTUDIOS_F = case_when(
      NIVELEST %in% c("0", "1", "2") ~ "1. Primarios o 1ª etapa ESO",
      NIVELEST %in% c("3", "4") ~ "2. Bachillerato / FP Medio",
      NIVELEST == "5" ~ "3. FP Grado Superior",
      NIVELEST %in% c("6", "7", "8") ~ "4. Universitarios (Grado/Máster/Doc)",
      NIVELEST == "9" ~ "5. No codificable",
      TRUE ~ NA_character_
    ) %>% factor(),
    
    ING_HOG_F = factor(
      ING_HOG,
      levels = c("1", "2", "3", "4", "5", "6"),
      labels = c("Q1 (Más bajo)", "Q2", "Q3", "Q4", "Q5 (Más alto)", "NS/NR")
    ),
    
    HABITAT_F = case_when(
      HABITAT_ESTRATO == "0" ~ "1. >= 500.000 hab.",
      HABITAT_ESTRATO %in% c("1", "2") ~ "2. 100.000 - 499.999 hab.",
      HABITAT_ESTRATO %in% c("3", "4") ~ "3. 20.000 - 99.999 hab.",
      HABITAT_ESTRATO %in% c("5", "6") ~ "4. < 20.000 hab.",
      TRUE ~ NA_character_
    ) %>% factor(),
    
    # Frecuencia de uso de Internet (T_INTFR)
    FREC_INT_F = case_when(
      is.na(FREC_INT) ~ "0. No usó Internet (3m)",
      FREC_INT == "1" ~ "1. Varias veces al día",
      FREC_INT == "2" ~ "2. Diariamente (no varias veces)",
      FREC_INT == "3" ~ "3. Semanalmente",
      FREC_INT == "4" ~ "4. Menos de una vez/semana",
      TRUE ~ NA_character_
    ) %>% factor(),
    
    # Bloque Privacidad y Competencias (solo universo USUARIO_INT_3M == 1)
    PREOPUB_F = factor(
      PREOPUB,
      levels = c("6", "2", "1"),
      labels = c("Nada preocupado", "Algo preocupado", "Muy preocupado")
    ),
    across(
      starts_with("GESINT"),
      ~ case_when(.x == "1" ~ 1L, .x == "6" ~ 0L, TRUE ~ NA_integer_),
      .names = "{.col}_NUM"
    ),
    INDICE_PRIVACIDAD = GESINT1_NUM + GESINT2_NUM + GESINT3_NUM + GESINT4_NUM + GESINT5_NUM + GESINT6_NUM,
    USO_IAG_F = factor(IAG, levels = c("6", "1"), labels = c("No usa IA Gen", "Usa IA Gen"))
  )

cat("\nNAs en COMPRA_3M tras verificar ULT_INT:", sum(is.na(datos$COMPRA_3M)), "\n")
cat("Registros Universo A (16-74):", nrow(datos), "| Universo B (Usuarios 3m):", sum(datos$USUARIO_INT_3M == 1), "\n")

# Guardar base limpia definitiva
dir.create("data-processed", showWarnings = FALSE)
saveRDS(datos, "data-processed/tich2026_definitivo.rds")

# 4. Diseños muestrales ponderados
diseno_A <- svydesign(ids = ~1, weights = ~FACTOR_P_NUM, data = datos)
diseno_B <- subset(diseno_A, USUARIO_INT_3M == 1)

# 5. Descriptivos bivariados ponderados clave
cat("\n=== 02. COMPRA ONLINE 3M POR QUINTIL DE INGRESOS (UNIVERSO A) ===\n")
print(svyby(~COMPRA_3M, ~ING_HOG_F, diseno_A, svymean) %>% mutate(pct = round(COMPRA_3M * 100, 2)))

cat("\n=== 03. COMPRA ONLINE 3M POR ESTUDIOS (UNIVERSO A) ===\n")
print(svyby(~COMPRA_3M, ~ESTUDIOS_F, diseno_A, svymean) %>% mutate(pct = round(COMPRA_3M * 100, 2)))

cat("\n=== 04. PREVALENCIA PONDERADA DE ACCIONES DE PRIVACIDAD (GESINT1-6, UNIVERSO B) ===\n")
prev_gesint <- svymean(~GESINT1_NUM + GESINT2_NUM + GESINT3_NUM + GESINT4_NUM + GESINT5_NUM + GESINT6_NUM, diseno_B) * 100
print(round(prev_gesint, 2))

cat("\n=== 05. COMPRA ONLINE 3M SEGÚN PREOPUB E IA GENERATIVA (UNIVERSO B) ===\n")
print(svyby(~COMPRA_3M, ~PREOPUB_F, diseno_B, svymean) %>% mutate(pct = round(COMPRA_3M * 100, 2)))
print(svyby(~COMPRA_3M, ~USO_IAG_F, diseno_B, svymean) %>% mutate(pct = round(COMPRA_3M * 100, 2)))

# 6. Estimación de Modelos Logísticos Ponderados (Odds Ratios)
mod_A <- svyglm(
  COMPRA_3M ~ GRUPO_EDAD + SEXO_F + ESTUDIOS_F + ING_HOG_F + TRAB_F + HABITAT_F + FREC_INT_F,
  design = diseno_A,
  family = quasibinomial()
)

mod_B <- svyglm(
  COMPRA_3M ~ GRUPO_EDAD + SEXO_F + ESTUDIOS_F + ING_HOG_F + TRAB_F + HABITAT_F + 
    FREC_INT_F + PREOPUB_F + INDICE_PRIVACIDAD + USO_IAG_F,
  design = diseno_B,
  family = quasibinomial()
)

cat("\n=== 06. MODELO A (POBLACIÓN 16-74, N=14.105) — ODDS RATIOS ===\n")
res_A <- cbind(OR = exp(coef(mod_A)), exp(confint(mod_A)), p_val = summary(mod_A)$coefficients[, 4])
print(round(res_A, 3))

cat("\n=== 07. MODELO B (USUARIOS INTERNET 3M, N=13.622) — ODDS RATIOS ===\n")
res_B <- cbind(OR = exp(coef(mod_B)), exp(confint(mod_B)), p_val = summary(mod_B)$coefficients[, 4])
print(round(res_B, 3))

# 7. Módulo complementario: Motivos de no uso de Internet (NOUSOINT_1 a 7)
datos_nousu <- datos %>%
  filter(USUARIO_INT_3M == 0) %>%
  mutate(across(starts_with("NOUSOINT_") & !matches("OTRAPERS"), ~ case_when(.x == "1" ~ 1L, .x == "6" ~ 0L, TRUE ~ NA_integer_)))

diseno_nousu <- svydesign(ids = ~1, weights = ~FACTOR_P_NUM, data = datos_nousu)
cat("\n=== 08. MÓDULO COMPLEMENTARIO: MOTIVOS NO USO INTERNET (N=483) ===\n")
print(round(svymean(~NOUSOINT_1 + NOUSOINT_2 + NOUSOINT_3 + NOUSOINT_4 + NOUSOINT_5 + NOUSOINT_6 + NOUSOINT_7, diseno_nousu, na.rm = TRUE) * 100, 2))