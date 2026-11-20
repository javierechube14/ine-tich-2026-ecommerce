# ==============================================================================
# FASE 2 (DEFINITIVA): RECODIFICACIÓN OFICIAL, CONSISTENCIA Y MODELOS A / B1 / B2
# ==============================================================================

library(tidyverse)
library(readxl)
library(survey)

# 1. Verificación documental de etiquetas de NOUSOINT_1 a NOUSOINT_7
dr_diseno <- read_excel("data-raw/dr_TICHcuestionario_2026.xlsx", sheet = 1, col_names = FALSE)
filas_nous <- which(grepl("^NOUSOINT_", toupper(as.character(dr_diseno[[1]]))))
cat("=== 01. ETIQUETAS OFICIALES DE MOTIVOS DE NO USO DE INTERNET ===\n")
print(dr_diseno[filas_nous, c(1, 2, 9)], n = Inf, width = Inf)

# 2. Importar microdatos y construir variables depuradas según diccionarios oficiales
cuest <- read_delim(
  "data-raw/CSV/TICHcuestionario_2026.tab",
  delim = "\t",
  col_types = cols(.default = col_character()),
  show_col_types = FALSE,
  trim_ws = TRUE
)

datos <- cuest %>%
  mutate(
    EDAD_NUM = as.numeric(EDAD),
    FACTOR_P_NUM = as.numeric(gsub(",", ".", FACTOR_P))
  ) %>%
  filter(!is.na(EDAD_NUM) & EDAD_NUM >= 16 & EDAD_NUM <= 74) %>%
  mutate(
    # Variable dependiente: Compra online en últimos 3 meses (1 = Sí, 0 = No)
    # ULT_INT == "3" (>1 año) y "4" (Nunca) tienen NA estructural en ULT_COM -> 0L
    COMPRA_3M = case_when(
      ULT_COM == "1" ~ 1L,
      ULT_COM %in% c("2", "3", "4") ~ 0L,
      is.na(ULT_COM) & ULT_INT %in% c("3", "4") ~ 0L,
      TRUE ~ NA_integer_
    ),
    USUARIO_INT_3M = if_else(ULT_INT == "1", 1L, 0L),
    
    # Sociodemográficas con etiquetas estrictas del Diseño de Registro
    SEXO_F = factor(SEXO, levels = c("1", "6"), labels = c("Hombre", "Mujer")),
    TRABAJO_SEMANA_F = factor(TRAB, levels = c("1", "6"), labels = c("Sí trabajó", "No trabajó")),
    GRUPO_EDAD = cut(
      EDAD_NUM,
      breaks = c(15, 24, 34, 44, 54, 64, 74),
      labels = c("16-24", "25-34", "35-44", "45-54", "55-64", "65-74")
    ),
    # T_ESTUD: 0-2 (Analfabetos/Primaria/1ª etapa ESO), 3-4 (2ª etapa Sec./Postsec. no sup.),
    # 5 (FP Grado Superior), 6-8 (Grado 240 ECTS / Máster / Doctorado), 9 (No codificable)
    ESTUDIOS_F = case_when(
      NIVELEST %in% c("0", "1", "2") ~ "1. Primarios o 1ª etapa ESO",
      NIVELEST %in% c("3", "4") ~ "2. Bachillerato / FP Medio / Postsec.",
      NIVELEST == "5" ~ "3. FP Grado Superior",
      NIVELEST %in% c("6", "7", "8") ~ "4. Universitarios (Grado/Máster/Doc)",
      NIVELEST == "9" ~ "5. No se puede codificar",
      TRUE ~ NA_character_
    ) %>% factor(),
    
    # T_INGRE: Primer a Quinto quintil + NS/NR
    ING_HOG_F = factor(
      ING_HOG,
      levels = c("1", "2", "3", "4", "5", "6"),
      labels = c("Primer quintil", "Segundo quintil", "Tercer quintil", "Cuarto quintil", "Quinto quintil", "NS/NR")
    ),
    
    # T_1HBT: Tamaño de municipio
    HABITAT_F = case_when(
      HABITAT_ESTRATO == "0" ~ "1. >= 500.000 hab.",
      HABITAT_ESTRATO %in% c("1", "2") ~ "2. 100.000 - 499.999 hab.",
      HABITAT_ESTRATO %in% c("3", "4") ~ "3. 20.000 - 99.999 hab.",
      HABITAT_ESTRATO %in% c("5", "6") ~ "4. < 20.000 hab.",
      TRUE ~ NA_character_
    ) %>% factor(),
    
    # T_INTFR: Frecuencia de uso en usuarios recientes (Modelo B)
    FREC_INT_USU = factor(
      FREC_INT,
      levels = c("1", "2", "3", "4"),
      labels = c("1. Varias veces al día", "2. Diariamente", "3. Semanalmente", "4. Menos de 1 vez/semana")
    ),
    
    # TPREOPB: Preocupación publicidad personalizada
    PREOPUB_F = factor(
      PREOPUB,
      levels = c("6", "2", "1"),
      labels = c("Nada preocupado", "Algo preocupado", "Muy preocupado")
    ),
    
    # Recuento de acciones de gestión de datos personales (GESINT1-6: 1 = Sí, 6 = No)
    across(
      starts_with("GESINT"),
      ~ case_when(.x == "1" ~ 1L, .x == "6" ~ 0L, TRUE ~ NA_integer_),
      .names = "{.col}_NUM"
    ),
    N_ACCIONES_GESTION_DATOS = GESINT1_NUM + GESINT2_NUM + GESINT3_NUM + 
      GESINT4_NUM + GESINT5_NUM + GESINT6_NUM,
    
    # IA Generativa (1 = Sí, 6 = No)
    USO_IAG_F = factor(IAG, levels = c("6", "1"), labels = c("No usa IA Gen", "Usa IA Gen"))
  )

cat("\n=== 02. CONTROL DE DENOMINADORES EN COMPRA_3M ===\n")
cat("N total 16-74:", nrow(datos),
    "| N válido:", sum(!is.na(datos$COMPRA_3M)),
    "| N compra=1:", sum(datos$COMPRA_3M == 1, na.rm = TRUE),
    "| N compra=0:", sum(datos$COMPRA_3M == 0, na.rm = TRUE),
    "| N ausentes:", sum(is.na(datos$COMPRA_3M)), "\n")

# Guardar base limpia definitiva para Quarto
dir.create("data-processed", showWarnings = FALSE)
saveRDS(datos, "data-processed/tich2026_definitivo.rds")

# 3. Consistencia interna (Alfa de Cronbach) de las 6 acciones GESINT
mat_gesint <- datos %>%
  filter(USUARIO_INT_3M == 1) %>%
  select(ends_with("_NUM") & starts_with("GESINT")) %>%
  na.omit()

k <- ncol(mat_gesint)
alpha_cronbach <- (k / (k - 1)) * (1 - sum(apply(mat_gesint, 2, var)) / var(rowSums(mat_gesint)))
cat("\n=== 03. CONSISTENCIA INTERNA DE GESINT1-6 (N =", nrow(mat_gesint), ") ===\n")
cat("Alfa de Cronbach (KR-20 para ítems dicotómicos):", round(alpha_cronbach, 3), "\n")

# 4. Diseños muestrales ponderados (provisionales bajo ids = ~1)
diseno_A <- svydesign(ids = ~1, weights = ~FACTOR_P_NUM, data = datos)
diseno_B <- subset(diseno_A, USUARIO_INT_3M == 1)

cat("\n=== 04. ESTIMACIÓN PONDERADA GLOBAL Y POR ESTRATOS (UNIVERSO A) ===\n")
print(svymean(~COMPRA_3M, diseno_A, na.rm = TRUE) * 100)
print(svyby(~COMPRA_3M, ~ING_HOG_F, diseno_A, svymean, na.rm = TRUE) %>% mutate(pct = round(COMPRA_3M * 100, 2)))
print(svyby(~COMPRA_3M, ~ESTUDIOS_F, diseno_A, svymean, na.rm = TRUE) %>% mutate(pct = round(COMPRA_3M * 100, 2)))
print(svyby(~COMPRA_3M, ~GRUPO_EDAD, diseno_A, svymean, na.rm = TRUE) %>% mutate(pct = round(COMPRA_3M * 100, 2)))
print(svyby(~COMPRA_3M, ~HABITAT_F, diseno_A, svymean, na.rm = TRUE) %>% mutate(pct = round(COMPRA_3M * 100, 2)))

# 5. Función de tablas limpias de Odds Ratios
tabla_or <- function(modelo) {
  coefs <- summary(modelo)$coefficients
  ic <- confint(modelo)
  tibble(
    Variable = rownames(coefs),
    OR = exp(coefs[, 1]),
    IC95_inf = exp(ic[, 1]),
    IC95_sup = exp(ic[, 2]),
    p_valor = coefs[, 4]
  ) %>%
    mutate(
      across(c(OR, IC95_inf, IC95_sup), ~ round(.x, 3)),
      p_valor = if_else(p_valor < 0.001, "<0.001", sprintf("%.3f", p_valor))
    )
}

# 6. Estimación de Modelo A (sin FREC_INT) y Modelos B1 (recuento) y B2 (ítems separados)
mod_A <- svyglm(
  COMPRA_3M ~ GRUPO_EDAD + SEXO_F + ESTUDIOS_F + ING_HOG_F + TRABAJO_SEMANA_F + HABITAT_F,
  design = diseno_A,
  family = quasibinomial()
)

mod_B1 <- svyglm(
  COMPRA_3M ~ GRUPO_EDAD + SEXO_F + ESTUDIOS_F + ING_HOG_F + TRABAJO_SEMANA_F + HABITAT_F +
    FREC_INT_USU + PREOPUB_F + N_ACCIONES_GESTION_DATOS + USO_IAG_F,
  design = diseno_B,
  family = quasibinomial()
)

mod_B2 <- svyglm(
  COMPRA_3M ~ GRUPO_EDAD + SEXO_F + ESTUDIOS_F + ING_HOG_F + TRABAJO_SEMANA_F + HABITAT_F +
    FREC_INT_USU + PREOPUB_F + GESINT1_NUM + GESINT2_NUM + GESINT3_NUM +
    GESINT4_NUM + GESINT5_NUM + GESINT6_NUM + USO_IAG_F,
  design = diseno_B,
  family = quasibinomial()
)

cat("\n=== 05. MODELO A: DETERMINANTES ESTRUCTURALES (n =", nobs(mod_A), ") ===\n")
print(tabla_or(mod_A), n = Inf)

cat("\n=== 06. MODELO B1: RECUENTO GESTIÓN DATOS (n =", nobs(mod_B1), ") ===\n")
print(tabla_or(mod_B1), n = Inf)

cat("\n=== 07. MODELO B2: ACCIONES INDIVIDUALES GESINT1-6 (n =", nobs(mod_B2), ") ===\n")
print(tabla_or(mod_B2), n = Inf)

# 7. Módulo complementario: No usuarios recientes de Internet (n = 483)
datos_nousu <- datos %>%
  filter(USUARIO_INT_3M == 0) %>%
  mutate(across(
    starts_with("NOUSOINT_") & !matches("OTRAPERS"),
    ~ case_when(.x == "1" ~ 1L, .x == "6" ~ 0L, TRUE ~ NA_integer_)
  ))

cat("\n=== 08. DENOMINADORES VÁLIDOS EN MOTIVOS DE NO USO DE INTERNET ===\n")
print(datos_nousu %>% summarise(across(starts_with("NOUSOINT_") & !matches("OTRAPERS"), ~ sum(!is.na(.x)))))

diseno_nousu <- svydesign(ids = ~1, weights = ~FACTOR_P_NUM, data = datos_nousu)
cat("\n=== 09. PREVALENCIA PONDERADA MOTIVOS NO USO INTERNET ===\n")
print(round(svymean(~NOUSOINT_1 + NOUSOINT_2 + NOUSOINT_3 + NOUSOINT_4 + NOUSOINT_5 + NOUSOINT_6 + NOUSOINT_7, diseno_nousu, na.rm = TRUE) * 100, 2))