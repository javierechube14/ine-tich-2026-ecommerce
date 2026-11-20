# ==============================================================================
# FASE 0 (CIERRE DOCUMENTAL): ETIQUETAS OFICIALES, FILTROS Y NOTAS METODOLÓGICAS
# ==============================================================================

library(tidyverse)
library(readxl)
library(janitor)

# 1. Leer Leeme.txt oficial del INE (instrucciones sobre FACTOR_P y diseño)
cat("=== 01. DOCUMENTACIÓN OFICIAL (Leeme.txt) ===\n")
cat(readLines("data-raw/Leeme.txt", warn = FALSE), sep = "\n")

# 2. Listar todas las hojas del Diseño de Registro Excel
ruta_dr <- "data-raw/dr_TICHcuestionario_2026.xlsx"
hojas <- excel_sheets(ruta_dr)
cat("\n=== 02. HOJAS EN EL EXCEL DE DISEÑO DE REGISTRO ===\n")
print(hojas)

# 3. Extraer filas de nuestras variables clave en la hoja principal
dr_vars <- read_excel(ruta_dr, sheet = 1, col_names = FALSE)
vars_clave <- c(
  "ULT_COM", "FREC_INT", "ING_HOG", "TRAB", "NIVELEST",
  "SEXO", "EDAD", "HABITAT_ESTRATO", "PREOPUB", "IAG",
  paste0("GESINT", 1:6), paste0("NOUSOINT_", 1:7)
)

# Buscar también cualquier variable de "uso de internet" (ej. USO_INT, INT_3M, etc.)
vars_int <- dr_vars[[1]][grep("INT|COM", toupper(as.character(dr_vars[[1]])))]
cat("\nVariables relacionadas con Internet/Compra en DR:\n")
print(na.omit(vars_int))

filas_clave <- which(toupper(as.character(dr_vars[[1]])) %in% c(vars_clave, toupper(vars_int)))
cat("\n=== 03. FILAS COMPLETAS EN HOJA 1 DEL DR ===\n")
print(dr_vars[filas_clave, ], n = Inf, width = Inf)

# 4. Extraer los diccionarios de códigos de la hoja de tablas/diccionarios
if (length(hojas) >= 2) {
  for (h in 2:length(hojas)) {
    cat("\n=== 04. CONTENIDO HOJA:", hojas[h], "===\n")
    hoja_actual <- read_excel(ruta_dr, sheet = h, col_names = FALSE)
    diccs_buscados <- c("T_INGRE", "T_ESTUD", "T_INTFR", "T_INTER", "TPREOPB", "T_1HBT", "T_SINO", "T_SEXO")
    filas_dicc <- which(toupper(as.character(hoja_actual[[1]])) %in% diccs_buscados)
    if (length(filas_dicc) > 0) {
      # Mostrar esas filas y las siguientes para capturar todas las categorías de cada tabla
      indices <- sort(unique(unlist(lapply(filas_dicc, function(i) i:min(i + 12, nrow(hoja_actual))))))
      print(hoja_actual[indices, ], n = Inf, width = Inf)
    } else {
      print(head(hoja_actual, 40), width = Inf)
    }
  }
}