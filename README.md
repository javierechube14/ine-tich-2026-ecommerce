# Desigualdad Digital, Prácticas de Privacidad y Comercio Electrónico en España (TIC-H 2026)

Análisis sociológico aplicado sobre estratificación socioeconómica, prácticas efectivas de gestión de privacidad y adopción de IA generativa en el comercio electrónico español, elaborado a partir de los microdatos oficiales de la **Encuesta sobre Equipamiento y Uso de TIC en los Hogares (TIC-H 2026)** del Instituto Nacional de Estadística (INE).

🔗 **[Ver informe interactivo completo (HTML)](https://javierechube14.github.io/ine-tich-2026-ecommerce/)**

---

## Resumen de resultados principales

* **Universo y muestra:** 14.105 personas residentes de 16 a 74 años (que representan a 37,63 millones de habitantes), subdivididas en población general (Modelo A, $n = 14.105$), usuarios recientes de Internet (Modelos B1 y B2, $n = 13.622$) y personas sin uso reciente de la red ($n = 483$).
* **Estratificación educativa y generacional:** El **65,73 %** compró online en los últimos tres meses (incluyendo los 443 casos sin respuesta explícita en `ULT_COM` clasificados como no compradores por el filtro de no uso de Internet en >1 año). El nivel educativo muestra una asociación graduada intensa ($\text{OR} = 4,76$ en estudios universitarios frente a primarios/ESO 1ª etapa, $p < 0,001$).
* **Actitud frente a práctica de privacidad:** Declarar preocupación por la publicidad personalizada (`PREOPUB`) no muestra asociación significativa con la compra online ($p = 0,375$), mientras que desplegar prácticas activas de protección (`GESINT3`, `GESINT4`, `GESINT5`) se asocia positivamente con participar en el comercio electrónico ($p < 0,001$).
* **Adopción tecnológica:** El uso de IA Generativa duplica con creces las *odds* de compra online ($\text{OR} = 2,37, p < 0,001$).

## Reproducibilidad y estructura del repositorio

Para reproducir el análisis localmente, descarga los microdatos públicos gratuitos de la **Encuesta TIC-H 2026** desde la web del INE (`TICHcuestionario_2026.tab` y `dr_TICHcuestionario_2026.xlsx`) en una carpeta `data-raw/` dentro del proyecto y ejecuta secuencialmente:

* `01_auditoria.R`: Auditoría de estructura, ponderaciones (`FACTOR_P`) y patrones de ausencia.
* `02_diccionario_oficial.R`: Extracción programática de etiquetas oficiales del Diseño de Registro del INE.
* `03_modelos_definitivos.R`: Contraste de especificaciones de regresión logística ponderada.
* `04_modelos_y_tablas_limpias.R`: Depuración definitiva, cálculo de consistencia interna (KR-20 = 0,782) y estimación de modelos (`survey::svyglm`).
* `informe_tich_2026.qmd` / `index.html`: Documento reproducible en Quarto y versión HTML autocontenida publicada en GitHub Pages.

---

## English Executive Summary

Applied sociological analysis of digital inequality, personal data management practices, and generative AI adoption in Spanish e-commerce using official microdata from the **Spanish National Statistics Institute (INE) ICT Household Survey 2026 (TIC-H 2026)** ($n = 14,105$ individuals aged 16–74).

* **Key Finding 1 (Structural Stratification):** Educational attainment exhibits a strong and graduated association with online purchasing ($\text{OR} = 4.76$ for university graduates vs. primary/lower-secondary education, $p < 0.001$), while household income also shows a positive socioeconomic gradient ($\text{OR} = 1.66$ in the top quintile, $p < 0.001$).
* **Key Finding 2 (Privacy Attitudes vs. Practices):** Among recent internet users ($n = 13,622$), stated concern about targeted advertising shows no significant association with e-commerce participation, whereas active privacy-management behaviors (limiting profile access, refusing advertising data use, and verifying website security) are positively associated with online purchasing ($p < 0.001$).
* **Methods & Stack:** Survey-weighted logistic regression (`survey::svyglm`), adjusted predicted probabilities with 95% confidence intervals, internal consistency testing (KR-20 = 0.782), R (`tidyverse`, `survey`), and Quarto.

