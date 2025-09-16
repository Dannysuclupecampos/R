# Cargar la librería 'haven' para leer archivos .dta (Stata)
library(haven)

# Leer el archivo .dta y almacenarlo en el objeto 'base'
base <- read_dta("datastata2024.dta")

# Verificar los nombres de las columnas en el dataframe
names(base)

# Cargar la librería 'dplyr' para manipulación de datos
library(dplyr)

# Contar el número de valores perdidos (NA) en cada columna
perdidos <- base %>%
  summarise(across(everything(), ~sum(is.na(.))))

# Crear un nuevo dataframe 'plaquetas_dengue' con solo las variables de interés para la curva ROC
plaquetas_dengue <- base %>% 
  select(plaquetas, resultado0_1)

# Instalar y cargar la librería 'pROC' para generar curvas ROC
install.packages("pROC")  # Si no está instalada, instala la librería
library(pROC)

# Crear el objeto 'roc_obj' que almacena la curva ROC basada en los datos de plaquetas y el resultado
roc_obj <- roc(plaquetas_dengue$resultado0_1, -plaquetas_dengue$plaquetas)

# Graficar la curva ROC con el título y color deseado
plot(roc_obj, main = "Curva ROC para el modelo de predicción de Dengue", col = "red", lwd = 2)

# Calcular el área bajo la curva (AUC) que mide la calidad del modelo de predicción
auc(roc_obj)  # Devuelve el área bajo la curva (AUC), indicador de rendimiento

# Obtener el mejor punto de corte usando el índice de Youden (mejor sensibilidad y especificidad)
coords(roc_obj, x="best", ret=c("threshold", "sensitivity", "specificity"))

# Establecer un nuevo punto de corte para la clasificación de plaquetas (valor de corte = 198,500)
plaquetas_dengue$categoria_plaquetas <- ifelse(plaquetas_dengue$plaquetas < 198500, "1", "0")

# Crear una tabla de contingencia entre el resultado de la prueba (resultado0_1) y la categoría de plaquetas
tabla <- table(plaquetas_dengue$categoria_plaquetas, plaquetas_dengue$resultado0_1)

# Alternativamente, se puede usar una matriz manual con los valores de la tabla de contingencia
tabla <- matrix(c(43, 8, 4, 44), nrow = 2, byrow = TRUE)

# Instalar y cargar el paquete 'epiR' si aún no está instalado, para realizar análisis epidemiológicos
install.packages("epiR")  # Si no está instalada, instala la librería
library(epiR)

# Realizar el análisis de los resultados con la función 'epi.tests' para obtener métricas como sensibilidad, especificidad, etc.
epi.tests(tabla)




# Este script en R realiza un análisis de predicción utilizando una curva ROC para evaluar el rendimiento de un modelo en base a los datos de plaquetas y los resultados de una prueba (probablemente relacionada con el diagnóstico de Dengue). Aquí te dejo un resumen de las principales tareas realizadas:
#   
#   ### 1. **Lectura de datos:**
#   
#   * Se carga un archivo de datos en formato `.dta` (Stata) utilizando la librería `haven` y se almacena en el objeto `base`.
# 
# ### 2. **Verificación de valores perdidos:**
# 
# * Se calcula el número de valores perdidos (NA) por columna usando la librería `dplyr` y se almacena en el objeto `perdidos`.
# 
# ### 3. **Selección de variables para la curva ROC:**
# 
# * Se crea un nuevo dataframe, `plaquetas_dengue`, que contiene solo las variables de interés para el análisis de la curva ROC: `plaquetas` y `resultado0_1`.
# 
# ### 4. **Generación de la curva ROC:**
# 
# * Se instala y carga la librería `pROC` para crear una curva ROC.
# * Se calcula la curva ROC utilizando los valores de `resultado0_1` y `plaquetas`. Este análisis muestra cómo el modelo predice la condición de Dengue en función de los niveles de plaquetas.
# 
# ### 5. **Evaluación de la curva ROC:**
# 
# * Se grafica la curva ROC para visualizar el desempeño del modelo.
# * Se calcula el área bajo la curva (AUC) para evaluar la capacidad predictiva del modelo, donde un valor cercano a 1 indica un buen modelo.
# * Se obtiene el mejor punto de corte (índice de Youden), que maximiza tanto la sensibilidad como la especificidad del modelo.
# 
# ### 6. **Definición de categorías con un nuevo punto de corte:**
# 
# * Se establece un punto de corte para clasificar los valores de plaquetas en dos categorías: "1" (cuando las plaquetas son menores a 198,500) y "0" (cuando las plaquetas son mayores o iguales a ese valor).
# 
# ### 7. **Creación de una tabla de contingencia:**
# 
# * Se genera una tabla de contingencia (`tabla`) que compara las predicciones del modelo (categorías de plaquetas) con los resultados reales (`resultado0_1`).
# 
# ### 8. **Análisis de las métricas del modelo:**
# 
# * Se instala y carga la librería `epiR` para realizar análisis epidemiológicos sobre la tabla de contingencia.
# * Se usan las funciones de `epiR` para calcular métricas como la sensibilidad, especificidad, valor predictivo positivo, valor predictivo negativo, entre otras, a partir de la tabla de contingencia.
# 
# ### Resumen de objetivos:
# 
# * El script tiene como objetivo principal crear un modelo de predicción de Dengue basado en los niveles de plaquetas, evaluar su desempeño con una curva ROC, y luego usar un punto de corte específico para clasificar los resultados y obtener métricas de rendimiento del modelo.
# 
# Si necesitas algún detalle más, ¡avísame!

  
