####################
#     Ejercicio: CLimpieza de archivo CSV
#     Objetivo: Limpieza de archivo CSV - Estación metereológica La Paz
#     Instrucciones: 
#       1) Establecer la direccion del archivo a trabajar
#       2) Cambiar los nombres de las columnas
#       3) Cambiar los balores nulos por 0.00
#       4) Cambiar el formato CHR al formato de fecha
#       5) Cambiar las columnas con numeros CHR a numericos
#       6) Sacar la mediana de cada columna
#       
#
#     Fecha: 18/05/2025
####################

#Establecer direccion de trabajo
setwd("C:/Users/Ricky/Documents/R/esme")
# install.packages("tidyverse") # Incluye readr y dplyr
library(tidyverse) # readr para leer CSV, dplyr para filtrar
# Cargar lubridate para extraer el año
library(lubridate)

# funcion moda
moda <- function(x) {
  # Elimina los NAs, si los hay, para que no afecten el conteo de frecuencias
  x <- x[!is.na(x)]
  # Calcula la tabla de frecuencias de los valores
  tabla_frecuencias <- table(x)
  # Encuentra el valor o valores con la frecuencia máxima
  moda_ <- names(tabla_frecuencias)[tabla_frecuencias == max(tabla_frecuencias)]
  # Si la moda es numérica, intenta convertirla a ese tipo
  # Esto es útil si los valores originales eran números
  if (is.numeric(x)) {
    moda_ <- as.numeric(moda_)
  }
  return(moda_)
}

#Importancia de CSV
EC_La_Paz <- read.csv("data.csv", header = TRUE)
#head(EC_La_Paz)
#str(EC_La_Paz)

#Correccion de valores faltantes
if (sum(is.na(EC_La_Paz)) > 0) {
  cat("Valores, vacíos o NA")
}

#Cambio de los nombres de las columnas
colnames(EC_La_Paz)[1] <- "Fecha"
colnames(EC_La_Paz)[2] <- "Precipitacion_mm"
colnames(EC_La_Paz)[3] <- "Temperatura_Med"
colnames(EC_La_Paz)[4] <- "Temperatura_Max"
colnames(EC_La_Paz)[5] <- "Temperatura_Min"
colnames(EC_La_Paz)[6] <- "Evaporacion_mm"

#Cambio del formato CHR al formato de fecha
EC_La_Paz$Fecha <- as.Date(EC_La_Paz$Fecha, format = "%d/%m/%Y")
EC_La_Paz <- EC_La_Paz %>%
  mutate(Año = year(Fecha))

#Cambio de valores nulos
EC_La_Paz$Precipitacion_mm[EC_La_Paz$Precipitacion_mm == ""] <- "0.00"
EC_La_Paz$Temperatura_Med[EC_La_Paz$Temperatura_Med == ""] <- "0.00"
EC_La_Paz$Temperatura_Max[EC_La_Paz$Temperatura_Max == ""] <- "0.00"
EC_La_Paz$Temperatura_Min[EC_La_Paz$Temperatura_Min == ""] <- "0.00"
EC_La_Paz$Evaporacion_mm[EC_La_Paz$Evaporacion_mm == ""] <- "0.00"

EC_La_Paz$Precipitacion_mm[EC_La_Paz$Precipitacion_mm == "-"] <- "0.00"
EC_La_Paz$Temperatura_Med[EC_La_Paz$Temperatura_Med == "-"] <- "0.00"
EC_La_Paz$Temperatura_Max[EC_La_Paz$Temperatura_Max == "-"] <- "0.00"
EC_La_Paz$Temperatura_Min[EC_La_Paz$Temperatura_Min == "-"] <- "0.00"
EC_La_Paz$Evaporacion_mm[EC_La_Paz$Evaporacion_mm == "-"] <- "0.00"

#Cambio de columnas con numeros CHR a numericos
EC_La_Paz$Precipitacion_mm <- as.numeric(EC_La_Paz$Precipitacion_mm)
EC_La_Paz$Temperatura_Med <- as.numeric(EC_La_Paz$Temperatura_Med)
EC_La_Paz$Temperatura_Max <- as.numeric(EC_La_Paz$Temperatura_Max)
EC_La_Paz$Temperatura_Min <- as.numeric(EC_La_Paz$Temperatura_Min)
EC_La_Paz$Evaporacion_mm <- as.numeric(EC_La_Paz$Evaporacion_mm)

#media de cada columna
mean(EC_La_Paz$Precipitacion_mm)
mean(EC_La_Paz$Temperatura_Med)
mean(EC_La_Paz$Temperatura_Max)
mean(EC_La_Paz$Temperatura_Min) 
mean(EC_La_Paz$Evaporacion_mm)

#mediana de cada columna
median(EC_La_Paz$Precipitacion_mm)
median(EC_La_Paz$Temperatura_Med)
median(EC_La_Paz$Temperatura_Max)
median(EC_La_Paz$Temperatura_Min)
median(EC_La_Paz$Evaporacion_mm)

#moda de cada columna
moda(EC_La_Paz$Precipitacion_mm)
moda(EC_La_Paz$Temperatura_Med)
moda(EC_La_Paz$Temperatura_Max)
moda(EC_La_Paz$Temperatura_Min)
moda(EC_La_Paz$Evaporacion_mm)

#Desviacion destandar de cada columna 
sd(EC_La_Paz$Precipitacion_mm)
sd(EC_La_Paz$Temperatura_Med)
sd(EC_La_Paz$Temperatura_Max)
sd(EC_La_Paz$Temperatura_Min)
sd(EC_La_Paz$Evaporacion_mm)

#rango de cada columba
range(EC_La_Paz$Precipitacion_mm)
range(EC_La_Paz$Temperatura_Med)
range(EC_La_Paz$Temperatura_Max)
range(EC_La_Paz$Temperatura_Min)
range(EC_La_Paz$Evaporacion_mm)

str(EC_La_Paz)

#percentiles para lluvia y temperatura a nivel diario, mensual y anual
#quantile(EC_La_Paz$Precipitacion_mm)
#quantile(EC_La_Paz$Temperatura_Med)
#quantile(EC_La_Paz$Temperatura_Max)
#quantile(EC_La_Paz$Temperatura_Min)

# --- Filtrar el dataframe para incluir solo los datos del año 1940 ---
#datos_1940 <- EC_La_Paz %>%
#  filter(year(Fecha) == 1940)

# --- Verificar los resultados ---
# Mostrar las primeras filas de los datos filtrados
#head(datos_1940)

# Verificar que solo hay fechas de 1940
#summary(datos_1940$Fecha) # Debería mostrar rangos de fechas solo en 1940

percentiles_temp_media_por_año <- EC_La_Paz %>%
  group_by(Año) %>%
  summarise(
    # Calcular múltiples percentiles con un solo quantile()
    # Usa `na.rm = TRUE` para ignorar los NA en el cálculo
    Precipitacion_mm = quantile(EC_La_Paz$Precipitacion_mm, probs = 0.25, na.rm = TRUE),
    Temperatura_Med = quantile(EC_La_Paz$Temperatura_Med, probs = 0.25, na.rm = TRUE),
    #Temperatura_Med = quantile(EC_La_Paz$Temperatura_Med, probs = 0.25, na.rm = TRUE),# O, si quieres un vector de percentiles en una sola celda (menos común si son muchos)
    # Todos_Cuartiles_Temp = list(quantile(Temperatura_Media_C, probs = c(0.25, 0.5, 0.75), na.rm = TRUE))
  )

# Mostrar el resultado
print(percentiles_temp_media_por_año)

