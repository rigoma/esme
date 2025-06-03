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
setwd("D:/Estadistica/Rstudio_AnandaGYCA")

#Importancia de CSV
EC_La_Paz <- read.csv("EC La Paz 03074 La Paz.csv", 
                      header = TRUE, 
                      sep = ",")
head(EC_La_Paz)
str(EC_La_Paz)

#Correccion de valores faltantes
datos_error <- sum(is.na(EC_La_Paz))
if(datos_error > 0)
   {cat("Valores, vacíos o NA")}

#Cambio de los nombres de las columnas
colnames(EC_La_Paz)[1] <- "Fecha"
colnames(EC_La_Paz)[2] <- "Precipitación_mm"
colnames(EC_La_Paz)[3] <- "Temperatura_Med"
colnames(EC_La_Paz)[4] <- "Temperatura_Máx"
colnames(EC_La_Paz)[5] <- "Temperatura_Mín"
colnames(EC_La_Paz)[6] <- "Evaporación_mm"

#Cambio de balores nulos
EC_La_Paz$Precipitación_mm[EC_La_Paz$Precipitación_mm == ""] <- "0.00"
EC_La_Paz$Temperatura_Med[EC_La_Paz$Temperatura_Med == ""] <- "0.00"
EC_La_Paz$Temperatura_Máx[EC_La_Paz$Temperatura_Máx == ""] <- "0.00"
EC_La_Paz$Temperatura_Mín[EC_La_Paz$Temperatura_Mín == ""] <- "0.00"
EC_La_Paz$Evaporación_mm[EC_La_Paz$Evaporación_mm == ""] <- "0.00"

EC_La_Paz$Precipitación_mm[EC_La_Paz$Precipitación_mm == "-"] <- "0.00"
EC_La_Paz$Temperatura_Med[EC_La_Paz$Temperatura_Med == "-"] <- "0.00"
EC_La_Paz$Temperatura_Máx[EC_La_Paz$Temperatura_Máx == "-"] <- "0.00"
EC_La_Paz$Temperatura_Mín[EC_La_Paz$Temperatura_Mín == "-"] <- "0.00"
EC_La_Paz$Evaporación_mm[EC_La_Paz$Evaporación_mm == "-"] <- "0.00"

str(EC_La_Paz)

#Cambio del formato CHR al formato de fecha
EC_La_Paz$Fecha <- as.Date(EC_La_Paz$Fecha, format = "%d/%m/%Y")

#Cambio de columnas con numeros CHR a numericos
EC_La_Paz$Precipitación_mm <- as.numeric(EC_La_Paz$Precipitación_mm)
EC_La_Paz$Temperatura_Med <- as.numeric(EC_La_Paz$Temperatura_Med)
EC_La_Paz$Temperatura_Máx <- as.numeric(EC_La_Paz$Temperatura_Máx)
EC_La_Paz$Temperatura_Mín <- as.numeric(EC_La_Paz$Temperatura_Mín)
EC_La_Paz$Evaporación_mm <- as.numeric(EC_La_Paz$Evaporación_mm)

str(EC_La_Paz)

#media de cada columna
mean(EC_La_Paz$Precipitación_mm)
mean(EC_La_Paz$Temperatura_Med)
mean(EC_La_Paz$Temperatura_Máx)
mean(EC_La_Paz$Temperatura_Mín) 
mean(EC_La_Paz$Evaporación_mm)

#mediana de cada columna
median(EC_La_Paz$Precipitación_mm)
median(EC_La_Paz$Temperatura_Med)
median(EC_La_Paz$Temperatura_Máx)
median(EC_La_Paz$Temperatura_Mín)
median(EC_La_Paz$Evaporación_mm)

#moda de cada columna
#mode(EC_La_Paz$Precipitación_mm)
#mode(EC_La_Paz$Temperatura_Med)
#mode(EC_La_Paz$Temperatura_Máx)
#mode(EC_La_Paz$Temperatura_Mín)
#mode(EC_La_Paz$Evaporación_mm)

#Desviacion destandar de cada columna 
sd(EC_La_Paz$Precipitación_mm)
sd(EC_La_Paz$Temperatura_Med)
sd(EC_La_Paz$Temperatura_Máx)
sd(EC_La_Paz$Temperatura_Mín)
sd(EC_La_Paz$Evaporación_mm)

#rango de cada columba
range(EC_La_Paz$Precipitación_mm)
range(EC_La_Paz$Temperatura_Med)
range(EC_La_Paz$Temperatura_Máx)
range(EC_La_Paz$Temperatura_Mín)
range(EC_La_Paz$Evaporación_mm)

#percentiles para lluvia y temperatura a nivel diario, mensual y anual
quantile(EC_La_Paz$Precipitación_mm)
quantile(EC_La_Paz$Temperatura_Med)
quantile(EC_La_Paz$Temperatura_Máx)
quantile(EC_La_Paz$Temperatura_Mín)








