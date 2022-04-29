# para leer el fichero de datos hay que ejecutar MD_EGHE_2019.R
# esto generará un data frame

# instalar packages
install.packages("dplyr")
install.packages("janitor")
install.packages("ggplot2")
install.packages("fdth")
install.packages("psych")
  
# librerías
library(dplyr)
library(janitor)
library(ggplot2)
library(fdth)
library(psych)

# FUNCIONES
# hola

# devuelve un vector con el contenido de un tibble
ObtenerVector <- function(tib) {
  for (i in tib)
    vec <- i
  return(vec)
}

# Añade a un tibble una columna con los intervalos indicados
CrearIntervalos <- function(vec, tib) {
  interv <- c()
  for (i in 1:(length(vec) - 1))
    interv <- c(interv, paste('(', vec[i], ', ', vec[i + 1], ']', sep = ''))
  # metemos los intervalos en el tibble
  tib <- cbind(tib, 'GE' = interv)
  return(tib)
}

# mete los datos del vector en una nueva columna del tibble
ObtenerTibbleFA <- function(vec, tib) {
  tib <- cbind(tib, 'f.a.' = vec)
  return(tib)
}

# calcula las frecuencias relativas
FrecuenciasRelativas <- function(vec_abs) {
  rel <- c()
  for (i in 1:length(vec_abs))
    rel <- c(rel, as.numeric(vec_abs[i] / n.total))
  return(rel)
}

# mete los datos del vector en un tibble que crea
ObtenerTibbleFR <- function(vec, tib) {
  tib <- cbind(tib, 'f.r.' = vec)
  return(tib)
}

# --------------------------------------------------------------------------------
# --------------------------------------------------------------------------------

# declaramos variable del dataframe
datos <- fichero_salida

# rm todas las col exc la 2 (ID), la 5 (NEH) y la 57 (GE) y las filas que sean NA
datos <- datos %>% select(2,5,57) %>% filter(!is.na(EHOGAR) & !is.na(GTT))

# pasamos EHOGAR de char a numeric
datos$NEH <- as.numeric(datos$EHOGAR)
datos <- datos %>% select(1,3,4)

# rehacemos el fichero con los gastos totales por hogar 
# (viene una entrada por cada estudiante)
datos <- datos %>% group_by(IDHOGAR, NEH) %>% summarise(GE = sum(GTT))
datos <- datos %>% ungroup() %>% select(2, 3)

# separamos en dos variables
num.est.hogar <- datos %>% select(1)
gasto.ed <- datos %>% select(2)

# número de datos del fichero
n.total <- count(datos)


# TABLA DE FRECUENCIAS DE NUM.EST.HOGAR

# frecuencias abs
freq.abs.NEH <- table(num.est.hogar)

# frecuencias rel
freq.rel.NEH <- prop.table(freq.abs.NEH)

# pasamos a tibble
freq.abs.NEH <- as_tibble(freq.abs.NEH)
freq.abs.NEH <- rename(freq.abs.NEH, NEH = num.est.hogar, f.a. = n)
freq.rel.NEH <- as_tibble(freq.rel.NEH)
freq.rel.NEH <- rename(freq.rel.NEH, NEH = num.est.hogar, f.r. = n)

# frecuencias acumuladas
freq.abs.acc.NEH <- cumsum(freq.abs.NEH[2])
freq.abs.acc.NEH <- rename(freq.abs.acc.NEH, f.a.a. = f.a.)
freq.rel.acc.NEH <- cumsum(freq.rel.NEH[2])
freq.rel.acc.NEH <- rename(freq.rel.acc.NEH, f.r.a. = f.r.)

# creamos tabla de frecuencias
NEH.aux <- num.est.hogar %>% group_by(NEH) %>% summarise(aux = 0)
freq.NEH.aux <- bind_cols(freq.abs.NEH[2], freq.abs.acc.NEH, freq.rel.NEH[2], freq.rel.acc.NEH)
tabla.freq.NEH <- bind_cols(NEH.aux[1], freq.NEH.aux)

# TABLA DE FRECUENCIAS DE GASTO.ED

# creamos intervalos y frecuencias abs
vector.GE <- ObtenerVector(gasto.ed)
intervalos <- c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000, 15000, 20000, 30000, 55000)
intervalos.GE <- hist(vector.GE, breaks = intervalos, include.lowest = TRUE, right = TRUE, plot = FALSE)
intervalos.GE <- intervalos.GE[1:2]

# metemos los datos en la tabla
tabla.freq.GE <- tibble(aux = 1:(length(intervalos.GE[[1]]) - 1))
tabla.freq.GE <- CrearIntervalos(intervalos.GE[[1]], tabla.freq.GE)
tabla.freq.GE <- ObtenerTibbleFA(intervalos.GE[[2]], tabla.freq.GE)
tabla.freq.GE <- tabla.freq.GE %>% select(2,3)
tabla.freq.GE <- as_tibble(tabla.freq.GE)

# frecuencias abs acumuladas
freq.abs.acc.GE <- cumsum(tabla.freq.GE[2])
freq.abs.acc.GE <- rename(freq.abs.acc.GE, f.a.a. = f.a.)

# frecuencias rel
vector.fa.GE <- ObtenerVector(tabla.freq.GE[2])
vector.fr.GE <- FrecuenciasRelativas(vector.fa.GE)
freq.rel.GE <- tibble(aux = 1:(length(intervalos.GE[[1]]) - 1))
freq.rel.GE <- ObtenerTibbleFR(vector.fr.GE, freq.rel.GE)
freq.rel.GE <- freq.rel.GE %>% select(2)
freq.rel.GE <- as_tibble(freq.rel.GE)

# frecuencias rel acumuladas
freq.rel.acc.GE <- cumsum(freq.rel.GE)
freq.rel.acc.GE <- rename(freq.rel.acc.GE, f.r.a. = f.r.)

# terminamos tabla de frecuencias
tabla.freq.GE <- bind_cols(tabla.freq.GE, freq.abs.acc.GE, freq.rel.GE, freq.rel.acc.GE)
