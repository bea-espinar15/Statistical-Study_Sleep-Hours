#####################################################################################
# 
# TRABAJO FINAL DE ESTADÍSTICA APLICADA
#
# Curso 2021-22
# Autores: Beatriz Espinar Aragón
#          Steven Mallqui Aguilar
# DNIs: 51139183V
#       51750233A
#
# Nota:
# El contenido del fichero de datos, los pasos necesarios para obtener el dataframe
# y otra información relevante quedan explicados en la memoria del proyecto
#
#####################################################################################


# PASO 1: Importamos las librerías necesarias
# -------------------------------------------

# Si no tenemos instalados los packages
# instalar packages
install.packages("dplyr")
install.packages("janitor")
install.packages("ggplot2")
install.packages("fdth")
install.packages("psych")
install.packages("moments")
install.packages("ggrepel")
install.packages("tidyverse")

library(readr)
library(dplyr)
library(janitor)
library(ggplot2)
library(fdth)
library(psych)
library(moments)
library(ggrepel)
library(tidyverse)


# PASO 2: Codificamos las funciones que vamos a utilizar
# ------------------------------------------------------

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
    interv <- c(interv, paste('[', vec[i], ', ', vec[i + 1], ')', sep = ''))
  # metemos los intervalos en el tibble
  tib <- cbind(tib, 'HS' = interv)
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
    rel <- c(rel, as.numeric(vec_abs[i] / n))
  return(rel)
}

# mete los datos del vector en un tibble que crea
ObtenerTibbleFR <- function(vec, tib) {
  tib <- cbind(tib, 'f.r.' = vec)
  return(tib)
}

# calcula la cuasivarianza
CalcularCuasivar <- function(var, n) {
  return(var * n / (n - 1))
}

# calcula el coeficiente de variación
CalcularCoefVar <- function(desv.tip, media) {
  return(desv.tip / media)
}


# PASO 3: Traducimos la información del fichero de datos (excel) "sleepdata_2.csv" a dataframe
# --------------------------------------------------------------------------------------------
datos <- read_delim("sleepdata_2.csv",
                           delim = ";", escape_double = FALSE, na = "NA",
                           trim_ws = TRUE)


# PASO 4: Nos quedamos con las variables de estudio, eliminando valores nulos
# -----------------------------------------------------------------------------------

# HORAS DE SUEÑO y CALIDAD DEL SUEÑO

# guardamos la variable como tibble
tib.HSyCS <- datos %>% select(3,13) %>% filter(!is.na(3) & !is.na(13))
# pasamos las horas de sueño de segundos a horas
tib.HSyCS$HS <- tib.HSyCS$`Time asleep (seconds)` / 3600
tib.HSyCS <- tib.HSyCS %>% select(1,3)
# pasamos la calidad del sueño de char a numeric (viene en %)
tib.HSyCS$CS <- parse_number(tib.HSyCS$`Sleep Quality`)
tib.HSyCS <- tib.HSyCS %>% select(2,3)

# separamos las variables
tib.HS <- tib.HSyCS[1]
tib.CS <- tib.HSyCS[2]

# guardamos las variables como vectores
vec.HS <- ObtenerVector(tib.HS[1])
vec.CS <- ObtenerVector(tib.CS[1])

# guardamos las variables como dataframes
df.HS <- data.frame(HS = vec.HS)
df.CS <- data.frame(CS = vec.CS)

# tamaño de la muestra
n <- as.numeric(count(df.HS))


# PASO 5: Aplicamos la estadística descriptiva para analizar la variable HS
# -------------------------------------------------------------------------

  # 5.1. Calculamos la tabla de frecuencias
  # ---------------------------------------

# creamos los intervalos (del tipo '[a,b)') y calculamos sus f.a.
interv.HS <- hist(vec.HS, breaks = 'Sturges', include.lowest = TRUE, right = FALSE, plot = FALSE)
interv.HS <- interv.HS[1:2]
# creamos un vector que guarde los intervalos para luego representar la variable
vec.interv.HS <- interv.HS[[1]]

# metemos los datos en la tabla
# creamos una columna auxiliar para luego añadir las que nos interesan
tabla.freq.HS <- tibble(aux = 1:(length(interv.HS[[1]]) - 1))
# generamos los intervalos (las cadenas de caracteres) y las añadimos a la tabla
tabla.freq.HS <- CrearIntervalos(interv.HS[[1]], tabla.freq.HS)
# añadimos las f.a. que obtuvimos a la tabla
tabla.freq.HS <- ObtenerTibbleFA(interv.HS[[2]], tabla.freq.HS)
# eliminamos la columna auxiliar
tabla.freq.HS <- tabla.freq.HS %>% select(2,3)
# pasamos a tibble
tabla.freq.HS <- as_tibble(tabla.freq.HS)

# calculamos f.a.a.
freq.abs.acc.HS <- cumsum(tabla.freq.HS[2])
freq.abs.acc.HS <- rename(freq.abs.acc.HS, f.a.a. = f.a.)

# obtenemos vectores que necesitaremos
vec.fa.HS <- ObtenerVector(tabla.freq.HS[2])
vec.fr.HS <- FrecuenciasRelativas(vec.fa.HS)

# calculamos f.r.
freq.rel.HS <- tibble(aux = 1:(length(interv.HS[[1]]) - 1))
freq.rel.HS <- ObtenerTibbleFR(vec.fr.HS, freq.rel.HS)
freq.rel.HS <- freq.rel.HS %>% select(2)
freq.rel.HS <- as_tibble(freq.rel.HS)

# calculamos f.r.a.
freq.rel.acc.HS <- cumsum(freq.rel.HS)
freq.rel.acc.HS <- rename(freq.rel.acc.HS, f.r.a. = f.r.)

# terminamos tabla de frecuencias
tabla.freq.HS <- bind_cols(tabla.freq.HS, freq.abs.acc.HS, freq.rel.HS, freq.rel.acc.HS)

  # 5.2. Calculamos las medidas de posición para HS
  # -----------------------------------------------

# media
media.HS <-  mean(vec.HS)

# mediana (percentil 50%)
mediana.HS <- median(vec.HS)

# intervalo modal
max.freq.abs.HS <- max(vec.fa.HS)
interv.modal.HS <- tabla.freq.HS %>% select(1) %>% filter(tabla.freq.HS[2] == max.freq.abs.HS)
interv.modal.HS <- rename(interv.modal.HS, Moda = HS)

# percentil 25%
perc25.HS <- quantile(vec.HS, probs = 1/4)

# percentil 75%
perc75.HS <- quantile(vec.HS, probs = 3/4)

  # 5.3. Calculamos las medidas de dispersión para HS
  # -------------------------------------------------

# varianza
var.HS <- var(vec.HS)

# cuasivarianza
cuasivar.HS <- CalcularCuasivar(var.HS, n)

# desviación típica
desv.tip.HS <- sd(vec.HS)

# cuasidesviación típica
cuasidesv.tip.HS <- sqrt(cuasivar.HS)

# coeficiente de variación
coef.var.HS <- CalcularCoefVar(desv.tip.HS, media.HS)

  # 5.4. Calculamos las medidas de forma para HS
  # --------------------------------------------

# coeficiente de asimetría
coef.asim.HS <- skewness(vec.HS)

# coeficiente de kurtosis
coef.kurt.HS <- kurtosis(vec.HS)

  # 5.5. Calculamos los gráficos correspondientes para HS
  # -----------------------------------------------------

# histograma y polígono de frecuencias (f.a.)
hist.HS <- ggplot(df.HS, aes(vec.HS)) + 
  geom_histogram(color = 1, fill = "#005c00", breaks = vec.interv.HS) +
  geom_freqpoly(data = df.HS, color = "#71c55b", breaks = vec.interv.HS) + 
  xlab("") +
  ylab("") +
  ggtitle("Histograma y polígono de frecuencias de las horas de sueño") +
  ylim(c(0,400)) +
  xlim(c(0, 13))

# polígono de frecuencias (f.r.a)
pol.fra.HS = ggplot(df.HS, aes(vec.HS)) + 
  geom_step(stat = "ecdf", color = "#71c55b", lwd = 1) +
  xlab("") + 
  ylab("") +
  ggtitle("Polígono de frecuencias f.r.a.")

# PASO 6: Aplicamos la estadística descriptiva para analizar las variables HS y CS
# --------------------------------------------------------------------------------

  # 6.1. Calculamos la tabla de frecuencias
  # ---------------------------------------

# tabla de frecuencias absolutas
# creamos los intervalos para la calidad del sueño
interv.CS <- hist(vec.CS, breaks = 'Sturges', include.lowest = TRUE, right = FALSE, plot = FALSE)
vec.interv.CS 
tabla.freq.abs.HSyCS <- table(vec.HS = cut(vec.HS, breaks = vec.interv.HS, right = FALSE))



# tabla de frecuencias absolutas
tabla.freq.abs.NEHyGE <- table(vec.NEHyGE.NEH, vec.NEHyGE.GE = cut(vec.NEHyGE.GE, breaks = intervalos, right = FALSE))

# tabla de frecuencias relativas
tabla.freq.rel.NEHyGE <- prop.table(tabla.freq.abs.NEHyGE)

# añadimos marginales
tabla.freq.abs.NEHyGE <- addmargins(tabla.freq.abs.NEHyGE)
tabla.freq.rel.NEHyGE <- addmargins(tabla.freq.rel.NEHyGE)