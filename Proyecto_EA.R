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
install.packages("nortest")

library(readr)
library(dplyr)
library(janitor)
library(ggplot2)
library(fdth)
library(psych)
library(moments)
library(ggrepel)
library(tidyverse)
library(nortest)


# PASO 2: Codificamos las funciones que vamos a utilizar
# ------------------------------------------------------

# devuelve un vector con el contenido de un tibble
ObtenerVector <- function(tib) {
  for (i in tib)
    vec <- i
  return(vec)
}

# Añade a un tibble una columna con los intervalos indicados
CrearIntervalosHC <- function(vec, tib, nom) {
  interv <- c()
  for (i in 1:(length(vec) - 1))
    interv <- c(interv, paste('[', vec[i], ', ', vec[i + 1], ')', sep = ''))
  # metemos los intervalos en el tibble
  tib <- cbind(tib, 'HC' = interv)
  return(tib)
}

# Añade a un tibble una columna con los intervalos indicados
CrearIntervalosHS <- function(vec, tib, nom) {
  interv <- c()
  for (i in 1:(length(vec) - 1))
    interv <- c(interv, paste('[', vec[i], ', ', vec[i + 1], ')', sep = ''))
  # metemos los intervalos en el tibble
  tib <- cbind(tib, 'HS' = interv)
  return(tib)
}

# Añade a un tibble una columna con los intervalos indicados
CrearIntervalosTA <- function(vec, tib, nom) {
  interv <- c()
  for (i in 1:(length(vec) - 1))
    interv <- c(interv, paste('[', vec[i], ', ', vec[i + 1], ')', sep = ''))
  # metemos los intervalos en el tibble
  tib <- cbind(tib, 'TA' = interv)
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

# calcula la probabilidad condicionada P(A/B), donde A = (HS >= 7), B = (HC >= 9)
ProbCondicionadaHCyHS <- function(df1, df2) {
  # P(A n B)
  c.fav.aux1 <- as.numeric(count(df1 %>% filter(HS >= 7 & HC >= 9)))
  c.pos.aux1 <- n
  prob.aux1 <- c.fav.aux1 / c.pos.aux1
  # P(B)
  c.fav.aux2 <- as.numeric(count(df2 %>% filter(HC >= 9)))
  c.pos.aux2 <- n
  prob.aux2 <- c.fav.aux2 / c.pos.aux2
  # P(A/B)
  prob <- prob.aux1 / prob.aux2
  return(prob)
}

# calcula la probabilidad condicionada P(A/B), donde A = (HS > 6), B = (TA > 30)
ProbCondicionadaHSyTA <- function(df1, df2) {
  # P(A n B)
  c.fav.aux1 <- as.numeric(count(df1 %>% filter(HS > 6 & TA > 30)))
  c.pos.aux1 <- n
  prob.aux1 <- c.fav.aux1 / c.pos.aux1
  # P(B)
  c.fav.aux2 <- as.numeric(count(df2 %>% filter(TA > 30)))
  c.pos.aux2 <- n
  prob.aux2 <- c.fav.aux2 / c.pos.aux2
  # P(A/B)
  prob <- prob.aux1 / prob.aux2
  return(prob)
}


# PASO 3: Traducimos la información del fichero de datos (excel) "sleepdata_2.csv" a dataframe
# --------------------------------------------------------------------------------------------
datos <- read_delim("sleepdata_2.csv",
                           delim = ";", escape_double = FALSE, na = "NA",
                           trim_ws = TRUE)


# PASO 4: Nos quedamos con las variables de estudio, eliminando valores nulos
# -----------------------------------------------------------------------------------
 
# HORAS EN LA CAMA (HC), HORAS DE SUEÑO (HS) y TIEMPO ANTES DE DORMIR (TA)

# guardamos la variable como tibble
tib <- datos %>% select(12,13,14) %>% filter(!is.na(12) & !is.na(13) & !is.na(14))
# pasamos las horas en la cama de segundos a horas
tib$HC <- tib$`Time in bed (seconds)` / 3600
tib <- tib %>% select(2,3,4)
# pasamos las horas de sueño de segundos a horas
tib$HS <- tib$`Time asleep (seconds)` / 3600
tib <- tib %>% select(2,3,4)
# pasamos el tiempo antes de dormir de segundos a minutos (es más representativo)
tib$TA <- tib$`Time before sleep (seconds)` / 60
tib <- tib %>% select(2,3,4)

# separamos las variables
tib.HC <- tib[1]
tib.HS <- tib[2]
tib.TA <- tib[3]

# guardamos las variables como vectores
vec.HC <- ObtenerVector(tib.HC[1])
vec.HS <- ObtenerVector(tib.HS[1])
vec.TA <- ObtenerVector(tib.TA[1])
# variable artificial: calidad del sueño (CS) = HS / HC
vec.CS <- round(vec.HS / vec.HC * 100, 2)

# guardamos las variables como dataframes por separado
df.HC <- data.frame(HC = vec.HC)
df.HS <- data.frame(HS = vec.HS)
df.TA <- data.frame(TA = vec.TA)

# guardamos las variables como dataframes agrupadas
df.HCyHS <- data.frame(HC = vec.HC,
                       HS = vec.HS)
df.HSyTA <- data.frame(HS = vec.HS,
                       TA = vec.TA)
df.TAyCS <- data.frame(TA = vec.TA,
                       CS = vec.CS)
df.HSyCS <- data.frame(HS = vec.HS,
                       CS = vec.CS)

# tamaño de la muestra
n <- as.numeric(count(df.HC))


# PASO 5: Aplicamos la estadística descriptiva para analizar la variable HC
# -------------------------------------------------------------------------

  # 5.1. Calculamos la tabla de frecuencias
  # ---------------------------------------

# creamos los intervalos (del tipo '[a,b)') y calculamos sus f.a.
interv.HC <- hist(vec.HC, breaks = 'Sturges', include.lowest = TRUE, right = FALSE, plot = FALSE)
interv.HC <- interv.HC[1:2]
# creamos un vector que guarde los intervalos para luego representar la variable
vec.interv.HC <- interv.HC[[1]]

# metemos los datos en la tabla
# creamos una columna auxiliar para luego añadir las que nos interesan
tabla.freq.HC <- tibble(aux = 1:(length(interv.HC[[1]]) - 1))
# generamos los intervalos (las cadenas de caracteres) y las añadimos a la tabla
tabla.freq.HC <- CrearIntervalosHC(interv.HC[[1]], tabla.freq.HC, 'HC')
# añadimos las f.a. que obtuvimos a la tabla
tabla.freq.HC <- ObtenerTibbleFA(interv.HC[[2]], tabla.freq.HC)
# eliminamos la columna auxiliar
tabla.freq.HC <- tabla.freq.HC %>% select(2,3)
# pasamos a tibble
tabla.freq.HC <- as_tibble(tabla.freq.HC)

# calculamos f.a.a.
freq.abs.acc.HC <- cumsum(tabla.freq.HC[2])
freq.abs.acc.HC <- rename(freq.abs.acc.HC, f.a.a. = f.a.)

# obtenemos vectores que necesitaremos
vec.fa.HC <- ObtenerVector(tabla.freq.HC[2])
vec.fr.HC <- FrecuenciasRelativas(vec.fa.HC)

# calculamos f.r.
freq.rel.HC <- tibble(aux = 1:(length(interv.HC[[1]]) - 1))
freq.rel.HC <- ObtenerTibbleFR(vec.fr.HC, freq.rel.HC)
freq.rel.HC <- freq.rel.HC %>% select(2)
freq.rel.HC <- as_tibble(freq.rel.HC)

# calculamos f.r.a.
freq.rel.acc.HC <- cumsum(freq.rel.HC)
freq.rel.acc.HC <- rename(freq.rel.acc.HC, f.r.a. = f.r.)

# terminamos tabla de frecuencias
tabla.freq.HC <- bind_cols(tabla.freq.HC, freq.abs.acc.HC, freq.rel.HC, freq.rel.acc.HC)

  # 5.2. Calculamos las medidas de posición para HC
  # -----------------------------------------------

# media
media.HC <-  mean(vec.HC)

# mediana (percentil 50%)
mediana.HC <- median(vec.HC)

# intervalo modal
max.freq.abs.HC <- max(vec.fa.HC)
interv.modal.HC <- tabla.freq.HC %>% select(1) %>% filter(tabla.freq.HC[2] == max.freq.abs.HC)
interv.modal.HC <- rename(interv.modal.HC, Moda = HC)

# percentil 25%
perc25.HC <- quantile(vec.HC, probs = 1/4)

# percentil 75%
perc75.HC <- quantile(vec.HC, probs = 3/4)

  # 5.3. Calculamos las medidas de dispersión para HC
  # -------------------------------------------------

# varianza
var.HC <- var(vec.HC)

# cuasivarianza
cuasivar.HC <- CalcularCuasivar(var.HC, n)

# desviación típica
desv.tip.HC <- sd(vec.HC)

# cuasidesviación típica
cuasidesv.tip.HC <- sqrt(cuasivar.HC)

# coeficiente de variación
coef.var.HC <- CalcularCoefVar(desv.tip.HC, media.HC)

  # 5.4. Calculamos las medidas de forma para HC
  # --------------------------------------------

# coeficiente de asimetría
coef.asim.HC <- skewness(vec.HC)

# coeficiente de kurtosis
coef.kurt.HC <- kurtosis(vec.HC)

  # 5.5. Calculamos los gráficos correspondientes para HC
  # -----------------------------------------------------

# histograma y polígono de frecuencias (f.a.)
hist.HC <- ggplot(df.HC, aes(vec.HC)) + 
  geom_histogram(color = 1, fill = "#005c00", breaks = vec.interv.HC) +
  geom_freqpoly(data = df.HC, color = "#71c55b", breaks = vec.interv.HC, lwd = 1) + 
  xlab("") +
  ylab("") +
  ggtitle("Histograma y polígono de frecuencias de las horas en la cama") +
  ylim(c(0,450)) +
  xlim(c(0, 13))

# polígono de frecuencias (f.r.a)
pol.fra.HC = ggplot(df.HC, aes(vec.HC)) + 
  geom_step(stat = "ecdf", color = "#71c55b", lwd = 1) +
  xlab("") + 
  ylab("") +
  ggtitle("Polígono de frecuencias f.r.a.")


# PASO 6: Aplicamos la estadística descriptiva para analizar la variable HS
# -------------------------------------------------------------------------

  # 6.1. Calculamos la tabla de frecuencias
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
tabla.freq.HS <- CrearIntervalosHS(interv.HS[[1]], tabla.freq.HS)
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

  # 6.2. Calculamos las medidas de posición para HS
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

  # 6.3. Calculamos las medidas de dispersión para HS
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

  # 6.4. Calculamos las medidas de forma para HS
  # --------------------------------------------

# coeficiente de asimetría
coef.asim.HS <- skewness(vec.HS)

# coeficiente de kurtosis
coef.kurt.HS <- kurtosis(vec.HS)

  # 6.5. Calculamos los gráficos correspondientes para HS
  # -----------------------------------------------------

# histograma y polígono de frecuencias (f.a.)
hist.HS <- ggplot(df.HS, aes(vec.HS)) + 
  geom_histogram(color = 1, fill = "#005c00", breaks = vec.interv.HS) +
  geom_freqpoly(data = df.HS, color = "#71c55b", breaks = vec.interv.HS, lwd = 1) + 
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


# PASO 7: Aplicamos la estadística descriptiva para analizar la variable TA
# -------------------------------------------------------------------------

  # 7.1. Calculamos la tabla de frecuencias
  # ---------------------------------------

# creamos los intervalos (del tipo '[a,b)') y calculamos sus f.a.
vec.interv.TA <- c(0, 5, 10, 20, 30, 40, 50, 60, 70, 100)
interv.TA <- hist(vec.TA, breaks = vec.interv.TA, include.lowest = TRUE, right = FALSE, plot = FALSE)
interv.TA <- interv.TA[1:2]

# metemos los datos en la tabla
# creamos una columna auxiliar para luego añadir las que nos interesan
tabla.freq.TA <- tibble(aux = 1:(length(interv.TA[[1]]) - 1))
# generamos los intervalos (las cadenas de caracteres) y las añadimos a la tabla
tabla.freq.TA <- CrearIntervalosTA(interv.TA[[1]], tabla.freq.TA)
# añadimos las f.a. que obtuvimos a la tabla
tabla.freq.TA <- ObtenerTibbleFA(interv.TA[[2]], tabla.freq.TA)
# eliminamos la columna auxiliar
tabla.freq.TA <- tabla.freq.TA %>% select(2,3)
# pasamos a tibble
tabla.freq.TA <- as_tibble(tabla.freq.TA)

# calculamos f.a.a.
freq.abs.acc.TA <- cumsum(tabla.freq.TA[2])
freq.abs.acc.TA <- rename(freq.abs.acc.TA, f.a.a. = f.a.)

# obtenemos vectores que necesitaremos
vec.fa.TA <- ObtenerVector(tabla.freq.TA[2])
vec.fr.TA <- FrecuenciasRelativas(vec.fa.TA)

# calculamos f.r.
freq.rel.TA <- tibble(aux = 1:(length(interv.TA[[1]]) - 1))
freq.rel.TA <- ObtenerTibbleFR(vec.fr.TA, freq.rel.TA)
freq.rel.TA <- freq.rel.TA %>% select(2)
freq.rel.TA <- as_tibble(freq.rel.TA)

# calculamos f.r.a.
freq.rel.acc.TA <- cumsum(freq.rel.TA)
freq.rel.acc.TA <- rename(freq.rel.acc.TA, f.r.a. = f.r.)

# terminamos tabla de frecuencias
tabla.freq.TA <- bind_cols(tabla.freq.TA, freq.abs.acc.TA, freq.rel.TA, freq.rel.acc.TA)

  # 7.2. Calculamos las medidas de posición para TA
  # -----------------------------------------------

# media
media.TA <-  mean(vec.TA)

# mediana (percentil 50%)
mediana.TA <- median(vec.TA)

# intervalo modal
max.freq.abs.TA <- max(vec.fa.TA)
interv.modal.TA <- tabla.freq.TA %>% select(1) %>% filter(tabla.freq.TA[2] == max.freq.abs.TA)
interv.modal.TA <- rename(interv.modal.TA, Moda = TA)

# percentil 25%
perc25.TA <- quantile(vec.TA, probs = 1/4)

# percentil 75%
perc75.TA <- quantile(vec.TA, probs = 3/4)

  # 7.3. Calculamos las medidas de dispersión para TA
  # -------------------------------------------------

# varianza
var.TA <- var(vec.TA)

# cuasivarianza
cuasivar.TA <- CalcularCuasivar(var.TA, n)

# desviación típica
desv.tip.TA <- sd(vec.TA)

# cuasidesviación típica
cuasidesv.tip.TA <- sqrt(cuasivar.TA)

# coeficiente de variación
coef.var.TA <- CalcularCoefVar(desv.tip.TA, media.TA)

  # 7.4. Calculamos las medidas de forma para TA
  # --------------------------------------------

# coeficiente de asimetría
coef.asim.TA <- skewness(vec.TA)

# coeficiente de kurtosis
coef.kurt.TA <- kurtosis(vec.TA)

  # 7.5. Calculamos los gráficos correspondientes para TA
  # -----------------------------------------------------

# histograma y polígono de frecuencias (f.a.)
hist.TA <- ggplot(df.TA, aes(vec.TA)) + 
  geom_histogram(color = 1, fill = "#005c00", breaks = vec.interv.TA) +
  geom_freqpoly(data = df.TA, color = "#71c55b", breaks = vec.interv.TA, lwd = 1) + 
  xlab("") +
  ylab("") +
  ggtitle("Histograma y polígono de frecuencias del tiempo antes de dormir") +
  ylim(c(0,350)) +
  xlim(c(0, 100))

# polígono de frecuencias (f.r.a)
pol.fra.TA = ggplot(df.TA, aes(vec.TA)) + 
  geom_step(stat = "ecdf", color = "#71c55b", lwd = 1) +
  xlab("") + 
  ylab("") +
  ggtitle("Polígono de frecuencias f.r.a.")


# PASO 8: Hacemos un modelo de regresión de las variables HC y HS
# ---------------------------------------------------------------

  # 8.1. Calculamos la tabla de frecuencias
  # ---------------------------------------

# tabla de frecuencias absolutas
tabla.freq.abs.HCyHS <- table(vec.HC = cut(vec.HC, breaks = vec.interv.HC, right = FALSE), vec.HS = cut(vec.HS, breaks = vec.interv.HS, right = FALSE))

# tabla de frecuencias relativas
tabla.freq.rel.HCyHS <- prop.table(tabla.freq.abs.HCyHS)

# añadimos marginales
tabla.freq.abs.HCyHS <- addmargins(tabla.freq.abs.HCyHS)
tabla.freq.rel.HCyHS <- addmargins(tabla.freq.rel.HCyHS)

  # 8.2. Calculamos el gráfico de dispersión (nube de puntos)
  # ---------------------------------------------------------

diag.disp.HCyHS <- ggplot(df.HCyHS, aes(x = HC, y = HS)) +
  geom_point(color = "#71c55b") +
  xlab("") + 
  ylab("") +
  ggtitle("Diagrama de dispersión de las horas en la cama y las horas de sueño efectivo")

  # 8.3. Hacemos el modelo de regresión
  # -----------------------------------

# regresión lineal
reg.HCyHS <- lm(vec.HC~vec.HS, data = df.HCyHS)
reg.HCyHS <- summary(reg.HCyHS)

# guardamos resultados
a.HCyHS <- 1.86645
b.HCyHS <- 0.86926

# utilizamos regresión para predecir:
# y = 1.86645 + 0.86926x

# si estás acostado durante 10 horas, ¿cuántas horas de sueño efectiva tendrás?
res1 <- a.HCyHS + b.HCyHS * 10

# covarianza
cov.HCyHS <- cov(vec.HC, vec.HS)

# coeficiente correlación lineal
coef.cor.HCyHS <- cor(vec.HC, vec.HS)

# coeficiente determinación
coef.det.HCyHS <- coef.cor.HCyHS ^ 2


# PASO 9: Hacemos un modelo de regresión de las variables HS y TA
# ---------------------------------------------------------------

  # 9.1. Calculamos la tabla de frecuencias
  # ---------------------------------------

# tabla de frecuencias absolutas
tabla.freq.abs.HSyTA <- table(vec.HS = cut(vec.HS, breaks = vec.interv.HS, right = FALSE), vec.TA = cut(vec.TA, breaks = vec.interv.TA, right = FALSE))

# tabla de frecuencias relativas
tabla.freq.rel.HSyTA <- prop.table(tabla.freq.abs.HSyTA)

# añadimos marginales
tabla.freq.abs.HSyTA <- addmargins(tabla.freq.abs.HSyTA)
tabla.freq.rel.HSyTA <- addmargins(tabla.freq.rel.HSyTA)

  # 9.2. Calculamos el gráfico de dispersión (nube de puntos)
  # ---------------------------------------------------------

diag.disp.HSyTA <- ggplot(df.HSyTA, aes(x = TA, y = HS)) +
  geom_point(color = "#71c55b") +
  xlab("") + 
  ylab("") +
  ggtitle("Diagrama de dispersión de las horas de sueño efectivo y el tiempo antes de dormir")

  # 9.3. Hacemos el modelo de regresión
  # -----------------------------------

# regresión lineal
reg.HSyTA <- lm(vec.HS~vec.TA, data = df.HSyTA)
reg.HSyTA <- summary(reg.HSyTA)

# guardamos resultados
a.HSyTA <- 6.747164
b.HSyTA <- -0.006275

# utilizamos regresión para predecir:
# y = 6.747164 - 0.006275x

# si tardas 15 min en dormirte, ¿cuántas horas dormirás?
res2 <- a.HSyTA + b.HSyTA * 15

# covarianza
cov.HSyTA <- cov(vec.HS, vec.TA)

# coeficiente correlación lineal
coef.cor.HSyTA <- cor(vec.HS, vec.TA)

# coeficiente determinación
coef.det.HSyTA <- coef.cor.HSyTA ^ 2


# PASO 10: Hacemos un modelo de regresión de las variables TA y CS
# ----------------------------------------------------------------

  # 10.1. Calculamos la tabla de frecuencias
  # ----------------------------------------

# tabla de frecuencias absolutas
vec.interv.CS <- c(0, 50, 60, 70, 80, 90, 100)
tabla.freq.abs.TAyCS <- table(vec.TA = cut(vec.TA, breaks = vec.interv.TA, right = FALSE), vec.CS = cut(vec.CS, breaks = vec.interv.CS, right = FALSE))

# tabla de frecuencias relativas
tabla.freq.rel.TAyCS <- prop.table(tabla.freq.abs.TAyCS)

# añadimos marginales
tabla.freq.abs.TAyCS <- addmargins(tabla.freq.abs.TAyCS)
tabla.freq.rel.TAyCS <- addmargins(tabla.freq.rel.TAyCS)

  # 10.2. Calculamos el gráfico de dispersión (nube de puntos)
  # ----------------------------------------------------------

diag.disp.TAyCS <- ggplot(df.TAyCS, aes(x = TA, y = CS)) +
  geom_point(color = "#71c55b") +
  xlab("") + 
  ylab("") +
  ggtitle("Diagrama de dispersión del tiempo antes de dormir y la calidad del sueño")

  # 10.3. Hacemos el modelo de regresión
  # ------------------------------------

# regresión lineal
reg.TAyCS <- lm(vec.TA~vec.CS, data = df.TAyCS)
reg.TAyCS <- summary(reg.TAyCS)

# guardamos resultados
a.TAyCS <- 65.08125
b.TAyCS <- -0.55875

# utilizamos regresión para predecir:
# y = 65.08125 - 0.55875x

# si tardas 15 min en dormirte, ¿cuál será tu calidad del sueño?
res3 <- a.TAyCS + b.TAyCS * 15

# covarianza
cov.TAyCS <- cov(vec.TA, vec.CS)

# coeficiente correlación lineal
coef.cor.TAyCS <- cor(vec.TA, vec.CS)

# coeficiente determinación
coef.det.TAyCS <- coef.cor.TAyCS ^ 2


# PASO 11: Calculamos cuestiones de probabilidad relevantes para el estudio
# -------------------------------------------------------------------------

  # P(TA < 10)
c.fav.1 <- as.numeric(count(df.TA %>% filter(TA < 10)))
c.pos.1 <- n
prob.1 <- c.fav.1 / c.pos.1

  # P(HS >= 7) -> Dormir más de las horas mínimas recomendadas para cualquier edad
c.fav.2 <- as.numeric(count(df.HC %>% filter(HC >= 7)))
c.pos.2 <- n
prob.2 <- c.fav.2 / c.pos.2

  # P(HS < 7 n CS > 60)
c.fav.3 <- as.numeric(count(df.HSyCS %>% filter(HS < 7 & CS > 60)))
c.pos.3 <- n
prob.3 <- c.fav.3 / c.pos.3

  # P(TA > 45 n CS > 70)
c.fav.4 <- as.numeric(count(df.TAyCS %>% filter(TA > 45 & CS > 70)))
c.pos.4 <- n
prob.4 <- c.fav.4 / c.pos.4

  # P(HS >= 7 | HC >= 9)
prob.5 <- ProbCondicionadaHCyHS(df.HCyHS, df.HC)

  # P(HS > 6 | TA > 30)
prob.6 <- ProbCondicionadaHSyTA(df.HSyTA, df.TA)


# PASO 12: Calculamos función de densidad y comparamos con modelos de probabilidad
# --------------------------------------------------------------------------------

# HS: variable continua --> FUNCIÓN DE DENSIDAD
func.dens.HS <-  ggplot(df.HS, aes(x = HS)) +
  geom_density(color = "#71c55b",
               fill = "#71c55b",
               alpha = 0.25,
               lwd = 1) +
  xlab("Horas de sueño") +
  ylab("Densidad") +
  ggtitle("Función de densidad de las horas de sueño efectivo")

# comparamos con una distribución normal
diag.cuant.norm.HS <- ggplot(df.HS, aes(sample = HS)) + 
  stat_qq(color = "#71c55b") + 
  stat_qq_line(color = "#005c00", lwd = 0.8) +
  xlab("") + 
  ylab("") +
  ggtitle("Gráfico cuantil-cuantil horas de sueño")

# se aproxima mucho a la recta, excepto por algunos valores atípicos
# eliminamos esos valores extremos
df.HS.2 <- df.HS %>% filter(HS >= 1 & HS < 12)
tib.HS.2 <- as_tibble(df.HS.2)
vec.HS.2 <- ObtenerVector(tib.HS.2[1])
# volvemos a comparar con una distribución normal
diag.cuant.norm.HS.2 <- ggplot(df.HS.2, aes(sample = HS)) + 
  stat_qq(color = "#71c55b") + 
  stat_qq_line(color = "#005c00", lwd = 0.8) +
  xlab("") + 
  ylab("") +
  ggtitle("Gráfico cuantil-cuantil horas de sueño (corregido)")
# ahora sí, podemos comprobarlo con el test Lilliefors (si p > 0.5 se asemeja a una normal)
test.norm.HS <- lillie.test(x = df.HS.2$HS)
# p-value = 0.094 > 0.05

