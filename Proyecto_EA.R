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
library(readr)
library(dplyr)
library(janitor)
library(ggplot2)
library(fdth)
library(psych)
library(moments)
library(ggrepel)
library(tidyverse)
library(car)


# PASO 2: Codificamos las funciones que vamos a utilizar
# ------------------------------------------------------

# devuelve un vector con el contenido de un tibble
ObtenerVector <- function(tib) {
  for (i in tib)
    vec <- i
  return(vec)
}


# PASO 3: Traducimos la información del fichero de datos (excel) "sleepdata_2.csv" a dataframe
# --------------------------------------------------------------------------------------------
datos <- read_delim("sleepdata_2.csv",
                           delim = ";", escape_double = FALSE, na = "NA",
                           trim_ws = TRUE)


# PASO 4: Nos quedamos con las variables de estudio, eliminando valores nulos
# -----------------------------------------------------------------------------------

# HORAS DE SUEÑO y CALIDAD DEL SUEÑO

# guardamos la variable como dataframe
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
