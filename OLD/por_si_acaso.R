# calcula f.r.a.
CalcularAcumulada <- function(frecuencias) {
  
  frecuencias <- c(frecuencias[2])
  # primera frecuencia acumulada
  acc <- c(frecuencias[1])
  # calculamos el resto de frecuencias
  for (i in 2:length(frecuencias)){
    acc <- c(frecuencias[i] + frecuencias[i - 1])
  }
  return(acc)
  
}

# frecuencias abs num.est.hogar
freq.abs.NEH <- num.est.hogar %>% group_by(NEH) %>% summarise(f.a. = n())
# frecuencias rel num.est.hogar
freq.rel.NEH <- num.est.hogar %>% group_by(NEH) %>% summarise(f.r. = EspecificarDecimal(n() / n.total, 9))

# quedarte sólo con las columnas que sean números
aux <- freq.rel.NEH |> select(where(is.numeric))

# añadir totales (param = 1 si sólo col, param = 2 si sólo fila)
freq.abs.NEH <- addmargins(freq.abs.NEH)
freq.rel.NEH <- addmargins(freq.rel.NEH)

# devuelve el número x con k decimales
EspecificarDecimal <- function(x, k) round(x, k)

# (AUX) muestra todas las variables
MostrarVariables <- function() {
  
  class(num.est.hogar)
  num.est.hogar
  
  class(gasto.ed)
  gasto.ed
  
  class(n.total)
  n.total
  
  class(freq.abs.NEH)
  freq.abs.NEH
  
  class(freq.rel.NEH)
  freq.rel.NEH
  
  class(freq.abs.acc.NEH)
  freq.abs.acc.NEH
  
  class(freq.rel.acc.NEH)
  freq.rel.acc.NEH
  
  class(tabla.freq.NEH)
  tabla.freq.NEH
  
}

# HACEMOS LO MISMO CON GASTO.ED

# frecuencias abs num.est.hogar
freq.abs.GE <- table(gasto.ed)

# frecuencias rel num.est.hogar
freq.rel.GE <- prop.table(freq.abs.GE)

# pasamos a tibble
freq.abs.GE <- as_tibble(freq.abs.GE)
freq.abs.GE <- rename(freq.abs.GE, GE = gasto.ed, f.a. = n)
freq.rel.GE <- as_tibble(freq.rel.GE)
freq.rel.GE <- rename(freq.rel.GE, GE = gasto.ed, f.r. = n)

# frecuencias acumuladas
freq.abs.acc.GE <- cumsum(freq.abs.GE[2])
freq.abs.acc.GE <- rename(freq.abs.acc.GE, f.a.a. = f.a.)
freq.rel.acc.GE <- cumsum(freq.rel.GE[2])
freq.rel.acc.GE <- rename(freq.rel.acc.GE, f.r.a. = f.r.)

# creamos tabla de frecuencias
GE.aux <- gasto.ed %>% group_by(GE) %>% summarise(aux = 0)
freq.GE.aux <- bind_cols(freq.abs.GE[2], freq.abs.acc.GE, freq.rel.GE[2], freq.rel.acc.GE)
tabla.freq.GE <- bind_cols(GE.aux[1], freq.GE.aux)


mean(fichero_salida$Edad)

median(fichero_salida$Edad)

mode<-function(x){which.max(tabulate(x))}
mode(fichero_salida$Edad)

quantile(fichero_salida$Edad, probs = 1/4)

quantile(fichero_salida$Edad, probs = 3/4)

max(fichero_salida$Edad) - min(fichero_salida$Edad)

IQR(fichero_salida$Edad)

# media intervalos
marcas.clase <- CalcularMarcasClase(intervalos.GE[[1]])
media.GE <- CalcularMedia(marcas.clase, vector.fr.GE)

# calcula las marcas de clase de los intervalos
CalcularMarcasClase <- function(v){
  mc <- c()
  for (i in 1:(length(v) - 1))
    mc <- c(mc, (v[i] + v[i + 1]) / 2)
  return(mc)
}

# calcula la media (cuando hay intervalos)
CalcularMedia <- function(mc, fr) {
  media <- 0
  for (i in 1:length(fr))
    media <- media + (mc[i] * fr[i])
  return(media)
}

coef_var <- function(x, na.rm = FALSE) {
  sd(x, na.rm=na.rm) / mean(x, na.rm=na.rm)
}

coef_var(x=w, na.rm=T)

tabla.freq.NEHyGE <- datos %>% group_by(NEH, GE = cut(GE, breaks = intervalos)) %>% 
  summarise(f.a. = n()) %>%
  mutate(f.a.a. = cumsum(f.a.)) %>%
  mutate(f.r. = f.a. / sum(f.a.)) %>%
  mutate(f.r.a. = cumsum(f.r.))

# pasamos EDAD de char a numeric
datos$ED <- as.numeric(datos$EDAD)
datos <- datos %>% select(1,3,4,5)

# ------------------------------------------------------------------------------------------

# EDAD Y C09A (COMEDOR)

datos_aux <- fichero_salida
datos_aux <- datos_aux %>% select(7,19) %>% filter(!is.na(EDAD) & !is.na(C09A))

datos_aux$ED <- as.numeric(datos_aux$EDAD)
datos_aux <- datos_aux %>% select(2,3)

datos_aux$NPH <- as.numeric(datos_aux$NHOGAR)
datos_aux <- datos_aux %>% select(2,3)

var1 <- datos_aux %>% select(1)
var2 <- datos_aux %>% select(2)

var1 <- as_tibble(var1)
var2 <- as_tibble(var2)

vector1 <- ObtenerVector(var1[1])
vector2 <- ObtenerVector(var2[1])

df <- data.frame(var1 = vector1, 
                 var2 = vector2)

ggplot(df, aes(x = var2, y = var1)) +
  geom_point(color = "#71c55b") +
  xlab("") + 
  ylab("")

cor(vector2, vector1)

# ------------------------------------------------------------------------------------------

# EHOGAR Y D48 (papeleria)

datos_aux <- fichero_salida
datos_aux <- datos_aux %>% select(5,50) %>% filter(!is.na(EHOGAR) & !is.na(D48))

datos_aux$NEH <- as.numeric(datos_aux$EHOGAR)
datos_aux <- datos_aux %>% select(2,3)

datos_aux$NPH <- as.numeric(datos_aux$NHOGAR)
datos_aux <- datos_aux %>% select(2,3)

var1 <- datos_aux %>% select(1)
var2 <- datos_aux %>% select(2)

var1 <- as_tibble(var1)
var2 <- as_tibble(var2)

vector1 <- ObtenerVector(var1[1])
vector2 <- ObtenerVector(var2[1])

df <- data.frame(var1 = vector1, 
                 var2 = vector2)

ggplot(df, aes(x = var2, y = var1)) +
  geom_point(color = "#71c55b") +
  xlab("") + 
  ylab("")

cor(vector2, vector1)

# elegimos una nueva variable: Gasto en libros
datos.GLyGEI <- datos %>% select(2,3)

# separamos las variables
gasto.lib <- datos.GLyGEI %>% select(1)
gasto.ed.ind <- datos.GLyGEI %>% select(2)
gasto.ed.ind$GEI <- gasto.ed.ind$GTT
gasto.ed.ind <- gasto.ed.ind %>% select(2)

# pasamos a tibble
gasto.lib <- as_tibble(gasto.lib)
gasto.ed.ind <- as_tibble(gasto.ed.ind)

# obtenemos los vectores necesarios y los data frames
vector.GL <- ObtenerVector(gasto.lib[1])
vector.GEI <- ObtenerVector(gasto.ed.ind[1])

# TABLA DE FRECUENCIAS GL Y GEI

# vemos qué intervalos son representativos para GL
intervalos2 <- c(0, 100, 200, 300, 400, 500, 600, 2200)
intervalos.GL <- hist(vector.GL, breaks = intervalos2, include.lowest = TRUE, right = FALSE, plot = TRUE)

# vemos qué intervalos son representativos para GEI
intervalos3 <- c(0, 1000, 2000, 3000, 4000, 5000, 10000, 20000, 50000)
intervalos.GEI <- hist(vector.GEI, breaks = intervalos3, include.lowest = TRUE, right = FALSE, plot = FALSE)

# tabla de frecuencias absolutas
tabla.freq.abs.GLyGEI <- table(vector.GL = cut(vector.GL, breaks = intervalos2, right = FALSE), vector.GEI = cut(vector.GEI, breaks = intervalos3, right = FALSE))

# tabla de frecuencias relativas
tabla.freq.rel.GLyGEI <- prop.table(tabla.freq.abs.GLyGEI)

# añadimos marginales
tabla.freq.abs.GLyGEI <- addmargins(tabla.freq.abs.GLyGEI)
tabla.freq.rel.GLyGEI <- addmargins(tabla.freq.rel.GLyGEI)

# DIAGRAMA DE DISPERSIÓN GL Y GEI (NUBE DE PUNTOS)

# obtenemos el data frame para representarlo
df.GLyGEI <- data.frame(GL = vector.GL, 
                        GEI = vector.GEI)

diag.disp.GLyGEI <- ggplot(df.GLyGEI, aes(x = GL, y = GEI)) +
  geom_point(color = "#71c55b") +
  xlab("") + 
  ylab("")

cor(vector.GL, vector.GEI)

# A = GE > 1000; B = NEH = 1. ¿P(A/B)?
# P(A n B)
c.fav.5 <- count(df.NEHyGE %>% filter(NEH == 1 & GE > 1000))
c.pos.5 <- count(df.NEHyGE)
prob.5 <- c.fav.5 / c.pos.5
# P(B)
c.fav.6 <- count(df.NEH %>% filter(NEH == 1))
c.pos.6 <- count(df.NEH)
prob.6 <- c.fav.6 / c.pos.6
# P(A/B)
prob.7 <- prob.5 / prob.6

# A = GE > 1000; B = NEH = 2. ¿P(A/B)?
# P(A n B)
c.fav.8 <- count(df.NEHyGE %>% filter(NEH == 2 & GE > 1000))
c.pos.8 <- count(df.NEHyGE)
prob.8 <- c.fav.8 / c.pos.8
# P(B)
c.fav.9 <- count(df.NEH %>% filter(NEH == 2))
c.pos.9 <- count(df.NEH)
prob.9 <- c.fav.9 / c.pos.9
# P(A/B)
prob.10 <- prob.8 / prob.9

# A = GE > 1000; B = NEH = 3. ¿P(A/B)?
# P(A n B)
c.fav.11 <- count(df.NEHyGE %>% filter(NEH == 3 & GE > 1000))
c.pos.11 <- count(df.NEHyGE)
prob.11 <- c.fav.11 / c.pos.11
# P(B)
c.fav.12 <- count(df.NEH %>% filter(NEH == 3))
c.pos.12 <- count(df.NEH)
prob.12 <- c.fav.12 / c.pos.12
# P(A/B)
prob.13 <- prob.11 / prob.12

# A = GE > 1000; B = NEH = 4. ¿P(A/B)?
# P(A n B)
c.fav.14 <- count(df.NEHyGE %>% filter(NEH == 4 & GE > 1000))
c.pos.14 <- count(df.NEHyGE)
prob.14 <- c.fav.14 / c.pos.14
# P(B)
c.fav.15 <- count(df.NEH %>% filter(NEH == 4))
c.pos.15 <- count(df.NEH)
prob.15 <- c.fav.15 / c.pos.15
# P(A/B)
prob.16 <- prob.14 / prob.15

# A = GE > 1000; B = NEH = 5. ¿P(A/B)?
# P(A n B)
c.fav.17 <- count(df.NEHyGE %>% filter(NEH == 5 & GE > 1000))
c.pos.17 <- count(df.NEHyGE)
prob.17 <- c.fav.17 / c.pos.17
# P(B)
c.fav.18 <- count(df.NEH %>% filter(NEH == 5))
c.pos.18 <- count(df.NEH)
prob.18 <- c.fav.18 / c.pos.18
# P(A/B)
prob.19 <- prob.17 / prob.18

# A = GE > 1000; B = NEH = 6. ¿P(A/B)?
# P(A n B)
c.fav.20 <- count(df.NEHyGE %>% filter(NEH == 6 & GE > 1000))
c.pos.20 <- count(df.NEHyGE)
prob.20 <- c.fav.20 / c.pos.20
# P(B)
c.fav.21 <- count(df.NEH %>% filter(NEH == 6))
c.pos.21 <- count(df.NEH)
prob.21 <- c.fav.21 / c.pos.21
# P(A/B)
prob.22 <- prob.20 / prob.21

hist(vector.GE, freq = FALSE, xlim = c(0,60000), breaks = intervalos)
lines(density(vector.GE))

# rm todas las col exc la 2 (ID), la 4 (NPH), la 5 (NEH), la 57 (GE) y las filas que sean NA
datos <- datos %>% select(2,4,5,57) %>% filter(!is.na(NHOGAR) & !is.na(EHOGAR) & !is.na(GTT))

# pasamos NHOGAR de char a numeric
datos$NPH <- as.numeric(datos$NHOGAR)
datos <- datos %>% select(1,3,4,5)

# pasamos EHOGAR de char a numeric
datos$NEH <- as.numeric(datos$EHOGAR)
datos <- datos %>% select(1,3,4,5)

# rehacemos el fichero con los gastos totales por hogar 
# (viene una entrada por cada estudiante)
datos.NEHyGE <- datos %>% group_by(IDHOGAR, NEH) %>% summarise(GE = sum(GTT))
datos.NEHyGE <- datos.NEHyGE %>% ungroup() %>% select(2, 3)

# separamos en dos variables
num.est.hogar <- datos.NEHyGE %>% select(1)
gasto.ed <- datos.NEHyGE %>% select(2)

# tomamos una nueva variable: número de personas por hogar
datos.NPHyNEH2 <- datos %>% select(1,3,4)
datos.NPHyNEH2 <- datos.NPHyNEH2 %>% group_by(IDHOGAR)
datos.NPHyNEH2 <- datos.NPHyNEH2 %>% ungroup() %>% select(2, 3)

# separamos variables
num.per.hogar <- datos.NPHyNEH2 %>% select(1)
num.est.hogar.2 <- datos.NPHyNEH2 %>% select(2)

# pasamos a tibble
num.per.hogar <- as_tibble(num.per.hogar)
num.est.hogar.2 <- as_tibble(num.est.hogar.2)

# calculamos vectores necesarios
vector.NPH <- ObtenerVector(num.per.hogar[1])
vector.NEH2 <- ObtenerVector(num.est.hogar.2[1])

# NPH
vec.NPH <- ObtenerVector(tib.NPH[1])

# NPH
tib.NPH <- as_tibble(df.NPH)

# NÚMERO DE PERSONAS POR HOGAR

# nos quedamos con la col 2 (ID) y la 4 (NPH) y eliminamos las filas que sean NA
df.NPH <- datos %>% select(2,4) %>% filter(!is.na(NHOGAR))

# pasamos NHOGAR de char a numeric
df.NPH$NPH <- as.numeric(df.NPH$NHOGAR)
df.NPH <- df.NPH %>% select(1,3)

# agrupamos por hogar (hay más de una fila por hogar)
df.NPH <- df.NPH %>% group_by(IDHOGAR)
df.NPH <- df.NPH %>% ungroup() %>% select(2)

# NPH
n.NPH <- as.numeric(count(df.NPH))

datos.NEHyGL <- fichero_salida %>% select(2,5,44) %>% filter(!is.na(EHOGAR) & !is.na(LIB))
# pasamos EHOGAR de char a numeric
datos.NEHyGL$NEH <- as.numeric(datos.NEHyGL$EHOGAR)
datos.NEHyGL <- datos.NEHyGL %>% select(1,3,4)
# rehacemos el fichero con los gastos totales por hogar
datos.NEHyGL <- datos.NEHyGL %>% group_by(IDHOGAR, NEH) %>% summarise(GL = sum(LIB))
datos.NEHyGL <- datos.NEHyGL %>% ungroup() %>% select(2, 3)

# PASO 1: Ejecutar MD_EGHE_2019.R para generar el dataframe "fichero_salida"
# --------------------------------------------------------------------------

# PASO 2: Incluimos las librerías necesarias
# ------------------------------------------

library(dplyr)

# PASO 3: Manipulamos el dataframe para obtener las variables de estudio
# ----------------------------------------------------------------------

# Guardamos en una variable las columnas que nos interesan:
datos <- fichero_salida %>% select(5,57) %>% filter(!is.na(EHOGAR) & !is.na(GTT))