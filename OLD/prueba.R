# para leer el fichero de datos hay que ejecutar MD_EGHE_2019.R
# esto generará un data frame

# instalar packages
install.packages("dplyr")
install.packages("janitor")
install.packages("ggplot2")
install.packages("fdth")
install.packages("psych")
install.packages("moments")
install.packages("ggrepel")
install.packages("tidyverse")
#install.packages("car")
  
# librerías
library(dplyr)
library(janitor)
library(ggplot2)
library(fdth)
library(psych)
library(moments)
library(ggrepel)
library(tidyverse)
#library(car)

# FUNCIONES

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
    rel <- c(rel, as.numeric(vec_abs[i] / n.GE))
  return(rel)
}

# mete los datos del vector en un tibble que crea
ObtenerTibbleFR <- function(vec, tib) {
  tib <- cbind(tib, 'f.r.' = vec)
  return(tib)
}

# calcula la moda (paramétro = vector)
CalcularModa <- function(v) {
  which.max(tabulate(v))
}

# calcula el coeficiente de variación
CalcularCoefVar <- function(desv.tip, media) {
  return(desv.tip / media)
}

# calcula la cuasivarianza
CalcularCuasivar <- function(var, n) {
  return(var * n / (n - 1))
}

# calcula la suma de los números combinatorios con n = v[1,..,n], k = k
SumaNumCombinatorios <- function(v, k) {
  sum <- 0
  for (i in 1:length(v))
    sum <- sum + choose(v[i], k)
  return(sum)
}

# calcula la probabilidad condicionada P(A/B), donde A = (1000 <= GE <= 2000), B = (NEH = i)
ProbCondicionadaGE <- function(df1, df2, i) {
  # P(A n B)
  c.fav.aux1 <- as.numeric(count(df1 %>% filter(NEH == i & GE >= 1000 & GE <= 2000)))
  c.pos.aux1 <- n.NEHyGE
  prob.aux1 <- c.fav.aux1 / c.pos.aux1
  # P(B)
  c.fav.aux2 <- as.numeric(count(df2 %>% filter(NEH == i)))
  c.pos.aux2 <- n.NEHyGE.NEH
  prob.aux2 <- c.fav.aux2 / c.pos.aux2
  # P(A/B)
  prob <- prob.aux1 / prob.aux2
  return(prob)
}

# calcula la probabilidad condicionada P(A/B), donde A = (GL > media.GL), B = (NEH = 2)
ProbCondicionadaGL <- function(df1, df2) {
  # P(A n B)
  c.fav.aux1 <- count(df1 %>% filter(NEH == 2 & GL > media.GL))
  c.pos.aux1 <- n.NEHyGL
  prob.aux1 <- c.fav.aux1 / c.pos.aux1
  # P(B)
  c.fav.aux2 <- count(df2 %>% filter(NEH == 2))
  c.pos.aux2 <- n.NEHyGL.NEH
  prob.aux2 <- c.fav.aux2 / c.pos.aux2
  # P(A/B)
  prob <- prob.aux1 / prob.aux2
  return(as.numeric(prob))
}

# calcula la probabilidad de que NEH = i
ProbNEH <- function(df, i) {
  c.fav <- as.numeric(count(df %>% filter(NEH == i)))
  c.pos <- n.NEH
  prob <- c.fav / c.pos
  return(prob)
}


# --------------------------------------------------------------------------------
# --------------------------------------------------------------------------------

# declaramos variable del dataframe
datos <- fichero_salida


# VARIABLES QUE NECESITAREMOS:
# ----------------------------
# · NPH = Número de Personas por Hogar
# · NEH = Número de Estudiantes por Hogar
# · GE = Gasto total en Educación
# · GL = Gasto total en Libros


# cogemos los dataframes de las variables que nos interesan por separado


# NEH

# nos quedamos con la col 2 (ID) y la 5 (NEH), y eliminamos las filas que sean NA
df.NEH <- datos %>% select(2,5) %>% filter(!is.na(EHOGAR))

# pasamos EHOGAR de char a numeric
df.NEH$NEH <- as.numeric(df.NEH$EHOGAR)
df.NEH <- df.NEH %>% select(1,3)

# agrupamos por hogar (hay más de una fila por hogar)
df.NEH <- df.NEH %>% group_by(IDHOGAR)
df.NEH <- df.NEH %>% ungroup() %>% select(2)


# GE

# nos quedamos con la col 2 (ID) y la 57 (GE) y eliminamos las filas que sean NA
df.GE <- datos %>% select(2,57) %>% filter(!is.na(GTT))

# agrupamos por hogar (hay más de una fila por hogar) y recalculamos GE
df.GE <- df.GE %>% group_by(IDHOGAR) %>% summarise(GE = sum(GTT))
df.GE <- df.GE %>% ungroup() %>% select(2)


# GA

# nos quedamos con la col 23 y eliminamos las filas que sean NA
df.GA <- datos %>% select(23) %>% filter(!is.na(C13A))


# cogemos los dataframes de las variables que nos interesan juntas


# NEH Y GE (regresión fallida)

# nos quedamos con la col 2 (ID), la 5 (NEH) y la 57 (GE) y eliminamos las filas que sean NA
df.NEHyGE <- datos %>% select(2,5,57) %>% filter(!is.na(EHOGAR) & !is.na(GTT))

# pasamos EHOGAR de char a numeric
df.NEHyGE$NEH <- as.numeric(df.NEHyGE$EHOGAR)
df.NEHyGE <- df.NEHyGE %>% select(1,3,4)

# agrupamos por hogar (hay más de una fila por hogar) y recalculamos GE
df.NEHyGE <- df.NEHyGE %>% group_by(IDHOGAR,NEH) %>% summarise(GE = sum(GTT))
df.NEHyGE <- df.NEHyGE %>% ungroup() %>% select(2,3)

# df de cada uno
df.NEHyGE.NEH <- df.NEHyGE %>% select(1)
df.NEHyGE.GE <- df.NEHyGE %>% select(2)


# NPH Y NEH (regresión)

# nos quedamos con la col 2 (ID), la 4 (NPH) y la 5 (NEH) y eliminamos las filas que sean NA
df.NPHyNEH <- datos %>% select(2,4,5) %>% filter(!is.na(NHOGAR) & !is.na(EHOGAR))

# pasamos NHOGAR de char a numeric
df.NPHyNEH$NPH <- as.numeric(df.NPHyNEH$NHOGAR)
df.NPHyNEH <- df.NPHyNEH %>% select(1,3,4)

# pasamos EHOGAR de char a numeric
df.NPHyNEH$NEH <- as.numeric(df.NPHyNEH$EHOGAR)
df.NPHyNEH <- df.NPHyNEH %>% select(1,3,4)

# agrupamos por hogar (hay más de una fila por hogar)
df.NPHyNEH <- df.NPHyNEH %>% group_by(IDHOGAR)
df.NPHyNEH <- df.NPHyNEH %>% ungroup() %>% select(2,3)

# df de cada uno
df.NPHyNEH.NPH <- df.NPHyNEH %>% select(1)
df.NPHyNEH.NEH <- df.NPHyNEH %>% select(2)


# NEH Y GL (probabilidad)

# nos quedamos con la col 2 (ID), la 5 (NEH) y la 44 (GL) y eliminamos las filas que sean NA
df.NEHyGL <- datos %>% select(2,5,44) %>% filter(!is.na(EHOGAR) & !is.na(LIB))

# pasamos EHOGAR de char a numeric
df.NEHyGL$NEH <- as.numeric(df.NEHyGL$EHOGAR)
df.NEHyGL <- df.NEHyGL %>% select(1,3,4)

# cambiamos el nombre LIB -> GL
df.NEHyGL$GL <- df.NEHyGL$LIB
df.NEHyGL <- df.NEHyGL %>% select(1,3,4)

# agrupamos por hogar (hay más de una fila por hogar)
df.NEHyGL <- df.NEHyGL %>% group_by(IDHOGAR)
df.NEHyGL <- df.NEHyGL %>% ungroup() %>% select(2,3)

# df de cada uno
df.NEHyGL.NEH <- df.NEHyGL %>% select(1)
df.NEHyGL.GL <- df.NEHyGL %>% select(2)


# obtenemos los tibbles de las variables que nos interesan por separado

# NEH
tib.NEH <- as_tibble(df.NEH)

# GE
tib.GE <- as_tibble(df.GE)

# GA
tib.GA <- as_tibble(df.GA)


# obtenemos los tibbles de las variables que nos interesan juntas

# NEH Y GE (regresión fallida)
tib.NEHyGE <- as_tibble(df.NEHyGE)
tib.NEHyGE.NEH <- as_tibble(df.NEHyGE$NEH)
tib.NEHyGE.GE <- as_tibble(df.NEHyGE$GE)

# NPH Y NEH (regresión)
tib.NPHyNEH <- as_tibble(df.NPHyNEH)
tib.NPHyNEH.NPH <- as_tibble(df.NPHyNEH$NPH)
tib.NPHyNEH.NEH <- as_tibble(df.NPHyNEH$NEH)

# NEH Y GL (probabilidad)
tib.NEHyGL <- as_tibble(df.NEHyGL)
tib.NEHyGL.NEH <- as_tibble(df.NEHyGL$NEH)
tib.NEHyGL.GL <- as_tibble(df.NEHyGL$GL)


# obtenemos los vectores de las variables que nos interesan por separado

# NEH
vec.NEH <- ObtenerVector(tib.NEH[1])

# GE
vec.GE <- ObtenerVector(tib.GE[1])

# GA
vec.GA <- ObtenerVector(tib.GA[1])


# obtenemos los vectores de las variables que nos interesan juntas

# NEH Y GE (regresión fallida)
vec.NEHyGE.NEH <- ObtenerVector(tib.NEHyGE.NEH[1])
vec.NEHyGE.GE <- ObtenerVector(tib.NEHyGE.GE[1])

# NPH Y NEH (regresión)
vec.NPHyNEH.NPH <- ObtenerVector(tib.NPHyNEH.NPH[1])
vec.NPHyNEH.NEH <- ObtenerVector(tib.NPHyNEH.NEH[1])

# NEH Y GL (probabilidad)
vec.NEHyGL.NEH <- ObtenerVector(tib.NEHyGL.NEH[1])
vec.NEHyGL.GL <- ObtenerVector(tib.NEHyGL.GL[1])


# n de cada variable por separado

# NEH
n.NEH <- as.numeric(count(df.NEH))

# GE
n.GE <- as.numeric(count(df.GE))


# n de cada variable que nos interesa junto a otra

# NEH Y GE (regresión fallida)
n.NEHyGE <- as.numeric(count(df.NEHyGE))
n.NEHyGE.NEH <- as.numeric(count(df.NEHyGE.NEH))
n.NEHyGE.GE <- as.numeric(count(df.NEHyGE.GE))

# NPH Y NEH (regresión)
n.NPHyNEH <- as.numeric(count(df.NPHyNEH))
n.NPHyNEH.NPH <- as.numeric(count(df.NPHyNEH.NPH))
n.NPHyNEH.NEH <- as.numeric(count(df.NPHyNEH.NEH))

# NEH Y GL (probabilidad)
n.NEHyGL <- as.numeric(count(df.NEHyGL))
n.NEHyGL.NEH <- as.numeric(count(df.NEHyGL.NEH))
n.NEHyGL.GL <- as.numeric(count(df.NEHyGL.GL))



# T1: ESTADÍSTICA DESCRIPTIVA
# ---------------------------


# TABLA DE FRECUENCIAS DE NEH

# frecuencias abs
freq.abs.NEH <- table(df.NEH)

# frecuencias rel
freq.rel.NEH <- prop.table(freq.abs.NEH)

# pasamos a tibble
freq.abs.NEH <- as_tibble(freq.abs.NEH)
freq.abs.NEH <- rename(freq.abs.NEH, NEH = df.NEH, f.a. = n)
freq.rel.NEH <- as_tibble(freq.rel.NEH)
freq.rel.NEH <- rename(freq.rel.NEH, NEH = df.NEH, f.r. = n)

# frecuencias acumuladas
freq.abs.acc.NEH <- cumsum(freq.abs.NEH[2])
freq.abs.acc.NEH <- rename(freq.abs.acc.NEH, f.a.a. = f.a.)
freq.rel.acc.NEH <- cumsum(freq.rel.NEH[2])
freq.rel.acc.NEH <- rename(freq.rel.acc.NEH, f.r.a. = f.r.)

# creamos tabla de frecuencias
tabla.freq.NEH <- bind_cols(freq.abs.NEH[1], freq.abs.NEH[2], freq.abs.acc.NEH, freq.rel.NEH[2], freq.rel.acc.NEH)

# obtenemos vectores que necesitaremos
vec.fa.NEH <- ObtenerVector(tabla.freq.NEH[2])
vec.fr.NEH <- ObtenerVector(tabla.freq.NEH[4])


# TABLA DE FRECUENCIAS DE GE

# creamos intervalos y frecuencias abs
intervalos <- c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000, 15000, 20000, 30000, 55000)
intervalos.GE <- hist(vec.GE, breaks = intervalos, include.lowest = TRUE, right = FALSE, plot = FALSE)
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

# obtenemos vectores que necesitaremos
vec.fa.GE <- ObtenerVector(tabla.freq.GE[2])
vec.fr.GE <- FrecuenciasRelativas(vec.fa.GE)

# frecuencias rel
freq.rel.GE <- tibble(aux = 1:(length(intervalos.GE[[1]]) - 1))
freq.rel.GE <- ObtenerTibbleFR(vec.fr.GE, freq.rel.GE)
freq.rel.GE <- freq.rel.GE %>% select(2)
freq.rel.GE <- as_tibble(freq.rel.GE)

# frecuencias rel acumuladas
freq.rel.acc.GE <- cumsum(freq.rel.GE)
freq.rel.acc.GE <- rename(freq.rel.acc.GE, f.r.a. = f.r.)

# terminamos tabla de frecuencias
tabla.freq.GE <- bind_cols(tabla.freq.GE, freq.abs.acc.GE, freq.rel.GE, freq.rel.acc.GE)


# MEDIDAS DE POSICIÓN NEH

# media NEH
media.NEH <- mean(vec.NEH)

# mediana NEH (percentil 50%)
mediana.NEH <- median(vec.NEH)

# moda NEH
moda.NEH <- CalcularModa(vec.NEH)

# percentil 25% NEH
perc25.NEH <- quantile(vec.NEH, probs = 1/4)

# percentil 75% NEH
perc75.NEH <- quantile(vec.NEH, probs = 3/4)


# MEDIDAS DE POSICIÓN GE

# media GE
media.GE <- mean(vec.GE)

# mediana GE (percentil 50%)
mediana.GE <- median(vec.GE)

# intervalo modal GE
max.freq.abs.GE <- max(vec.fa.GE)
intervalo.modal.GE <- tabla.freq.GE %>% select(1) %>% filter(tabla.freq.GE[2] == max.freq.abs.GE)
intervalo.modal.GE <- rename(intervalo.modal.GE, Moda = GE)

# percentil 25% GE
perc25.GE <- quantile(vec.GE, probs = 1/4)

# percentil 75% GE
perc75.GE <- quantile(vec.GE, probs = 3/4)


# MEDIDAS DE DISPERSIÓN NEH

# varianza NEH
var.NEH <- var(vec.NEH)

# cuasivarianza NEH
cuasivar.NEH <- CalcularCuasivar(var.NEH, n.NEH)

# desviación típica NEH
desv.tip.NEH <- sd(vec.NEH)

# cuasidesviación típica NEH
cuasidesv.tip.NEH <- sqrt(cuasivar.NEH)

# coeficiente de variación NEH
coef.var.NEH <- CalcularCoefVar(desv.tip.NEH, media.NEH)


# MEDIDAS DE DISPERSIÓN GE

# varianza GE
var.GE <- var(vec.GE)

# cuasivarianza GE
cuasivar.GE <- CalcularCuasivar(var.GE, n.GE)

# desviación típica GE
desv.tip.GE <- sd(vec.GE)

# cuasidesviación típica GE
cuasidesv.tip.GE <- sqrt(cuasivar.GE)

# coeficiente de variación GE
coef.var.GE <- CalcularCoefVar(desv.tip.GE, media.GE)


# MEDIDAS DE FORMA NEH

# coeficiente de asimetría NEH
coef.asim.NEH <- skewness(vec.NEH)

# coeficiente de kurtosis NEH
coef.kurt.NEH <- kurtosis(vec.NEH)


# MEDIDAS DE FORMA GE

# coeficiente de asimetría GE
coef.asim.GE <- skewness(vec.GE)

# coeficiente de kurtosis GE
coef.kurt.GE <- kurtosis(vec.GE)


# GRÁFICOS NEH

# obtenemos los vectores necesarios
vec.val.NEH <- ObtenerVector(tabla.freq.NEH[1])
freq.rel.porc.NEH <- round(tabla.freq.NEH[4] * 100,2)
vec.fr.porc.NEH <- ObtenerVector(freq.rel.porc.NEH[1])

# obtenemos el data frame NEH para representarlo
df.NEH.db <- data.frame(value = vec.fa.NEH,
                        NEH = vec.val.NEH,
                        rel = vec.fr.NEH,
                        porcentaje = vec.fr.porc.NEH)

# colores
colores <- c("#005200", "#007000", "#258d19", "#71c55b", "#92e27a", "#b4ff9a")

# diagrama de barras NEH
diag.bar.NEH <- ggplot(df.NEH.db, aes(x = NEH, y = value)) +
                  geom_bar(stat = "identity", fill = "#005c00") +
                  xlab("") +
                  ylab("") + 
                  ggtitle("Diagrama de barras del número de estudiantes por hogar") +
                  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6")) +
                  geom_text(aes(label = value), vjust = -1, colour = "black") +
                  ylim(c(0, 4500))


# diagrama de sectores NEH
df.NEH.ds <- df.NEH.db %>%
              arrange(NEH) %>%
              mutate(lab.ypos = cumsum(porcentaje) - porcentaje/2)

df.NEH.ds2 <- df.NEH.ds %>% mutate(csum = rev(cumsum(rev(value))), 
                                   pos = value/2 + lead(csum, 1),
                                   pos = if_else(is.na(pos), value/2, pos))

diag.sec.NEH <- ggplot(data = df.NEH.ds, aes(x = "", y = value, fill = factor(NEH))) +
                  geom_col(color = "black") +
                  coord_polar(theta = "y", start = 0) +
                  geom_label_repel(data = df.NEH.ds2,
                          aes(y = pos, label = paste0(porcentaje, "%")),
                          size = 4, nudge_x = 1, show.legend = FALSE) +
                  scale_fill_manual(values = colores) +
                  guides(fill = guide_legend(title = "NEH")) +
                  ggtitle("Diagrama de sectores del número de estudiantes por hogar") +
                  theme_void()


# GRÁFICOS GE

# histograma y polígono de frecuencias (f.a.)
hist.GE <- ggplot(df.GE, aes(vec.GE)) + 
              geom_histogram(color = 1, fill = "#005c00",
                             breaks = intervalos) +
              geom_freqpoly(data = df.GE, breaks = intervalos, color = "red") + 
              xlab("") +
              ylab("") +
              ggtitle("Histograma y polígono de frecuencias del gasto en educación por hogar") +
              xlim(c(0, 56000))

# polígono de frecuencias (f.r.a)
pol.fra.GE = ggplot(df.GE, aes(vec.GE)) + 
              geom_step(stat = "ecdf", color = "#71c55b", lwd = 1) +
              xlab("") + 
              ylab("") +
              ggtitle("Polígono de frecuencias f.r.a.")


# TABLA DE FRECUENCIAS NEH Y GE

# tabla de frecuencias absolutas
tabla.freq.abs.NEHyGE <- table(vec.NEHyGE.NEH, vec.NEHyGE.GE = cut(vec.NEHyGE.GE, breaks = intervalos, right = FALSE))

# tabla de frecuencias relativas
tabla.freq.rel.NEHyGE <- prop.table(tabla.freq.abs.NEHyGE)

# añadimos marginales
tabla.freq.abs.NEHyGE <- addmargins(tabla.freq.abs.NEHyGE)
tabla.freq.rel.NEHyGE <- addmargins(tabla.freq.rel.NEHyGE)


# DIAGRAMA DE DISPERSIÓN NEH Y GE (NUBE DE PUNTOS)

diag.disp.NEHyGE <- ggplot(df.NEHyGE, aes(x = NEH, y = GE)) +
                      geom_point(color = "#71c55b") +
                      xlab("") + 
                      ylab("")

# parece que no tienen una relación lineal, calculamos coef correlación
coef.cor.NEHyGE <- cor(vec.NEHyGE.NEH, vec.NEHyGE.GE)


# NUEVA VARIABLE (REGRESIÓN)

# TABLA DE FRECUENCIAS NPH Y NEH

# tabla de frecuencias absolutas
tabla.freq.abs.NPHyNEH <- table(vec.NPHyNEH.NPH, vec.NPHyNEH.NEH)

# tabla de frecuencias relativas
tabla.freq.rel.NPHyNEH <- prop.table(tabla.freq.abs.NPHyNEH)

# añadimos marginales
tabla.freq.abs.NPHyNEH <- addmargins(tabla.freq.abs.NPHyNEH)
tabla.freq.rel.NPHyNEH <- addmargins(tabla.freq.rel.NPHyNEH)


# DIAGRAMA DE DISPERSIÓN NPH Y NEH (NUBE DE PUNTOS)

diag.disp.NPHyNEH <- ggplot(df.NPHyNEH, aes(x = NPH, y = NEH)) +
                      geom_point(color = "#71c55b") +
                      xlab("") + 
                      ylab("") +
                      ylim(c(0, 7)) +
                      ggtitle("Diagrama de dispersión Nº personas por hogar y Nº estudiantes por hogar")


# REGRESIÓN NPH Y NEH

reg.NPHyNEH <- lm(vec.NPHyNEH.NPH~vec.NPHyNEH.NEH, data = df.NPHyNEH)
reg.NPHyNEH <- summary(reg.NPHyNEH)

# guardamos resultados
a.NPHyNEH <- 2.622
b.NPHyNEH <- 0.637

# utilizamos regresión para predecir:
# y = 2.38759 + 0.66514x

# si viven 8 personas en un hogar, ¿cuántos serán estudiantes?
res <- a.NPHyNEH + b.NPHyNEH * 8

# covarianza
cov.NPHyNEH <- cov(vec.NPHyNEH.NPH, vec.NPHyNEH.NEH)

# coeficiente correlación lineal
coef.cor.NPHyNEH <- cor(vec.NPHyNEH.NPH, vec.NPHyNEH.NEH)

# coeficiente determinación
coef.det.NPHyNEH <- coef.cor.NPHyNEH ^ 2



# T2: PROBABILIDAD
# ----------------


# P(GE >= 1000) - Salario mínimo
c.fav.1 <- as.numeric(count(df.GE %>% filter(GE >= 1000)))
c.pos.1 <- n.GE
prob.1 <- c.fav.1 / c.pos.1

# P(GE >= 12000) - Salario mínimo x12
c.fav.2 <- as.numeric(count(df.GE %>% filter(GE >= 12000)))
c.pos.2 <- n.GE
prob.2 <- c.fav.2 / c.pos.2

# Dada la muestra de NEH, P de que al coger 3 hogares (sin remp) tengan
# 1,2 y 3 estudiantes
c.fav.3 <- choose(vec.fa.NEH[1], 1) * choose(vec.fa.NEH[2], 1) * choose(vec.fa.NEH[3], 1)
c.pos.3 <- choose(n.NEH, 3)
prob.3 <- c.fav.3 / c.pos.3

# Dada la muestra de NEH, P de que al coger 3 hogares (sin remp) tengan el mismo número
# de estudiantes
c.fav.4 <- SumaNumCombinatorios(vec.fa.NEH, 3)
c.pos.4 <- choose(n.NEH, 3)
prob.4 <- c.fav.4 / c.pos.4

# Calculamos la probabilidad de que en un hogar se gasten entre 1000 y 2000 (mediana = 1598)
# sabiendo que hay x estudiantes en ese hogar
prob.5 <- c()
for (i in 1:6)
  prob.5 <- c(prob.5, ProbCondicionadaGE(df.NEHyGE, df.NEHyGE.NEH, i))

# nº de estudiantes por hogar más probable de tener un gasto promedio: 2

# Calculamos la probabilidad de que en un hogar promedio (2 estudiantes) haya un gasto
# en libros mayor de la media
# calculamos media GL
media.GL <- mean(vec.NEHyGL.GL)
# P(A/B)
prob.6 <- ProbCondicionadaGL(df.NEHyGL, df.NEHyGL.NEH)

# Calculamos la probabilidad de que NEH = i (i in 1:6) para obtener la función de masa
prob.7 <- c()
for  (i in 1:6)
  prob.7 <- c(prob.7, ProbNEH(df.NEH, i))



# T3: VARIABLES ALEATORIAS Y MODELOS DE PROBABILIDAD
# --------------------------------------------------


# NEH: variable discreta --> FUNCIÓN DE MASA
func.mas.NEH <- function(x) {
  return(prob.7[x])
}

# función de distribución
func.dist.NEH <- function(x) {
  sum <- 0
  for (i in 1:x)
    sum <- sum + func.mas.NEH(i)
  return(sum)
}

# GE: variable continua --> FUNCIÓN DE DENSIDAD
func.dens.GE <-  ggplot(df.GE, aes(x = GE)) +
                    geom_density(color = "#71c55b",
                                 fill = "#71c55b",
                                 alpha = 0.25,
                                 lwd = 1) +
                    xlab("Gasto en educación") +
                    ylab("Densidad") +
                    ggtitle("Función de densidad del gasto en educación por hogar")


qqnorm(vec.GE)
qqline(vec.GE)
shapiro.test(vec.GE)

qqnorm(vec.GA)
qqline(vec.GA)
shapiro.test(vec.GA)

car::qqplot(vec.GE)

