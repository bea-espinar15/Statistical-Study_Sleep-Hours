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
