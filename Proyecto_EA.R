#####################################################################################
# 
# TRABAJO FINAL DE ESTADÍSTICA APLICADA
#
# Curso 2021-22
# Autor: Beatriz Espinar Aragón
# DNI: 51139183V
#
# Nota:
# El contenido del fichero de datos, los pasos necesarios para obtener el dataframe
# y otra información relevante quedan explicados en la memoria del proyecto
#
#####################################################################################


# PASO 1: Ejecutar MD_EGHE_2019.R para generar el dataframe "fichero_salida"
# --------------------------------------------------------------------------

# PASO 2: Incluimos las librerías necesarias
# ------------------------------------------

library(dplyr)

# PASO 3: Manipulamos el dataframe para obtener las variables de estudio
# ----------------------------------------------------------------------

# Guardamos en una variable las columnas que nos interesan:
datos <- fichero_salida %>% select(5,57) %>% filter(!is.na(EHOGAR) & !is.na(GTT))
