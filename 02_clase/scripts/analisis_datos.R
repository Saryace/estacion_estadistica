
# Instalacion de paquetes -------------------------------------------------
# los paquetes se instalas solo una vez

# install.packages(c("readxl", "fs", "broom"))

# Librerias ---------------------------------------------------------------
# Una vez instaladas, llamamos a cada libreria usando library()

library(tidyverse) 
library(readxl) # libreria para leer archivos excels
library(fs) # libreria para leer directorios
library(broom) # libreria para ordenar datos estadisticos

# Info datos --------------------------------------------------------------

# Descargados desde:
# https://statistics.cepal.org/portal/cepalstat/dashboard.html?theme=3&lang=es

# Usaremos dos fuentes de información:
# 1. Total emisiones de todos los paises de Latam
# 2. Detalle de material particulado (pm2.5) por pais

# Cargar datos ------------------------------------------------------------

# Importamos los datos desde Excel usando read_excel
# esta funcion necesita el directorio indicando donde esta el archivo
# la funcion lee la primera hoja a menos que le indiquemos lo contrario
# usando el argumento "sheet". Para mas detalle: help(read_excel)
# creamos un objeto llamado calidad_aire_latam

calidad_aire_latam <-
  read_excel("02_clase/data/calidad_aire_latinoamerica.xlsx")

# Leer documentos paises seleccionados ------------------------------------

# Como tenemos varios Excels con la misma estructura
# podemos automatizar leerlos + unirlos por filas en un solo objeto

# Primero, usamos dir_ls para crear los directorios de cada Excel
# dir_ls necesita el directorio de la carpeta + una expresion regular
# "\\.xlsx$" significa "todo lo que termine en .xlsx"
# el objeto lista_excels_emision es un vector de directorios

lista_excels_emision <- fs::dir_ls("02_clase/data/emision_dioxido_por_pais",
                        regexp = "\\.xlsx$") # lista de excels

# Ahora automatizamos usando map_dfr
# map_dfr lee usando la funcion que se le indique (aca read_excel),
# y luego pega las filas de cada Excel en un nuevo objeto

emision_paises <-
lista_excels_emision %>% 
  map_dfr(read_excel)

# el objeto emision_paises contiene la informacion de todos
# los Excel de la carpeta emision_dioxido_por_pais

# glimpse calidad aire ----------------------------------------------------
# siempre es una buena practica mirar los datos
# glimpse() permite ver numero de fila, columnas y tipos de datos

glimpse(calidad_aire_latam)

# glimpse emision paises seleccionados ------------------------------------

glimpse(emision_paises) # resumen en consola

# hay dos problemas a solucionar:
# 1. el nombre de las variables no es consistente
# 2. la variable Años__ESTANDAR fue leida como <chr> o texto
# los anios deberian ser tipo numerico

# Hacemos una limpieza de datos -------------------------------------------

calidad_aire_latam_limpio <-
calidad_aire_latam %>% 
  # selecciono las variables de importancia
  select(País__ESTANDAR, Años__ESTANDAR, value) %>% 
  # le cambio el nombre variables de importancia
  rename(pais = País__ESTANDAR, 
         anio = Años__ESTANDAR, 
         pm2.5 = value) %>% 
  # creo una nueva columna anio pasando los anios a numeric
  mutate(anio = as.numeric(anio)) 

emision_paises_limpio <- 
emision_paises %>% 
  select(País__ESTANDAR, Años__ESTANDAR, value) %>% 
  rename(pais = País__ESTANDAR,
         anio = Años__ESTANDAR, 
         emision_porc = value) %>% 
  mutate(anio = as.numeric(anio))

# Nos piden comparar emision y calidad de los paises seleccionados --------

# tenemos dos objetos con informacion los cuales deben ser unidos
# para hacer comparaciones
# como vimos, operaciones join unen datasets.

# Primer ejemplo: inner join
# las lineas de codigo 103 a 106 unen ambos dataset usando como ID
# el pais y el anio

calidad_emision_inner <- 
  emision_paises_limpio %>% 
  inner_join(calidad_aire_latam_limpio,
            by = c("pais", "anio"))

# Segundo ejemplo: left join
# las lineas de codigo 112 a 115 unen ambos dataset usando como ID
# el pais y el anio

calidad_emision_left <-
emision_paises_limpio %>% 
  left_join(calidad_aire_latam_limpio,
             by = c("pais", "anio"))

# El nuevo objeto calidad_emision_inner no tiene valores ausentes (NA)
# El nuevo objeto calidad_emision_left tiene valores ausentes (NA)

# esto se debe a que los inner join mantienen filas con datos completos
# mientras que left_join mantiene filas y rellena con NA

# No hay un dataset correcto, solo son diferentes enfoques 
# para el analisis de datos

# Ahora con el dataset armado, comenzamos con estadistica -----------------
# Usaremos el dataset inner -----------------------------------------------

estadistica_descriptiva <-
calidad_emision_inner %>% 
  # queremos hacer estadistica por pais, el primer paso es agrupar
  group_by(pais) %>% 
  # luego usamos summarise_at, donde debemos indicar que variables:
  # vars() queremos resumir. Luego hacemos una lista de funciones 
  # que se aplicaran (mean, median, ...)
  summarise_at(vars(emision_porc,pm2.5),
               list(mean = mean, median = median,
                    IQR = IQR, sd = sd,
                    var = var))

# aca hacemos lo mismo pero agrupando por anio
estadistica_descriptiva_por_anio <-
  calidad_emision_inner %>% 
  group_by(anio) %>% 
  summarise_at(vars(emision_porc,pm2.5),
               list(mean = mean, median = median,
                    IQR = IQR, sd = sd,
                    var = var))

# Manipulamos datos estadisticos ------------------------------------------

# una vez calculados los estadisticos descriptivos podemos seguir analizando
# por ejemplo arrange() reordena y desc() indica order descendiente
# arrange(desc()) ordena de mayor a menor

estadistica_descriptiva %>% 
  arrange(desc(emision_porc_mean))

estadistica_descriptiva_por_anio %>% 
  arrange(desc(pm2.5_mean))

# Pensemos en un informe --------------------------------------------------

# en algun momento queremos describir los resultados en tablas o pdf
# podemos crear texto en base a numeros

estadistica_descriptiva %>% 
  arrange(desc(emision_porc_mean)) %>% 
  # creamos una columna que se llama promedio ± desviación estándar
  # usamos la funcion paste0() que pega data de las columnas que indiquemos
  mutate("promedio ± desviación estándar" =
          paste0(emision_porc_mean," ± ",emision_porc_sd)) %>% 
  select(pais, "promedio ± desviación estándar") 

# el resultado del codigo anterior tiene muchas cifras
# podemos usar la funcion round en una funcion mutate para cambiarlos

estadistica_descriptiva %>% 
  arrange(desc(emision_porc_mean)) %>% 
  # a mutate_at hay que indicarle que columnas cambiaremos
  # en este caso emision_porc_mean y pm2.5_var
  # luego indicamos la funcion "round" y el numero de cifras (3)
  mutate_at(vars(c(emision_porc_mean,emision_porc_sd)), round, 3) %>% 
  # ahora hacemos las mismas operaciones de mas arriba
  mutate("promedio ± desviación estándar" =
           paste0(emision_porc_mean," ± ",emision_porc_sd)) %>% 
  select(pais, "promedio ± desviación estándar") 
  
# Un avance de las proximas clases ----------------------------------------
# aca solo unos ejemplos de funciones mas avanzadas 

calidad_emision_inner %>% 
  # pasamos de formato wide a long
  pivot_longer(-c(pais,anio), 
               names_to = "variable",
               values_to = "valor") %>% 
  # graficamos usando ggplot
  ggplot(aes(x = pais, y = valor, color = pais)) +
  geom_boxplot() +
  facet_wrap(.~ variable, scales = "free") +
  coord_flip()

calidad_emision_inner %>% 
  # pasamos de formato wide a long
  pivot_longer(-c(pais,anio), 
               names_to = "variable",
               values_to = "valor") %>% 
  # graficamos usando ggplot
  ggplot(aes(x = anio, y = valor, color = pais)) +
  geom_line() +
  facet_wrap(.~ variable, scales = "free")


# Avance lm() -------------------------------------------------------------
# esto es lo que veremos las subsiguientes clases
# codigo solo de ejemplo

modelos_lm <-
calidad_emision_inner %>% 
  # creamos listas por paises
  nest(data = -pais) %>% 
  # creamos un modelo lineal para cada pais (modelo)
  # creamos un modelo limpio (tidy_modelo)
  # extraemos metricas (glance_modelo)
  mutate(modelo = map(data,
                      ~lm(pm2.5 ~
                          emision_porc, data = .)),
         tidy_modelo = map(modelo, tidy),
         glance_modelo = map(modelo, glance))

# extraemos el detalle de tidy_modelo
# aca podemos ver el intercepto y coeficientes de la regresion
modelos_lm %>% 
  unnest(tidy_modelo)

# extraemos el detalle de glance_modelo
# aca podemos ver el R cuadrado y error residual de cada modelo
modelos_lm %>% 
  unnest(glance_modelo)






