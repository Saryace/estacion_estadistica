
# Instalacion de paquetes -------------------------------------------------
# los paquetes se instalas solo una vez

# install.packages("tidyverse", "broom")

# Librerias ---------------------------------------------------------------
# Una vez instaladas, llamamos a cada libreria usando library()

library(tidyverse) 
library(broom) # libreria para ordenar datos estadisticos

# Info datos --------------------------------------------------------------

# Datos ficticios de peso de galletas y notas de estudiantes

# Cargar datos ------------------------------------------------------------

# Importamos los datos tipo .csv usando readr::read_csv() 
# creamos dos objeto llamados oreos y notas estudiantes

oreos <-
  read_csv("03_clase/data/data_oreos.csv")

notas_estudiantes <-
  read_csv("03_clase/data/notas_estudiantes.csv")

# glimpse -----------------------------------------------------------------

# siempre es una buena practica mirar los datos
# glimpse() permite ver numero de fila, columnas y tipos de datos

glimpse(oreos)

glimpse(notas_estudiantes)

# shapiro.test ------------------------------------------------------------

# test de normalidad, revisemos los argumentos de la funcion

help("shapiro.test")

# requiere x	= a numeric vector of data values = solo una columna

# entrega una lista con: statistic, p.value, method y data.name

# ejemplo shapiro.test ----------------------------------------------------

info_mascotas <- data.frame("edad" = c(1,12,11,2,1,2,3,4,5,5,5,6,6,6,6,8,11,12),
                            "pulgas" = c(T,T,T,F,F,F,F,T,F,T,F,F,F,F,T,F,F,F))

shapiro.test(info_mascotas$edad)

# acceder info shapiro.test -----------------------------------------------

shapiro.test(info_mascotas$edad)$p.value

shapiro.test(info_mascotas$edad)$statistic

shapiro.test(info_mascotas$edad)$method

shapiro.test(info_mascotas$edad)$data.name

estadistica_mascotas <- shapiro.test(info_mascotas$edad)

# que pasa si el argumento no es solo 1 columna? --------------------------

# shapiro.test(info_mascotas) 

# Error in shapiro.test(info_mascotas) : is.numeric(x) is not TRUE

# revisemos la funcion t.test ---------------------------------------------

# prueba t-student, revisemos los argumentos de la funcion

help("t.test")

# argumento x	: a (non-empty) numeric vector of data values.
# argumento y : an optional (non-empty) numeric vector of data values.
# mu: a number indicating the true value of the mean 
# (or difference in means if you are performing a two sample test).
# paired: a logical indicating whether you want a paired t-test.

# Revisemos el caso Oreo --------------------------------------------------

t.test(oreos$peso_empaque, mu = 154)

t.test(oreos$peso_empaque, mu = 154)$p.value

t.test(oreos$peso_empaque, mu = 154)$method


# Veamos errores ----------------------------------------------------------

t.test(oreos) # alternative hypothesis: true mean is not equal to 0

t.test(oreos$peso_empaque) # alternative hypothesis: true mean is not equal to 0


# Veamos test de normalidad en notas --------------------------------------

notas_estudiantes %>% 
  head(6) # vemos en consola las primeras 6 filas para ver estructura datos

notas_estudiantes %>% 
  tail(6) # vemos en consola las ultimas 6 filas para ver estructura datos


# Como shapiro.test() requiere UNA columna, lo puedo hacer por separado

shapiro.test(notas_estudiantes$prueba_a)

shapiro.test(notas_estudiantes$prueba_a)$p.value

shapiro.test(notas_estudiantes$prueba_b)

shapiro.test(notas_estudiantes$prueba_b)$p.value


# Veamos t.test -----------------------------------------------------------

# Como el formato es wide, ya tenemos dos columnas

t.test(notas_estudiantes$prueba_a, notas_estudiantes$prueba_b, paired = TRUE)$p.value

# En este caso, nos sirve el formato long ---------------------------------

notas_estudiantes %>% 
  pivot_longer(
    cols = c(prueba_a,prueba_b),
    names_to = "prueba",
    values_to = "notas"
  )

notas_estudiantes %>% 
  pivot_longer(
    cols = c(-colegio,-id),
    names_to = "prueba",
    values_to = "notas"
  )

# Hacemos shapiro agrupando -----------------------------------------------

notas_estudiantes %>% 
  pivot_longer(
    cols = c(prueba_a,prueba_b),
    names_to = "prueba",
    values_to = "notas"
  ) %>% 
  group_by(prueba, colegio) %>% # group by puede ser con varias variables
  summarise(p_value_normalidad = shapiro.test(notas)$p.value)


# Agrupar por colegio  ----------------------------------------------------

notas_estudiantes %>% 
  pivot_longer(
    cols = c(prueba_a,prueba_b),
    names_to = "prueba",
    values_to = "notas"
  ) %>% 
  group_by(colegio, prueba) %>% 
  summarise(p_value_normalidad = shapiro.test(notas)$p.value)

# Ahora hacemos t.test() --------------------------------------------------

notas_estudiantes %>% 
  group_by(colegio) %>% 
  summarise(p_value_t.test = t.test(prueba_a, prueba_b,
                                    paired = TRUE)$p.value)

# evaluacion_notas --------------------------------------------------------

evaluacion_notas <-
  notas_estudiantes %>%
  group_by(colegio) %>%
  summarise(p_value_t.test = t.test(prueba_a, prueba_b,
                                    paired = TRUE)$p.value)

evaluacion_notas %>% 
  mutate("diferencia significativa" = case_when(p_value_t.test < 0.05  ~ "*",
                                                p_value_t.test >= 0.05  ~ "N.S."))

# Formato long nos sirve para plotear -------------------------------------

notas_estudiantes %>% 
  pivot_longer(
    cols = c(prueba_a,prueba_b),
    names_to = "prueba",
    values_to = "notas"
  ) %>% 
  ggplot(aes(x = prueba, # eje x que tiene dos categorias
             y = notas)) + # eje y numerico
  geom_boxplot() # tipo de plot

# Podemos graficar por grupos ---------------------------------------------

notas_estudiantes %>% 
  pivot_longer(
    cols = c(prueba_a,prueba_b),
    names_to = "prueba",
    values_to = "notas"
  ) %>% 
  ggplot(aes(x = prueba, # eje x que tiene dos categorias
             y = notas)) + # eje y
  geom_boxplot() + # tipo de plot
  facet_grid(.~ colegio)

# Formato wide no nos deja plotear ----------------------------------------

notas_estudiantes %>% 
  ggplot(aes(x = id, # eje x que tiene categorias
             y = prueba_a)) + # eje y
  geom_boxplot()

notas_estudiantes %>% 
  ggplot(aes(x = prueba_a, # eje x que tiene dos categorias
             y = prueba_b)) + # eje y
  geom_point() +
  geom_smooth()

# ggplot nos deja cambiar colores y formas --------------------------------

notas_estudiantes %>% 
  pivot_longer(
    cols = c(prueba_a,prueba_b),
    names_to = "prueba",
    values_to = "notas"
  ) %>% 
  ggplot(aes(x = prueba, # eje x que tiene dos categorias
             y = notas, # eje y
             color = prueba)) + # color diferente segun prueba
  geom_boxplot() + # tipo de plot
  facet_grid(.~ colegio) + # dividimos el plot por colegio
  labs(x = "Tipo de prueba efectuada (a ó b)", # texto eje x
       y = "Nota (en escala 1 al 7)", # texto eje y
       title = "Comparación pruebas (a ó b) en Colegio Sol y Luna", # texto titulo
       subtitle = "Número de estudiantes por colegio = 100") + # texto subtitulo
  scale_x_discrete(labels = c("prueba a",'prueba b')) + # texto categorias de x
  theme_bw() + # estilo de plot
  theme(legend.position = "none",
        strip.text.x = element_text(size = 15)) # remover legenda



