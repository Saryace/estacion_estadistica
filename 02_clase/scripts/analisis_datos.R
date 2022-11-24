
# Instalacion de paquetes -------------------------------------------------

# install.packages(c("readxl", "fs", "broom"))

# Librerias ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(fs)
library(broom)

# Info datos --------------------------------------------------------------

# Descargados desde:
# https://statistics.cepal.org/portal/cepalstat/dashboard.html?theme=3&lang=es

# Cargar datos ------------------------------------------------------------

calidad_aire_latam <-
  read_excel("02_clase/data/calidad_aire_latinoamerica.xlsx")

# Leer documentos paises seleccionados ------------------------------------

lista_excels_emision <- fs::dir_ls("02_clase/data/emision_dioxido_por_pais",
                        regexp = "\\.xlsx$")

emision_paises <-
lista_excels_emision %>% 
  map_dfr(read_excel)

# glimpse calidad aire ----------------------------------------------------

glimpse(calidad_aire_latam)

# glimpse emision paises seleccionados ------------------------------------

glimpse(emision_paises)

# Hacemos una limpieza de datos -------------------------------------------

calidad_aire_latam_limpio <-
calidad_aire_latam %>% 
  select(País__ESTANDAR, Años__ESTANDAR, value) %>% 
  rename(pais = País__ESTANDAR,
         anio = Años__ESTANDAR, 
         pm2.5 = value) %>% 
  mutate(anio = as.numeric(anio))

emision_paises_limpio <- 
emision_paises %>% 
  select(País__ESTANDAR, Años__ESTANDAR, value) %>% 
  rename(pais = País__ESTANDAR,
         anio = Años__ESTANDAR, 
         emision_porc = value) %>% 
  mutate(anio = as.numeric(anio))

# Nos piden comparar emision y calidad de los paises seleccionados --------

calidad_emision_inner <- 
  emision_paises_limpio %>% 
  inner_join(calidad_aire_latam_limpio,
            by = c("pais", "anio"))

calidad_emision_left <-
emision_paises_limpio %>% 
  left_join(calidad_aire_latam_limpio,
             by = c("pais", "anio"))

# Ahora con el dataset armado, comenzamos con estadistica -----------------
# Usaremos el dataset inner -----------------------------------------------

estadistica_descriptiva <-
calidad_emision_inner %>% 
  group_by(pais) %>% 
  summarise_at(vars(emision_porc,pm2.5),
               list(mean = mean, median = median,
                    IQR = IQR, sd = sd,
                    var = var))

estadistica_descriptiva_por_anio <-
  calidad_emision_inner %>% 
  group_by(anio) %>% 
  summarise_at(vars(emision_porc,pm2.5),
               list(mean = mean, median = median,
                    IQR = IQR, sd = sd,
                    var = var))

# Manipulamos datos estadisticos ------------------------------------------

estadistica_descriptiva %>% 
  arrange(desc(emision_porc_mean))

estadistica_descriptiva_por_anio %>% 
  arrange(desc(pm2.5_mean))

# Pensemos en un informe --------------------------------------------------

estadistica_descriptiva %>% 
  arrange(desc(emision_porc_mean)) %>% 
  mutate("promedio ± desviación estándar" =
          paste0(emision_porc_mean," ± ",emision_porc_sd)) %>% 
  select(pais, "promedio ± desviación estándar") 

estadistica_descriptiva %>% 
  arrange(desc(emision_porc_mean)) %>% 
  mutate_at(vars(emision_porc_mean:
                 pm2.5_var), round, 3) %>% 
  mutate("promedio ± desviación estándar" =
           paste0(emision_porc_mean," ± ",emision_porc_sd)) %>% 
  select(pais, "promedio ± desviación estándar") 
  

# Un avance de las proximas clases ----------------------------------------

calidad_emision_inner %>% 
  pivot_longer(-c(pais,anio), 
               names_to = "variable",
               values_to = "valor") %>% 
  ggplot(aes(x = pais, y = valor, color = pais)) +
  geom_boxplot() +
  facet_wrap(.~ variable, scales = "free") +
  coord_flip()

calidad_emision_inner %>% 
  pivot_longer(-c(pais,anio), 
               names_to = "variable",
               values_to = "valor") %>% 
  ggplot(aes(x = anio, y = valor, color = pais)) +
  geom_line() +
  facet_wrap(.~ variable, scales = "free")


# Avance lm() -------------------------------------------------------------

modelos_lm <-
calidad_emision_inner %>% 
  nest(data = -pais) %>% 
  mutate(modelo = map(data,
                      ~lm(pm2.5 ~
                          emision_porc, data = .)),
         tidy_modelo = map(modelo, tidy),
         glance_modelo = map(modelo, glance))

modelos_lm %>% 
  unnest(tidy_modelo)

modelos_lm %>% 
  unnest(glance_modelo)






