
# Instalacion de paquetes -------------------------------------------------

# install.packages(c("readxl", "fs"))

# Librerias ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(fs)
library(broom)

# Info datos --------------------------------------------------------------

# Descargados desde:
# https://statistics.cepal.org/portal/cepalstat/dashboard.html?theme=3&lang=es

# Cargar datos ------------------------------------------------------------

calidad_aire_latinamerica <-
  read_excel("02_clase/data/calidad_aire_latinoamerica.xlsx")


# Leer documentos paises seleccionados ------------------------------------

lista_excels_emision <- fs::dir_ls("02_clase/data/emision_dioxido_por_pais",
                        regexp = "\\.xlsx$")

emision_paises_seleccionados <-
lista_excels_emision %>% 
  map_dfr(read_excel)

# glimpse calidad aire ----------------------------------------------------

glimpse(calidad_aire_latinamerica)

# glimpse emision paises seleccionados ------------------------------------

glimpse(emision_paises_seleccionados)


# Hacemos una limpieza de datos -------------------------------------------

calidad_aire_latinamerica_limpio <-
calidad_aire_latinamerica %>% 
  select(País__ESTANDAR, Años__ESTANDAR, value) %>% 
  rename(pais = País__ESTANDAR,
         anio = Años__ESTANDAR, 
         material_particulado_fino_MP2.5 = value) %>% 
  mutate(anio = as.numeric(anio))

emision_paises_seleccionados_limpio <- 
emision_paises_seleccionados %>% 
  select(País__ESTANDAR, Años__ESTANDAR, value) %>% 
  rename(pais = País__ESTANDAR,
         anio = Años__ESTANDAR, 
         propor_total_emision_porc = value) %>% 
  mutate(anio = as.numeric(anio))

# Nos piden comparar emision y calidad de los paises seleccionados --------

calidad_emision_inner <- 
  emision_paises_seleccionados_limpio %>% 
  inner_join(calidad_aire_latinamerica_limpio,
            by = c("pais", "anio"))

calidad_emision_left <-
emision_paises_seleccionados_limpio %>% 
  left_join(calidad_aire_latinamerica_limpio,
             by = c("pais", "anio"))


# Ahora con el dataset armado, comenzamos con estadistica -----------------
# Usaremos el dataset inner -----------------------------------------------

estadistica_descriptiva <-
calidad_emision_inner %>% 
  group_by(pais) %>% 
  summarise_at(vars(propor_total_emision_porc,material_particulado_fino_MP2.5),
               list(mean = mean, median = median,
                    IQR = IQR, sd = sd,
                    var = var))


# Un avance de las proximas clases ----------------------------------------

calidad_emision_inner %>% 
  pivot_longer(-c(pais,anio), 
               names_to = "variable",
               values_to = "valor") %>% 
  ggplot(aes(x = anio, y = valor, color = pais)) +
  geom_line() +
  facet_wrap(.~ variable, scales = "free")

calidad_emision_inner %>% 
  pivot_longer(-c(pais,anio), 
               names_to = "variable",
               values_to = "valor") %>% 
  ggplot(aes(x = pais, y = valor, color = pais)) +
  geom_boxplot() +
  facet_wrap(.~ variable, scales = "free") +
  coord_flip()


# Avance lm() -------------------------------------------------------------

calidad_emision_inner %>% 
  nest(data = -pais) %>% 
  mutate(modelo = map(data,
                      ~lm(material_particulado_fino_MP2.5 ~
                            propor_total_emision_porc, data = .)),
         tidy_modelo = map(modelo, tidy))






