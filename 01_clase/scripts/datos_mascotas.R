
# librerias ---------------------------------------------------------------

library(tidyverse)
library(googlesheets4)

# Cargamos los datos desde Google Drive -----------------------------------

datos_mascotas <- read_sheet("https://docs.google.com/spreadsheets/d/1i3J18HyHm4T-yJP1LkMa5qyhjIbJSR4VuMma42I00QA/edit#gid=0")

# Lo primero es ver los datos ---------------------------------------------

colnames(datos_mascotas)
glimpse(datos_mascotas)

# Ordenamos por edad ------------------------------------------------------

datos_mascotas_edad <-
  datos_mascotas %>%
  select(nombre, starts_with("edad")) %>%
  pivot_longer(
    cols = !nombre,
    names_to = c("dato_edad", "numero_mascota"),
    names_sep = "_",
    values_to = "edad"
  )
datos_mascotas_edad

# Ordenamos por tipo de mascota -------------------------------------------

datos_mascotas_info <-
  datos_mascotas %>%
  select(!starts_with("edad") &
           !starts_with("interes") & !starts_with("mascota")) %>%
  pivot_longer(
    cols = !nombre,
    names_to = c("info_mascota", "numero_mascota"),
    names_sep = "_",
    values_to = "tipo_mascota"
  )
datos_mascotas_info

# Unimos la información ---------------------------------------------------

datos_mascotas_edad %>% 
  left_join(datos_mascotas_info, by = c("nombre", "numero_mascota"))


# Comenzamos la estadistica descriptiva -----------------------------------

datos_mascotas_edad %>% 
  left_join(datos_mascotas_info, by = c("nombre", "numero_mascota")) %>% 
  group_by(tipo_mascota) %>% 
  drop_na() %>% 
  summarise(promedio_edad = mean(edad),
            min_edad = min(edad),
            max_edad = max(edad))

# Ahora por asistente -----------------------------------------------------

datos_mascotas_edad %>% 
  left_join(datos_mascotas_info, by = c("nombre", "numero_mascota")) %>% 
  group_by(nombre) %>% 
  drop_na() %>% 
  summarise(promedio_edad = mean(edad),
            min_edad = min(edad),
            max_edad = max(edad))

