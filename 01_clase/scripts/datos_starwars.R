
# Instalacion de paquetes -------------------------------------------------

# install.packages("tidyverse") solo correr una vez

# librerias ---------------------------------------------------------------

library(tidyverse)

# cargar los datos de starwars --------------------------------------------

data(starwars) #función para cargar datos
head(starwars) # función para imprimir las primeras filas

# analizamos los nombres de la variables ----------------------------------

colnames(starwars) #nombres de variables

# Chequeamos los tipos de datos -------------------------------------------

typeof(starwars$height)
typeof(starwars$height)

# Calculamos promedios sin na.rm = TRUE -----------------------------------

mean(starwars$height)

# Calculamos promedios con na.rm = TRUE -----------------------------------

mean(starwars$height, na.rm = TRUE)

# Investiguemos que pasó --------------------------------------------------

glimpse(starwars) # ver los datos en forma resumida

summary(starwars) # ver los datos en forma resumida

# Ejemplo operadores ------------------------------------------------------

## Ejemplo uso operadores 

starwars %>% 
  group_by(sex) %>% # agrupacion antes de filtrar
  filter(height > 160) %>% 
  count()

starwars %>% 
  filter(hair_color != "blond") %>% 
  count()

# Ejemplo operador logico -------------------------------------------------

starwars %>% 
  filter(name == "R2-D2" | name == "C-3PO") %>% 
  select(name,skin_color,mass) %>% 
  mutate(es_dorado = ifelse(skin_color=='gold',TRUE,FALSE))

