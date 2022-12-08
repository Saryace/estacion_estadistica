
# Librerias ---------------------------------------------------------------

library(tidyverse)


# Creamos un vector -------------------------------------------------------

uno_al_diez <- 1:10

# Creamos un dataframe ----------------------------------------------------

uno_al_diez_df <- data.frame("datos" = 1:10,
                                "tipo" = c("a", "a", "a", "a", "a",
                                           "b", "b", "b", "b", "b"))

# Creamos una lista -------------------------------------------------------

uno_al_diez_lista <- list("datos" = 1:10,
                                "tipo" = c("a", "a", "a", "a", "a",
                                           "b", "b", "b", "b", "b"))


# Creamos una funcion -----------------------------------------------------

por_diez <- function(x) x*10


# Revisamos a funcion map() -----------------------------------------------

purrr::map(uno_al_diez, por_diez)

purrr::map_dbl(uno_al_diez, por_diez)

purrr::map(uno_al_diez_lista$datos, por_diez)

# Revisamos map2() --------------------------------------------------------

dataset_a <- c(3, 2 ,1)
dataset_b <- c(1, 2, 3)

sumar_filas <- function(x, y) {x+y}

purrr::map2(dataset_a, dataset_b, sumar_filas)

purrr::map2_dbl(dataset_a, dataset_b, sumar_filas)

