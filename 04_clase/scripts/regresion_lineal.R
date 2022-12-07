# Instalacion de paquetes -------------------------------------------------
# los paquetes se instalas solo una vez

# install.packages("tidyverse", "broom")

# Librerias ---------------------------------------------------------------
# Una vez instaladas, llamamos a cada libreria usando library()

library(tidyverse) 
library(broom) # libreria para ordenar datos estadisticos

# Info datos --------------------------------------------------------------

# Datos reales de arboles de guindas negras
# Unidades 
# * Girth : circunferencia (pulgadas)
# * Height: altura (pies)
# * Volume: volumen (pies cubicos)

# Cargar datos ------------------------------------------------------------

data(trees)

# Un poco de limpieza -----------------------------------------------------

# Debemos hacer un informe en español y con unidades de medida en metro

arboles <- 
  trees %>%
  rename(circunferencia = Girth, # cambio el nombre de la variable
         altura = Height, # nombre nuevo = nombre viejo
         volumen =  Volume) %>% 
  mutate(circunferencia_m = circunferencia / 39.37, # hacemos aritmetica
         altura_m = altura / 3.28,
         volumen_m3 = circunferencia / 35.32)


# glimpse -----------------------------------------------------------------

glimpse(arboles)

# Glimpse usando ggplot ---------------------------------------------------

arboles %>% 
  ggplot(aes(x = circunferencia_m, # definimos el eje x
             y = altura_m)) + # definimos el eje y
  geom_point() + # creamos plot de puntos
  geom_smooth(method = "lm",
              se = FALSE) # no queremos intervalos de confianza


# Hacemos un poco de estadistica descriptiva ------------------------------

arboles %>% 
  summarise_at(vars(circunferencia_m:volumen_m3), #seleccionar vars
               list(promedio = mean, # le indicamos la lista de funciones
                    desv = sd,
                    min = min,
                    max = max))

# Creamos el modelo -------------------------------------------------------

# lm(formula = y ~ x, data = mis_datos)

modelo_arboles_cherry <- lm(altura_m ~ circunferencia_m, data = arboles)

modelo_arboles_cherry

# Revisamos la funcion  ---------------------------------------------------

modelo_arboles_cherry$coefficients # intercepto (b0) y pendiente (b1)

modelo_arboles_cherry$residuals # diferencia entre los valores observados y los predichos 

modelo_arboles_cherry$fitted.values # valores predichos 

# Revisamos los residuales ------------------------------------------------

plot(modelo_arboles_cherry)

# Residuals vs Fitted: Una relación lineal mostrará aquí una línea roja horizontal.
# Las desviaciones de una línea horizontal sugieren no linealidad. 
# 
# Normal Q-Q: comprueba si los residuos del modelo se distribuyen normalmente.
# Los puntos del modelo ajustado se sitúan a lo largo de la línea discontinua del gráfico.
# 
# Scale-Location: comprueba la homocedasticidad del modelo. Una línea roja horizontal
# con puntos repartidos por igual indica un modelo bien ajustado.
# 
# Residuals vs Leverage: identifica valores atípicos que pueden afectar al modelo.
# Su inclusión o exclusión del análisis puede afectar a los resultados del mismo. 
# standardized residuals superiores a 3 o inferiores a -3 deben considerarse valores atípicos.

# Revisemos residuales con ggplot -----------------------------------------

ggplot(modelo_arboles_cherry, # datos
       aes(x = modelo_arboles_cherry$residuals)) + #geom_histogram solo requiere x
  geom_histogram(bins = 5) # argumento para agrupar y contar

# aca gatillamos un error al no asignar y

# ggplot(modelo_arboles_cherry,
#        aes(x = modelo_arboles_cherry$residuals)) +
#   geom_point()

# Usemos broom ------------------------------------------------------------

broom::tidy(modelo_arboles_cherry) # misma informacion que summary()

broom::glance(modelo_arboles_cherry) # mas detalle summary(), incluye r2

# Usemos el modelo que creamos con otros datos ----------------------------

arboles_nuez <- data.frame("altura_m" = c(21.8, 29.4, 35.5, 46.3, 37.5, 20.5, 21.2, 24.5),
                             "circunferencia_m" = c(0.11, 0.22, 0.32, 0.48, 0.49, 0.19, 0.09, 0.19),
                             "arbol_tipo" = c("a", "a", "a", "a", "b", "b", "b", "b"))

glimpse(arboles_nuez)


# Tengo nogales, quiero probar el modelo de guindas ------------------------

class(modelo_arboles_cherry) # tenemos un objeto tipo modelo

predict(modelo_arboles_cherry, # objeto tipo modelo
        newdata = arboles_nuez) # nuevos datos

# Hacemos uso de tidyverse para ordenar datos -----------------------------

arboles_nuez_analisis <-
  arboles_nuez  %>% 
  mutate(altura_ml_cherry = predict(modelo_arboles_cherry,
                                           newdata = arboles_nuez))

# veamos la funcion cor()

cor(arboles_nuez_analisis$altura_ml_cherry,
    arboles_nuez_analisis$altura_m,
    method = "pearson")

# hagamos cor() y otras funciones a mano usando tidy y grupos -------------

arboles_nuez_ml_cherry <-
arboles_nuez_analisis %>% 
group_by(arbol_tipo) %>% 
summarise(RMSE = mean((altura_m - altura_ml_cherry)^2),
         r.pearson = cor(altura_ml_cherry,altura_m),
         r.cuadrado = sqrt(cor(altura_ml_cherry,altura_m)),
         prediccion_modelo = "altura_ml_cherry")

# Que tal si hacemos un nuevo lm() por cada tipo de arbol -----------------

arboles_nuez_ml <-
  arboles_nuez_analisis %>%  # creamos listas por tipo de arbol
  nest(datos = -arbol_tipo) %>%
  # creamos un modelo lineal para cada pais (ml)
  # creamos un modelo limpio (tidy_ml)
  # extraemos metricas (glance_ml)
  mutate(
    ml_nuez = map(datos,
             ~ lm(altura_m ~
                    circunferencia_m, data = .)),
    altura_ml_nuez = map2(.x = ml_nuez, .y = datos, ~predict(object = .x, newdata = .y)),
    tidy_ml = map(ml_nuez , tidy),
    glance_ml = map(ml_nuez , glance)
  )

# extraemos el detalle de tidy_modelo
# aca podemos ver el intercepto y coeficientes de la regresion
arboles_nuez_ml %>% 
  unnest(tidy_ml)

# extraemos el detalle de glance_modelo
# aca podemos ver el R cuadrado y error residual de cada modelo
arboles_nuez_ml %>% 
  unnest(glance_ml)


# Preparemos informacion para un plot -------------------------------------

arboles_nuez_ml_info <-
arboles_nuez_ml %>% 
  unnest(glance_ml) %>% 
  select(arbol_tipo, r.squared) %>% 
  rename(r.cuadrado = r.squared) %>% 
  mutate(prediccion_modelo = "altura_ml_nuez")


datos_r_plot <-
full_join(arboles_nuez_ml_info, arboles_nuez_ml_cherry) %>% 
  select(arbol_tipo,
         r.cuadrado,
         prediccion_modelo) %>% 
  mutate(r.cuadrado = round(r.cuadrado,2))
  
# Hagamos un plot final ---------------------------------------------------

arboles_nuez_ml %>% 
  select(arbol_tipo, datos, altura_ml_nuez) %>% 
  unnest(cols = c(datos,  altura_ml_nuez)) %>% 
  pivot_longer(cols = altura_ml_cherry:altura_ml_nuez,
               names_to = "prediccion_modelo",
               values_to = "altura_predicha")  %>% 
  left_join(datos_r_plot) %>% 
  ggplot(aes(x = altura_predicha,
             y = altura_m,
             color = prediccion_modelo)) +
  scale_color_discrete(name = "modelo", label = c("Modelo Cherry","Modelo Nuez")) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  geom_abline() +
  geom_text(x = 40, y = 25,
            aes(label = paste0("~R^{2} == ", r.cuadrado)), #forma de escribir al cuadrado
            parse = TRUE, # para que transforme a R2
            color = 'blue', # color
            check_overlap = TRUE) + # para que se vea bien 
  xlim(0, 50) + # limites eje x
  ylim(0, 50) + # limites eje y
  facet_grid(. ~ arbol_tipo + prediccion_modelo,
             labeller = as_labeller(c(altura_ml_cherry="Modelo Cherry",
                                      altura_ml_nuez="Modelo Nuez",
                                      a = "Tipo Nogal A",
                                      b = "Tipo Nogal B"))) +
  labs(x = "Altura predicha por cada modelo (metros)",
       y = "Altura real (metros)",
       title = "Predicción altura usando modelo lineal Cherry y Nuez") +
  theme_bw()
  



