
# Librerias a instalar ----------------------------------------------------

# Solo instalarlas una vez
# install.packages(c("tidyverse",
#                    "datos",
#                    "broom"))

# Librerias ---------------------------------------------------------------

library(tidyverse) # analisis de datos
library(datos) # dataset en español
library(broom) # extraer informacion


# Codigo ------------------------------------------------------------------

# Como base use codigo de estas web: https://rpubs.com/jsloane/stats_in_r
# https://statsandr.com/blog/anova-in-r/

# Cargamos los datos de pinguinos -----------------------------------------

# revisamos que las variables categorias sean <fct>

glimpse(pinguinos)

# Resumen de lo visto en el curso -----------------------------------------

# Pregunta:
# Son los machos mas pesados que las hembras?

pinguinos %>%
  group_by(sexo) %>%
  summarise(promedio = mean(masa_corporal_g), #promedio
            sd = sd(masa_corporal_g), # desv estandar
            n = n(), # contar por grupos
            se = sd/sqrt(n)) # error estandar

# Creamos un objeto para ahorrar codigo -----------------------------------

analisis_masa_por_sexo <-
  pinguinos %>%
  group_by(sexo) %>%
  drop_na() %>% # agregamos sacar los NA
  summarise(promedio = mean(masa_corporal_g), #promedio
            sd = sd(masa_corporal_g), # desv estandar
            n = n(), # contar por grupos
            se = sd/sqrt(n))

# Grafiquemos -------------------------------------------------------------

analisis_masa_por_sexo %>% 
  ggplot(aes(x = sexo, y = promedio, fill = sexo)) +
  geom_col() +
  geom_errorbar(aes(ymin = promedio - se, # operaciones dentro 
                    ymax = promedio + se),
                width = .2) # el ancho de la columna

# Hacemos un t-test -------------------------------------------------------

# Primer paso: chequear normalidad ----------------------------------------

pinguinos %>% 
  select(especie, sexo, masa_corporal_g) %>% 
  drop_na() %>% 
  group_by(sexo) %>% 
  summarise(p_value_normalidad = shapiro.test(masa_corporal_g)$p.value)

# realizar un t-test no pareado -------------------------------------------

# Como el n() de hembras y machos es diferente, podemos hacer objetos

hembras <- pinguinos %>%
  filter(sexo == "hembra")

machos <- pinguinos %>%
  filter(sexo == "macho")

#  al intentar armar un dataframe tenemos problemas -----------------------
# 
# data.frame(hembras = hembras,
#            machos = machos)

# Podemos trabajar por separado -------------------------------------------

t.test(hembras$masa_corporal_g, machos$masa_corporal_g,
       paired = FALSE)

# O usando listas ---------------------------------------------------------

lista_machos_vs_hembras <-
  list(hembras = hembras,
       machos = machos) # creamos una lista

t.test(lista_machos_vs_hembras$hembras$masa_corporal_g, #accedemos usando $
       lista_machos_vs_hembras$machos$masa_corporal_g,
       paired = FALSE)

# Ahora codigo de ANOVA ---------------------------------------------------

# Pregunta:
# ¿Tienen las diferentes especies el mismo o diferente largo medio de aleta? 

pinguinos %>%
  drop_na() %>% 
  group_by(especie) %>% #especie
  summarise(promedio = mean(largo_aleta_mm), #promedio
            sd = sd(largo_aleta_mm), # desv estandar
            n = n(), # contar por grupos
            se = sd/sqrt(n)) # error estandar

# Graficamos --------------------------------------------------------------

# la media total del largo de aleta es 201.1 mm

plot_aleta_especie <-
ggplot(data = pinguinos %>% drop_na(), # podemos operar dentro de ggplot!
       aes(x = especie, y = largo_aleta_mm)) +
  geom_boxplot(aes(color = especie), width = 0.3,
               show.legend = FALSE) +
  geom_hline(aes(yintercept = 201.1)) + # dato entregado
  labs(x = "Especie",
       y = "Largo aleta (mm)")

plot_aleta_especie

# ANOVA -------------------------------------------------------------------

anova_aleta <- aov(largo_aleta_mm ~ especie, data = pinguinos %>% drop_na())

# histograma de residuos
hist(anova_aleta$residuals) # revisamos normalidad de residuos

summary(anova_aleta) # version r base

tidy(anova_aleta) # revisamos p.value de la hipotesis (todas las medias iguales)

datos_tidy_aleta <- tidy(anova_aleta)

# Como el p.value es menos a 0.05, podemos concluir que al menos una especie
# es diferente de las demás en cuanto a la longitud de las aletas 


# TukeyHSD ----------------------------------------------------------------

# Si un ANOVA resulta significativo, implica que al menos dos
# de las medias comparadas son significativamente distintas entre sí, pero no
# se indica cuáles. Para identificarlas hay que comparar dos a dos las medias
# de todos los grupos introducidos en el análisis mediante un t-test u otro test
# que compare 2 grupos, ha esto se le conoce como análisis POST-HOC

tukey_aleta <- TukeyHSD(anova_aleta) # funcion de r base que hace t.test combinados

plot(tukey_aleta)

tabla_tukey_ordenada <- tidy(tukey_aleta)

# Exportemos la info en diferentes formatos -------------------------------

# 1. Guardar csv y pegar word

write.csv(tabla_tukey_ordenada,
          "05_clase/datos_exportados/tabla_tukey.csv",
          row.names = FALSE) # no queremos los numeros de fila

# 2. Guardar ggplot como imagen

ggsave("05_clase/img/aleta_especie.png", # poner el directorio
       plot_aleta_especie) # aca el objeto

# 3. Para ver como hacer codigo + texto, abrir el archivo ejemplos_exportar.Rmd


# Extra: Regresion lineal -------------------------------------------------

ggplot(pinguinos %>% drop_na(), # operar dentro de ggplot
       aes(x = largo_pico_mm, y = largo_aleta_mm)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylim(125,250) +
  xlim(0,70)

# Modelos lm() son independientes

modelo_ml_pingui <- lm(largo_aleta_mm ~  largo_pico_mm,
                       data = pinguinos %>% drop_na())

# Correlacion global
pinguinos %>% 
  drop_na() %>% 
  summarise(cor.pearson = cor(largo_pico_mm,largo_aleta_mm)) 

# Correlacion por grupos (sexo y especie)
pinguinos %>% 
  drop_na() %>% 
  group_by(sexo, especie) %>% 
  summarise(cor.pearson = cor(largo_pico_mm,largo_aleta_mm)) 

# Los resultados globales (r de 0.65) son diferentes a los agrupados


