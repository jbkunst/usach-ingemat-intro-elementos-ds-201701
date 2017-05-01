#' --- 
#' title: Tarea 01
#' output:
#'   html_document:
#'     theme: yeti
#'     toc: true
#'     toc_float: true
#' ---
#+ echo = FALSE, message = FALSE, warning = FALSE
knitr::opts_chunk$set(message = FALSE, warning = FALSE)
library(jbkmisc)
library(ggplot2)
theme_set(theme_jbk())
#' 
#' ## Evaluación y Consideraciones
#' 
#' Cada grupo deberá enviar un mail a <jbkunst@gmail.com> con el asunto 
#' "Tarea namber wan" (sin comillas) adjuntando un script el cual deberá 
#' correr sin errores al ser ejecutado y debe claramente señalar los comandos
#' (esto es el código mismo) con el cual obtuvo las respuestas incluyendo 
#' comentarios.
#' 
#' Se evaluará tanto el código como el resultado con comentarios.
#' Con esto se intenta evitar las  respuestas _al ojo_ o usando otras
#'  herramientas como _excel_.
#' 
#' Por ejemplo, supongamos el archivo `script_grupo_1.R`
#' 
#' ```
#' # Pregunta 1
#' # Para esta pregunta debemos seleccionar filtrar el data frame y segun x > 5
#' # y reordenar segun la variable y:
#' 
#' data %>% 
#'   filter(x > 5) %>% 
#'   arrange(y)
#' 
#' ```
#' 
#' Son libres de hacer preguntas vía mail pero siempre háganlo escribiéndolo a todo el grupo.
#' 
#' Espero los script a __más tardar__ el miércoles 26 de este més (y de este año)
#' a las 11.59 PM. Obviamente lo pueden enviar el script más tarde, la única 
#' salvedad es que la nota final será la nota por el script dividido por 1.25 por 
#' cada hora que pase desde el límite. A modo de ejemplo si la nota es un 5.5, 
#' y fue entregado a las 3AM, la nota final será:
#' 
nota_script <- 5.5
nota_final <- nota_script/(1.25^3)
round(nota_final, 1)
#' 
#' 
#' ## Requerimientos
#' 
#' Necesitaremos algunos paquetes para trabajar. El siguiente código
#' instalará lo requerido:
#' 
library(tidyverse)

paquetes_a_instalar <- c("gapminder", "babynames", "nasaweather",
                         "fueleconomy", "nycflights13", "maps")

for(p in paquetes_a_instalar) {
  message("Instalando ", p)
  if(!p %in% rownames(installed.packages())) install.packages(p)
  library(p, character.only = TRUE)
}

#' ![](https://az616578.vo.msecnd.net/files/2015/11/06/6358243226338872701442058152_oq731NH.gif)
#' 
#' ## Pregunta 0 (el punto base)
#' 
#' Considere, la tabla `births` del paquete `babynames`. Esto es:
data("births", package = "babynames")
head(births)

#' ¿Cuántas columnas y filas posee la tabla _births_?
#' 
#' ### Respuesta
dim(births)

#' o:
c(filas = nrow(births), columnas = ncol(births))

#' ## Pregunta 1
#' 
#' Considere, la tabla `births` del paquete `babynames`. Estudie los 
#' nacimientos por años en EE.UU:
ggplot(births) +
  geom_line(aes(year, births)) 

#' ¿En que _decada_ se obtuvo la menor cantidad de nacimientos?.
#'
#' ### Respuesta
#' 
births %>% 
  mutate(decada = 10 * floor(year/10)) %>% 
  group_by(decada) %>% 
  summarise(n = n(), births = sum(births)) %>% 
  arrange(births)

#' Podría responderse que la decada de los '00 es la que tiene menos 
#' pero está incompleta, un año. 

#' Un grupo para remover el 1909 hizo:
births[-c(1),]

#' En lugar de:
filter(births, year >= 1910)

#' Es lo mismo, cierto?
#' Que pasa si la tabla viene en ningún orden en particular?
#' 
births <- sample_frac(births, 1) # desorden, desorden, esto es un desorden (8)
births
#' Comparar
births[-c(1),]
filter(births, year >= 1910)

#' El `c(1)` no es _explícito_. Prefieran lo explícito antes de lo que no. 
#' Segun el _zen de Pytohn_.
#' 
# Beautiful is better than ugly.
# Explicit is better than implicit.
# Simple is better than complex.
# Complex is better than complicated.
# Flat is better than nested.
# Sparse is better than dense.
# Readability counts.
# Special cases aren't special enough to break the rules.
# Although practicality beats purity.
# Errors should never pass silently.
# Unless explicitly silenced.
# In the face of ambiguity, refuse the temptation to guess.
# There should be one-- and preferably only one --obvious way to do it.
# Although that way may not be obvious at first unless you're Dutch.
# Now is better than never.
# Although never is often better than *right* now.
# If the implementation is hard to explain, it's a bad idea.
# If the implementation is easy to explain, it may be a good idea.
# Namespaces are one honking great idea -- let's do more of those! _python_ existe  

#'     
#' ## Pregunta 2
#' 
#' Considerando ahora la tabla `babynames` del paquete `babynames`:
data("babynames", package = "babynames")
glimpse(babynames)

#' Realice el proceso necesario para obtener el mismo estructura en cuanto
#' a _columnas_ que la tabla `births`. 
#' 
#' ### Respuesta
#'
#' Esta respuesta se anuló dado que solo un grupo supo responder lo que yo
#' _intenté_ pedir. Seguramente no fui explícito en lo que quería por lo que
#' esta pregunta no suma ni resta puntos en la nota final de la tarea.
#'
babynames %>% 
  group_by(year) %>% 
  summarize(births = sum(n))
#' 
#' ## Pregunta 3
#' 
#' Genere un data frame partiendo de la tabla babyanes `babynames` y conteniendo
#' los nacimientos de personas de género femenino con el nombre _Nala_, _Ariel_
#' y _Elsa_ desde los años 1980 en adelante. 
#' 
#' ### Respuesta
#' 
bbnames2 <- babynames %>% 
  filter(sex == "F") %>% 
  filter(name %in% c("Nala", "Ariel", "Elsa")) %>% 
  filter(year >= 1980)
bbnames2
  
#' 
#' ## Pregunta 4
#' 
#' Con el data frame obtenido en la pregunta anterior genere un gráfico que 
#' contenga la información de los nacimientos por año de cada uno de los
#' nombres mencionados y __mencione__ una hipótesis/suposición al respecto de lo 
#' observado.
#' 
#' hint: Use `facet_wrap(~ name, scales = "free_y")`.
#' 
#' ### Respuesta
#' 
ggplot(bbnames2) +
  geom_line(aes(year, n)) +
  facet_wrap(~ name, scales = "free_y")

#' Recuerden, todo puede ser _pior_.
#' ![](https://gcdn.emol.cl/fotografia/files/2013/11/nombres-raros-8.jpg)
#' 
#' ## Pregunta 5
#' 
#' Utilizando la tabla `airports` y `flights` del paquete `nycflights13`
#' obtenga una tabla que contenga conteos de vuelos según su destino además 
#' de la longitud y latidud del aeropuerto (de destino).
#' 
#' ### Respuesta
#' 
data("airports", package = "nycflights13")
data("flights", package = "nycflights13")

airprts2  <- flights %>% 
  count(faa = dest) %>% 
  left_join(airports)
  
airprts2 
 
#' 
#' ## Pregunta 6
#' 
#' Apoyándose del siguiente gráfico
us <- map_data("state")

glimpse(us)

ggmap <- ggplot() +
  geom_polygon(data = us, aes(long, lat, group = group), 
               alpha = 0.25) +
  coord_fixed() # esto es para mantener la razón 1:1

ggmap 

#' Agregue una capa de de puntos ubicando los aeropuertos obtenidos de la pregunta
#' anterior y usando además: `aes(size = la_cantidad_de_vuelos_a_dicho_aeropuerto)`.
#'
#' ### Respuesta
#' 
ggmap +
  geom_point(data = airprts2, aes(lon, lat, size = n),
             alpha = 0.6, color = "#030367")  + # a lo grupo 2 
  scale_size_continuous(range = c(1, 4)) # a lo grupo 1

#'
#' ## Pregunta 7
#' 
#' A la izquiera del gráfico anterior se observan 2 puntos. Genere el/los pasos
#' necesarios para seleccionarlos usando la tabla resultante de la pregunta 5
#' para identificar los nombres de dichos areopuertos y responda ¿Dónde están?
#' ¿Que gracias tienen?
#' 
#' ### Respuesta
#' 
#' Gracia? Que están _leeejos_.
#' 
filter(airprts2, lon <= -140)
#' 
#' 
#' ## Pregunta 8
#' 
#' Considere la tabla `vehicles` del paquete `fueleconomy`.
#' 
#' Genere un subconjunto de la tabla `vehicles` considerando las 10 clases 
#' (columna `class`) más comunes y:
#' 
#' 1. Genere un gráfico de densidades del consumo en carretera (`hwy`)
#' separando por clase de vehículo. 
#' 
#' 2. Averigué como usar la función `geom_boxplot` para comparar las
#' distribuciones de rendimiento según clase.
#' 
#' ### Respuesta
#' 
data("vehicles", package = "fueleconomy")

vehicles2 <- vehicles %>% 
  count(class) %>% 
  arrange(desc(n)) %>% 
  top_n(10) %>% 
  semi_join(vehicles, .)

ggplot(vehicles2) +
  geom_density(aes(hwy), fill = "gray50", alpha = 0.5) +
  facet_wrap(~class)

ggplot(vehicles2) +
  geom_boxplot(aes(x = class, y = hwy)) +
  coord_flip() # a lo equipo 5

#' 
#' ## Pregunta 9
#' 
#' Con los gráficos anteriormente obtenido argumente cuales son las debidildades y
#' fortalezas de cada tipo de visualización. 
#' 
#' ### Respuesta
#' 
#' Muchas respuestas posibles, todo depende del caso particular. No hay 
#' _verdad_ absoluta, por lo que nada es verdad, ¿verdad?.
#' 
#' ## Pregunta 10
#' 
#' Usando la tabla anterior calcule el promedio, el mínimo y el máximo de
#' rendimiento según clase de vehículo.
#' 
#' ### Respuesta
#' 
vehicles2 %>% 
  group_by(class) %>% 
  summarise(
    n = n(),
    min(hwy), max(hwy)
  )
