#' ---
#' title: "Data frames dplyr"
#' output: github_document
#' ---
library(dplyr) # no tienen que cargar magrittr

#+echo=FALSE
# No miren esto!
knitr::opts_chunk$set(error=TRUE, warning = FALSE)
#+echo=TRUE

#' dplyr ofrece funciones (en forma de verbos)
#' para manipular data frames.

#' Cargamos nuestra super encuesta _flash_
#' url: https://docs.google.com/spreadsheets/d/1JNwZmAdsClL6hAlnqT6VTu-DiKRohArft3W7I5J1PPk/edit#gid=0 
# install.packages('gsheet')
library(gsheet)
data <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1JNwZmAdsClL6hAlnqT6VTu-DiKRohArft3W7I5J1PPk/edit#gid=0')
data

#' ## Explorar que es lo que hay
#' Funcion: glimpse, srt, dim
data

glimpse(data)

dim(data)

#' ## Verbos dplyr
#'  - Seleccionar: Seleccionar columnas
#'  - Filtrar: escoger filas
#'  - Mutar: Crear columnas
#'  - Arrange: Ordena filas
data2 <- select(data, satisfaccion, ramo_semestre_anterior)
filter(data2, ano_vas >= 5)

# Douhg!
data2 <- select(data, ano_vas, satisfaccion, ramo_semestre_anterior)
data2 <- filter(data2, ano_vas >= 5)
data2 <- mutate(data2, ratio_sat_ram = satisfaccion/ramo_semestre_anterior)
data2 <- select(data2, -satisfaccion)
data2 <- arrange(data2, ratio_sat_ram)
data2

filter(data, TRUE)
filter(data, FALSE) # SIEMPRE!! un data frame :D

filter(data, satisfaccion == max(satisfaccion)
       | ano_vas == min(ano_vas))

#' Donde esta pipe?!
data3 <- data %>% 
  select(ano_vas, satisfaccion, ramo_semestre_anterior) %>% 
  filter(ano_vas >= 5) %>% 
  mutate(ratio_sat_ram = satisfaccion/ramo_semestre_anterior) %>% 
  select(-satisfaccion) %>% 
  arrange(ratio_sat_ram)

identical(data2, data3)

#' **Imporante** Cada funcion toma un data.frame
#' y retorna un data.frame**
#' 
#  - Agrupar: Agrupar y resumir
datag <- group_by(data, ano_vas)

x <- seq(1, 10)
x
cumsum(x)

datag <- mutate(datag, satisfaccioncum = cumsum(satisfaccion))
datag <- select(datag, ano_vas, satisfaccion, satisfaccioncum)
datag

datag <- arrange(datag, ano_vas)
datag

datag <- ungroup(datag)
datag

datag <- mutate(datag, satisfaccioncum2 = cumsum(satisfaccion))
datag

# Sumarizar: Resumir

summarise(data, mav = max(ano_vas), ms = min(satisfaccion))
data %>% summarise(mav = max(ano_vas), ms = min(satisfaccion))

group_by(data, ramo_semestre) %>% 
  summarise(mav = max(ano_vas), ms = min(satisfaccion))

group_by(data, ramo_semestre) %>% 
  summarise(mav = max(ano_vas), ms = min(satisfaccion)) %>% 
  summarise(mav = max(mav), ms = min(ms)) 



