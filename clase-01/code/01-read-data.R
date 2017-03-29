# paquetes ----------------------------------------------------------------
# esto se hace con CTRL + R
# cargo los paquetes
library(tidyverse) # ayuda al manejo datos map, select  
library(stringr) # para manejos de stringr


# lectura datos -----------------------------------------------------------
dir("data/")

v <- c(1, 2, 3, 4)

res1 <- log(v)
res2 <- map(v, log)
res3 <- map_dbl(v, log)

res1 == res3
identical(res1, res3)

files <- dir("data/", full.names = TRUE)
files

texto <- map(files, readLines)

texto

map_chr(texto, 1)
map_chr(texto, 2)

# data frames
data("mtcars")
mtcars

mtcars

# data frames son tablas
df <- data_frame(
  col1 = c("var1", "var2"),
  col2 = c(1, 3)
)
df

data <- data_frame(
  file = files,
  prob = map_chr(texto, 1),
  texto = map_chr(texto, 2)
) 


write_tsv(data, "output/datos_casi_limpios.txt")

