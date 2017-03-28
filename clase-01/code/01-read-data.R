library(tidyverse)
library(stringr)


files <- dir("data/", full.names = TRUE) 
texto <- map(files, readLines)


map_chr(texto, 1)
map_chr(texto, 2)

data <- data_frame(
  file = files,
  prob = map_chr(texto, 1),
  texto = map_chr(texto, 2)
)  

data <- mutate(data, prob = as.numeric(prob))

glimpse(data)
