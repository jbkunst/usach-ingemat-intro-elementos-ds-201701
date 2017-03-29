# paquetes ----------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(stringr)
library(tidytext)
library(ggplot2)

# data --------------------------------------------------------------------
data <- read_tsv("output/datos_casi_limpios.txt")
glimpse(data) # otra forma de ver al ojo lo que hay


# explore -----------------------------------------------------------------
data

summarise(data, min(prob), max(prob))

# grammar graphics
ggplot(data) +
  geom_histogram(aes(x = prob))

ggplot(data) +
   geom_point(aes(x = file, y = prob))

data$texto

datap <- unnest_tokens(data, palabra, texto)
datap

datac <- count(datap, palabra)
datac <- arrange(datac, -n)
datac


# stopwords ---------------------------------------------------------------
library(rvest)

html <- read_html("http://www.ranks.nl/stopwords/spanish")
stopwords <- html_node(html, "table") 
stopwords <- html_text(stopwords)
stopwords <- str_split(stopwords, " ")
stopwords <- unlist(stopwords)


html <- read_html("https://es.wikipedia.org/wiki/Preposici%C3%B3n")
dataprep <- html_table(html)[[1]]

datac <- filter(datac, !palabra %in% stopwords)
datac <- filter(datac, !palabra %in% dataprep$Preposición)
datac <- filter(datac, !palabra %in% c("que", "n"))
datac <- filter(datac, n > 2)

ggplot(datac) +
  geom_col(aes(x = palabra, y = n))

datas <- summarise(group_by(datap, palabra),
          probmedia = mean(prob),
          probmin = min(prob),
          probmax = max(prob),
          n = n())

datas

datas <- filter(datas, !palabra %in% stopwords)
datas <- filter(datas, !palabra %in% dataprep$Preposición)
datas <- filter(datas, !palabra %in% c("que", "n"))
datas <- filter(datas, n > 2)


datas2 <- gather(datas, caract, valor, -palabra)
datas2 <- filter(datas2, caract != "n")

p <- ggplot(datas2) +
  geom_line(aes(palabra, y = valor, group = caract, color = caract)) +
  scale_color_manual(values = c("red", "blue", "green")) +
  theme_minimal() +
  ggtitle("asdas")

p

ggsave(p, filename = "output/nuestro_primer_grafico.pdf", width = 16, height = 9)
