rm(list = ls())
library(tidyverse)
library(h2o)
h2o.init()

# data <- read_csv("https://raw.githubusercontent.com/jbkunst/riskr/master/data/data.csv")
data <- read_csv("train.csv")
names(data)

glimpse(data)

data <- mutate(data, label = factor(label))

set.seed(123)
s <- sample(c("train", "test"), size = nrow(data), replace = TRUE)

data_test <- filter(data, s == "train")
data_train <- filter(data, s == "test")

# neurona iluso -------------------------------
dh2o <- as.h2o(data_train)

mod_autoenc <- h2o.deeplearning(
  x = names(dh2o)[-1],
  y = names(dh2o)[1],
  training_frame = dh2o,
  hidden = c(100, 100),
  epochs = 50,
  activation = "Tanh"
)

dh2o2 <- as.h2o(data_test)

dautoenc <- h2o.deepfeatures(mod_autoenc, dh2o2, layer = 2) %>% 
  as.data.frame() %>% 
  tbl_df() %>% 
  setNames(c("x", "y")) %>% 
  mutate(label = data2$label)

data_test2 <- h2o.predict(mod_autoenc, dh2o2) %>% 
  as_data_frame() %>% 
  bind_cols(data_test)
data_test2

data_test2 %>% 
  group_by(label, predict) %>%
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(que_pasho = ifelse(label == predict, ":)", ":(")) %>% 
  group_by(que_pasho) %>% 
  summarise(n = sum(n)) %>% 
  mutate(tasa_clasificacion_mundial = n/sum(n))

# autoenoder --------------------------------------------------------------
data2 <- sample_n(data, 1000) 

dh2o <- as.h2o(data2)

mod_autoenc <- h2o.deeplearning(
  x = names(dh2o)[-1],
  training_frame = dh2o,
  hidden = c(400, 100, 2, 100, 400),
  epochs = 50,
  activation = "Tanh",
  autoencoder = TRUE
)

dautoenc <- h2o.deepfeatures(mod_autoenc, dh2o, layer = 3) %>% 
  as.data.frame() %>% 
  tbl_df() %>% 
  setNames(c("x", "y")) %>% 
  mutate(label = data2$label)

ggplot(dautoenc) + 
  geom_point(aes(x, y, color = factor(label))) +
  viridis::scale_color_viridis(discrete = TRUE) +
  facet_wrap(~ label)
theme_minimal()

