rm(list = ls())
library(tidyverse)
library(stringr)
library(tidyverse)
library(partykit)

data <- read_csv("https://raw.githubusercontent.com/jadeyee/r2d3-part-1-data/master/part_1_data.csv", skip = 2)

data <- mutate(data, in_sf = ifelse(in_sf == 1, "Y", "N"))

s <- sample(c("train", "test"), size = nrow(data), replace = TRUE, c(2, 1))

data_test <- filter(data, s == "train")
data_train <- filter(data, s == "test")


# arbol iluso -------------------------------------------------------------
mod_ilu <- ctree(as.factor(in_sf) ~ ., data = data_test) #  :O
plot(mod_ilu)
plot(mod_ilu, gp = gpar(fontsize = 5)) 

data_test <- data_test %>%
  mutate(mod_ilu_pred = predict(mod_ilu, newdata = data_test))

data_test %>% 
  group_by(in_sf, mod_ilu_pred) %>%
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(que_pasho = ifelse(in_sf == mod_ilu_pred, ":)", ":(")) %>% 
  group_by(que_pasho) %>% 
  summarise(n = sum(n)) %>% 
  mutate(tasa_clasificacion_mundial = n/sum(n))

data_test %>% 
  group_by(in_sf, mod_ilu_pred) %>%
  summarise(n = n()) %>% 
  spread(mod_ilu_pred, n) %>% 
  ungroup() 

# mod bagging -------------------------------------------------------------
M <- 500 
dim(data)
P <- round(sqrt(ncol(data_train)))

haceme_un_arbolito <- function() {
  
  p <- sample(2:ncol(data), size = P)
  print(names(data_train)[p])
  
  data_pelotita <- data_train %>% 
    select(p, in_sf) %>% 
    sample_n(nrow(data_train), replace = TRUE)
  
  arbol_pelotita <- ctree(as.factor(in_sf) ~ ., data_pelotita, control = ctree_control(maxdepth = 5))
  
  # plot(arbol_pelotita, gp = gpar(fontsize = 5)) 
  
  arbol_pelotita
  
}

haceme_un_arbolito()

bosque <- list()


for(i in 1:M) {
  
  message("Yay! iteracion ", i)
  
  mod <- haceme_un_arbolito()
  
  bosque[i] <- list(mod)
  
}

# bosque

# predecir ----------------------------------------------------------------
df_pred <- map(1:M, function(i){
  message("Yay! prediciendo con el arbol ", i)
  mod <- bosque[[i]]
  
  data_test %>% 
    # mutate(pred = predict(mod, newdata = data_test, type = "prob")[,1]) %>%
    mutate(pred = predict(mod, newdata = data_test)) %>% 
    select(pred) %>% 
    mutate(id = 1:nrow(data_test), arbolito = i)
  
}) %>% 
  reduce(bind_rows)

df_pred

na_a_0 <- function(x) ifelse(is.na(x), 0, x)
na_a_0(c(NA, 1, 2,3,4, NA))

data_test %>% 
  count(in_sf) %>% 
  mutate(p = n/sum(n))

df_pred_f <- df_pred %>% 
  count(id, pred) %>% 
  spread(pred, n)

data_test2 <- data_test %>% 
  select(in_sf) %>% 
  bind_cols(df_pred_f) %>% 
  ungroup() %>% 
  mutate_all(na_a_0) %>% 
  mutate(pred = ifelse(N >= Y, "N", "Y"))

glimpse(data_test2)

data_test2

data_test2 %>% 
  group_by(in_sf, pred) %>%
  summarise(n = n()) %>% 
  mutate(que_pasho = ifelse(in_sf == pred, ":)", ":(")) %>% 
  group_by(que_pasho) %>% 
  summarise(n = sum(n)) %>% 
  mutate(tasa_clasificacion_mundial = n/sum(n))


# install.packages("randomForest")
library(randomForest)

data_train <- filter(data_train, complete.cases(data_train))

bosque_verdad <- randomForest(as.factor(in_sf) ~ ., data = data_train, ntree = M, do.trace = TRUE)

predict(bosque_verdad, newdata = data_test)

data_test <- data_test %>%
  mutate(mod_rf = predict(bosque_verdad, newdata = data_test))

data_test %>% 
  group_by(in_sf, mod_rf) %>%
  summarise(n = n()) %>% 
  mutate(que_pasho = ifelse(in_sf == mod_rf, ":)", ":(")) %>% 
  group_by(que_pasho) %>% 
  summarise(n = sum(n)) %>% 
  mutate(tasa_clasificacion_mundial = n/sum(n))


data_test %>% 
  group_by(in_sf, mod_rf) %>%
  summarise(n = n()) %>% 
  spread(mod_rf, n) %>% 
  ungroup() 


