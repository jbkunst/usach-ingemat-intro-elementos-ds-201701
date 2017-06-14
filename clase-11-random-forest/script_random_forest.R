library(tidyverse)
library(partykit)

# data <- read_csv("https://raw.githubusercontent.com/jbkunst/riskr/master/data/data.csv")
data <- read_csv("train.csv")
names(data)

glimpse(data)

row <- sample(nrow(data), size = 1)

data %>% filter(row_number() == row)

photo <- data_frame(x = rep(1:28, times=28), y = rep(28:1, each = 28),
                    shade = as.numeric(data[row,-1]),
                    px = 1:(28*28))

photo
summary(photo$shade)
ggplot(data=photo) +
  geom_point(aes(x = x, y = y, color = shade), size=11, shape=15) +
  scale_color_gradient(low = "white", high = "black") +
  theme(legend.position = "none")

ggplot(data=photo) +
  geom_label(aes(x = x, y = y, label = px), size = 2) +
  theme(legend.position = "none")


count(data, label)  

# algunos cosas necesarias
data <- mutate(data, label = factor(label))

# tamohs ready
glimpse(data)

set.seed(123)
s <- sample(c("train", "test"), size = nrow(data), replace = TRUE)

data_test <- filter(data, s == "train")
data_train <- filter(data, s == "test")


# arbol iluso -------------------------------------------------------------
mod_ilu <- ctree(label ~ ., data = data_test) #  :O
plot(mod_ilu)
plot(mod_ilu, gp = gpar(fontsize = 5)) 

plot( ctree(label ~ ., data = data_test, control = ctree_control(maxdepth = 4)),
      gp = gpar(fontsize = 5)) 

data_test <- data_test %>%
  mutate(mod_ilu_pred = predict(mod_ilu, newdata = data_test))

data_test %>% 
  group_by(label, mod_ilu_pred) %>%
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(que_pasho = ifelse(label == mod_ilu_pred, ":)", ":(")) %>% 
  group_by(que_pasho) %>% 
  summarise(n = sum(n)) %>% 
  mutate(tasa_clasificacion_mundial = n/sum(n))


data_test %>% 
  group_by(label, mod_ilu_pred) %>%
  summarise(n = n()) %>% 
  spread(mod_ilu_pred, n) %>% 
  ungroup() 

# mod bagging -------------------------------------------------------------
M <- 50 
dim(data)
P <- round(sqrt(ncol(data_train)))

haceme_un_arbolito <- function() {
  
  p <- sample(2:ncol(data), size = P)
  print(names(data_train)[p])
  
  data_pelotita <- data_train %>% 
    select(p, label) %>% 
    sample_n(nrow(data_train), replace = TRUE)
  
  arbol_pelotita <- ctree(label ~ ., data_pelotita) #, control = ctree_control(maxdepth = 5))
  
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
    mutate(pred = predict(mod, newdata = data_test)) %>% 
    select(pred) %>% 
    mutate(id = 1:nrow(data_test), arbolito = i)
  
}) %>% 
  reduce(bind_rows)

df_pred

na_a_0 <- function(x) ifelse(is.na(x), 0, x)
na_a_0(c(NA, 1, 2,3,4, NA))

data_test %>% 
  count(label) %>% 
  mutate(p = n/sum(n))

df_pred_f <- df_pred %>% 
  group_by(id) %>% 
  count(pred) %>% 
  spread(pred, n) %>%
  ungroup() %>% 
  mutate_all(na_a_0)

df_pred_f

df_pred_f <- df_pred %>% 
  group_by(id) %>% 
  count(pred) %>%
  group_by(id) %>% 
  arrange(id, desc(n)) %>% 
  distinct(id, .keep_all = TRUE) %>% 
  left_join(df_pred_f, ., by = "id")
  
df_pred_f
  
  
data_test2 <- data_test %>% 
  select(label) %>% 
  bind_cols(df_pred_f)

glimpse(data_test2)

data_test2 %>% 
  group_by(label, pred) %>%
  summarise(n = n()) %>% 
  mutate(que_pasho = ifelse(label == pred, ":)", ":(")) %>% 
  group_by(que_pasho) %>% 
  summarise(n = sum(n)) %>% 
  mutate(tasa_clasificacion_mundial = n/sum(n))


# install.packages("randomForest")
library(randomForest)

data_train <- filter(data_train, complete.cases(data_train))

bosque_verdad <- randomForest(label ~ ., data = data_train, ntree = M, do.trace = TRUE)

predict(bosque_verdad, newdata = data_test)

data_test <- data_test %>%
  mutate(mod_rf = predict(bosque_verdad, newdata = data_test))

data_test %>% 
  group_by(label, mod_rf) %>%
  summarise(n = n()) %>% 
  mutate(que_pasho = ifelse(label == mod_rf, ":)", ":(")) %>% 
  group_by(que_pasho) %>% 
  summarise(n = sum(n)) %>% 
  mutate(tasa_clasificacion_mundial = n/sum(n))


data_test %>% 
  group_by(label, mod_rf) %>%
  summarise(n = n()) %>% 
  spread(mod_rf, n) %>% 
  ungroup() 

dfimp <- importance(bosque_verdad) %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column() %>% 
  tbl_df() %>%
  mutate(
    x = rep(1:28, times=28), y = rep(28:1, each = 28)
  )

dfimp

ggplot(dfimp) + 
  geom_point(aes(rowname, MeanDecreaseGini))
  
ggplot(dfimp) + 
  geom_tile(aes(x = x, y = y, fill = MeanDecreaseGini))

