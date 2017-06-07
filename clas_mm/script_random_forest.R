library(tidyverse)
library(partykit)

credit <- read_csv("https://raw.githubusercontent.com/jbkunst/riskr/master/data/credit.csv")
glimpse(credit)

# removemos algunas columnas constantes
credit <- select(credit, -flag_other_card, -flag_mobile_phone, -flag_contact_phone)

# algunos cosas necesarias
credit <- mutate_if(credit, is.character, as.factor)
credit <- mutate(credit, bad = factor(bad))

# tamohs ready
glimpse(credit)


data_test <- filter(credit, row_number() <= 25000)
data_train <- filter(credit, row_number() > 25000)


# arbol iluso -------------------------------------------------------------
mod_ilu <- ctree(bad ~ ., data = data_test) #  :O
plot(mod_ilu)
plot(mod_ilu, gp = gpar(fontsize = 5)) 

data_test <- data_test %>%
  mutate(mod_ilu_pred = predict(mod_ilu, newdata = data_test))

data_test %>% 
  group_by(bad, mod_ilu_pred) %>%
  summarise(n = n()) %>% 
  mutate(que_pasho = ifelse(bad == mod_ilu_pred, ":)", ":(")) %>% 
  group_by(que_pasho) %>% 
  summarise(n = sum(n)) %>% 
  mutate(tasa_clasificacion_mundial = n/sum(n))


# mod bagging -------------------------------------------------------------
M <- 612 
dim(credit)
P <- round(sqrt(14))

map(1:10, log)

haceme_un_arbolito <- function() {
  
  
  p <- sample(1:13, size = P)
  print(names(data_train)[p])
  
  data_pelotita <- data_train %>% 
    select(p, bad) %>% 
    sample_n(nrow(data_train), replace = TRUE)
  
  arbol_pelotita <- ctree(bad ~ ., data_pelotita, control = ctree_control(maxdepth = 3))
  
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
    mutate(id = 1:25000, arbolito = i)
  
}) %>% 
  reduce(bind_rows)

df_pred

na_a_0 <- function(x) ifelse(is.na(x), 0, x)
na_a_0(c(NA, 1, 2,3,4, NA))

data_test %>% 
  count(bad) %>% 
  mutate(p = n/sum(n))

df_pred_f <- df_pred %>% 
  group_by(id) %>% 
  count(pred) %>% 
  spread(pred, n) %>%
  rename(ceros = `0`, unos = `1`) %>% 
  ungroup() %>% 
  mutate_all(na_a_0) %>% 
  mutate(perct_0 = ceros/M, 
         pred_f = ifelse(perct_0 > 0.8, "0", "1"))  

data_test2 <- bind_cols(data_test, df_pred_f)
glimpse(data_test2)
df_pred_f %>% 
  arrange(desc(unos))

ggplot(df_pred_f) + 
  geom_histogram(aes(perct_0))

data_test2 %>% 
  group_by(bad, pred_f) %>%
  summarise(n = n()) %>% 
  mutate(que_pasho = ifelse(bad == pred_f, ":)", ":(")) %>% 
  group_by(que_pasho) %>% 
  summarise(n = sum(n)) %>% 
  mutate(tasa_clasificacion_mundial = n/sum(n))

# MEA MEA culpa 

# install.packages("randomForest")
library(randomForest)

data_train <- filter(data_train, complete.cases(data_train))

bosque_verdad <- randomForest(bad ~ ., data = data_train, ntree = M, do.trace = TRUE)

predict(bosque_verdad, newdata = data_test)

data_test <- data_test %>%
  mutate(mod_rf = predict(bosque_verdad, newdata = data_test))

data_test %>% 
  group_by(bad, mod_rf) %>%
  summarise(n = n()) %>% 
  mutate(que_pasho = ifelse(bad == mod_rf, ":)", ":(")) %>% 
  group_by(que_pasho) %>% 
  summarise(n = sum(n)) %>% 
  mutate(tasa_clasificacion_mundial = n/sum(n))



