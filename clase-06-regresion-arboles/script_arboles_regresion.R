library(tidyverse)
library(partykit)

data(credit, package = "riskr")

glimpse(credit)

# credit <- mutate(credit, bad = factor(bad))

credit %>% 
  summarise_all(function(x) length(unique(x))) %>% 
  gather(variable, unicos)

count(credit, flag_other_card)

credit <- select(credit, -flag_other_card, -flag_mobile_phone, -flag_contact_phone)
credit

# factor
# x <- c("hola", "chao")
# x <- c(x, "buenas tardes")
# 
# x <- factor(c("hola", "chao"))
# x
# 
# x <- c(x, "buenas tardes")
# x

# riskr::gg_ba2(cut_width(credit$months_in_the_job, 12*5), as.numeric(credit$bad) - 1)


credit <- tbl_df(credit)
credit <- mutate_if(credit, is.character, as.factor)

mod <- ctree(bad ~ sex + age + months_in_residence, data = head(credit, 2000))
mod <- ctree(bad ~ ., data = credit)

mod

plot(mod)
# UFFF!! JUERAA!!!

mod <- ctree(factor(bad) ~ ., data = credit, control = ctree_control(maxdepth = 4))
mod
plot(mod)
plot(mod, gp = gpar(fontsize = 7)) 

# evaluar el modelo
pred <- predict(mod)
real <- credit$bad

table(pred, real)


riskr::conf_matrix(pred_class = pred, target = real)




