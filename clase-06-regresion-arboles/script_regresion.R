rm(list = ls())
library(tidyverse)
library(stringr)

if("jbkmisc" %in% installed.packages()) {
  library(jbkmisc)
  theme_set(theme_jbk())
}

dfanscombe <- tbl_df(anscombe) %>% 
  gather(key, value) %>% 
  separate(key, c("valor", "set"), sep = 1)

dfanscombe <- bind_cols(
  dfanscombe %>% filter(valor == "x") %>% rename(x = value) %>% select(-set, -valor),
  dfanscombe %>% filter(valor == "y") %>% rename(y = value) %>% select(-valor)
 ) %>% 
  mutate(set2 = factor(set, labels = c("Normal (x1, y1)", "Falta de Ajuste (x2, y2)",
                                      "Outiler (x3, y3)", "Influencia (x4, y4)")))

dfanscombe <- dfanscombe %>% 
  arrange(set, x)

dfanscombe

ggplot(dfanscombe, aes(x = x, y = y)) +
  geom_point(color = "darkred", size = 4, shape = 1) +
  geom_smooth(method = "lm", color = "navy", alpha = 0.1) +
  facet_wrap(~set2, scales = "fixed") +
  labs(x = NULL, y = NULL)

dfanscombe
filter(dfanscombe, set == 4)

ggplot(filter(dfanscombe, set == 1), aes(x = x, y = y)) +
  geom_point(color = "darkred", size = 4, shape = 1) +
  geom_smooth(method = "lm", color = "navy", alpha = 0.1) +
  facet_wrap(~set2, scales = "fixed") +
  labs(x = NULL, y = NULL)


lm1 <- lm(y ~ x, data = filter(dfanscombe, set == 1))
lm1

lm2 <- lm(y ~ x, data = filter(dfanscombe, set == 2))
lm2

lm3 <- lm(y ~ x, data = filter(dfanscombe, set == 3))
lm3

lm4 <- lm(y ~ x, data = filter(dfanscombe, set == 4))
lm4

lm1$residuals

#' Evaluacion de calidad del modelo estudiando errores (residuos)
dfanscombe <- dfanscombe %>% 
  mutate(
    res = c(lm1$residuals, lm2$residuals, lm3$residuals, lm4$residuals)
  )
dfanscombe

ggplot(dfanscombe, aes(x = x, y = res)) +
  geom_point(color = "darkred", size = 4, shape = 1) +
  geom_smooth(method = "lm", color = "navy", alpha = 0.1) +
  geom_smooth(method = "loess", color = "skyblue", alpha = 0.1, span = 1, se = FALSE) +
  facet_wrap(~set2, scales = "fixed") +
  labs(x = NULL, y = NULL)




# transformaciones --------------------------------------------------------
df <- data_frame(
  x = 1:50,
  y = x^2 + rnorm(length(x), sd = 25) + 10
)
df

ggplot(df, aes(x, y)) + 
  geom_point(shape = 1)

ggplot(df, aes(x, y)) + 
  geom_point(shape = 1) +
  geom_smooth(method = "lm")

mod <- lm(y ~ x, data = df)
df <- mutate(df, er = as.vector(mod$residuals))

ggplot(df) + 
  geom_point(aes(x, er))

ggplot(df, aes(x, log(y))) + 
  geom_point(shape = 1) +
  geom_smooth(method = "lm")

ggplot(df, aes(x, sqrt(y))) + 
  geom_point(shape = 1) +
  geom_smooth(method = "lm")


mod2 <- lm(sqrt(y) ~ x, data = df)
df <- mutate(df, er2 = as.vector(mod2$residuals))

ggplot(df) + 
  geom_point(aes(x, er2))


# heterocedasticidad
df <- data_frame(
  x = 1:100,
  y = 50 +  10 * x + (10 + x^1.2) * rnorm(length(x), sd = 1)
)
df

ggplot(df, aes(x, y)) + 
  geom_point(shape = 1)

ggplot(df, aes(x, y)) + 
  geom_point(shape = 1) +
  geom_smooth(method = "lm")

ggplot(df, aes(x, log(y))) + 
  geom_point(shape = 1) +
  geom_smooth(method = "lm")

ggplot(df, aes(x, sqrt(y))) + 
  geom_point(shape = 1) +
  geom_smooth(method = "lm")


# seleccion de variables para otro momento!
lm(y ~ x1 + x2 + x3 + x4 + x5)


