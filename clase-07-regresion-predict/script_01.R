rm(list = ls())
library(tidyverse)


# data --------------------------------------------------------------------
data <- data_frame(
  x = seq(1, 100, length.out = 100),
  y = 0.001 * (x - mean(x)) ^ 3  + rnorm(length(x)) * sqrt(x)
)

data <- mutate(data, y =  1 + (y - min(y)) / (max(y) - min(y))  )

data

ggplot(data) + 
  geom_point(aes(x, y))

ggplot(data) + 
  geom_point(aes(x, y)) + 
  geom_smooth(aes(x, y), method = "lm")

plot(resid(lm(y ~ x, data = data)))

ggplot(data) + 
  geom_point(aes(x, log(y)))

ggplot(data) + 
  geom_point(aes(x, sqrt(y)))

ggplot(data) + 
  geom_point(aes(x, sqrt(y)))



# ?! ----------------------------------------------------------------------
ggplot(data) + 
  geom_point(aes(x^1, y))

ggplot(data) + 
  geom_point(aes(x^2, y))

ggplot(data) + 
  geom_point(aes(x^3, y))

data <- data %>% 
  mutate(
    x2 = x^2,
    x3 = x^4,
    x5 = x^5,
    x6 = x^7,
    x7 = x^7
    )

plot(resid(lm(y ~ ., data = data)))



"https://archive.ics.uci.edu/ml/datasets/bike+sharing+dataset"


