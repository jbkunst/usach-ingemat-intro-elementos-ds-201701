library(tidyverse)
data("anscombe")

lm1 <- lm(y1 ~ x1, data = anscombe)
lm1

lm2 <- lm(y2 ~ x2, data = anscombe)
lm2

lm3 <- lm(y3 ~ x3, data = anscombe)
lm3

lm4 <- lm(y4 ~ x4, data = anscombe)
lm4

anscombe <- anscombe %>% 
  mutate(
    res1 = lm1$residuals,
    res2 = lm2$residuals,
    res3 = lm3$residuals,
    res4 = lm4$residuals
  )

ggplot(arrange(anscombe, x1)) + 
  geom_point(aes(x1, res1))

ggplot(arrange(anscombe, x2)) + 
  geom_point(aes(x2, res2))

ggplot(arrange(anscombe, x3)) + 
  geom_point(aes(x3, res3))

ggplot(arrange(anscombe, x4)) + 
  geom_point(aes(x4, res4))



# ?!
summary(lm1)
summary(lm2)
summary(lm3)
summary(lm4)



x <- rnorm(100)
y <- 2 + 0.5 * x

n <- rnorm(length(y),s=0.4)

nfit <- lm(n ~ x)

y1 <- y + nfit$residuals

plot(x, n)
plot(x, y)
plot(x, y1)

lm(y1 ~ x)




