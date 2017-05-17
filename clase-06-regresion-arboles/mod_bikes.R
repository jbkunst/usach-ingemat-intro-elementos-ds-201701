rm(list = ls())
library(tidyverse)
library(lubridate)

data <- read_csv("day.csv")
glimpse(data)
View(data)




ggplot(data) +
  geom_point(aes(dteday, cnt), shape = 1)


ggplot(data) +
  geom_point(aes(dteday, cnt), shape = 1) +
  geom_smooth(aes(dteday, cnt), method = "lm")


data <- mutate(data, muestra = ifelse(dteday <= ymd("20120701"), "d", "v"))


ggplot(data) +
  geom_point(aes(dteday, cnt, color = muestra)) +
  geom_smooth(aes(dteday, cnt), method = "lm")

count(data, muestra)
dmod <- filter(data, muestra == "d")

ggplot(dmod) +
  geom_point(aes(dteday, cnt), shape = 1)


# modelo 0 ----------------------------------------------------------------
mod0 <- lm(cnt ~ dteday, data = dmod)

dmod <- mutate(dmod, pred0 = predict(mod0))

ggplot(dmod) +
  geom_point(aes(dteday, cnt), shape = 1) +
  geom_point(aes(dteday, pred0), color = "red")


# modelo 1 ----------------------------------------------------------------
dmod <- mutate(dmod, dteday2 = as.numeric(dteday)^2)

# as.numeric(dmod$dteday)

mod1 <- lm(cnt ~ dteday + dteday2, data = dmod)
mod1

dmod <- mutate(dmod, pred1 = predict(mod1))

ggplot(dmod) +
  geom_point(aes(dteday, cnt), shape = 1) +
  geom_point(aes(dteday, pred0), color = "red", alpha = 0.2) +
  geom_point(aes(dteday, pred1), color = "blue", alpha = 0.2)



# modelo 2 ----------------------------------------------------------------
dmod <- mutate(dmod, dteday3 = as.numeric(dteday)^3)

# as.numeric(dmod$dteday)
mod2 <- lm(cnt ~ dteday + dteday2 + dteday3, data = dmod)

dmod <- mutate(dmod, pred2 = predict(mod2))

ggplot(dmod) +
  geom_point(aes(dteday, cnt), shape = 1) +
  geom_point(aes(dteday, pred2), color = "red")


# modelo 3 ----------------------------------------------------------------
# mas variables
dmod <- mutate(dmod, dteday5 = as.numeric(dteday)^5)

# as.numeric(dmod$dteday)
mod3 <- lm(cnt ~ dteday + dteday2 + dteday3 + dteday5, data = dmod)

dmod <- mutate(dmod, pred3 = predict(mod3))

ggplot(dmod) +
  geom_point(aes(dteday, cnt), shape = 1) +
  geom_point(aes(dteday, pred2), color = "red") +
  geom_line(aes(dteday, pred3), color = "blue", alpha = 0.5)

# no mejora, puede que dteday ya no aporte con mas información


# modelo 4 ----------------------------------------------------------------
names(dmod)
mod3 <- lm(cnt ~ dteday + dteday2 + dteday3 + , data = dmod)

# que variable creen que "aportaria" a tener una mejor prediccion?
# dijeron por ahí, temperatura
ggplot(dmod) +
  geom_point(aes(temp, cnt))

ggplot(dmod) +
  geom_point(aes(temp, cnt, color = yr))
  
ggplot(dmod) +
  geom_point(aes(temp, cnt, color = yr)) +
  facet_wrap(~factor(yr))

ggplot(dmod) +
  geom_point(aes(temp, cnt))

ggplot(dmod) +
  geom_point(aes(temp, cnt, color = factor(season)))

ggplot(dmod) +
  geom_point(aes(temp, cnt, color = season)) +
  facet_wrap(~factor(season))


ggplot(dmod) +
  geom_point(aes(season, cnt, color = season)) +
  geom_smooth(aes(season, cnt), method = "lm")


names(dmod)
mod4 <- lm(cnt ~ dteday + dteday2 + dteday3 + season, data = dmod)
mod4

dmod$season
mod41 <- lm(cnt ~ dteday + dteday2 + dteday3 + factor(season), data = dmod)
mod41

dmod <- dmod %>% 
  mutate(
    pred4 = predict(mod4),
    pred41 = predict(mod41)
  )



ggplot(dmod) +
  geom_point(aes(dteday, cnt), shape = 1) +
  geom_line(aes(dteday, pred3), color = "red", size = 1.2) +
  geom_line(aes(dteday, pred4), color = "blue", alpha = 0.5, size = 1.2) +
  geom_line(aes(dteday, pred41), color = "green", size = 1.2)



# modelo 5 ----------------------------------------------------------------
names(dmod)

head(dmod)

ggplot(dmod) +
  geom_point(aes(instant, cnt))

glimpse(dmod)

mod5 <- lm(cnt ~ dteday + dteday2 + dteday3 + factor(season) + 
             factor(weathersit), data = dmod)
mod5


dmod <- dmod %>% 
  mutate(
    pred5 = predict(mod5))

ggplot(dmod) +
  geom_point(aes(windspeed, cnt)) +
  facet_wrap(~season)

ggplot(dmod) +
  geom_boxplot(aes(factor(weathersit), cnt))

count(dmod, weathersit)

ggplot(dmod) +
  geom_point(aes(dteday, cnt), shape = 1, alpha = 0.3) +
  geom_line(aes(dteday, pred41), color = "yellow", size = 1.1) +
  geom_line(aes(dteday, pred5), color = "red", size = 1.1, alpha = 0.4)

# aplicar modelo al futuro ------------------------------------------------

dim(dmod)
dim(data)

data <- data %>% 
  mutate(
    dteday2 = as.numeric(dteday)^2,
    dteday3 = as.numeric(dteday)^3,
    dteday4 = as.numeric(dteday)^4
  )

data <- data %>% mutate(pred5 = predict(mod5, newdata = data))


ggplot(data) +
  geom_point(aes(dteday, cnt, color = muestra)) +
  geom_smooth(aes(dteday, cnt), method = "lm") +
  geom_line(aes(dteday, pred5), color = "red", size = 1.1, alpha = 0.4)






