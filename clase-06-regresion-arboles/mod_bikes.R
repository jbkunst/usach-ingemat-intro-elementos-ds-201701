rm(list = ls())
library(tidyverse)
library(lubridate)




data <- read_csv("day.csv")
# data <- read.csv("day.csv")
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
mod0

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
mod2

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
# que variable creen que "aportaria" a tener una mejor prediccion?
# dijeron por ahí, temperatura
mod3 <- lm(cnt ~ dteday + dteday2 + dteday3 + temp, data = dmod)
mod3

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
  geom_point(aes(temp, cnt, color = as.character(season))) +
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

dmod$season
mod42 <- lm(cnt ~ factor(season), data = dmod)
mod42


dmod <- dmod %>% 
  mutate(
    pred4 = predict(mod4),
    pred41 = predict(mod41),
    pred42 = predict(mod42)
  )

ggplot(dmod) +
  geom_point(aes(dteday, cnt), shape = 1, alpha = 0.2) +
  geom_line(aes(dteday, pred3), color = "red", size = 1.2) +
  geom_line(aes(dteday, pred42), color = "green", size = 1.2) + 
  geom_line(aes(dteday, pred4), color = "blue", alpha = 0.5, size = 1.2) +
  geom_line(aes(dteday, pred41), color = "pink", size = 1.2)

#tenemos una escalurva

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
  facet_grid(yr ~season)

ggplot(dmod) +
  geom_boxplot(aes(factor(weathersit), cnt))

count(dmod, weathersit)

ggplot(dmod) +
  geom_point(aes(dteday, cnt), shape = 1, alpha = 0.3) +
  geom_line(aes(dteday, pred41), color = "yellow", size = 1.1) +
  geom_line(aes(dteday, pred5), color = "red", size = 1.1, alpha = 0.4)


# mod6 --------------------------------------------------------------------
# modelo 5 ----------------------------------------------------------------
names(dmod)
mod6 <- lm(cnt ~ dteday + factor(mnth), data = dmod)
mod6

mod61 <- lm(cnt ~ dteday + factor(mnth) + factor(weathersit), data = dmod)
mod61



vum13 <- function(x = c(1:10,10:5)) {
  unicos <- x %>% 
    unique() %>% 
    length() 
  unicos <= 13
}

vum13(c(1,2,3,3,3,3,3,3,3))
vum13()

dmod2 <- dmod %>% 
  select(-contains("pred"), -matches("dteday\\d"), -muestra,
         -casual, -registered, -instant) %>% 
  # mutate(
  #   season = factor(season),
  #   mnth = factor(mnth)
  # )
  # mutate_at(vars(season, mnth), factor)
  mutate_if(vum13, factor)

dmod2 %>% glimpse()

names(dmod2) %>% 
  paste(collapse = "")
head(dmod2)

mod62 <- lm(cnt ~ ., data = dmod2)

# aplicar modelo al futuro ------------------------------------------------
dim(dmod)
dim(data)


length(predict(mod5))

predict(mod5, newdata = data)

data <- data %>% 
  mutate(
    dteday2 = as.numeric(dteday)^2,
    dteday3 = as.numeric(dteday)^3,
    dteday4 = as.numeric(dteday)^4
    )

length(predict(mod5, newdata = data))

p62 <- data %>% 
  mutate_if(vum13, factor) %>% 
  predict(mod62, newdata = .)

data <- data %>%
  mutate(
    pred5 = predict(mod5, newdata = data),
    pred6 = predict(mod6, newdata = data),
    pred61 = predict(mod61, newdata = data),
    pred62 = p62 
    )


ggplot(data) +
  geom_point(aes(dteday, cnt, color = muestra, alpha = 0.2)) +
  geom_smooth(aes(dteday, cnt), method = "lm") +
  # geom_line(aes(dteday, pred5), color = "red", size = 1.1, alpha = 0.4) + + # :B
  geom_line(aes(dteday, pred61), color = "red", size = 1, alpha = 1) + #  :'(
 geom_line(aes(dteday, pred6), color = "navy", size = 1, alpha = 1) +
 geom_line(aes(dteday, pred62), color = "green", size = 1, alpha = 0.51)

# cual modelo es el mejorsh -----------------------------------------------
glimpse(data)
data <- data %>% 
  mutate(
    e6 = (pred6 - cnt)^2,
    e61 = (pred61 - cnt)^2,
    e62 = (pred62 - cnt)^2
  )

mod6
mod61
mod62

data %>% 
  summarise(
    sep6 = mean(e6),
    sep61 = mean(e61),
    sep62 = mean(e62)
   )


data %>% 
  group_by(muestra) %>% 
  summarise(
    sep6 = mean(e6),
    sep61 = mean(e61),
    sep62 = mean(e62)
  )

data %>% 
  group_by(muestra) %>% 
  summarise(
    sep6 = mean(abs(pred6 - cnt)),
    sep61 = mean(abs(pred61 - cnt)),
    sep62 = mean(abs(pred62 - cnt))
  )

# separa los datos para tener 

s <- 1:10
for(i in s) {
  print(i)
  
}

d <- data_frame(
 
)

d
d1 <- data_frame(
  y = 2, z = 3
)
d1

bind_cols(d, d1)
bind_rows(d, d1)

d
for(i in s) {
  print(i)
  d2 <- data_frame(y = i, x = i*i)
  
  d <- bind_rows(d, d2)

}

d




