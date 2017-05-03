library(dplyr)
library(ggplot2)
library(nycflights13)
library(purrr)


glimpse(flights)

set.seed(123)
flights2 <- flights %>% 
  filter(!is.na(air_time), !is.na(distance)) %>% 
  select(distance, air_time) %>% 
  filter(distance < quantile(distance, .99)) %>% 
  sample_n(1000)

flights2 <- flights2 %>% 
  mutate(
    distance_km = distance * 1.60934,
    air_time_hr = air_time / 60
  )

flights2

gg <- ggplot(flights2) +
  geom_point(aes(distance_km, air_time_hr),
             alpha = 0.25) +
  xlim(0, 4500) + ylim(0, 6.5)
gg
# df <- data_frame(
#   x = c(rep(1, 1), rep(2, 10)),
#   y = 0
# )
# df
# ggplot(df) + geom_point(aes(x, y))
# ggplot(df) + geom_point(aes(x, y), alpha = 0.1)

model <- data_frame(
  b0 = runif(100, 0, 0.6),
  b1 = runif(100, 0.0005, 0.002)
)

gg <- gg + 
  geom_abline(aes(intercept = b0, slope = b1),
              data = model, alpha = 0.2,
              color = "skyblue")
gg

# modelo mejor? ah?!
gg +
  geom_segment(aes(x = 500, xend = 4000,
                y = 1, yend = 5.5),
               color = "red", size = 1.2)

model
x <- c(500, 4000)
y <- c(1, 5.5)

m <- (y[2]-y[1])/(x[2]-x[1])
b <- -m*x[1] + y[1]


modelojo <- data_frame(b0 = b, b1 = m, tipo = "ojo")  
modelojo


1/0.001285714

gg <- gg + 
  geom_abline(aes(intercept = b0, slope = b1),
              data = modelojo, alpha = 0.5,
              size = 1.1, color = "red")
gg

modelos <- bind_rows(model, modelojo)
tail(modelos)


ecm <- function(b0, b1) {
  # b0 <- 0.3; b1 <- 0.002
  ecmdf <- flights2 %>% 
    mutate(
      air_time_hr_pred = b0 + b1 * distance_km,
      e_i = air_time_hr - air_time_hr_pred,
      e_i2 = e_i ^ 2
    ) %>% 
    summarise(
      ecm = sum(e_i2)
    )
    
  ecmdf$ecm
    
}

ecm(0.4, 0.001)
ecm(0.4, 0.002)

s <- 1:10
s

# sqrt(s)
map(s, sqrt)
map_dbl(s, sqrt)

map2(x, y, sum)
map2_dbl(x, y, sum)
map2_dbl(x, y, ecm)

modelos <- modelos %>% 
  mutate(error_cuad = map2_dbl(b0, b1, ecm),
         bla = cut_interval(error_cuad, 10))
modelos

ggplot(flights2) +
  xlim(0, 4500) + ylim(0, 6.5) +
  geom_abline(aes(intercept = b0, slope = b1, color = bla),
              data = modelos, size = 1.2, alpha = 0.2) +
  geom_point(aes(distance_km, air_time_hr), alpha = 0.25) +
  viridis::scale_color_viridis(discrete = TRUE, direction = -1)


modelos %>%
  arrange(error_cuad)

modelos <- mutate(
  modelos,
  tipo = ifelse(is.na(tipo), "random", tipo)
  )

ggplot(modelos) +
  geom_point(aes(b0, b1, color = tipo, alpha = 1/error_cuad),
             size = 3) +
  viridis::scale_color_viridis(direction = -1, discrete = TRUE)
 
# al fin llegamos! Perdimos todo este rato para que?
flights2
modelojo
mod <- lm(air_time_hr ~ distance_km, data = flights2)
coefficients(mod)

gg + 
  geom_smooth(aes(distance_km, air_time_hr),
              method = "lm", color = "darkgreen", alpha = 0.5)

modelos %>%
  arrange(error_cuad) %>% 
  head(1)

ecm(coefficients(mod)[1], coefficients(mod)[2])

mod

summary(mod)

mod$coefficients
mod$residuals

qplot(x = 1:1000, y = mod$residuals, geom = "point")



df <- data_frame(
  x = 1:10,
  y = x*x 
)

ggplot(df) +
  geom_point(aes(x, y)) 

ggplot(df) +
  geom_point(aes(x, y)) +
  geom_smooth(aes(x, y), method = "lm")

mod2 <- lm(y ~ x, data = df)

qplot(x = 1:10, y = mod2$residuals, geom = "point")
# no son independientes

qplot(x = 1:20, y = head(mod$residuals, 20), geom = "point")

df

ggplot(df) +
  geom_point(aes(x, y))

df <- mutate(df, sqrty = sqrt(y))

ggplot(df) +
  geom_point(aes(x, sqrty))

mod3 <- lm(sqrty ~ x, data = df)

qplot(x = 1:10, y = mod3$s, geom = "point")

