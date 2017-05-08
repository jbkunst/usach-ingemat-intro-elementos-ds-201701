library(dplyr)
library(purrr)
library(ggplot2)
data(flights, package = "nycflights13")


if("jbkmisc" %in% installed.packages()) {
  library(jbkmisc)
  theme_set(theme_jbk())
}

glimpse(flights)

set.seed(123)
flights2 <- flights %>% 
  select(distance, air_time) %>% 
  filter(!is.na(air_time) & !is.na(distance)) %>% 
  filter(distance > quantile(distance, 0.01)) %>% 
  filter(distance < quantile(distance, 0.99)) %>% 
  mutate(air_time_horas = air_time/60,
         distance_km = distance * 1.60934) %>% 
  sample_n(1000)

gg <- ggplot(flights2) + 
  geom_point(aes(distance_km, air_time_horas), alpha = 0.5, size = 0.5) +
  xlim(0, 4500) + 
  ylim(0, 6.5) 
gg


# lm(air_time_horas ~ distance_km, data = flights2)  # ??!!
models <- data_frame(
  b0 = runif(100, 0.0, .6),
  b1 = runif(100, 0.0000, 0.002),
  tipo = "random"
)
models

gg <- gg + 
  geom_abline(aes(intercept = b0, slope = b1), data = models, alpha = 0.25, 
              color = "skyblue")
gg 

# modelo al ojo
gg <- gg + 
  geom_segment(aes(x = 500, xend = 4000, y = 1, yend = 6),
               color = "navy", size = 1.2, alpha = 0.5)

# llevarlo a pendiente intercepto
x <- c(500, 4000)
y <- c(1, 6)

m <- (y[2] - y[1])/(x[2] - x[1])
b <- -m*x[1] + y[1]

model2 <- data_frame(b0 = b, b1 = m, tipo = "ojimetro")

gg <- gg + 
  geom_abline(aes(intercept = b0, slope = b1), data = model2, alpha = 0.5,
              color = "red", size = 1.2)
gg


error_cuadratico_medio <- function(a = 1, b = 1) {
  ecm <- flights2 %>% 
    mutate(air_time_horas_pred = distance_km * a + b,
           diff = air_time_horas_pred - air_time_horas,
           diff2 = diff^2) %>% 
    summarise(ecm = mean(diff2)) %>% 
    .[[1]]
  ecm
    
}

models <- bind_rows(models, model2)
models

models <- mutate(models, ecm = map2_dbl(b0, b1, error_cuadratico_medio))
models



