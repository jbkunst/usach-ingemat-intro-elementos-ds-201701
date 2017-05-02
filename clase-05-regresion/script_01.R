library(dplyr)
library(ggplot2)
data("cars")

cars <- tbl_df(cars)
cars

ggplot(cars) + 
  geom_point(aes(speed, dist))


plot(AirPassengers)
x <- AirPassengers
ret <- data.frame(data.frame(x), index = zoo::index(x))
ret <- tidyr::gather(ret, series, value, -index)
ret <- dplyr::tbl_df(ret)
ret <- mutate(ret, year = floor(index))
ret <- summarise(group_by(ret, year), passengers = sum(value))
ret

ggplot(ret) +
  geom_point(aes(year, passengers))

broom::tidy(AirPassengers)
