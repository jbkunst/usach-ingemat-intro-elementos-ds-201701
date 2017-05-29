library(ggplot2)
library(tibble)
library(dplyr)
data("mtcars")

head(mtcars)
mtcars <- tibble::rownames_to_column(mtcars)
glimpse(mtcars)
ggplot(mtcars) +
  geom_col(aes(rowname, disp)) 

glimpse(iris)
View(iris)

iris <- mutate(iris, nc = 
                 ifelse(Species == "setosa",
                         "setosa doh",
                         ifelse(Species == "versicolor", "Versicolor2", "otracosa")))

df <- data_frame(
  Species = c("setosa", "versicolor", "virginica"),
  nc2 = c("setosa do", "versicolor2", "otracosa")
)

df

count(iris, Species, nc)

iris <- left_join(iris, df)




