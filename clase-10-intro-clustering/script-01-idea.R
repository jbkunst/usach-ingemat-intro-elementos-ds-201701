library(tidyverse)
library(gsheet)
library(ggdendro)



data <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1JNwZmAdsClL6hAlnqT6VTu-DiKRohArft3W7I5J1PPk/edit#gid=0")
data

data2 <- as.data.frame(select(data, -persona,-profesor_buena_onda))
rownames(data2) <- data$persona 
data2


mdist <- dist(data2)

hc <- hclust(mdist)
hc

plot(hc, labels = data$persona)
ggdendrogram(hc, labels = TRUE, rotate = TRUE) 


data3 <- mutate_all(data2, scale)
rownames(data3) <- data$persona 

mdist <- dist(data3)

hc <- hclust(mdist)
hc

plot(hc, labels = data$persona)
ggdendrogram(hc, labels = TRUE, rotate = TRUE) 



# kmeans ------------------------------------------------------------------

data <- select(iris, contains("Sepal"))
data

ggplot(iris) +
  geom_point(aes(Sepal.Width, Sepal.Length))

# 2 grupos? Lvelpc
km <- kmeans(data, centers = 2)
km
km$centers
km$cluster
iris <- mutate(iris, cluster = as.character(km$cluster))



data <- iris %>% 
  select(contains("Sepal")) %>% 
  mutate_all(scale)
data

km <- kmeans(data, centers = 2)
km
km$centers
km$cluster
iris <- mutate(iris, cluster2 = as.character(km$cluster))


ggplot(iris) +
  geom_point(aes(Sepal.Width, Sepal.Length, color = cluster2)) +
  coord_equal()

iris <- mutate(iris, cluster3 = paste(cluster, cluster2))
head(iris)

ggplot(iris) +
  geom_point(aes(Sepal.Width, Sepal.Length, color = cluster3))


# mas data ----------------------------------------------------------------
data <- select(iris, matches("Wid|Len")) %>% 
  mutate_all(scale)
head(data)
k <- 1:15

df <- data_frame()

for(i in k) {
  message(i)  
  km <- kmeans(data, i)
  daux <- data_frame(k = i, sw = sum(km$withinss))
  df<- bind_rows(df, daux)
  }

beepr::beep(sound = 8)

df

ggplot(df) +
  geom_line(aes(k, sw)) 

km <- kmeans(data, 4)
iris <- mutate(iris, clusterg = as.character(km$cluster))
head(iris)


ggplot(iris) +
  geom_point(aes(Sepal.Width, Sepal.Length, color = clusterg))

ggplot(iris) +
  geom_point(aes(Petal.Width, Petal.Length, color = clusterg))


count(iris, Species, sort = TRUE)
count(iris, clusterg, sort = TRUE)


count(iris, Species, clusterg, sort = TRUE)

    ggplot(iris) +
  geom_point(aes(Petal.Width, Petal.Length, color = Species))

