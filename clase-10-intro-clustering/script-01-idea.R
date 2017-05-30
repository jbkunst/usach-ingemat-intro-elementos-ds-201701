library(tidyverse)
library(gsheet)
library(ggdendro)


data <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1JNwZmAdsClL6hAlnqT6VTu-DiKRohArft3W7I5J1PPk/edit#gid=0")
data

data2 <- as.data.frame(select(data, -persona))
rownames(data2) <- data$persona 
data2

hc <- hclust(dist(data2))
hc

plot(hc, labels = data$persona)
ggdendrogram(hc, labels = TRUE, rotate = TRUE) 


for (i in 2:15) mss[i] <- sum(kmeans(mydata_scaled,centers=i)$withinss)


