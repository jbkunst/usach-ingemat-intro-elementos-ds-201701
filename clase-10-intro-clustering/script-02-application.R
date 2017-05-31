# install.packages(c("jpeg", "threejs"))
library(jpeg)
library(threejs)
library(tidyverse)



image <- readJPEG("82127.jpg")
image

dim(image)
dim(image[,,2])



matrix_to_df <- function(m) {
  # m <- matrix(round(runif(12), 2), nrow = 4)
  m %>% 
    tbl_df() %>% 
    mutate(y = seq_len(nrow(.))) %>% 
    gather(x, c, -y) %>% 
    mutate(
      x = as.numeric(gsub("V", "", x)),
      y = rev(y)
    ) %>% 
    select(x, y, c)
  
}


df_image <- map(1:3, function(i) image[,,i]) %>% 
  map(matrix_to_df) %>% 
  reduce(left_join, by = c("x", "y")) %>% 
  rename(r = c.x, g = c.y, b = c) %>% 
  mutate(rgb = rgb(r, g, b)) 
df_image


plot(df_image$x, df_image$y, col = df_image$rgb)

df_image <- mutate(df_image, rgb2 = rgb(r, g, b, 0.8))
df_image

df_image_sample <- sample_n(df_image, 10000)
plot(df_image_sample$x, df_image_sample$y, col = df_image_sample$rgb)

df_image_sample <- sample_n(df_image, 1000)
plot(df_image_sample$x, df_image_sample$y, col = df_image_sample$rgb)
df_image_sample

scatterplot3js(df_image_sample$r, df_image_sample$g, df_image_sample$b,
               color = df_image_sample$rgb)

km <- kmeans(df_image %>% select(r, g, b, x, y), 200)
km$centers

df_centers <- km$centers %>% 
  as_data_frame() %>% 
  mutate(cluster = seq(1, nrow(km$centers))) %>% 
  mutate(rgbc = rgb(r*255, g*255, b*255, maxColorValue = 255)) %>% 
  select(cluster, rgbc)

df_image <- df_image %>% 
  mutate(cluster = km$cluster) %>% 
  left_join(df_centers, by = "cluster")

df_image_sample <- sample_n(df_image, 1000)

scatterplot3js(df_image_sample$r, df_image_sample$g, df_image_sample$b,
               color = df_image_sample$rgbc)

plot(df_image$x, df_image$y, col = df_image$rgb, axes = FALSE)
plot(df_image$x, df_image$y, col = df_image$rgbc, axes = FALSE)




