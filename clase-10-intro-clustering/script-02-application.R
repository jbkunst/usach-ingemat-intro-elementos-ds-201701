library(jpeg)
library(threejs)


image <- readJPEG(input$image)
image

df_image <- map(1:3, function(i) image[,,i]) %>% 
  map(matrix_to_df) %>% 
  reduce(left_join, by = c("x", "y")) %>% 
  rename(r = c.x, g = c.y, b = c) %>% 
  mutate(rgb = rgb(r, g, b)) 



kMeans <- df_image %>% 
  select(r, g, b) %>% 
  kmeans(centers = input$k1)

df_image <- df_image %>%
  mutate(rbgApp = rgb(kMeans$centers[kMeans$cluster, c("r", "g", "b")])) %>% 
  tbl_df()

plot(df_image$x, df_image$y, col = df_image$rbgApp, asp = 1, pch = ".",
     ylab = "", xlab = "", xaxt="n", yaxt="n", axes=FALSE)



scatterplot3js(df_image_aux[,c("red", "green", "blue")], size = log(df_image_aux$proportion*1000),
               #                  color=df_image_aux$rbgApp, renderer="canvas")