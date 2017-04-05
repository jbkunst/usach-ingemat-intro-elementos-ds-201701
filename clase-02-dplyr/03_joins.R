#' ---
#' title: "Joins"
#' output: github_document
#' ---
library(dplyr)

x <- data_frame(key = c(1, 2, 3), val_x = c("x1", "x2", "x3"))
x

y <- data_frame(key = c(1, 2, 4), val_y = c("y1", "y2", "y3"))
y


left_join(x, y)
right_join(x, y)

x %>% 
  left_join(y)

semi_join(x, y)

anti_join(x, y)

#' Hasta ahora todo es bonito por que las llaves (_keys_)
#' en cada uno de los dfs son unicos.
#' 
z <- data_frame(key2 = c(1, 1, 2, 2, 2, 3, 4, 4),
                val_z = c(1, 2, 3, 3, 5, 6, 7, 8))
z

left_join(x, z)

xz <- left_join(x, z, by = c("key" = "key2"))
xz

xz %>% 
  group_by(key) %>% 
  summarise(max_z = max(val_z))

#' Que pasó con val_x?
z %>% 
  group_by(key2) %>% 
  summarise(max_z = max(val_z)) %>% 
  left_join(x, by = c("key2" = "key"))

z %>% 
  group_by(key2) %>% 
  summarise(max_z = max(val_z)) %>% 
  left_join(x, ., by = c("key" = "key2"))

#' Yay!
