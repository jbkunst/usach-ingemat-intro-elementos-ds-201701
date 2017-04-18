library(stringr)
library(purrr)

als <- readLines("alumnos.txt")

als <- als %>% 
  str_split(" ") %>% 
  map(str_sub, 0, 1) %>% 
  map_chr(str_c, collapse = "") %>% 
  sort()

als

chunk2 <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE)) 

# setseed
set.seed(123)

grps <- chunk2(sample(als), 4) %>% 
  map_chr(str_c, collapse = " ")
grps

writeLines(grps, "grupos.txt")