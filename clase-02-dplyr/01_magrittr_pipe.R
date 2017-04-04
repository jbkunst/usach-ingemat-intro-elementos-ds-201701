#' ---
#' title: "Magrittr Pipes"
#' output: github_document
#' ---
library(magrittr) # librería para usar "pipes"

#' Ejemplo:
#' 
x <- 34
tan(cos(sqrt(log(x))))

y <- x
y <- log(y)
y <- sqrt(y)
y <- cos(y)
y <- tan(y)
y

#' Eureka!  Doh!
# %>%  # shortcut CTRL + SHIFT + M 
x %>%
  log %>%
  sqrt %>% 
  cos %>% 
  tan

c(2, 34) %>% 
  log %>%
  sqrt %>% 
  cos %>% 
  tan

#' legible, orden
#' Yay!