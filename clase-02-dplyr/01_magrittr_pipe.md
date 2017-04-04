Magrittr Pipes
================
Joshua
Tue Apr 04 20:24:17 2017

``` r
library(magrittr) # librería para usar "pipes"
```

Ejemplo:

``` r
x <- 34
tan(cos(sqrt(log(x))))
```

    ## [1] -0.311816

``` r
y <- x
y <- log(y)
y <- sqrt(y)
y <- cos(y)
y <- tan(y)
y
```

    ## [1] -0.311816

Eureka! Doh!

``` r
# %>%  # shortcut CTRL + SHIFT + M 
x %>%
  log %>%
  sqrt %>% 
  cos %>% 
  tan
```

    ## [1] -0.311816

``` r
c(2, 34) %>% 
  log %>%
  sqrt %>% 
  cos %>% 
  tan
```

    ## [1]  0.7971299 -0.3118160

legible, orden Yay!
