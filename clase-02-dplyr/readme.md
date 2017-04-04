Clase 02: Data frames y `dplyr`
================

Temario
-------

1.  Magrittr y pipe `%>%`
2.  Definiendo data frames
3.  Manipulando data frames. Principales verbos: select, filter, mutate, summarize, group\_by.
4.  Joins

Ejemplos
--------

``` r
library(dplyr)

x <- data_frame(key = c(1, 2, 3), val_x = c("x1", "x2", "x3"))
x
```

    ## # A tibble: 3 × 2
    ##     key val_x
    ##   <dbl> <chr>
    ## 1     1    x1
    ## 2     2    x2
    ## 3     3    x3

``` r
y <- data_frame(key = c(1, 2, 4), val_y = c("y1", "y2", "y3"))
y
```

    ## # A tibble: 3 × 2
    ##     key val_y
    ##   <dbl> <chr>
    ## 1     1    y1
    ## 2     2    y2
    ## 3     4    y3

``` r
left_join(x, y)
```

    ## # A tibble: 3 × 3
    ##     key val_x val_y
    ##   <dbl> <chr> <chr>
    ## 1     1    x1    y1
    ## 2     2    x2    y2
    ## 3     3    x3  <NA>

``` r
semi_join(x, y)
```

    ## # A tibble: 2 × 2
    ##     key val_x
    ##   <dbl> <chr>
    ## 1     1    x1
    ## 2     2    x2

``` r
anti_join(x, y)
```

    ## # A tibble: 1 × 2
    ##     key val_x
    ##   <dbl> <chr>
    ## 1     3    x3

``` r
z <- data_frame(key = c(1, 1, 2, 2, 2, 3), val_z = c(1, 2, 3, 3, 5, 6))
z
```

    ## # A tibble: 6 × 2
    ##     key val_z
    ##   <dbl> <dbl>
    ## 1     1     1
    ## 2     1     2
    ## 3     2     3
    ## 4     2     3
    ## 5     2     5
    ## 6     3     6

``` r
xz <- left_join(x, z) 
xz
```

    ## # A tibble: 6 × 3
    ##     key val_x val_z
    ##   <dbl> <chr> <dbl>
    ## 1     1    x1     1
    ## 2     1    x1     2
    ## 3     2    x2     3
    ## 4     2    x2     3
    ## 5     2    x2     5
    ## 6     3    x3     6

``` r
xz %>% 
  group_by(key) %>% 
  summarise(conteo = n(),
            dinstintos = n_distinct(val_z),
            max_z = max(val_z))
```

    ## # A tibble: 3 × 4
    ##     key conteo dinstintos max_z
    ##   <dbl>  <int>      <int> <dbl>
    ## 1     1      2          2     2
    ## 2     2      3          2     5
    ## 3     3      1          1     6

``` r
xz %>% 
  select(key, val_z) %>% 
  mutate(cumsum_z = cumsum(val_z))
```

    ## # A tibble: 6 × 3
    ##     key val_z cumsum_z
    ##   <dbl> <dbl>    <dbl>
    ## 1     1     1        1
    ## 2     1     2        3
    ## 3     2     3        6
    ## 4     2     3        9
    ## 5     2     5       14
    ## 6     3     6       20

``` r
xz %>% 
  select(key, val_z) %>% 
  group_by(key) %>% 
  mutate(cumsum_z = cumsum(val_z))
```

    ## Source: local data frame [6 x 3]
    ## Groups: key [3]
    ## 
    ##     key val_z cumsum_z
    ##   <dbl> <dbl>    <dbl>
    ## 1     1     1        1
    ## 2     1     2        3
    ## 3     2     3        3
    ## 4     2     3        6
    ## 5     2     5       11
    ## 6     3     6        6
