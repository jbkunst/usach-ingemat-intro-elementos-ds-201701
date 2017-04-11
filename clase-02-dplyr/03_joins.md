Joins
================
joshua.kunst
Tue Apr 11 13:20:15 2017

Lectura obligada: <http://r4ds.had.co.nz/relational-data.html> ![inner-join](http://r4ds.had.co.nz/diagrams/join-inner.png)

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
right_join(x, y)
```

    ## # A tibble: 3 × 3
    ##     key val_x val_y
    ##   <dbl> <chr> <chr>
    ## 1     1    x1    y1
    ## 2     2    x2    y2
    ## 3     4  <NA>    y3

``` r
x %>% 
  left_join(y)
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

Hasta ahora todo es bonito por que las llaves (*keys*) en cada uno de los dfs son unicos.

``` r
z <- data_frame(key2 = c(1, 1, 2, 2, 2, 3, 4, 4),
                val_z = c(1, 2, 3, 3, 5, 6, 7, 8))
z
```

    ## # A tibble: 8 × 2
    ##    key2 val_z
    ##   <dbl> <dbl>
    ## 1     1     1
    ## 2     1     2
    ## 3     2     3
    ## 4     2     3
    ## 5     2     5
    ## 6     3     6
    ## 7     4     7
    ## 8     4     8

``` r
left_join(x, z)
```

    ## Error: No common variables. Please specify `by` param.

``` r
xz <- left_join(x, z, by = c("key" = "key2"))
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
  summarise(max_z = max(val_z))
```

    ## # A tibble: 3 × 2
    ##     key max_z
    ##   <dbl> <dbl>
    ## 1     1     2
    ## 2     2     5
    ## 3     3     6

Que pasó con val\_x?

``` r
z %>% 
  group_by(key2) %>% 
  summarise(max_z = max(val_z)) %>% 
  left_join(x, by = c("key2" = "key"))
```

    ## # A tibble: 4 × 3
    ##    key2 max_z val_x
    ##   <dbl> <dbl> <chr>
    ## 1     1     2    x1
    ## 2     2     5    x2
    ## 3     3     6    x3
    ## 4     4     8  <NA>

``` r
z %>% 
  group_by(key2) %>% 
  summarise(max_z = max(val_z)) %>% 
  left_join(x, ., by = c("key" = "key2"))
```

    ## # A tibble: 3 × 3
    ##     key val_x max_z
    ##   <dbl> <chr> <dbl>
    ## 1     1    x1     2
    ## 2     2    x2     5
    ## 3     3    x3     6

Yay!
