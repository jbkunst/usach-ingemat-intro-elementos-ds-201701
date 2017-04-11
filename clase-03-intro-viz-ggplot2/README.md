Clase 03: `ggplot2`
================

Temario
-------

1.  Para que visualizar
2.  ggplot2

Ejemplos
--------

``` r
library(ggplot2)
data(mpg, package = "ggplot2")


mpg
```

    ## # A tibble: 234 × 11
    ##    manufacturer      model displ  year   cyl      trans   drv   cty   hwy
    ##           <chr>      <chr> <dbl> <int> <int>      <chr> <chr> <int> <int>
    ## 1          audi         a4   1.8  1999     4   auto(l5)     f    18    29
    ## 2          audi         a4   1.8  1999     4 manual(m5)     f    21    29
    ## 3          audi         a4   2.0  2008     4 manual(m6)     f    20    31
    ## 4          audi         a4   2.0  2008     4   auto(av)     f    21    30
    ## 5          audi         a4   2.8  1999     6   auto(l5)     f    16    26
    ## 6          audi         a4   2.8  1999     6 manual(m5)     f    18    26
    ## 7          audi         a4   3.1  2008     6   auto(av)     f    18    27
    ## 8          audi a4 quattro   1.8  1999     4 manual(m5)     4    18    26
    ## 9          audi a4 quattro   1.8  1999     4   auto(l5)     4    16    25
    ## 10         audi a4 quattro   2.0  2008     4 manual(m6)     4    20    28
    ## # ... with 224 more rows, and 2 more variables: fl <chr>, class <chr>

``` r
ggplot(mpg)
```

![](README_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
ggplot(mpg) + 
  geom_point(aes(hwy, displ))
```

![](README_files/figure-markdown_github/unnamed-chunk-1-2.png)
