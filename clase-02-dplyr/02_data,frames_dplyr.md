Data frames dplyr
================
Joshua
Tue Apr 04 21:04:28 2017

``` r
library(dplyr) # no tienen que cargar magrittr
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

dplyr ofrece funciones (en forma de verbos) para manipular data frames. Cargamos nuestra super encuesta *flash* url: <https://docs.google.com/spreadsheets/d/1JNwZmAdsClL6hAlnqT6VTu-DiKRohArft3W7I5J1PPk/edit#gid=0>

``` r
# install.packages('gsheet')
library(gsheet)
data <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1JNwZmAdsClL6hAlnqT6VTu-DiKRohArft3W7I5J1PPk/edit#gid=0')
data
```

    ## # A tibble: 12 × 8
    ##    persona ano_vas satisfaccion ramo_semestre ramos_que_creo_pasar
    ##      <chr>   <int>        <int>         <int>                <int>
    ## 1       jk       8           90            10                    5
    ## 2       gt       4          100             7                    7
    ## 3       jp       7           80             5                    5
    ## 4       lm       7           80             7                    7
    ## 5       jz       7           90             5                    5
    ## 6       eh       5           85             7                    7
    ## 7       nd       7           85             8                    8
    ## 8       cm       3           85             7                    5
    ## 9       jc       5           85             6                    6
    ## 10      pm       5           85             7                    7
    ## 11      fc       6           95             7                    7
    ## 12      yd       5           80             6                    6
    ## # ... with 3 more variables: ramo_semestre_anterior <int>,
    ## #   ramos_que_pase_sem_anterior <int>, profesor_buena_onda <chr>

Explorar que es lo que hay
--------------------------

Funcion: glimpse, srt, dim

``` r
data
```

    ## # A tibble: 12 × 8
    ##    persona ano_vas satisfaccion ramo_semestre ramos_que_creo_pasar
    ##      <chr>   <int>        <int>         <int>                <int>
    ## 1       jk       8           90            10                    5
    ## 2       gt       4          100             7                    7
    ## 3       jp       7           80             5                    5
    ## 4       lm       7           80             7                    7
    ## 5       jz       7           90             5                    5
    ## 6       eh       5           85             7                    7
    ## 7       nd       7           85             8                    8
    ## 8       cm       3           85             7                    5
    ## 9       jc       5           85             6                    6
    ## 10      pm       5           85             7                    7
    ## 11      fc       6           95             7                    7
    ## 12      yd       5           80             6                    6
    ## # ... with 3 more variables: ramo_semestre_anterior <int>,
    ## #   ramos_que_pase_sem_anterior <int>, profesor_buena_onda <chr>

``` r
glimpse(data)
```

    ## Observations: 12
    ## Variables: 8
    ## $ persona                     <chr> "jk", "gt", "jp", "lm", "jz", "eh"...
    ## $ ano_vas                     <int> 8, 4, 7, 7, 7, 5, 7, 3, 5, 5, 6, 5
    ## $ satisfaccion                <int> 90, 100, 80, 80, 90, 85, 85, 85, 8...
    ## $ ramo_semestre               <int> 10, 7, 5, 7, 5, 7, 8, 7, 6, 7, 7, 6
    ## $ ramos_que_creo_pasar        <int> 5, 7, 5, 7, 5, 7, 8, 5, 6, 7, 7, 6
    ## $ ramo_semestre_anterior      <int> 10, 4, 4, 6, 3, 6, 6, 4, 6, 5, 6, 4
    ## $ ramos_que_pase_sem_anterior <int> 5, 4, 3, 4, 3, 6, 4, 1, 4, 5, 5, 4
    ## $ profesor_buena_onda         <chr> "yo", "sz", "ig", "ig", "gp", "vg"...

``` r
dim(data)
```

    ## [1] 12  8

Verbos dplyr
------------

Seleccionar: Seleccionar columnas Filtrar: escoger filas Mutar: Crear columnas Arrange: Ordena filas

``` r
data2 <- select(data, satisfaccion, ramo_semestre_anterior)
filter(data2, ano_vas >= 5)
```

    ## Error in eval(substitute(expr), envir, enclos): objeto 'ano_vas' no encontrado

``` r
# Douhg!
data2 <- select(data, ano_vas, satisfaccion, ramo_semestre_anterior)
data2 <- filter(data2, ano_vas >= 5)
data2 <- mutate(data2, ratio_sat_ram = satisfaccion/ramo_semestre_anterior)
data2 <- select(data2, -satisfaccion)
data2 <- arrange(data2, ratio_sat_ram)
data2
```

    ## # A tibble: 10 × 3
    ##    ano_vas ramo_semestre_anterior ratio_sat_ram
    ##      <int>                  <int>         <dbl>
    ## 1        8                     10       9.00000
    ## 2        7                      6      13.33333
    ## 3        5                      6      14.16667
    ## 4        7                      6      14.16667
    ## 5        5                      6      14.16667
    ## 6        6                      6      15.83333
    ## 7        5                      5      17.00000
    ## 8        7                      4      20.00000
    ## 9        5                      4      20.00000
    ## 10       7                      3      30.00000

Donde esta pipe?!

``` r
data3 <- data %>% 
  select(ano_vas, satisfaccion, ramo_semestre_anterior) %>% 
  filter(ano_vas >= 5) %>% 
  mutate(ratio_sat_ram = satisfaccion/ramo_semestre_anterior) %>% 
  select(-satisfaccion) %>% 
  arrange(ratio_sat_ram)

identical(data2, data3)
```

    ## [1] TRUE

**Imporante** Cada funcion toma un data.frame y retorna un data.frame\*\*
