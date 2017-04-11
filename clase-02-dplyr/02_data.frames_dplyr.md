Data frames dplyr
================
joshua.kunst
Tue Apr 11 13:14:56 2017

``` r
library(dplyr) # no tienen que cargar magrittr
```

dplyr ofrece funciones (en forma de verbos) para manipular data frames.

Cargamos nuestra super encuesta *flash* url: <https://docs.google.com/spreadsheets/d/1JNwZmAdsClL6hAlnqT6VTu-DiKRohArft3W7I5J1PPk/edit#gid=0>

``` r
# install.packages('gsheet')
library(gsheet)
data <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1JNwZmAdsClL6hAlnqT6VTu-DiKRohArft3W7I5J1PPk/edit#gid=0')
data
```

    ## # A tibble: 14 × 8
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
    ## 13      hg       8           75             4                    4
    ## 14      yr       7           75             6                    6
    ## # ... with 3 more variables: ramo_semestre_anterior <int>,
    ## #   ramos_que_pase_sem_anterior <int>, profesor_buena_onda <chr>

Explorar que es lo que hay
--------------------------

Funcion: glimpse, srt, dim

``` r
data
```

    ## # A tibble: 14 × 8
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
    ## 13      hg       8           75             4                    4
    ## 14      yr       7           75             6                    6
    ## # ... with 3 more variables: ramo_semestre_anterior <int>,
    ## #   ramos_que_pase_sem_anterior <int>, profesor_buena_onda <chr>

``` r
glimpse(data)
```

    ## Observations: 14
    ## Variables: 8
    ## $ persona                     <chr> "jk", "gt", "jp", "lm", "jz", "eh"...
    ## $ ano_vas                     <int> 8, 4, 7, 7, 7, 5, 7, 3, 5, 5, 6, 5...
    ## $ satisfaccion                <int> 90, 100, 80, 80, 90, 85, 85, 85, 8...
    ## $ ramo_semestre               <int> 10, 7, 5, 7, 5, 7, 8, 7, 6, 7, 7, ...
    ## $ ramos_que_creo_pasar        <int> 5, 7, 5, 7, 5, 7, 8, 5, 6, 7, 7, 6...
    ## $ ramo_semestre_anterior      <int> 10, 4, 4, 6, 3, 6, 6, 4, 6, 5, 6, ...
    ## $ ramos_que_pase_sem_anterior <int> 5, 4, 3, 4, 3, 6, 4, 1, 4, 5, 5, 4...
    ## $ profesor_buena_onda         <chr> "yo", "sz", "ig", "ig", "gp", "vg"...

``` r
dim(data)
```

    ## [1] 14  8

Verbos dplyr
------------

-   Seleccionar: Seleccionar columnas
-   Filtrar: escoger filas
-   Mutar: Crear columnas
-   Arrange: Ordena filas

``` r
data2 <- select(data, satisfaccion, ramo_semestre_anterior)
filter(data2, ano_vas >= 5)
```

    ## Error in filter_impl(.data, dots): objeto 'ano_vas' no encontrado

Douhg!

``` r
data2 <- select(data, ano_vas, satisfaccion, ramo_semestre_anterior)
data2 <- filter(data2, ano_vas >= 5)
data2 <- mutate(data2, ratio_sat_ram = satisfaccion/ramo_semestre_anterior)
data2 <- select(data2, -satisfaccion)
data2 <- arrange(data2, ratio_sat_ram)
data2
```

    ## # A tibble: 12 × 3
    ##    ano_vas ramo_semestre_anterior ratio_sat_ram
    ##      <int>                  <int>         <dbl>
    ## 1        8                     10       9.00000
    ## 2        7                      6      13.33333
    ## 3        5                      6      14.16667
    ## 4        7                      6      14.16667
    ## 5        5                      6      14.16667
    ## 6        7                      5      15.00000
    ## 7        6                      6      15.83333
    ## 8        5                      5      17.00000
    ## 9        8                      4      18.75000
    ## 10       7                      4      20.00000
    ## 11       5                      4      20.00000
    ## 12       7                      3      30.00000

``` r
filter(data, TRUE)
```

    ## # A tibble: 14 × 8
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
    ## 13      hg       8           75             4                    4
    ## 14      yr       7           75             6                    6
    ## # ... with 3 more variables: ramo_semestre_anterior <int>,
    ## #   ramos_que_pase_sem_anterior <int>, profesor_buena_onda <chr>

``` r
filter(data, FALSE) # SIEMPRE!! un data frame :D
```

    ## # A tibble: 0 × 8
    ## # ... with 8 variables: persona <chr>, ano_vas <int>, satisfaccion <int>,
    ## #   ramo_semestre <int>, ramos_que_creo_pasar <int>,
    ## #   ramo_semestre_anterior <int>, ramos_que_pase_sem_anterior <int>,
    ## #   profesor_buena_onda <chr>

``` r
filter(data, satisfaccion == max(satisfaccion)
       | ano_vas == min(ano_vas))
```

    ## # A tibble: 2 × 8
    ##   persona ano_vas satisfaccion ramo_semestre ramos_que_creo_pasar
    ##     <chr>   <int>        <int>         <int>                <int>
    ## 1      gt       4          100             7                    7
    ## 2      cm       3           85             7                    5
    ## # ... with 3 more variables: ramo_semestre_anterior <int>,
    ## #   ramos_que_pase_sem_anterior <int>, profesor_buena_onda <chr>

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

**Imporante** Cada funcion toma un data.frame y retorna un data.frame.

-   Agrupar: Agrupar y resumir

``` r
datag <- group_by(data, ano_vas)

x <- seq(1, 10)
x
```

    ##  [1]  1  2  3  4  5  6  7  8  9 10

``` r
cumsum(x)
```

    ##  [1]  1  3  6 10 15 21 28 36 45 55

``` r
datag <- mutate(datag, satisfaccioncum = cumsum(satisfaccion))
datag <- select(datag, ano_vas, satisfaccion, satisfaccioncum)
datag
```

    ## Source: local data frame [14 x 3]
    ## Groups: ano_vas [6]
    ## 
    ##    ano_vas satisfaccion satisfaccioncum
    ##      <int>        <int>           <int>
    ## 1        8           90              90
    ## 2        4          100             100
    ## 3        7           80              80
    ## 4        7           80             160
    ## 5        7           90             250
    ## 6        5           85              85
    ## 7        7           85             335
    ## 8        3           85              85
    ## 9        5           85             170
    ## 10       5           85             255
    ## 11       6           95              95
    ## 12       5           80             335
    ## 13       8           75             165
    ## 14       7           75             410

``` r
datag <- arrange(datag, ano_vas)
datag
```

    ## Source: local data frame [14 x 3]
    ## Groups: ano_vas [6]
    ## 
    ##    ano_vas satisfaccion satisfaccioncum
    ##      <int>        <int>           <int>
    ## 1        3           85              85
    ## 2        4          100             100
    ## 3        5           85              85
    ## 4        5           85             170
    ## 5        5           85             255
    ## 6        5           80             335
    ## 7        6           95              95
    ## 8        7           80              80
    ## 9        7           80             160
    ## 10       7           90             250
    ## 11       7           85             335
    ## 12       7           75             410
    ## 13       8           90              90
    ## 14       8           75             165

``` r
datag <- ungroup(datag)
datag
```

    ## # A tibble: 14 × 3
    ##    ano_vas satisfaccion satisfaccioncum
    ##      <int>        <int>           <int>
    ## 1        3           85              85
    ## 2        4          100             100
    ## 3        5           85              85
    ## 4        5           85             170
    ## 5        5           85             255
    ## 6        5           80             335
    ## 7        6           95              95
    ## 8        7           80              80
    ## 9        7           80             160
    ## 10       7           90             250
    ## 11       7           85             335
    ## 12       7           75             410
    ## 13       8           90              90
    ## 14       8           75             165

``` r
datag <- mutate(datag, satisfaccioncum2 = cumsum(satisfaccion))
datag
```

    ## # A tibble: 14 × 4
    ##    ano_vas satisfaccion satisfaccioncum satisfaccioncum2
    ##      <int>        <int>           <int>            <int>
    ## 1        3           85              85               85
    ## 2        4          100             100              185
    ## 3        5           85              85              270
    ## 4        5           85             170              355
    ## 5        5           85             255              440
    ## 6        5           80             335              520
    ## 7        6           95              95              615
    ## 8        7           80              80              695
    ## 9        7           80             160              775
    ## 10       7           90             250              865
    ## 11       7           85             335              950
    ## 12       7           75             410             1025
    ## 13       8           90              90             1115
    ## 14       8           75             165             1190

Sumarizar: Resumir

``` r
summarise(data, mav = max(ano_vas), ms = min(satisfaccion))
```

    ## # A tibble: 1 × 2
    ##     mav    ms
    ##   <int> <int>
    ## 1     8    75

``` r
data %>% summarise(mav = max(ano_vas), ms = min(satisfaccion))
```

    ## # A tibble: 1 × 2
    ##     mav    ms
    ##   <int> <int>
    ## 1     8    75

``` r
group_by(data, ramo_semestre) %>% 
  summarise(mav = max(ano_vas), ms = min(satisfaccion))
```

    ## # A tibble: 6 × 3
    ##   ramo_semestre   mav    ms
    ##           <int> <int> <int>
    ## 1             4     8    75
    ## 2             5     7    80
    ## 3             6     7    75
    ## 4             7     7    80
    ## 5             8     7    85
    ## 6            10     8    90

``` r
group_by(data, ramo_semestre) %>% 
  summarise(mav = max(ano_vas), ms = min(satisfaccion)) %>% 
  summarise(mav = max(mav), ms = min(ms)) 
```

    ## # A tibble: 1 × 2
    ##     mav    ms
    ##   <int> <int>
    ## 1     8    75
