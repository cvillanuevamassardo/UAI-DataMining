Librerias R
================

## Principales Librerias para R

## Tidyverse

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.0     v purrr   0.3.3
    ## v tibble  3.0.0     v dplyr   1.0.2
    ## v tidyr   1.0.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ---------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
# https://www.tidyverse.org/
# Coleccion de paquetes de R diseñados para data science
# dplyr
# ggplot2
# tidyr
# readr
# stringr
# forcats
# tibble
# purrr
```

## Dplyr

    Principales Funciones 

``` r
library(dplyr)

#help("dplyr")

#select()
#filter()
#arrange()
#mutate()
#summarise()
```

## Ggplot2

``` r
library(ggplot2)

#help("ggplot2")

# https://rdocumentation.org/packages/ggplot2/versions/3.1.0
```

## Tidyr

``` r
library(tidyr)

#help("tidyr")

#replace_na()
#drop_na()
```

## Forcats

``` r
library(forcats)
library(dplyr)
library(ggplot2)

#help("forcats")

starwars %>% 
  filter(!is.na(species)) %>%
  count(species, sort = TRUE)
```

    ## # A tibble: 37 x 2
    ##    species      n
    ##    <chr>    <int>
    ##  1 Human       35
    ##  2 Droid        6
    ##  3 Gungan       3
    ##  4 Kaminoan     2
    ##  5 Mirialan     2
    ##  6 Twi'lek      2
    ##  7 Wookiee      2
    ##  8 Zabrak       2
    ##  9 Aleena       1
    ## 10 Besalisk     1
    ## # ... with 27 more rows

``` r
ggplot(starwars, aes(x = eye_color)) + 
  geom_bar() + 
  coord_flip()
```

![](LibreriasR_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
starwars %>%
  mutate(eye_color = fct_infreq(eye_color)) %>%
  ggplot(aes(x = eye_color)) + 
  geom_bar() + 
  coord_flip()
```

![](LibreriasR_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->
