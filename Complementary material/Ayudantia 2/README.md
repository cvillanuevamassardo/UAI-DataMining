Ayudantia 2
================

## Actividad Ayudantia 2

Replicar lo realizado en la ayudantia (analisis exploratorio de los
datos, limpieza, sub dataframes, filtro y graficar ), pueden elegir otro
equipo para que puedan explorar otros datos

## Analisis Exploratorio de Datos

``` r
library(quanteda)
```

    ## Package version: 2.1.2

    ## Parallel computing: 2 of 4 threads used.

    ## See https://quanteda.io for tutorials and examples.

    ## 
    ## Attaching package: 'quanteda'

    ## The following object is masked from 'package:utils':
    ## 
    ##     View

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.0     v purrr   0.3.3
    ## v tibble  3.0.0     v stringr 1.4.0
    ## v tidyr   1.0.2     v forcats 0.5.0
    ## v readr   1.3.1

    ## -- Conflicts ---------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(utf8)
library(ggplot2)

setwd("D:/Users/Italo/Documents/Italo Felipe/UAI/Semestre 11/Ayudantia Mineria de Datos/Ayudantia_DataMining01_2021/Ayudantia 2")
primer_tiempo2020 <- read_csv("Primer_Tiempo2020.csv", col_names = TRUE)
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   torneo = col_character(),
    ##   equipo = col_character(),
    ##   id_partido = col_character(),
    ##   partido = col_character(),
    ##   fasepartido = col_character(),
    ##   local = col_logical(),
    ##   tiempo = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
#str(primer_tiempo2020)
#attach(primer_tiempo2020)
summary(primer_tiempo2020)
```

    ##     torneo             equipo           id_partido          partido         
    ##  Length:130         Length:130         Length:130         Length:130        
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##  fasepartido          local            tiempo           accuratePass  
    ##  Length:130         Mode :logical   Length:130         Min.   : 62.0  
    ##  Class :character   FALSE:65        Class :character   1st Qu.:115.2  
    ##  Mode  :character   TRUE :65        Mode  :character   Median :143.5  
    ##                                                        Mean   :147.5  
    ##                                                        3rd Qu.:181.2  
    ##                                                        Max.   :269.0  
    ##    wonTackle       lostCorners    goalsConceded        saves    
    ##  Min.   : 0.000   Min.   :0.000   Min.   :0.0000   Min.   :0.0  
    ##  1st Qu.: 3.000   1st Qu.:1.000   1st Qu.:0.0000   1st Qu.:1.0  
    ##  Median : 5.000   Median :2.000   Median :0.0000   Median :1.0  
    ##  Mean   : 5.154   Mean   :2.277   Mean   :0.5923   Mean   :1.5  
    ##  3rd Qu.: 7.000   3rd Qu.:3.000   3rd Qu.:1.0000   3rd Qu.:2.0  
    ##  Max.   :14.000   Max.   :7.000   Max.   :3.0000   Max.   :5.0  
    ##  ontargetScoringAtt totalScoringAtt     subsMade       totalThrows   
    ##  Min.   :0.000      Min.   : 0.000   Min.   :0.0000   Min.   : 3.00  
    ##  1st Qu.:1.000      1st Qu.: 4.000   1st Qu.:0.0000   1st Qu.: 8.00  
    ##  Median :2.000      Median : 6.000   Median :0.0000   Median :11.00  
    ##  Mean   :2.108      Mean   : 5.938   Mean   :0.1077   Mean   :10.98  
    ##  3rd Qu.:3.000      3rd Qu.: 7.750   3rd Qu.:0.0000   3rd Qu.:13.00  
    ##  Max.   :5.000      Max.   :14.000   Max.   :1.0000   Max.   :26.00  
    ##  totalYellowCard    goalKicks        totalPass       fkFoulWon     
    ##  Min.   :0.0000   Min.   : 0.000   Min.   : 93.0   Min.   : 2.000  
    ##  1st Qu.:0.0000   1st Qu.: 2.000   1st Qu.:159.5   1st Qu.: 5.000  
    ##  Median :1.0000   Median : 4.000   Median :189.0   Median : 6.000  
    ##  Mean   :0.9077   Mean   : 3.962   Mean   :190.9   Mean   : 6.338  
    ##  3rd Qu.:1.0000   3rd Qu.: 5.000   3rd Qu.:222.5   3rd Qu.: 8.000  
    ##  Max.   :3.0000   Max.   :11.000   Max.   :304.0   Max.   :12.000  
    ##   totalTackle       fkFoulLost     possessionPercentage totalClearance  
    ##  Min.   : 1.000   Min.   : 2.000   Min.   :23.60        Min.   : 0.000  
    ##  1st Qu.: 5.000   1st Qu.: 6.000   1st Qu.:45.62        1st Qu.: 4.000  
    ##  Median : 7.000   Median : 7.000   Median :50.00        Median : 7.000  
    ##  Mean   : 7.192   Mean   : 7.054   Mean   :50.00        Mean   : 7.385  
    ##  3rd Qu.: 9.000   3rd Qu.: 9.000   3rd Qu.:54.38        3rd Qu.:10.000  
    ##  Max.   :15.000   Max.   :13.000   Max.   :76.40        Max.   :16.000  
    ##  formationUsed blockedScoringAtt   goalAssist         goals       
    ##  Min.   :0     Min.   :0.000     Min.   :0.0000   Min.   :0.0000  
    ##  1st Qu.:0     1st Qu.:0.000     1st Qu.:0.0000   1st Qu.:0.0000  
    ##  Median :0     Median :1.000     Median :0.0000   Median :0.0000  
    ##  Mean   :0     Mean   :1.262     Mean   :0.3769   Mean   :0.5923  
    ##  3rd Qu.:0     3rd Qu.:2.000     3rd Qu.:1.0000   3rd Qu.:1.0000  
    ##  Max.   :0     Max.   :6.000     Max.   :2.0000   Max.   :3.0000  
    ##   totalOffside   shotOffTarget     wonCorners     cornerTaken   
    ##  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :0.000  
    ##  1st Qu.:0.000   1st Qu.:1.000   1st Qu.:1.000   1st Qu.:1.000  
    ##  Median :1.000   Median :2.000   Median :2.000   Median :2.000  
    ##  Mean   :1.038   Mean   :2.569   Mean   :2.277   Mean   :2.269  
    ##  3rd Qu.:2.000   3rd Qu.:4.000   3rd Qu.:3.000   3rd Qu.:3.000  
    ##  Max.   :5.000   Max.   :7.000   Max.   :7.000   Max.   :7.000  
    ##  penaltyConceded   penaltyFaced    penGoalsConceded   penaltyWon    
    ##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
    ##  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
    ##  Median :0.0000   Median :0.0000   Median :0.0000   Median :0.0000  
    ##  Mean   :0.1692   Mean   :0.1692   Mean   :0.1308   Mean   :0.1692  
    ##  3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0.0000  
    ##  Max.   :2.0000   Max.   :2.0000   Max.   :1.0000   Max.   :2.0000  
    ##     ownGoals        penaltySave       secondYellow      totalRedCard    
    ##  Min.   :0.00000   Min.   :0.00000   Min.   :0.00000   Min.   :0.00000  
    ##  1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.00000  
    ##  Median :0.00000   Median :0.00000   Median :0.00000   Median :0.00000  
    ##  Mean   :0.02308   Mean   :0.02308   Mean   :0.01538   Mean   :0.04615  
    ##  3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:0.00000  
    ##  Max.   :1.00000   Max.   :1.00000   Max.   :1.00000   Max.   :1.00000  
    ##  posesion_Rival  precision_pases precision_tiros  minutos_juego  
    ##  Min.   :23.60   Min.   :50.68   Min.   :  0.00   Min.   :10.62  
    ##  1st Qu.:45.62   1st Qu.:70.71   1st Qu.: 25.00   1st Qu.:20.53  
    ##  Median :50.00   Median :76.40   Median : 40.00   Median :22.50  
    ##  Mean   :50.00   Mean   :75.99   Mean   : 41.20   Mean   :22.50  
    ##  3rd Qu.:54.38   3rd Qu.:82.28   3rd Qu.: 57.14   3rd Qu.:24.47  
    ##  Max.   :76.40   Max.   :89.43   Max.   :100.00   Max.   :34.38  
    ##  minutos_juegorival golesSalvados   foulsInofensivos cortarJuegoContrario
    ##  Min.   :10.62      Min.   :0.000   Min.   : 1.000   Min.   : 4.00       
    ##  1st Qu.:20.53      1st Qu.:1.000   1st Qu.: 5.000   1st Qu.:10.00       
    ##  Median :22.50      Median :1.000   Median : 6.000   Median :12.00       
    ##  Mean   :22.50      Mean   :1.523   Mean   : 6.146   Mean   :12.21       
    ##  3rd Qu.:24.47      3rd Qu.:2.000   3rd Qu.: 8.000   3rd Qu.:15.00       
    ##  Max.   :34.38      Max.   :5.000   Max.   :11.000   Max.   :24.00       
    ##   juegoCortado  
    ##  Min.   : 8.00  
    ##  1st Qu.:17.00  
    ##  Median :20.00  
    ##  Mean   :20.64  
    ##  3rd Qu.:25.00  
    ##  Max.   :40.00

## Borrar Datos Char

``` r
primer_tiempo2020
```

    ## # A tibble: 130 x 49
    ##    torneo equipo id_partido partido fasepartido local tiempo accuratePass
    ##    <chr>  <chr>  <chr>      <chr>   <chr>       <lgl> <chr>         <dbl>
    ##  1 Prime~ Uni<f~ 6xszsf73j~ Univer~ Regular Se~ FALSE fh              235
    ##  2 Prime~ Unive~ 6xszsf73j~ Univer~ Regular Se~ TRUE  fh              199
    ##  3 Prime~ Evert~ e88gat05j~ Univer~ Regular Se~ FALSE fh              157
    ##  4 Prime~ Unive~ e88gat05j~ Univer~ Regular Se~ TRUE  fh              192
    ##  5 Prime~ Curic~ 35ijq76er~ Univer~ Regular Se~ FALSE fh              142
    ##  6 Prime~ Unive~ 35ijq76er~ Univer~ Regular Se~ TRUE  fh              168
    ##  7 Prime~ Coqui~ 9o9ji2f68~ Univer~ Regular Se~ FALSE fh              190
    ##  8 Prime~ Unive~ 9o9ji2f68~ Univer~ Regular Se~ TRUE  fh              200
    ##  9 Prime~ Santi~ 357wqv370~ Univer~ Regular Se~ FALSE fh              156
    ## 10 Prime~ Unive~ 357wqv370~ Univer~ Regular Se~ TRUE  fh               92
    ## # ... with 120 more rows, and 41 more variables: wonTackle <dbl>,
    ## #   lostCorners <dbl>, goalsConceded <dbl>, saves <dbl>,
    ## #   ontargetScoringAtt <dbl>, totalScoringAtt <dbl>, subsMade <dbl>,
    ## #   totalThrows <dbl>, totalYellowCard <dbl>, goalKicks <dbl>, totalPass <dbl>,
    ## #   fkFoulWon <dbl>, totalTackle <dbl>, fkFoulLost <dbl>,
    ## #   possessionPercentage <dbl>, totalClearance <dbl>, formationUsed <dbl>,
    ## #   blockedScoringAtt <dbl>, goalAssist <dbl>, goals <dbl>, totalOffside <dbl>,
    ## #   shotOffTarget <dbl>, wonCorners <dbl>, cornerTaken <dbl>,
    ## #   penaltyConceded <dbl>, penaltyFaced <dbl>, penGoalsConceded <dbl>,
    ## #   penaltyWon <dbl>, ownGoals <dbl>, penaltySave <dbl>, secondYellow <dbl>,
    ## #   totalRedCard <dbl>, posesion_Rival <dbl>, precision_pases <dbl>,
    ## #   precision_tiros <dbl>, minutos_juego <dbl>, minutos_juegorival <dbl>,
    ## #   golesSalvados <dbl>, foulsInofensivos <dbl>, cortarJuegoContrario <dbl>,
    ## #   juegoCortado <dbl>

``` r
primer_tiempo2020 <- primer_tiempo2020[,!(colnames(primer_tiempo2020) %in% c("id_partido", "fasepartido", "local", "tiempo","formationUsed", "torneo"))]
primer_tiempo2020
```

    ## # A tibble: 130 x 43
    ##    equipo partido accuratePass wonTackle lostCorners goalsConceded saves
    ##    <chr>  <chr>          <dbl>     <dbl>       <dbl>         <dbl> <dbl>
    ##  1 Uni<f~ Univer~          235         2           3             1     4
    ##  2 Unive~ Univer~          199         2           0             0     1
    ##  3 Evert~ Univer~          157         7           6             0     3
    ##  4 Unive~ Univer~          192         4           1             0     2
    ##  5 Curic~ Univer~          142         6           3             2     2
    ##  6 Unive~ Univer~          168         8           1             1     2
    ##  7 Coqui~ Univer~          190         3           0             0     2
    ##  8 Unive~ Univer~          200         5           0             0     0
    ##  9 Santi~ Univer~          156         4           3             0     0
    ## 10 Unive~ Univer~           92         4           1             1     1
    ## # ... with 120 more rows, and 36 more variables: ontargetScoringAtt <dbl>,
    ## #   totalScoringAtt <dbl>, subsMade <dbl>, totalThrows <dbl>,
    ## #   totalYellowCard <dbl>, goalKicks <dbl>, totalPass <dbl>, fkFoulWon <dbl>,
    ## #   totalTackle <dbl>, fkFoulLost <dbl>, possessionPercentage <dbl>,
    ## #   totalClearance <dbl>, blockedScoringAtt <dbl>, goalAssist <dbl>,
    ## #   goals <dbl>, totalOffside <dbl>, shotOffTarget <dbl>, wonCorners <dbl>,
    ## #   cornerTaken <dbl>, penaltyConceded <dbl>, penaltyFaced <dbl>,
    ## #   penGoalsConceded <dbl>, penaltyWon <dbl>, ownGoals <dbl>,
    ## #   penaltySave <dbl>, secondYellow <dbl>, totalRedCard <dbl>,
    ## #   posesion_Rival <dbl>, precision_pases <dbl>, precision_tiros <dbl>,
    ## #   minutos_juego <dbl>, minutos_juegorival <dbl>, golesSalvados <dbl>,
    ## #   foulsInofensivos <dbl>, cortarJuegoContrario <dbl>, juegoCortado <dbl>

## Analisis descriptivo

``` r
fh2020 <- primer_tiempo2020[order(primer_tiempo2020$accuratePass, decreasing = TRUE),]
fh2020
```

    ## # A tibble: 130 x 43
    ##    equipo partido accuratePass wonTackle lostCorners goalsConceded saves
    ##    <chr>  <chr>          <dbl>     <dbl>       <dbl>         <dbl> <dbl>
    ##  1 Unive~ Univer~          269         2           3             0     2
    ##  2 Depor~ Deport~          253        11           1             1     1
    ##  3 Uni<f~ Uni<f3~          252         6           3             1     3
    ##  4 Curic~ Univer~          245         5           2             0     2
    ##  5 Uni<f~ Uni<f3~          237         5           2             0     0
    ##  6 Uni<f~ Univer~          235         2           3             1     4
    ##  7 Colo ~ Colo C~          231         4           0             0     0
    ##  8 Uni<f~ Uni<f3~          224         7           0             1     0
    ##  9 Audax~ Audax ~          220         2           1             1     1
    ## 10 Audax~ Audax ~          219         6           1             0     0
    ## # ... with 120 more rows, and 36 more variables: ontargetScoringAtt <dbl>,
    ## #   totalScoringAtt <dbl>, subsMade <dbl>, totalThrows <dbl>,
    ## #   totalYellowCard <dbl>, goalKicks <dbl>, totalPass <dbl>, fkFoulWon <dbl>,
    ## #   totalTackle <dbl>, fkFoulLost <dbl>, possessionPercentage <dbl>,
    ## #   totalClearance <dbl>, blockedScoringAtt <dbl>, goalAssist <dbl>,
    ## #   goals <dbl>, totalOffside <dbl>, shotOffTarget <dbl>, wonCorners <dbl>,
    ## #   cornerTaken <dbl>, penaltyConceded <dbl>, penaltyFaced <dbl>,
    ## #   penGoalsConceded <dbl>, penaltyWon <dbl>, ownGoals <dbl>,
    ## #   penaltySave <dbl>, secondYellow <dbl>, totalRedCard <dbl>,
    ## #   posesion_Rival <dbl>, precision_pases <dbl>, precision_tiros <dbl>,
    ## #   minutos_juego <dbl>, minutos_juegorival <dbl>, golesSalvados <dbl>,
    ## #   foulsInofensivos <dbl>, cortarJuegoContrario <dbl>, juegoCortado <dbl>

## Sub DataFrames

``` r
fh2020_pases = fh2020[,colnames(primer_tiempo2020) %in% c("equipo", "partido", "accuratePass", "totalPass", "precision_pases")]
fh2020_pases = fh2020_pases[order(fh2020_pases$precision_pases, decreasing = TRUE),]

fh2020_pases
```

    ## # A tibble: 130 x 5
    ##    equipo        partido                  accuratePass totalPass precision_pases
    ##    <chr>         <chr>                           <dbl>     <dbl>           <dbl>
    ##  1 Audax Italia~ Audax Italiano vs Cobre~          220       246            89.4
    ##  2 Universidad ~ Universidad Cat<f3>lica~          269       304            88.5
    ##  3 Uni<f3>n La ~ Uni<f3>n La Calera vs L~          252       285            88.4
    ##  4 Deportivo An~ Deportivo Antofagasta v~          253       289            87.5
    ##  5 Uni<f3>n La ~ Universidad de Chile vs~          235       271            86.7
    ##  6 Universidad ~ Huachipato vs Universid~          202       233            86.7
    ##  7 Colo Colo     Colo Colo vs Universida~          231       267            86.5
    ##  8 Deportivo An~ Deportivo Antofagasta v~          162       188            86.2
    ##  9 Universidad ~ Deportivo Antofagasta v~          202       235            86.0
    ## 10 Uni<f3>n La ~ Uni<f3>n La Calera vs U~          176       205            85.9
    ## # ... with 120 more rows

``` r
fh2020_tiros <- NULL

fh2020_tiros = fh2020[,colnames(primer_tiempo2020) %in% c("equipo", "partido", "goals", "ontargetScoringAtt", "totalScoringAtt", "blockedScoringAtt", "shotOffTarget", "precision_tiros")]
fh2020_tiros = fh2020_tiros[order(fh2020_tiros$goals, decreasing = TRUE),]
fh2020_tiros
```

    ## # A tibble: 130 x 8
    ##    equipo partido ontargetScoring~ totalScoringAtt blockedScoringA~ goals
    ##    <chr>  <chr>              <dbl>           <dbl>            <dbl> <dbl>
    ##  1 Uni<f~ Uni<f3~                4               5                0     3
    ##  2 Unive~ Univer~                3               8                2     2
    ##  3 Colo ~ Colo C~                4              10                2     2
    ##  4 Audax~ Audax ~                3               9                2     2
    ##  5 Unive~ Deport~                5               9                2     2
    ##  6 Unive~ Univer~                4               7                1     2
    ##  7 Depor~ Deport~                5              14                4     2
    ##  8 Uni<f~ O'Higg~                3               4                1     2
    ##  9 Depor~ Deport~                2               7                2     2
    ## 10 La Se~ La Ser~                4               7                1     2
    ## # ... with 120 more rows, and 2 more variables: shotOffTarget <dbl>,
    ## #   precision_tiros <dbl>

## Filtrar Datos

``` r
huachipato <- filter(primer_tiempo2020, equipo == "Huachipato")
huachipato_tiros <- filter(fh2020_tiros, equipo == "Huachipato")
huachipato_pases <- filter(fh2020_pases, equipo == "Huachipato")
```

## Agregar Promedio/Suma Total/Min/…

``` r
huachipato_pases <- huachipato_pases[,!(colnames(huachipato_pases) %in% c("equipo"))] 

Promedios_Pas <- c("Promedio Pases",mean(huachipato_pases$accuratePass),mean(huachipato_pases$totalPass),mean(huachipato_pases$precision_pases))
huachipato_pases <- rbind(huachipato_pases, Promedios_Pas)

Max_Pas <- c("Max Pases",max(huachipato_pases$accuratePass),max(huachipato_pases$totalPass),max(huachipato_pases$precision_pases))
huachipato_pases <- rbind(huachipato_pases, Max_Pas)

Min_Pas <- c("Min Pases",min(huachipato_pases$accuratePass),min(huachipato_pases$totalPass),min(huachipato_pases$precision_pases))
huachipato_pases <- rbind(huachipato_pases, Min_Pas)

huachipato_pases
```

    ## # A tibble: 10 x 4
    ##    partido                        accuratePass    totalPass      precision_pases
    ##    <chr>                          <chr>           <chr>          <chr>          
    ##  1 Huachipato vs Universidad Con~ 208             250            83.2           
    ##  2 Huachipato vs Cobresal         193             232            83.18965517241~
    ##  3 Palestino vs Huachipato        152             206            73.78640776699~
    ##  4 Huachipato vs Universidad de ~ 97              135            71.85185185185~
    ##  5 Uni<f3>n Espa<f1>ola vs Huach~ 71              105            67.61904761904~
    ##  6 Huachipato vs Audax Italiano   89              136            65.44117647058~
    ##  7 Curic<f3> Unido vs Huachipato  84              143            58.74125874125~
    ##  8 Promedio Pases                 127.7142857142~ 172.428571428~ 71.97562823173~
    ##  9 Max Pases                      97              250            83.2           
    ## 10 Min Pases                      127.7142857142~ 105            58.74125874125~

## Graficos

``` r
pases_hua <- huachipato$accuratePass
huachipato2 <- huachipato[order(huachipato$accuratePass, decreasing = FALSE),]

#dotchart(huachipato$totalPass, labels = huachipato$partido, cex=0.5, xlab = "Pases", ylab = "Partido")

dotchart(huachipato$totalPass, labels = utf8_encode(huachipato$partido), cex=0.5, xlab = "Pases", ylab = "Partido")
```

![](Ayudantia-2_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
dotchart(huachipato$accuratePass, labels = utf8_encode(huachipato$partido), cex=0.5, xlab = "Pases", ylab = "Partido")
```

![](Ayudantia-2_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
dotchart(huachipato2$totalPass, labels = utf8_encode(huachipato$partido), cex=0.5, xlab = "Pases", ylab = "Partido")
```

![](Ayudantia-2_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->

``` r
dotchart(huachipato2$totalPass, labels = utf8_encode(huachipato$partido), main="Pases Acertados Huachipato", pch = 16, col=c("darkblue","dodgerblue"),lcolor="gray90", cex=0.8, xlab = "Pases", ylab = "Partido", cex.main=2,cex.lab=1.5)
```

![](Ayudantia-2_files/figure-gfm/unnamed-chunk-6-4.png)<!-- -->

## Analisis de Texto

``` r
texto <- primer_tiempo2020$partido
texto <- char_tolower(texto)
texto <- iconv(texto, to = "ASCII//TRANSLIT")

a <- dfm(texto, remove = c(stopwords("es"), "vs", "Universidad"))
dim(a)
```

    ## [1] 130  31
