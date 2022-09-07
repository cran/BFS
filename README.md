
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/BFS)](https://CRAN.R-project.org/package=BFS)
[![Grand
total](https://cranlogs.r-pkg.org/badges/grand-total/BFS)](https://cran.r-project.org/package=BFS)
[![R build
status](https://github.com/lgnbhl/BFS/workflows/R-CMD-check/badge.svg)](https://github.com/lgnbhl/BFS/actions)
<!-- badges: end -->

# BFS <img src="man/figures/logo.png" align="right" />

> Search and download data from the Swiss Federal Statistical Office

The `BFS` package allows to search and download public data from the <a
href="https://www.bfs.admin.ch/bfs/en/home/statistics/catalogues-databases/data.html"
target="_blank">Swiss Federal Statistical Office</a> (BFS stands for
*Bundesamt für Statistik* in German) in a dynamic and reproducible way.

## Installation

``` r
install.packages("BFS")
```

You can also install the development version from Github.

``` r
devtools::install_github("lgnbhl/BFS")
```

## Usage

``` r
library(BFS)
```

### Get the data catalog

To search and download data from the Swiss Federal Statistical Office,
you first need to retrieve information about the available public
datasets.

You can get the data catalog by language based on the official [RSS
feed](https://www.bfs.admin.ch/bfs/en/home/statistiken/kataloge-datenbanken/daten/_jcr_content/par/ws_catalog.rss.xml?skipLimit=true).
Unfortunately, it seems that not the all public datasets are in the RSS
feed, but only the most recently udpated. Note also that Italian and
English give access to less datasets.

``` r
catalog_data_en <- bfs_get_catalog_data(language = "en")

catalog_data_en
```

    ## # A tibble: 180 x 5
    ##    title                                          langu~1 publi~2 url_bfs url_px
    ##    <chr>                                          <chr>   <chr>   <chr>   <chr> 
    ##  1 Businesses by difficulties in recruiting staf~ en      Busine~ https:~ https~
    ##  2 Businesses by difficulties in recruiting staf~ en      Busine~ https:~ https~
    ##  3 Businesses by employment prospects and econom~ en      Busine~ https:~ https~
    ##  4 Businesses by employment prospects and major ~ en      Busine~ https:~ https~
    ##  5 Job vacancies by economic divisions (selectio~ en      Job va~ https:~ https~
    ##  6 Job vacancies by major region                  en      Job va~ https:~ https~
    ##  7 Jobs by economic division, employment rate an~ en      Jobs b~ https:~ https~
    ##  8 Jobs by major region, economic sector, employ~ en      Jobs b~ https:~ https~
    ##  9 Retail Trade Turnover Statistics - monthly se~ en      Retail~ https:~ https~
    ## 10 Retail Trade Turnover Statistics - quarterly ~ en      Retail~ https:~ https~
    ## # ... with 170 more rows, and abbreviated variable names 1: language,
    ## #   2: published

To find older datasets, you can use the search bar in the [official BFS
website](https://www.bfs.admin.ch/bfs/de/home/statistiken/kataloge-datenbanken/daten.html).

### Search for a specific dataset

You could use for example `dplyr` to search for a given dataset.

``` r
library(dplyr)

catalog_data_uni <- catalog_data_en %>%
  filter(title == "University students by year, ISCED field, sex and level of study")

catalog_data_uni
```

    ## # A tibble: 1 x 5
    ##   title                                           langu~1 publi~2 url_bfs url_px
    ##   <chr>                                           <chr>   <chr>   <chr>   <chr> 
    ## 1 University students by year, ISCED field, sex ~ en      Univer~ https:~ https~
    ## # ... with abbreviated variable names 1: language, 2: published

### Download a dataset in any language

To download a BFS dataset, you have two options. You can add the
official BFS URL webpage to the `url_bfs` argument to the
`bfs_get_data()`. For example, you can use the URL of a given dataset
you found using `bfs_get_catalog_data()`.

``` r
# https://www.bfs.admin.ch/content/bfs/en/home/statistiken/kataloge-datenbanken/daten.assetdetail.16324907.html
df_uni <- bfs_get_data(url_bfs = catalog_data_uni$url_bfs, language = "en")
```

    ##   Downloading large query (in 4 batches):
    ##   |                                                                              |                                                                      |   0%  |                                                                              |==================                                                    |  25%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================================| 100%

``` r
df_uni
```

    ## # A tibble: 17,640 x 5
    ##    Year    `ISCED Field`     Sex    `Level of study`                     Unive~1
    ##    <chr>   <chr>             <chr>  <chr>                                  <dbl>
    ##  1 1980/81 Education science Male   First university degree or diploma       545
    ##  2 1980/81 Education science Male   Bachelor                                   0
    ##  3 1980/81 Education science Male   Master                                     0
    ##  4 1980/81 Education science Male   Doctorate                                 93
    ##  5 1980/81 Education science Male   Further education, advanced studies~      13
    ##  6 1980/81 Education science Female First university degree or diploma       946
    ##  7 1980/81 Education science Female Bachelor                                   0
    ##  8 1980/81 Education science Female Master                                     0
    ##  9 1980/81 Education science Female Doctorate                                 70
    ## 10 1980/81 Education science Female Further education, advanced studies~      52
    ## # ... with 17,630 more rows, and abbreviated variable name
    ## #   1: `University students`

Note that some datasets are only accessible in German and French.

In case the data is not accessible using `bfs_get_catalog_data()`, you
can manually add the BFS number in the `bfs_get_data()` function using
the `number_bfs` argument.

``` r
# open webpage
browseURL("https://www.bfs.admin.ch/content/bfs/en/home/statistiken/kataloge-datenbanken/daten.assetdetail.16324907.html")
```

<img style="border:1px solid black;" src="https://raw.githubusercontent.com/lgnbhl/BFS/master/man/figures/screenshot.png" align="center" />

<br/>

Use again `bfs_get_data()` but this time with the `number_bfs` argument.

``` r
bfs_get_data(number_bfs = "px-x-1502040100_131", language = "en")
```

Please privilege the `number_bfs` argument of the `bfs_get_data()` if
you want more stable and reproducible code.

You can access additional information about the dataset by running
`bfs_get_data_comments()`.

``` r
bfs_get_data_comments(number_bfs = "px-x-1502040100_131", language = "en")
```

    ##   Downloading large query (in 4 batches):
    ##   |                                                                              |                                                                      |   0%  |                                                                              |==================                                                    |  25%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================================| 100%

    ## # A tibble: 1 x 4
    ##   row_no col_no comment_type   comment                                          
    ##    <int>  <int> <chr>          <chr>                                            
    ## 1     NA      4 column_comment "To ensure that the presentations from cubes con~

### Catalog of tables

A lot of tables are not accessible through the official API, but they
are still present in the official BFS website. You can access the [RSS
feed tables
catalog](https://www.bfs.admin.ch/bfs/en/home/statistiken/kataloge-datenbanken/tabellen/_jcr_content/par/ws_catalog.rss.xml?skipLimit=true)
using `bfs_get_catalog_tables()`. Most of these tables are Excel or CSV
files. Note again that only a part of all the public tables accessible
are in the RSS feed (the most recently updated datasets).

``` r
catalog_tables_en <- bfs_get_catalog_tables(language = "en")

catalog_tables_en
```

    ## # A tibble: 350 x 5
    ##    title                                         langu~1 publi~2 url_bfs url_t~3
    ##    <chr>                                         <chr>   <chr>   <chr>   <chr>  
    ##  1 "Difficulties in recruiting staff with educa~ en      "Diffi~ https:~ https:~
    ##  2 "Difficulties in recruiting staff with educa~ en      "Diffi~ https:~ https:~
    ##  3 "Difficulties in recruiting staff with highe~ en      "Diffi~ https:~ https:~
    ##  4 "Difficulties in recruiting staff with unive~ en      "Diffi~ https:~ https:~
    ##  5 "Full-time job equivalent per sector"         en      "Full-~ https:~ https:~
    ##  6 "Full-time job per sector and gender"         en      "Full-~ https:~ https:~
    ##  7 "Index of employment evolution prospects per~ en      "Index~ https:~ https:~
    ##  8 "Job vacancy per branch of economic activity~ en      "Job v~ https:~ https:~
    ##  9 "Jobs per sector and gender, gross and seaso~ en      "Jobs ~ https:~ https:~
    ## 10 "Jobs per sector and main region"             en      "Jobs ~ https:~ https:~
    ## # ... with 340 more rows, and abbreviated variable names 1: language,
    ## #   2: published, 3: url_table

``` r
library(dplyr)
library(openxlsx)

index_table_url <- catalog_tables_en %>%
  filter(grepl("index", title)) %>% # search table
  slice(1) %>%
  pull(url_table)

df <- tryCatch(expr = openxlsx::read.xlsx(index_table_url, startRow = 1),
    error = function(e) "Failed reading table") %>%
  as_tibble()

df
```

    ## # A tibble: 466 x 19
    ##    Landesind~1 X2    X3    X4    X5    ©.Bun~2 X7    Indic~3 X9    Indic~4 X11  
    ##    <chr>       <chr> <chr> <chr> <chr> <chr>   <chr> <chr>   <chr> <chr>   <chr>
    ##  1 Warenkorbs~ <NA>  <NA>  <NA>  <NA>  "Ausku~ <NA>  "Panie~ <NA>  "Panie~ <NA> 
    ##  2 Basis Deze~ <NA>  <NA>  <NA>  <NA>  "http:~ <NA>  "Base ~ <NA>  "Base ~ <NA> 
    ##  3 Code        PosNo PosT~ Level COIC~ "Posit~ PosT~ "Posit~ PosT~ "Posiz~ PosT~
    ##  4 100_100     100   1     1     <NA>  "Total" Total "Total" Total "Total~ Tota~
    ##  5 100_1       1     2     2     '01   "    N~ Nahr~ "    A~ Alim~ "    P~ Prod~
    ##  6 100_1001    1001  3     3     '01.1 "     ~ Nahr~ "     ~ Alim~ "     ~ Prod~
    ##  7 100_1002    1002  3     4     '01.~ "     ~ Brot~ "     ~ Pain~ "     ~ Pane~
    ##  8 100_1003    1003  4     5     '01.~ "     ~ Reis  "     ~ Riz   "     ~ Riso 
    ##  9 100_1008    1008  4     5     '01.~ "     ~ Mehl~ "     ~ Fari~ "     ~ Fari~
    ## 10 100_1014    1014  3     5     <NA>  "     ~ Brot~ "     ~ Pain~ "     ~ Pane~
    ## # ... with 456 more rows, 8 more variables: Swiss.Consumer.Price.Index <chr>,
    ## #   X13 <chr>, Gewicht <chr>, X15 <chr>, X16 <chr>, X17 <chr>, X18 <chr>,
    ## #   X19 <chr>, and abbreviated variable names
    ## #   1: Landesindex.der.Konsumentenpreise,
    ## #   2: `©.Bundesamt.für.Statistik,.Espace.de.l'Europe.10,.CH-2010.Neuchâtel`,
    ## #   3: Indice.des.prix.à.la.consommation,
    ## #   4: Indice.nazionale.dei.prezzi.al.consumo

## Other information

A [blog
article](https://felixluginbuhl.com/blog/posts/2019-11-07-swiss-data/)
showing a concrete example about how to use the BFS package and to
visualize the data in a Swiss map.

The BFS package is using the
<a href="https://github.com/rOpenGov/pxweb" target="_blank">pxweb</a> R
package under the hood to access the Swiss Federal Statistical Office
pxweb API and <a href="https://github.com/RobertMyles/tidyRSS"
target="_blank">tidyRSS</a> to scrap the official BFS RSS feeds.

This package is in no way officially related to or endorsed by the Swiss
Federal Statistical Office (BFS).

## Contribute

Any contribution is strongly appreciated. Feel free to report a bug, ask
any question or make a pull request for any remaining
[issue](https://github.com/lgnbhl/BFS/issues).
