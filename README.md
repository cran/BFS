
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/BFS)](https://CRAN.R-project.org/package=BFS)
[![Grand
total](https://cranlogs.r-pkg.org/badges/grand-total/BFS)](https://cran.r-project.org/package=BFS)
[![R-CMD-check](https://github.com/lgnbhl/BFS/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lgnbhl/BFS/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# BFS <img src="man/figures/logo.png" align="right" height="138" />

> Search and download data from the Swiss Federal Statistical Office

The `BFS` package allows to search and download public data from the <a
href="https://www.bfs.admin.ch/bfs/en/home/statistics/catalogues-databases/data.html"
target="_blank">Swiss Federal Statistical Office</a> (BFS stands for
*Bundesamt für Statistik* in German) API in a dynamic and reproducible
way.

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

Retrieve the list of publicly available datasets from the [data
catalog](https://www.bfs.admin.ch/bfs/de/home/statistiken/kataloge-datenbanken/daten.html)
in any language (“de”, “fr”, “it” or “en”) by calling
`bfs_get_catalog_data()`.

``` r
catalog_data_en <- bfs_get_catalog_data(language = "en")

catalog_data_en
```

    ## # A tibble: 184 × 7
    ##    title                language publication_date    number_asset url_bfs url_px
    ##    <chr>                <chr>    <dttm>                     <dbl> <chr>   <chr> 
    ##  1 Acknowledgment of p… en       2023-06-22 08:30:00     25945442 https:… https…
    ##  2 Adoptions by differ… en       2023-06-22 08:30:00     25945406 https:… https…
    ##  3 Deaths by instituti… en       2023-06-22 08:30:00     25945423 https:… https…
    ##  4 Deaths by sex, citi… en       2023-06-22 08:30:00     25945436 https:… https…
    ##  5 Deaths since 1803    en       2023-06-22 08:30:00     25945437 https:… https…
    ##  6 Dissolved partnersh… en       2023-06-22 08:30:00     25945438 https:… https…
    ##  7 Divorces by canton,… en       2023-06-22 08:30:00     25945378 https:… https…
    ##  8 Divorces by duratio… en       2023-06-22 08:30:00     25945381 https:… https…
    ##  9 Divorces by institu… en       2023-06-22 08:30:00     25945387 https:… https…
    ## 10 Live births by inst… en       2023-06-22 08:30:00     25945410 https:… https…
    ## # ℹ 174 more rows
    ## # ℹ 1 more variable: catalog_date <dttm>

You can search in the data catalog using the following arguments:

- `language`: The language of a BFS catalog, i.e. “de”, “fr”, “it” or
  “en”.
- `title`: to search in title, subtitle and supertitle.
- `spatial_division`: choose between “Switzerland”, “Cantons”,
  “Districts”, “Communes”, “Other spatial divisions” or “International”.
- `prodima`: b specific BFS themes using one or multiple prodima
  numbers.
- `inquiry`: by inquiry.
- `institution`: by institution.
- `publishing_year_start`: by publishing year start.
- `publishing_year_end`: by publishing year end.
- `order_nr`: by BFS Number (FSO number).

For example, you can search data related to students:

``` r
bfs_get_catalog_data(language = "en", title = "students")
```

    ## # A tibble: 4 × 7
    ##   title                 language publication_date    number_asset url_bfs url_px
    ##   <chr>                 <chr>    <dttm>                     <dbl> <chr>   <chr> 
    ## 1 University of applie… en       2023-03-28 08:30:00     24367605 https:… https…
    ## 2 University of applie… en       2023-03-28 08:30:00     24367607 https:… https…
    ## 3 University students … en       2023-03-28 08:30:00     24367723 https:… https…
    ## 4 University students … en       2023-03-28 08:30:00     24367729 https:… https…
    ## # ℹ 1 more variable: catalog_date <dttm>

English (“en”) and Italian (“it”) data catalogs offer a limited list of
datasets. For the full list please get the French (“fr”) or German
(“de”) data catalogs.

### Download data in any language

The function `bfs_get_data()` allows you to download any dataset from
the [data
catalog](https://www.bfs.admin.ch/bfs/de/home/statistiken/kataloge-datenbanken/daten.html)
using its BFS number (FSO number).

You need first to find the asset number of the dataset.

``` r
library(dplyr) #install.packages("dplyr")

asset_number_students <- bfs_get_catalog_data(language = "en", title = "students") |>
  dplyr::filter(title == "University students by year, ISCED field, sex and level of study") |>
  dplyr::pull(number_asset)

asset_number_students
```

    ## [1] 24367729

You can then find the BFS number by calling `bfs_get_asset_metadata()`.
This function returns a list containing the metadata of the asset. For
the student data, the BFS number is in the `orderNR` variable.

``` r
asset_meta_students <- bfs_get_asset_metadata(number_asset = asset_number_students)

bfs_number_students <- asset_meta_students$shop$orderNr

bfs_number_students
```

    ## [1] "px-x-1502040100_131"

You can also manually find the BFS number (FSO number) by opening the
related URL official webpage.

``` r
url_bfs_students <- bfs_get_catalog_data(language = "en", title = "students") |>
  dplyr::filter(title == "University students by year, ISCED field, sex and level of study") |>
  dplyr::pull(url_bfs)

# open students dataset webpage
browseURL(url_bfs_students)
```

<img style="border:1px solid black;" src="https://raw.githubusercontent.com/lgnbhl/BFS/master/man/figures/stat-tab.png" align="center" />

<br/>

Finally you can get the data using the `number_bfs` argument in a given
language (“en”, “de”, “fr” or “it”) from the official PXWeb API of the
Swiss Federal Statistical Office.

``` r
bfs_get_data(number_bfs = bfs_number_students, language = "en")
```

    ## # A tibble: 18,060 × 5
    ##    Year    `ISCED Field`     Sex    `Level of study`       `University students`
    ##    <chr>   <chr>             <chr>  <chr>                                  <dbl>
    ##  1 1980/81 Education science Male   First university degr…                   545
    ##  2 1980/81 Education science Male   Bachelor                                   0
    ##  3 1980/81 Education science Male   Master                                     0
    ##  4 1980/81 Education science Male   Doctorate                                 93
    ##  5 1980/81 Education science Male   Further education, ad…                    13
    ##  6 1980/81 Education science Female First university degr…                   946
    ##  7 1980/81 Education science Female Bachelor                                   0
    ##  8 1980/81 Education science Female Master                                     0
    ##  9 1980/81 Education science Female Doctorate                                 70
    ## 10 1980/81 Education science Female Further education, ad…                    52
    ## # ℹ 18,050 more rows

### “Too Many Requests” error message

When running the `bfs_get_data()` function you may get the following
error message (issue [\#7](https://github.com/lgnbhl/BFS/issues/7)).

    Error in pxweb_advanced_get(url = url, query = query, verbose = verbose) : 
      Too Many Requests (RFC 6585) (HTTP 429).

This could happen because you ran too many times a `bfs_get_*()`
function (API config is
[here](https://www.pxweb.bfs.admin.ch/api/v1/de/?config)). A solution is
to wait a few seconds before running the next `bfs_get_*()` function.
You can add a delay in your R code, for instance using `Sys.sleep(11)`
(11 seconds delay).

If the error message remains, it could be because you are querying a
very large BFS dataset. Two workarounds exist: a) download the BFS file
using `bfs_download_asset()` to read it locally or b) query only
specific elements of the data to reduce the API call (see next section).

Here an example using the `bfs_download_asset()` function:

``` r
BFS::bfs_download_asset(
  number_bfs = "px-x-1502040100_131", #number_asset also possible
  destfile = "px-x-1502040100_131.px"
)

library(pxR) # install.packages("pxR")
large_dataset <- pxR::read.px(filename = "px-x-1502040100_131.px") |>
  as.data.frame()
```

Note that reading a PX file using `pxR::read.px()` gives access only to
the German version.

### Query specific elements

First you want to get the variable names, i.e. `code`, and categories,
i.e. `values`, of your dataset. For example:

``` r
# choose a BFS number and language
metadata <- bfs_get_metadata(number_bfs = "px-x-1502040100_131", language = "en")

str(metadata)
```

    ## tibble [4 × 7] (S3: tbl_df/tbl/data.frame)
    ##  $ code       : chr [1:4] "Jahr" "ISCED Fach" "Geschlecht" "Studienstufe"
    ##  $ text       : chr [1:4] "Year" "ISCED Field" "Sex" "Level of study"
    ##  $ values     :List of 4
    ##   ..$ : chr [1:43] "0" "1" "2" "3" ...
    ##   ..$ : chr [1:42] "0" "1" "2" "3" ...
    ##   ..$ : chr [1:2] "0" "1"
    ##   ..$ : chr [1:5] "0" "1" "2" "3" ...
    ##  $ valueTexts :List of 4
    ##   ..$ : chr [1:43] "1980/81" "1981/82" "1982/83" "1983/84" ...
    ##   ..$ : chr [1:42] "Education science" "Teacher training without subject specialisation" "Teacher training with subject specialisation" "Fine arts" ...
    ##   ..$ : chr [1:2] "Male" "Female"
    ##   ..$ : chr [1:5] "First university degree or diploma" "Bachelor" "Master" "Doctorate" ...
    ##  $ time       : logi [1:4] TRUE NA NA NA
    ##  $ elimination: logi [1:4] NA TRUE TRUE TRUE
    ##  $ title      : chr [1:4] "University students by Year, ISCED Field, Sex and Level of study" "University students by Year, ISCED Field, Sex and Level of study" "University students by Year, ISCED Field, Sex and Level of study" "University students by Year, ISCED Field, Sex and Level of study"

Then you can manually select the dimensions of the dataset you want to
query.

``` r
# Manually create BFS query dimensions
# Use `code` and `values` elements
# Use "*" to select all
dimensions <- list(
  "Jahr" = c("40", "41"),
  "ISCED Fach" = c("0"),
  "Geschlecht" = c("*"),
  "Studienstufe" = c("2", "3"))

# Query BFS data with specific dimensions
BFS::bfs_get_data(
  number_bfs = "px-x-1502040100_131",
  language = "en",
  query = dimensions
  )
```

    ## # A tibble: 8 × 5
    ##   Year    `ISCED Field`     Sex    `Level of study` `University students`
    ##   <chr>   <chr>             <chr>  <chr>                            <dbl>
    ## 1 2020/21 Education science Male   Master                             151
    ## 2 2020/21 Education science Male   Doctorate                          121
    ## 3 2020/21 Education science Female Master                             555
    ## 4 2020/21 Education science Female Doctorate                          306
    ## 5 2021/22 Education science Male   Master                             143
    ## 6 2021/22 Education science Male   Doctorate                          115
    ## 7 2021/22 Education science Female Master                             599
    ## 8 2021/22 Education science Female Doctorate                          318

### Catalog of tables

A lot of datasets are not accessible through the official PXWeb API.
They are listed in the [catalog of
tables](https://www.bfs.admin.ch/bfs/de/home/statistiken/kataloge-datenbanken/tabellen.html).
You can search for specific tables using `bfs_get_catalog_tables()`.

``` r
catalog_tables_en_students <- bfs_get_catalog_tables(language = "en", title = "students")

catalog_tables_en_students
```

    ## # A tibble: 5 × 7
    ##   title              language publication_date    number_asset url_bfs url_table
    ##   <chr>              <chr>    <dttm>                     <dbl> <chr>   <chr>    
    ## 1 Students at unive… en       2023-04-05 00:00:00     24865589 https:… https://…
    ## 2 Students at unive… en       2023-04-05 00:00:00     24865590 https:… https://…
    ## 3 Students at unive… en       2023-03-28 08:30:00     24345362 https:… https://…
    ## 4 Students at unive… en       2023-03-28 08:30:00     24345374 https:… https://…
    ## 5 Students at unive… en       2023-03-28 08:30:00     24345366 https:… https://…
    ## # ℹ 1 more variable: catalog_date <dttm>

Most of the BFS tables are Excel or CSV files. You can download an table
with `bfs_download_asset()` using the `number asset`.

``` r
library(dplyr)

tables_asset_number_students <- catalog_tables_en_students |>
  dplyr::filter(title == "Students at universities and institutes of technology: Basistables") |>
  dplyr::pull(number_asset)

file_path <- BFS::bfs_download_asset(
  number_asset = tables_asset_number_students,
  destfile = "su-e-15.02.04.01.xlsx"
)
```

## Get geodata catalog

Display available geodata using `bfs_get_catalog_geodata()`.

``` r
catalog_geodata <- bfs_get_catalog_geodata(include_metadata = TRUE)

catalog_geodata
```

    ## # A tibble: 281 × 12
    ##    collection_id     type  href  title description created updated crs   license
    ##    <chr>             <chr> <chr> <chr> <chr>       <chr>   <chr>   <chr> <chr>  
    ##  1 ch.are.agglomera… API   http… Citi… "The list … 2021-1… 2023-0… http… propri…
    ##  2 ch.are.alpenkonv… API   http… Alpi… "The perim… 2021-1… 2022-0… http… propri…
    ##  3 ch.are.belastung… API   http… Load… "Passenger… 2021-1… 2022-0… http… propri…
    ##  4 ch.are.belastung… API   http… Load… "Passenger… 2021-1… 2022-0… http… propri…
    ##  5 ch.are.belastung… API   http… Load… "Vehicles … 2021-1… 2022-0… http… propri…
    ##  6 ch.are.belastung… API   http… Load… "Vehicles … 2021-1… 2022-0… http… propri…
    ##  7 ch.are.erreichba… API   http… Acce… "Accessibi… 2021-1… 2022-0… http… propri…
    ##  8 ch.are.erreichba… API   http… Acce… "Accessibi… 2021-1… 2022-0… http… propri…
    ##  9 ch.are.gemeindet… API   http… Typo… "The typol… 2021-1… 2022-0… http… propri…
    ## 10 ch.are.gueteklas… API   http… Publ… "The publi… 2021-1… 2023-0… http… propri…
    ## # ℹ 271 more rows
    ## # ℹ 3 more variables: provider_name <chr>, bbox <list>, inverval <list>

### Explore geodata catalog

Get the geographic dataset “Generalised borders G1 and area with urban
character”.

``` r
library(dplyr)

catalog_geodata |>
  filter(title == "Generalised borders G1 and area with urban character")
```

    ## # A tibble: 1 × 12
    ##   collection_id      type  href  title description created updated crs   license
    ##   <chr>              <chr> <chr> <chr> <chr>       <chr>   <chr>   <chr> <chr>  
    ## 1 ch.bfs.generalisi… API   http… Gene… Administra… 2022-0… 2023-0… http… propri…
    ## # ℹ 3 more variables: provider_name <chr>, bbox <list>, inverval <list>

### Download geodata

Download dataset and unzip file if needed.

``` r
# Access Generalised borders G1 and area with urban character
borders_g1_path <- bfs_download_geodata(
  collection_id = "ch.bfs.generalisierte-grenzen_agglomerationen_g1", 
  output_dir = tempdir() #  temporary directory
)

# you may need to unzip the file
unzip(borders_g1_path[4], exdir = "borders_G1")
```

By default, the files are downloaded in a temporary directory. You can
specify the folder where saving the files using the `output_dir`
argument.

### Explore and visualize data

You can then easily read and visualize geodata, for example using “sf”
and “ggplot2”.

``` r
library(sf) # read sf data
library(ggplot2) # data visualization

# explore available layers
sf::st_layers(dsn = "borders_G1")

swiss_cantons <- sf::st_read(dsn = "borders_G1", layer = "k4k23")
swiss_communes <- sf::st_read(dsn = "borders_G1", layer = "k4g23")

swiss_communes |> 
  ggplot() + 
  geom_sf() + 
  theme_minimal() +
  labs(caption = "Source: BFS Generalised borders G1 - www.bfs.admin.ch")
```

<img style="border:1px solid black;" src="https://raw.githubusercontent.com/lgnbhl/BFS/master/man/figures/communes_g1.png" align="center" />

Alternatively you can use the R package “mapview” to create an
interactive map.

``` r
library(mapview) # create interactive maps

swiss_communes |> 
  mapview::mapview(zcol = "AREA_HA", layer.name = "area ha", label = "GMDNAME")
```

<img style="border:1px solid black;" src="https://raw.githubusercontent.com/lgnbhl/BFS/master/man/figures/mapview.png" align="center" />

You can also download [cartographic base
maps](https://www.bfs.admin.ch/bfs/en/home/statistics/regional-statistics/base-maps/cartographic-bases.assetdetail.24025646.html)
using `bfs_download_asset()`. For instance you can get the communes and
the main lakes.

``` r
# asset file: https://dam-api.bfs.admin.ch/hub/api/dam/assets/24025646/master
base_maps_path <- bfs_download_asset(
  number_asset = "24025646",
  #number_bfs = "KM04-00-c-suis-2023-q",
  destfile = "base_maps.zip")

library(zip) #install.packages("zip")
zip::unzip(zipfile = base_maps_path, exdir = "base_maps")

lakes_file_path <- list.files(
  path = "base_maps", 
  pattern = "k4seenyyyymmdd11_ch2007Poly.shp", 
  recursive = TRUE, 
  full.names = TRUE)

lakes_sf <- read_sf(lakes_file_path)

swiss_communes_path <- list.files(
  path = "base_maps", 
  pattern = "K4polg20230101gf_ch2007Poly.shp", 
  recursive = TRUE, 
  full.names = TRUE)

swiss_communes_sf <- read_sf(swiss_communes_path)

swiss_communes_sf |> 
  ggplot() + 
  geom_sf() + 
  # add lakes
  geom_sf(
    data = lakes_sf,
    fill = "lightblue4",
    color = "lightblue4"
  ) +
  theme_minimal() +
  labs(caption = "Source: BFS ThemaKart - www.bfs.admin.ch")
```

<img style="border:1px solid black;" src="https://raw.githubusercontent.com/lgnbhl/BFS/master/man/figures/communes_themakart.png" align="center" />

## Main dependencies of the package

Under the hood, this package is using the
<a href="https://ropengov.github.io/pxweb/index.html"
target="_blank">pxweb</a> package to query the Swiss Federal Statistical
Office PXWEB API. PXWEB is an API structure developed by Statistics
Sweden and other national statistical institutions (NSI) to disseminate
public statistics in a structured way. To query the Geo Admin STAC API,
this package is using the
[rstac](https://brazil-data-cube.github.io/rstac/) package. STAC is a
specification of files and web services used to describe geospatial
information assets.

You can clean the column names of the datasets automatically using
`janitor::clean_names()` by adding the argument `clean_names = TRUE` in
the `bfs_get_data()` function.

## Other information

This package is in no way officially related to or endorsed by the Swiss
Federal Statistical Office (BFS).

## Contribute

Any contribution is strongly appreciated. Feel free to report a bug, ask
any question or make a pull request for any remaining
[issue](https://github.com/lgnbhl/BFS/issues).
