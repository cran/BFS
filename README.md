
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/BFS)](https://CRAN.R-project.org/package=BFS)
[![Grand
total](https://cranlogs.r-pkg.org/badges/grand-total/BFS)](https://cran.r-project.org/package=BFS)
[![R-CMD-check](https://github.com/lgnbhl/BFS/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lgnbhl/BFS/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/lgnbhl/BFS/branch/master/graph/badge.svg)](https://app.codecov.io/gh/lgnbhl/BFS?branch=master)
[![LinkedIn](https://img.shields.io/badge/LinkedIn-Follow-E4405F?style=social&logo=linkedin)](https://www.linkedin.com/in/FelixLuginbuhl/)
<!-- badges: end -->

# BFS <img src="man/figures/logo.png" align="right" height="138" />

> Search and download data from the Swiss Federal Statistical Office

The `BFS` package allows to search and download public data from the
<a href="https://www.bfs.admin.ch/bfs/en/home/statistics/catalogue.html"
target="_blank">Swiss Federal Statistical Office</a> (BFS stands for
*Bundesamt für Statistik* in German) APIs in a dynamic and reproducible
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
catalog](https://www.bfs.admin.ch/bfs/en/home/statistics/catalogue.html)
in any language (“de”, “fr”, “it” or “en”) by calling
`bfs_get_catalog_data()`.

``` r
bfs_get_catalog_data(language = "en")
```

    ## # A tibble: 202 × 9
    ##    title                  language number_asset publication_date order_nr url_px
    ##    <chr>                  <chr>    <chr>        <date>           <chr>    <chr> 
    ##  1 Air emissions account… en       32331273     2024-09-26       px-x-02… https…
    ##  2 Deaths per month and … en       32506839     2024-09-26       px-x-01… https…
    ##  3 Divorces and divortia… en       32506841     2024-09-26       px-x-01… https…
    ##  4 Energy accounts of ec… en       32331272     2024-09-26       px-x-02… https…
    ##  5 Live births per month… en       32506840     2024-09-26       px-x-01… https…
    ##  6 Marriages and nuptial… en       32506838     2024-09-26       px-x-01… https…
    ##  7 New registrations of … en       32466294     2024-09-16       px-x-11… https…
    ##  8 Hotel sector: arrival… en       32427368     2024-09-05       px-x-10… https…
    ##  9 Hotel sector: arrival… en       32427369     2024-09-05       px-x-10… https…
    ## 10 Hotel sector: arrival… en       32427370     2024-09-05       px-x-10… https…
    ## # ℹ 192 more rows
    ## # ℹ 3 more variables: language_available <list>, url_structure_json <chr>,
    ## #   damId <int>

You can search in the data catalog using the following arguments:

- `language`: The language of a BFS catalog, i.e. “de”, “fr”, “it” or
  “en”.
- `title`: to search in title, subtitle and supertitle.
- `extended_search`: extended search in (sub/super)title, orderNr,
  summary, shortSummary, shortTextGNP.
- `spatial_division`: choose between “Switzerland”, “Cantons”,
  “Districts”, “Communes”, “Other spatial divisions” or “International”.
- `prodima`: by specific BFS themes using a unique prodima number.
- `inquiry`: by inquiry number.
- `institution`: by institution.
- `publishing_year_start`: by publishing year start.
- `publishing_year_end`: by publishing year end.
- `order_nr`: by BFS Number (FSO number).

For example, you can search data related to students using the extend
search:

``` r
catalog_student <- bfs_get_catalog_data(language = "en", extended_search = "student")

catalog_student
```

    ## # A tibble: 4 × 9
    ##   title                   language number_asset publication_date order_nr url_px
    ##   <chr>                   <chr>    <chr>        <date>           <chr>    <chr> 
    ## 1 University of applied … en       31306033     2024-03-28       px-x-15… https…
    ## 2 University of applied … en       31306029     2024-03-28       px-x-15… https…
    ## 3 University students by… en       31305852     2024-03-28       px-x-15… https…
    ## 4 University students by… en       31305854     2024-03-28       px-x-15… https…
    ## # ℹ 3 more variables: language_available <list>, url_structure_json <chr>,
    ## #   damId <int>

Note the the BFS number (FSO number) is available in column `order_nr`.

English (“en”) and Italian (“it”) data catalogs offer a limited list of
datasets. For the full list please get the French (“fr”) or German
(“de”) data catalogs (see `language_available` column).

### Download data in any language

The function `bfs_get_data()` allows you to download any dataset from
the [data
catalog](https://www.bfs.admin.ch/bfs/en/home/statistics/catalogue.html)
using its BFS number (FSO number).

Using the `number_bfs` argument (FSO number), you can get BFS data in a
given language (“en”, “de”, “fr” or “it”) from the official PXWeb API of
the Swiss Federal Statistical Office.

``` r
#catalog_student$order_nr[1] # px-x-1502040100_131

bfs_get_data(number_bfs = "px-x-1502040100_131", language = "en")
```

    ## # A tibble: 18,480 × 5
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
    ## # ℹ 18,470 more rows

### “Too Many Requests” error message

When running the `bfs_get_data()` function you may get the following
error message (issue [\#7](https://github.com/lgnbhl/BFS/issues/7)).

    Error in pxweb_advanced_get(url = url, query = query, verbose = verbose) : 
      Too Many Requests (RFC 6585) (HTTP 429).

This could happen because you ran too many times a `bfs_get_*()`
function (API config is
[here](https://www.pxweb.bfs.admin.ch/api/v1/de/?config)). A solution is
to wait a few seconds before running the next `bfs_get_*()` function.
You can add a delay in your R code using the `delay` argument.

``` r
bfs_get_data(
  number_bfs = "px-x-1502040100_131", 
  language = "en", 
  delay = 10
)
```

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

First you want to get the metadata of your dataset, i.e. the variables
(`code` and `text`) and dimensions (`values` and `valueTexts`). For
example:

``` r
metadata <- bfs_get_metadata(number_bfs = "px-x-1502040100_131", language = "en")

# tidy metadata
library(dplyr)
library(tidyr) # for unnest_longer

metadata_tidy <- metadata |>
  unnest_longer(c(values, valueTexts))

metadata_tidy
```

    ## # A tibble: 92 × 7
    ##    code  text  values valueTexts time  elimination
    ##    <chr> <chr> <chr>  <chr>      <lgl> <lgl>      
    ##  1 Jahr  Year  0      1980/81    TRUE  NA         
    ##  2 Jahr  Year  1      1981/82    TRUE  NA         
    ##  3 Jahr  Year  2      1982/83    TRUE  NA         
    ##  4 Jahr  Year  3      1983/84    TRUE  NA         
    ##  5 Jahr  Year  4      1984/85    TRUE  NA         
    ##  6 Jahr  Year  5      1985/86    TRUE  NA         
    ##  7 Jahr  Year  6      1986/87    TRUE  NA         
    ##  8 Jahr  Year  7      1987/88    TRUE  NA         
    ##  9 Jahr  Year  8      1988/89    TRUE  NA         
    ## 10 Jahr  Year  9      1989/90    TRUE  NA         
    ## # ℹ 82 more rows
    ## # ℹ 1 more variable: title <chr>

Then you can filter the dimensions you want to query using the `text`
and `valueTexts` variables and build the query dimension object with the
`code` and `values` variables.

``` r
# select dimensions
dim1 <- metadata_tidy |>
  filter(text == "Year" & valueTexts %in% c("2020/21", "2021/22"))
dim2 <- metadata_tidy |>
  filter(text == "Level of study" & valueTexts %in% c("Master", "Doctorate"))
dim3 <- metadata_tidy |>
  filter(text == "ISCED Field" & valueTexts %in% c("Education science"))
dim4 <- metadata_tidy |>
  filter(text == "Sex") # all valueTexts dimensions

# build dimensions list object
dimensions <- list(
  dim1$values,
  dim2$values,
  dim3$values,
  dim4$values
)

names(dimensions) <- c(
  unique(dim1$code), 
  unique(dim2$code), 
  unique(dim3$code), 
  unique(dim4$code)
)

dimensions
```

    ## $Jahr
    ## [1] "40" "41"
    ## 
    ## $Studienstufe
    ## [1] "2" "3"
    ## 
    ## $`ISCED Fach`
    ## [1] "0"
    ## 
    ## $Geschlecht
    ## [1] "0" "1"

Finally you can query BFS data with specific dimensions.

``` r
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
tables](https://www.bfs.admin.ch/bfs/en/home/statistics/catalogue.html).
You can search for specific tables using `bfs_get_catalog_tables()`.

``` r
catalog_tables_en_students <- bfs_get_catalog_tables(language = "en", extended_search = "students")

catalog_tables_en_students
```

    ## # A tibble: 5 × 7
    ##   title       language number_asset publication_date order_nr language_available
    ##   <chr>       <chr>    <chr>        <date>           <chr>    <list>            
    ## 1 Students a… en       31826381     2024-05-01       ts-x-15… <chr [4]>         
    ## 2 Students a… en       31826380     2024-05-01       ts-x-15… <chr [4]>         
    ## 3 Students a… en       31185431     2024-03-28       su-e-15… <chr [1]>         
    ## 4 Students a… en       31185438     2024-03-28       su-e-15… <chr [1]>         
    ## 5 Students a… en       31185427     2024-03-28       su-e-15… <chr [1]>         
    ## # ℹ 1 more variable: damId <int>

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

## Access geodata catalog

Display geo-information catalog of the Swiss Official STAC API using
`bfs_get_catalog_geodata()`.

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

### Download geodata

For example you can get information about the dataset “Generalised
borders G1 and area with urban character”.

``` r
library(dplyr)

geodata_g1 <- catalog_geodata |>
  filter(title == "Generalised borders G1 and area with urban character")
  
geodata_g1
```

    ## # A tibble: 1 × 12
    ##   collection_id      type  href  title description created updated crs   license
    ##   <chr>              <chr> <chr> <chr> <chr>       <chr>   <chr>   <chr> <chr>  
    ## 1 ch.bfs.generalisi… API   http… Gene… Administra… 2022-0… 2023-0… http… propri…
    ## # ℹ 3 more variables: provider_name <chr>, bbox <list>, inverval <list>

Download dataset by collection id with `bfs_download_geodata()` and
unzip file if needed.

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

Some layers are accessible using WMS (Web Map Service):

``` r
library(leaflet)

leaflet() %>% 
  setView(lng = 8, lat = 46.8, zoom = 8) %>%
  addWMSTiles(
    baseUrl = "https://wms.geo.admin.ch/?", 
    layers = "ch.bfs.generalisierte-grenzen_agglomerationen_g2",
    options = WMSTileOptions(format = "image/png", transparent = TRUE),
    attribution = "Generalised borders G1 © 2024 BFS")
```

<img style="border:1px solid black;" src="https://raw.githubusercontent.com/lgnbhl/BFS/master/man/figures/leaflet_g1.png" align="center" />

### Cartographic base maps

You can get [cartographic base
maps](https://www.bfs.admin.ch/bfs/en/home/statistics/regional-statistics/base-maps/cartographic-bases.assetdetail.24025646.html)
from the ThemaKart project using `bfs_get_base_maps()`. The list of
available geometries in the [official
documentation](https://www.bfs.admin.ch/asset/en/24025645).

The default arguments of `bfs_get_base_maps()` can be change to access
specific files:

``` r
# default arguments
bfs_get_base_maps(
  geom = NULL,
  category = "gf", # "gf" for total area (i.e. "Gesamtflaeche")
  type = "Poly",
  date = NULL,
  most_recent = TRUE, #get most recent file by default
  format = "shp",
  asset_number = "24025646" #change to get older ThemaKart data
)
```

A typical base maps ThemaKart file looks like this:

<img style="border:0px solid black;" src="https://raw.githubusercontent.com/lgnbhl/BFS/master/man/figures/base_maps_file.png" align="center" />

``` r
# list of geometry names: https://www.bfs.admin.ch/asset/en/24025645
switzerland_sf <- bfs_get_base_maps(geom = "suis")
communes_sf <- bfs_get_base_maps(geom = "polg", date = "20230101")
districts_sf <- bfs_get_base_maps(geom = "bezk")
cantons_sf <- bfs_get_base_maps(geom = "kant")
cantons_capitals_sf <- bfs_get_base_maps(geom = "stkt", type = "Pnts", category = "kk")
lakes_sf <- bfs_get_base_maps(geom = "seen", category = "11")

library(ggplot2)

ggplot() + 
  geom_sf(data = communes_sf, fill = "snow", color = "grey45") + 
  geom_sf(data = lakes_sf, fill = "lightblue2", color = "black") +
  geom_sf(data = districts_sf, fill = "transparent", color = "grey65") + 
  geom_sf(data = cantons_sf, fill = "transparent", color = "black") +
  geom_sf(data = cantons_capitals_sf, shape = 18, size = 3) +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  labs(caption = "Source: ThemaKart, © BFS")
```

<img style="border:1px solid black;" src="https://raw.githubusercontent.com/lgnbhl/BFS/master/man/figures/base_maps.png" align="center" />

You can create an interactive map easily with the mapview R package.

``` r
library(mapview)

BFS::bfs_get_base_maps(geom = "bezk") |>
  mapview(zcol = "name", legend = FALSE)
```

<img style="border:1px solid black;" src="https://raw.githubusercontent.com/lgnbhl/BFS/master/man/figures/mapview.png" align="center" />

### Swiss Official Commune Register

The package also contains the official Swiss official commune registers
for different administrative levels:

- `register_gde`
- `register_gde_other`
- `register_bzn`
- `register_kt`
- `register_kt_seeanteile`
- `register_dic`

``` r
# commune register data
BFS::register_gde
```

    ## # A tibble: 2,136 × 8
    ##    GDEKT GDEBZNR GDENR GDENAME            GDENAMK      GDEBZNA GDEKTNA GDEMUTDAT
    ##    <chr>   <dbl> <dbl> <chr>              <chr>        <chr>   <chr>   <chr>    
    ##  1 ZH        101     1 Aeugst am Albis    Aeugst am A… Bezirk… Zürich  1976-11-…
    ##  2 ZH        101     2 Affoltern am Albis Affoltern a… Bezirk… Zürich  1848-09-…
    ##  3 ZH        101     3 Bonstetten         Bonstetten   Bezirk… Zürich  1848-09-…
    ##  4 ZH        101     4 Hausen am Albis    Hausen am A… Bezirk… Zürich  1911-01-…
    ##  5 ZH        101     5 Hedingen           Hedingen     Bezirk… Zürich  1848-09-…
    ##  6 ZH        101     6 Kappel am Albis    Kappel am A… Bezirk… Zürich  1911-01-…
    ##  7 ZH        101     7 Knonau             Knonau       Bezirk… Zürich  1848-09-…
    ##  8 ZH        101     8 Maschwanden        Maschwanden  Bezirk… Zürich  1848-09-…
    ##  9 ZH        101     9 Mettmenstetten     Mettmenstet… Bezirk… Zürich  1848-09-…
    ## 10 ZH        101    10 Obfelden           Obfelden     Bezirk… Zürich  1848-09-…
    ## # ℹ 2,126 more rows

You can use registers to ease geodata analysis.

``` r
library(dplyr)
library(sf)

communes_sf <- bfs_get_base_maps(geom = "polg", date = "20230101")

communes_ge <- communes_sf |>
  inner_join(BFS::register_gde |> 
               filter(GDEKTNA == "Genève"), 
             by = c("id" = "GDENR"))

bbox_ge <- sf::st_bbox(communes_ge)

lake_leman <- bfs_get_base_maps(geom = "seen", category = "11") |>
  filter(name == "Lac Léman")

communes_ge |> 
  ggplot() + 
  geom_sf(data = lake_leman, fill = "lightblue2", color = "grey65") +
  geom_sf(fill = "snow", color = "grey65") + 
  geom_sf_text(aes(label = name), size = 3, check_overlap = T) + 
  # bounding box
  coord_sf(
    xlim = c(bbox_ge$xmin, bbox_ge$xmax),
    ylim = c(bbox_ge$ymin, bbox_ge$ymax)
  ) +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  labs(title = "Communes du canton de Genève",
       x = NULL, y = NULL, 
       caption = "Source: ThemaKart, © BFS")
```

<img style="border:1px solid black;" src="https://raw.githubusercontent.com/lgnbhl/BFS/master/man/figures/base_maps_ge.png" align="center" />

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
