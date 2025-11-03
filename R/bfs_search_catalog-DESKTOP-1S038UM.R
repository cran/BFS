#' Get the BFS data catalog
#'
#' Get the list of the data available in the official DAM-API of the Swiss Federal Statistical Office asset catalog.
#'
#' @param language character The language of a BFS catalog, i.e. "de", "fr", "it" or "en".
#' @param title character String to search in (sub/super)title
#' @param search character String for an extended search in (sub/super)title, orderNr, summary, shortSummary, shortTextGNP
#' @param format string. The format (masterFormat) of the data ("px", "csv", "json", "xlsx").
#' @param spatial_division BFS datasets by spatial division, choose between "Switzerland", "Cantons", "Districts", "Communes", "Other spatial divisions" or "International"
#' @param theme charater Get only specific BFS themes using a unique theme (in English)
#' @param prodima numeric Get only specific BFS themes using a unique prodima number
#' @param inquiry character BFS datasets for an inquiry
#' @param institution character BFS datasets for an institution
#' @param publishing_year_start character BFS datasets for a publishing year start
#' @param publishing_year_end character BFS datasets for a publishing year end
#' @param order_nr character Filter by BFS Number (FSO number)
#' @param limit integer limit of query results (1000 by default)
#' @param article_model_group integer articleModel parameter query
#' @param article_model integer articleModel parameter query
#'
#' @examples
#' \donttest{
#' bfs_search_catalog(
#'   language = "en", 
#'   title = "students", 
#'   format = "px", 
#'   theme = "Education and science"
#' )
#' }
#'
#' @return A tbl_df (a type of data frame; see tibble or
#' dplyr packages). Returns NULL if no connection.
#'
#' @export
bfs_search_catalog <- function(
    language = "de", 
    search = NULL, 
    format = NULL, 
    theme = NULL,
    spatial_division = NULL, 
    title = NULL, 
    prodima = NULL, 
    inquiry = NULL, 
    institution = NULL, 
    publishing_year_start = NULL, 
    publishing_year_end = NULL, 
    order_nr = NULL, 
    limit = 1000, 
    article_model = NULL, 
    article_model_group = NULL
) {
  spatial_division_selected <- if(!is.null(spatial_division)) {
    switch(
      match.arg(
        arg = spatial_division,
        choices = c("Switzerland", "Cantons", "Districts", "Communes", "Other spatial divisions", "International")
      ),
      "Switzerland" = 900091,
      "Cantons" = 900092,
      "Districts" = 900093,
      "Communes" = 900004,
      "Other spatial divisions" = 900008,
      "International" = 900068
    )
  }
  theme <- if(!is.null(theme)) {
    switch(
      match.arg(
        arg = theme, 
        choices = c("Statistical basis and overviews", "Population", "Territory and environment", "Work and income", "National economy", "Prices", "Industry and services", "Agriculture and forestry", "Energy", "Construction and housing", "Tourism", "Mobility and transport", "Money, banks and insurance", "Social security", "Health", "Education and science", "Culture, media, information society, sports", "Politics", "General Government and finance", "Crime and criminal justice", "Economic and social situation of the population", "Sustainable development, regional and international disparities")
      ),
      "Statistical basis and overviews" = 900001,
      "Population" = 900010,
      "Territory and environment" = 900035,
      "Work and income" = 900051,
      "National economy" = 900075,
      "Prices" = 900084,
      "Industry and services" = 900092,
      "Agriculture and forestry" = 900104,
      "Energy" = 900127,
      "Construction and housing" = 900140,
      "Tourism" = 900160,
      "Mobility and transport" = 900169,
      "Money, banks and insurance" = 900191,
      "Social security" = 900198,
      "Health" = 900210,
      "Education and science" = 900212,
      "Culture, media, information society, sports" = 900214,
      "Politics" = 900226,
      "General Government and finance" = 900239,
      "Crime and criminal justice" = 900257,
      "Economic and social situation of the population" = 900269,
      "Sustainable development, regional and international disparities" = 900276
    )
  }
  prodima <- if(!is.null(theme)) theme else prodima
  
  if (!curl::has_internet()) {
    message("No internet connection")
    return(NULL)
  }
  req <- httr2::request("https://dam-api.bfs.admin.ch/hub/api")
  req <- httr2::req_headers(req, accept = "application/json", `Accept-Language` = language)
  req <- httr2::req_url_path_append(req, "dam/assets")
  req <- httr2::req_url_query(req,
                              language = language,
                              articleModelGroup = article_model_group,
                              articleModel = article_model, # 900029 and 900033 seems to be for 'article type' = "data"
                              title = title,
                              extendedSearch = search,
                              masterFormat = format,
                              spatialdivision = spatial_division_selected,
                              prodima = prodima,
                              inquiry = inquiry,
                              institution = institution,
                              periodStart = publishing_year_start,
                              periodEnd = publishing_year_end,
                              orderNr = order_nr,
                              limit = limit)
  req <- httr2::req_retry(req, max_tries = 2)
  req <- httr2::req_perform(req)
  resp <- httr2::resp_body_json(req, simplifyVector = TRUE, flatten = TRUE)
  
  if("data" %in% names(resp)) {
    tbl <- tibble::as_tibble(resp$data)
    return(tbl)
  }
  tbl <- tibble::as_tibble(resp)
  return(tbl)
}
