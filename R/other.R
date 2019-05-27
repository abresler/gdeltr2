

#' GDELT Outlets
#'
#' Returns a dictionary of GDELT's tracked outlets
#' including website, logo, icon and English name
#'
#' @return
#' @export
#'
#' @examples
#' dictionary_outlets()
dictionary_outlets <- function() {
  data <-
    "http://data.gdeltproject.org/blog/2018-news-outlets-domain-info-may2018/MASTER-GDELTDOMAINSINFO-MAY2018.TXT" %>%
    read_tsv(col_names = F) %>%
    suppressWarnings() %>%
    suppressMessages()

  data <-
    data %>%
    purrr::set_names(c(
      "slugWebsite",
      "nameWebsite",
      "urlWebsite",
      'urlLogo',
      "urlIcon"
    )) %>%
    mutate(urlWebsite  = ifelse(urlWebsite %>% is.na(), glue::glue("https://{slugWebsite}"), urlWebsite) %>% as.character()) %>%
    select(slugWebsite, nameWebsite, urlWebsite, everything())

  data
}