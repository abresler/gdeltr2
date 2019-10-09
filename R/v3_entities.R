

# dict --------------------------------------------------------------------


.dictionary_v3_entities <-
  function() {
    data <-
      "http://data.gdeltproject.org/gdeltv3/geg_gcnlapi/MASTERFILELIST.TXT" %>%
      fread() %>%
      as_tibble() %>%
      setNames(c("urlAPI"))

    data %>%
      mutate(
        datetimeData = urlAPI %>% str_remove_all("http://data.gdeltproject.org/gdeltv3/geg_gcnlapi/") %>%
          str_remove_all(".geg-gcnlapi.json.gz") %>% as.numeric() %>% lubridate::ymd_hms() %>% lubridate::with_tz(Sys.timezone()),
        dateData = as.Date(datetimeData),
        yearData = year(dateData) %>% as.numeric(),
        monthData = month(dateData) %>% as.numeric()
      ) %>%
      select(yearData, monthData, dateData, datetimeData, everything())
  }

#' V3 Entity API URL dictionary
#'
#' Dictionary of URLs from the V3 API
#' updated on 15 minute intervals
#'
#' @return
#' @export
#' @import memoise data.table dplyr lubridate lubridate
#' @examples
#' dictionary_v3_entity_urls()
dictionary_v3_entity_urls <-
  function(){
    .tt <- memoise::memoise(.dictionary_v3_entities)
    .tt()
  }



# parse -------------------------------------------------------------------


.parse_json_entity_data <-
  function(data) {
    df_entities <-
      data %>%
      select(urlArticle, dataEntities) %>%
      unnest(cols = dataEntities) %>%
      setNames(c("urlArticle", "nameEntity", "typeEntity", "countMentions", "meanSalience", "midGoogle", "urlWikipedia"))

    df_entities <-
      df_entities %>%
      mutate(countMentions = as.numeric(countMentions)) %>%
      nest_legacy(-urlArticle, .key = "dataEntities") %>%
      mutate(countEntitesArticle = dataEntities %>% map_dbl(nrow)) %>%
      select(urlArticle, countEntitesArticle, everything())

    data <- data %>%
      select(-dataEntities) %>%
      left_join(df_entities, by = "urlArticle")

    data
  }

.parse_entity_json_url <-
  function(url = "http://data.gdeltproject.org/gdeltv3/geg_gcnlapi/20170709211500.geg-gcnlapi.json.gz",
           return_message = T) {

    if (return_message) {
    time <- url %>% str_remove_all("http://data.gdeltproject.org/gdeltv3/geg_gcnlapi/|.geg-gcnlapi.json.gz")  %>%
        lubridate::ymd_hms() %>% lubridate::with_tz(Sys.timezone())
    glue::glue("Acquiring V3 Entity API data for {time}") %>% message()
    }

    data <- curl(url) %>%
      gzcon() %>%
      stream_in() %>%
      as_tibble() %>%
      setNames(c("datetimeArticle", "urlArticle", "dataEntities")) %>%
      mutate(
        datetimeArticle = ymd_hms(datetimeArticle),
        dateArticle = as.Date(datetimeArticle),
        yearArticle = year(dateArticle) %>% as.numeric(),
        monthArticle = month(dateArticle) %>% as.numeric(),
        domainArticle = urlArticle %>% urltools::domain(),
        urlAPI = url
      ) %>%
      select(yearArticle, monthArticle, dateArticle, datetimeArticle, domainArticle, everything()) %>%
      .parse_json_entity_data()

    data
  }

.parse_entity_json_url_m <-
  function(url, return_message = T) {
    .parse_entity_json_url_safe <- possibly(.parse_entity_json_url, tibble())
    .tt <- memoise::memoise(.parse_entity_json_url_safe)
    .tt(url = url, return_message = return_message)
  }

#' Parse V3 Entity API URLs
#'
#' Parse vector of V3 entity API urls
#'
#' @param urls vector of URLs
#' @param return_message if \code{TRUE} returns message
#'
#' @return \code{tibble}
#' @export
#'
#' @examples
parse_v3_entity_api_urls <-
  function(urls = c(
    "http://data.gdeltproject.org/gdeltv3/geg_gcnlapi/20190214011500.geg-gcnlapi.json.gz",
    "http://data.gdeltproject.org/gdeltv3/geg_gcnlapi/20190130204500.geg-gcnlapi.json.gz",
    "http://data.gdeltproject.org/gdeltv3/geg_gcnlapi/20190912134500.geg-gcnlapi.json.gz"
  )
  ,
  return_message = TRUE) {
    df <-
      tibble()

    .parse_entity_json_url_m_safe <-
      purrr::possibly(.parse_entity_json_url_m, tibble())

    success <- function(res) {
      data <-
        res$url %>%
        .parse_entity_json_url_m_safe(return_message = return_message)

      df <<-
        df %>%
        bind_rows(data)
    }
    failure <- function(msg) {
      tibble()
    }
    urls %>%
      walk(function(x) {
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()
    df
  }


#' V3 Entity API data
#'
#' Acquires data from the GDELT
#' entity mention API for period of specified dates
#'
#'
#' @param start_date start date in YMD format
#' @param end_date if not \code{NULL} end date
#' @param return_message if \code{TRUE} return message
#'
#' @return
#' @export
#'
#' @examples
v3_entity_api <-
  function(start_date = Sys.Date()-10,
           end_date =Sys.Date()-10,
           return_message) {
    if (length(start_date) == 0) {
      stop("Enter start date")
    }

    start_date <-
      start_date %>%
      as.character() %>%
      parse_date()

    if (start_date < ymd("2016-07-17")){
      message("Entity API Data starts 2016-07-17")
    }

    df_urls <- dictionary_v3_entity_urls()

    df_urls <-
      df_urls %>%
      filter(dateData >= start_date)

    if (length(end_date) > 0) {
      end_date <-
        end_date %>%
        as.character() %>%
        parse_date()
      df_urls <-
        df_urls %>%
        filter(dateData <= end_date)
      end_slug <-
        glue(" to {end_date}")
    } else {
      end_slug <- ""
    }

    if (return_message) {
      glue("Acquiring V3 entity API data for {start_date}{end_slug}") %>% message()
    }


    data <-
      parse_v3_entity_api_urls(urls = df_urls$urlAPI, return_message = return_message)

    data
  }
