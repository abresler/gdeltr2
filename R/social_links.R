#' Social Media Link Dictionary
#'
#' @return
#' @export
#'
#' @examples
dictionary_social_links <-
  function() {
  data <- read_tsv("http://data.gdeltproject.org/gdeltv3/gkg/sociallinks/FILELIST.TXT", col_names = F) %>%
      set_names("urlGDELT")

  data <- data %>%
    mutate(dateData = urlGDELT %>% str_remove_all("http://data.gdeltproject.org/gdeltv3/gkg/sociallinks/|\\.txt.gz|sociallinks|\\.") %>% ymd()) %>%
    select(dateData, everything())

  data
  }

.parse_social_link <-
  function(url = "http://data.gdeltproject.org/gdeltv3/gkg/sociallinks/20190910.sociallinks.txt.gz" ) {
    data <-
      url %>%
      data.table::fread() %>%
      setNames(c("datetimePublished", "urlArticle", "urlSocialMedia")) %>%
      as_tibble()

    data %>%
      mutate(datetimePublished = datetimePublished %>% lubridate::ymd_hms() %>% lubridate::with_tz(Sys.timezone())) %>%
      mutate(urlGDELT = url)
  }

.parse_social_urls <-
  function(urls = c(
    "http://data.gdeltproject.org/gdeltv3/gkg/sociallinks/20190910.sociallinks.txt.gz"
  ),
  return_message = TRUE) {
    df <-
      tibble()

    .parse_social_link_safe <-
      purrr::possibly(.parse_social_link, tibble())

    success <- function(res) {
      if (return_message) {
        glue("Parsing {url}") %>% message()
      }

      data <-
        res$url %>%
        .parse_social_link_safe()

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


#' GDELT Social Media Links
#'
#' Data of articles linking
#' to social media mentions
#'
#' @param dates vector of dates
#' @param return_message if \code{t}
#'
#' @return
#' @export
#'
#' @examples
gdelt_social_data <-
  function(dates = NULL, return_message = T) {
    if (length(dates) == 0) {
      stop("Enter dates")
    }

    dict_urls <- dictionary_social_links()

    df_dates <-
      dict_urls %>% filter(dateData %in% parse_date(dates))

    if (nrow(df_dates) == 0) {
      stop("No Valid dates")
    }

    all_data <-
      .parse_social_urls(urls = df_dates$urlGDELT, return_message = return_message)

    all_data <-
      all_data %>%
      left_join(df_dates, by = "urlGDELT") %>%
      select(dateData, everything())

    all_data

  }