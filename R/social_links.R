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
      fread() %>%
      setNames(c("datetimeArticle", "urlArticle", "urlSocialMedia")) %>%
      as_tibble()

    data <-
      data %>%
      mutate(datetimeArticle = datetimeArticle %>% lubridate::ymd_hms() %>% lubridate::with_tz(Sys.timezone()),
             domainArticle = urlArticle %>% urltools::domain(),
             dateArticle = as.Date(datetimeArticle)) %>%
      mutate(urlGDELT = url) %>%
      select(dateArticle, datetimeArticle, domainArticle, everything())

    data
  }

.parse_social_link_m <-
  function(url = "http://data.gdeltproject.org/gdeltv3/gkg/sociallinks/20190910.sociallinks.txt.gz", return_message = T) {
    .tt <- memoise(.parse_social_link)
    if (return_message) {
      glue("Parsing {url}") %>% message()
    }
    data <- .tt(url = url)



    data
  }

#' Parse V3 Social Link URLs
#'
#' @param urls vector of social link urls
#' @param return_message if \code{TRUE} rturns message
#'
#' @return
#' @export
#'
#' @examples
parse_v3_social_urls <-
  function(urls = NULL, return_message = T) {
    if (length(urls) ==0) {
      stop("Enter Social URLs")
    }
    df <-
      tibble()

    .parse_social_link_m_safe <-
      possibly(.parse_social_link_m, tibble())

    success <- function(res) {
      data <-
        res$url %>%
        .parse_social_link_m_safe(return_message = return_message)

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
#' @param return_message if \code{true} returns message
#'
#' @return
#' @export
#'
#' @examples
v3_social_data <-
  function(dates = NULL, return_message = T) {
    if (length(dates) == 0) {
      stop("Enter dates")
    }

    dict_urls <- dictionary_social_links()
    dates <- dates %>% as.character() %>% parse_date()
    df_dates <-
      dict_urls %>% filter(dateData %in% dates)

    if (nrow(df_dates) == 0) {
      stop("No Valid dates")
    }

    all_data <-
      parse_v3_social_urls(urls = df_dates$urlGDELT, return_message = return_message)

    all_data <-
      all_data %>%
      left_join(df_dates, by = "urlGDELT") %>%
      select(dateData, everything())

    all_data

  }