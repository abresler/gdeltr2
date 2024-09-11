.parse_v1_url_parameters <-
  function(url = "https://api.gdeltproject.org/api/v1/search_ftxtsearch/search_ftxtsearch?query='donald%20trump'%20sourcelang:english&output=artimglist&sort=date&maxrows=1000&dropdup=true&trans=googtrans") {
    url_parts <- url |> str_split("\\?") |> flatten_chr()
    query_params <- url_parts[[2]]
    query_params |> str_remove_all("")
    tibble(query_params) |>
      separate_rows(query_params, sep = "\\:")
  }

# url_gen -----------------------------------------------------------------


.v1_term_url <-
  function(term = "'Donald Trump'",
           output = "artimglist",
           output_timeline_type = "volume",
           domain = NA,
           source_country = NULL,
           exclude_country = NULL,
           source_language = "English",
           exclude_language = NULL,
           last_minutes = NA,
           max_rows = 1000,
           absolute_tone_more_than = NA,
           absolute_tone_less_than = NA,
           tone_less_than = NA,
           tone_more_than = NA,
           dedeup_results = T,
           sort_by = 'date',
           use_google_translate = TRUE) {
    url_base <-
      'https://api.gdeltproject.org/api/v1/search_ftxtsearch/search_ftxtsearch?query='
    no_term <- is.na(term)
    if (no_term) {
      term_slug <-
        ''
      term_word <-
        'all words'
    } else {
      term_slug <-
        term %>%
        str_to_lower() %>%
        URLencode()
      term_word <-
        term
      search_term <-
        glue::glue("term:{term_word}")
    }

    if (no_term & !domain %>% is.na()) {
      term_word <-
        domain
    }

    if (!domain %>% is.na()) {
      domain_slug <-
        '%20domain:' %>%
        paste0(domain %>% URLencode())
      search_term <-
        glue::glue("domain:{domain}")
    } else {
      domain_slug <-
        ''
    }

    if (!last_minutes %>% is.na()) {
      last_minute_slug <-
        '%20lastminutes:' %>%
        paste0(last_minutes)
    } else {
      last_minute_slug <-
        ''
    }

    if (!tone_more_than %>% is.na()) {
      if (tone_more_than > 100) {
        stop("Tone can't be over 100")
      }
      tone_more_slug <-
        '%20tonemorethan:' %>%
        paste0(tone_more_than)
    } else {
      tone_more_slug <-
        ''
    }

    if (!absolute_tone_more_than %>% is.na()) {
      if (absolute_tone_more_than > 100) {
        stop("Tone can't be over 100")
      }
      absolute_tone_more_slug <-
        '%20tonemorethanabs:' %>%
        paste0(tone_more_than)
    } else {
      absolute_tone_more_slug <-
        ''
    }

    if (!tone_less_than %>% is.na()) {
      if (tone_less_than < -100) {
        stop("Tone can't be under -100")
      }
      tone_less_slug <-
        '%20tonelessthan:' %>%
        paste0(tone_less_than)
    } else {
      tone_less_slug <-
        ''
    }

    if (!absolute_tone_less_than %>% is.na()) {
      if (absolute_tone_less_than < 0) {
        stop("Absolute tone can't be under 0")
      }
      absolute_tone_less_slug <-
        '%20tonelessthanabs:' %>%
        paste0(tone_less_than)
    } else {
      absolute_tone_less_slug <-
        ''
    }

    term_slug <-
      term_slug %>%
      paste0(domain_slug,
             last_minute_slug,
             tone_more_slug,
             tone_less_slug)

    sort_df <-
      tibble(
        sort_term = c('date', 'relevence', 'tone ascending', 'tone descending'),
        sort_slug = c('date', 'rel', 'toneasc', 'tonedesc')
      )

    if (str_to_lower(sort_by) %in% sort_df$sort_term == F) {
      stop("Sorry sort terms can only be\n" %>%
             paste0(paste0(sort_df$sort_term, collapse = '\n')))
    }

    slug_sort <-
      sort_df %>%
      dplyr::filter(sort_term == str_to_lower(sort_by)) %>%
      .$sort_slug

    slug_sort <-
      '&sort=' %>%
      paste0(slug_sort)

    if (!max_rows %>% is.na()) {
      max_row_slug <-
        '&maxrows=' %>%
        paste0(max_rows)

    } else {
      max_row_slug <-
        ''
    }

    if (length(source_country) > 0) {
      source_country <-
        source_country |> str_to_lower() |> str_remove_all("\\-|\\ ")
      source_country_slug <-
        '%20sourcecountry:' %>%
        paste0(source_country)
    } else {
      source_country_slug <-
        ''
    }

    if (length(source_language) > 0) {
      source_language <-
        source_language |> str_to_lower() |> str_remove_all("\\-|\\ ")
      source_language_slug <-
        '%20sourcelang:' %>%
        paste0(source_language)
    } else {
      source_language_slug <-
        ''
    }

    if (length(exclude_country) > 0) {
      exclude_country <-
        exclude_country |> str_to_lower() |> str_remove_all("\\-|\\ ")
      exclude_country_slug <-
        '%20sourcecountry:' %>%
        paste0(source_country)
    } else {
      exclude_country_slug <-
        ''
    }

    if (length(exclude_language) > 0) {
      exclude_language <-
        exclude_language |> str_to_lower() |> str_remove_all("\\-|\\ ")
      exclude_language_slug <-
        '%20sourcelangnot:' %>%
        paste0(exclude_language)
    } else {
      exclude_language_slug <-
        ''
    }

    if (dedeup_results) {
      dup_slug <-
        '&dropdup=true'
    } else {
      dup_slug <-
        ''
    }
    output_type <-
      str_to_lower(output) |> str_remove_all("\\ |\\_")
    output_slug <-
      glue::glue("&output={output_type}") |> as.character()

    if (str_to_lower(output) |> str_detect("timeline")) {
      output_type_slug <-
        glue::glue("&outputtype={output_timeline_type}") |> as.character()
    }

    if (!str_to_lower(output) |> str_detect("timeline")) {
      output_type_slug <- ""
    }

    if (use_google_translate) {
      translate_slug <- "&trans=googtrans"
    }

    if (!use_google_translate) {
      translate_slug <- ""
    }


    url <-
      url_base %>%
      paste0(
        term_slug,
        source_language_slug,
        exclude_language_slug,
        source_country_slug,
        exclude_country_slug,
        output_slug,
        output_type_slug,
        last_minute_slug,
        slug_sort,
        max_row_slug,
        dup_slug,
        translate_slug
      )

    tibble(parameter_search_term = as.character(search_term),
           url_gdelt_v1_api = url)
  }

.v1_terms <-
  function(terms = "'Donald Trump'",
           use_exact_term = FALSE,
           output = "artimglist",
           output_timeline_type = "volume",
           domain = NA,
           source_country = NULL,
           exclude_country = NULL,
           source_language = "English",
           exclude_language = NULL,
           last_minutes = NA,
           max_rows = 1000,
           absolute_tone_more_than = NA,
           absolute_tone_less_than = NA,
           tone_less_than = NA,
           tone_more_than = NA,
           dedeup_results = T,
           sort_by = 'date',
           use_google_translate = TRUE) {
    tbl_terms <-
      expand.grid(term = terms, stringsAsFactors = F) |> as_tibble()

    all_data <-
      seq_along(terms) |>
      map_dfr(function(x) {
        term <- tbl_terms[x, ] |> pull()

        if (use_exact_term) {
          term <- glue::glue('"{term}"') |> as.character()
        }

        .v1_term_url(
          term = term,
          domain = NA,
          output = output,
          output_timeline_type = output_timeline_type,
          source_country = source_country,
          exclude_country = exclude_country,
          source_language = source_language,
          exclude_language = exclude_language,
          last_minutes = last_minutes,
          max_rows = max_rows,
          absolute_tone_more_than = absolute_tone_more_than,
          absolute_tone_less_than = absolute_tone_less_than,
          tone_less_than = tone_less_than,
          tone_more_than = tone_more_than,
          dedeup_results = dedeup_results,
          sort_by = sort_by,
          use_google_translate = use_google_translate
        )
      })

    all_data
  }

.v1_domains <-
  function(domains = c(
    "wsj.com",
    "dailymail",
    "forbes.com",
    "nydailynews.com",
    "breitbart.com",
    "nypost.com",
    "gatewaypundit",
    "renewamerica.com",
    "freerepublic.com",
    "theconservativetreehouse.com",
    "gothamgazette.com",
    "stratechery.com",
    "csis.org",
    "snopes.com",
    "tmz.com",
    "bizjournals.com",
    "nytimes.com",
    "washingtonpost.com",
    "dailymail.co.uk",
    "reddit.com"
  ),
  output = "artimglist",
  output_timeline_type = "volume",
  source_country = NULL,
  exclude_country = NULL,
  source_language = "English",
  exclude_language = NULL,
  last_minutes = NA,
  max_rows = 1000,
  absolute_tone_more_than = NA,
  absolute_tone_less_than = NA,
  tone_less_than = NA,
  tone_more_than = NA,
  dedeup_results = T,
  sort_by = 'date',
  use_google_translate = TRUE) {
    tbl_domains <-
      expand.grid(domain = domains, stringsAsFactors = F) |> as_tibble()

    all_data <-
      seq_along(domains) |>
      map_dfr(function(x) {
        domain <- tbl_domains[x, ] |> pull()
        .v1_term_url(
          term = NA,
          output = output,
          output_timeline_type = output_timeline_type,
          domain = domain,
          source_country = source_country,
          exclude_country = exclude_country,
          source_language = source_language,
          exclude_language = exclude_language,
          last_minutes = last_minutes,
          max_rows = max_rows,
          absolute_tone_more_than = absolute_tone_more_than,
          absolute_tone_less_than = absolute_tone_less_than,
          tone_less_than = tone_less_than,
          tone_more_than = tone_more_than,
          dedeup_results = dedeup_results,
          sort_by = sort_by,
          use_google_translate = use_google_translate
        )
      })

    all_data
  }

#' Generate GDELT V1 API Searhc URLs
#'
#' @param terms Vector of terms if not NA
#' @param domains Vector of domains if not NA
#' @param output URL output  \itemize{
#' \item urllist `Generates standard machine-friendly list of URLs optimized for ingesting into automated processing workflows and cross referencing with the GDELT GKG and GDELT Event datasets.`
#' \item artlist `This produces an HTML formatted text-only article list suitable for iframe embedding`
#' \item artimgonlylist `This produces an HTML formatted article list suitable for iframe embedding that includes ONLY those articles that include a featured social sharing image`
#' \item artimglist `This produces an HTML formatted article list suitable for iframe embedding that includes both image and non-image matching articles, displaying images for articles that include them and a blank square for articles that do not include them`
#' \item artimgonlycollage `This produces an HTML formatted article list suitable for iframe embedding that includes ONLY those articles that include a featured social sharing image and displays each article as a simple thumbnail image linked to the article. Thumbnails automatically tile to fill the available display space by rows`
#' \item output=timelinecsv `CSV output suitable for using with your own alternative visualization tools or ingesting into automated workflows. Supports the same “outputtype” options.`
#' }
#' @param output_timeline_type  \itemize{
#' \item volume `Amount of articles`
#' \item tone `Tone of articles`
#' }
#' @param source_country
#' @param exclude_country
#' @param source_language
#' @param exclude_language
#' @param last_minutes
#' @param max_rows
#' @param absolute_tone_more_than
#' @param absolute_tone_less_than
#' @param tone_less_than
#' @param tone_more_than
#' @param dedeup_results
#' @param sort_by
#' @param use_google_translate
#' @param use_exact_term
#'
#' @return
#' @export
#'
#' @examples
ft_v1_api_urls <-
  function(terms = NA,
           domains = NA,
           use_exact_term = FALSE,
           output = "artimglist",
           output_timeline_type = "volume",
           source_country = NULL,
           exclude_country = NULL,
           source_language = "English",
           exclude_language = NULL,
           last_minutes = NA,
           max_rows = 1000,
           absolute_tone_more_than = NA,
           absolute_tone_less_than = NA,
           tone_less_than = NA,
           tone_more_than = NA,
           dedeup_results = T,
           sort_by = 'date',
           use_google_translate = TRUE) {
    all_data <- tibble()

    if (!is.na(terms) |> sum() >= 1) {
      dat <- .v1_terms(
        terms = terms,
        output = output,
        output_timeline_type = output_timeline_type,
        domain = domain,
        source_country = source_country,
        exclude_country = exclude_country,
        source_language = source_language,
        exclude_language = exclude_language,
        last_minutes = last_minutes,
        max_rows = max_rows,
        absolute_tone_more_than = absolute_tone_more_than,
        absolute_tone_less_than = absolute_tone_less_than,
        tone_less_than = tone_less_than,
        tone_more_than = tone_more_than,
        dedeup_results = dedeup_results,
        sort_by = sort_by,
        use_google_translate = use_google_translate,
        use_exact_term = use_exact_term
      )

      all_data <-
        all_data |> bind_rows(dat)
    }

    if (!is.na(domains) |> sum() >= 1) {
      dat <- .v1_domains(
        domains =domains,
        output = output,
        output_timeline_type = output_timeline_type,
        source_country = source_country,
        exclude_country = exclude_country,
        source_language =source_language,
        exclude_language = exclude_language,
        last_minutes = last_minutes,
        max_rows = max_rows,
        absolute_tone_more_than = absolute_tone_more_than,
        absolute_tone_less_than = absolute_tone_less_than,
        tone_less_than = tone_less_than,
        tone_more_than = tone_more_than,
        dedeup_results = dedeup_results,
        sort_by = sort_by,
        use_google_translate = use_google_translate
      )

      all_data <-
        all_data |> bind_rows(dat)
    }

    all_data <- all_data |>
      separate(
        parameter_search_term,
        into = c("parameter", "term"),
        sep = "\\:",
        remove = F
      )

    all_data
  }

# parse -------------------------------------------------------------------


.parse_v1_csv <-
  function(url = "https://api.gdeltproject.org/api/v1/search_ftxtsearch/search_ftxtsearch?query=donald%20trump%20sourcelang:english&output=timelinecsv&outputtype=volume&sort=date&maxrows=1000&dropdup=true&trans=googtrans") {
    value_type <- case_when(url |> str_detect("outputtype=volume") ~ "value_volume",
                            TRUE ~ "value_tone")

    dat <-
      readr::read_csv(url) |>
      setNames(c("datetime_numeric", "datetime_human", "value"))
    dat <-
      dat |>
      mutate(
        datetime_human = datetime_human |> lubridate::mdy_hms(tz = 'UTC') %>%  lubridate::with_tz(Sys.timezone()),
        datetime_numeric = datetime_numeric |> lubridate::ymd_hms() |> lubridate::with_tz(Sys.timezone())
      ) |>
      select(datetime_data = datetime_numeric, value) |>
      filter(!datetime_data |> str_detect("00:00:00")) |>
      rename(UQ(value_type) := value) |>
      mutate(url_gdelt_v1_api = url)


    dat

  }

.parse_v1_url <-
  function(url = "https://api.gdeltproject.org/api/v1/search_ftxtsearch/search_ftxtsearch?query=donald%20trump%20sourcelang:english&output=artimglist&sort=date&maxrows=1000&dropdup=true&trans=googtrans"){
   page <- read_html(url)
   article_nodes <- page |> html_nodes("b")
   titles <-
     article_nodes |> html_text() |> str_squish()
   page_attr <- page |> html_nodes("a") |> html_attr("href")
   urls <- page_attr[!page_attr |> str_detect("^javascript")]
   domain <- urltools::domain(urls) |> str_remove_all("^www.")

   data <- tibble(
     domain_article = domain,
     title_article = titles,
     url_article = urls,
     url_gdelt_v1_api = url
   )

   sources_css <- page %>%
     html_nodes('.sourceinfo')


   sources <-
     sources_css |>
     html_text()

   has_sources <- length(sources) > 0

   if (has_sources) {
     all_text <- page |> as.character()
     json_char <-
       all_text |> str_split("\\writedate") |> flatten_chr()
     dates <-
       json_char[json_char |> str_detect("^\\('source")]

     all_dates <- dates |> map_chr(list(function(z) {
       z |> .extract_date_json() |> as.character()
     }))

     all_date_times <-
       all_dates |> ymd_hms()
     all_dates <-
       as.character(all_dates) |> substr(1, 10) |> ymd()

     data <- data |>
       mutate(source = sources) |>
       separate(source,
                into = c("source", "language_country"),
                sep = "\\ - ") |>
       separate("language_country",
                into = c("language", "country"),
                sep = "\\ / ") |>
       mutate_if(is.character, list(function(x) {
         x |> str_squish() |> str_remove_all("\\(|\\)")
       })) |>
       mutate(
         date_article = all_dates,
         datetime_article = all_date_times,
         .before = "domain_article"
       )
   }

   img_css <- page |> html_nodes(".thumbimg")

   has_image <-
     img_css |> length() > 0

   if (has_image) {
     url_thumbnail <-
       img_css |>
       html_attr("src")


     data <- data |>
       mutate(url_thumbnail,) |>
       mutate(url_thumbnail = case_when(
         url_thumbnail == "" ~ NA_character_,
         TRUE ~ url_thumbnail
       ))
   }

   data
  }

.parse_v1_ft <-
  function(url = "https://api.gdeltproject.org/api/v1/search_ftxtsearch/search_ftxtsearch?query=j.d.%20vance%20sourcelang:english&output=timelinecsv&outputtype=volume&sort=date&maxrows=1000&dropdup=true&trans=googtrans",
           return_message = TRUE) {

    is_csv <- url |> str_detect("timelinecsv")

    if (is_csv) {
      .parse_v1_csv_safe <- purrr::possibly(.parse_v1_csv, tibble())
      data <-
        .parse_v1_csv_safe(url = url)
    }

    if (!is_csv) {
      .parse_v1_url_safe <-
        purrr::possibly(.parse_v1_url, tibble())
      data <- .parse_v1_url_safe(url = url)
    }

    if (return_message) {
      count_matches <- nrow(data)
      term <-
        url |> str_split("\\?") |> flatten_chr() |> str_split("query=") |> flatten_chr() |> last() |> str_split("sourcelang") |> flatten_chr() |> first() |> str_replace_all("%20", " ") |> str_to_upper() |> str_trim()
      glue::glue(
        "Found {crayon::red(count_matches)} results for {crayon::blue(term)} using {crayon::green('GDELT V1 Freetext API')}"
      ) %>% message()
    }

    data

  }

#' Parse Vector of V1 API Urls
#'
#' @param urls Vector of URLs
#' @param return_message If \code{TRUE} returns message
#'
#' @return
#' @export
#'
#' @examples
parse_v1_ft_urls <-
  function(urls, return_message = TRUE) {
    all_data <-
      tibble()

    .parse_v1_ft_safe <-
      possibly(.parse_v1_ft, tibble())

    success <- function(res) {

      data <-
        res$url |>
        .parse_v1_ft_safe(return_message = return_message)

      all_data <<-
        all_data %>%
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

    all_data

  }

#' GDELT V1 Free Text API
#'
#' @param terms Vector of terms if not NA
#' @param domains Vector of domains if not NA
#' @param output URL output  \itemize{
#' \item urllist `Generates standard machine-friendly list of URLs optimized for ingesting into automated processing workflows and cross referencing with the GDELT GKG and GDELT Event datasets.`
#' \item artlist `This produces an HTML formatted text-only article list suitable for iframe embedding`
#' \item artimgonlylist `This produces an HTML formatted article list suitable for iframe embedding that includes ONLY those articles that include a featured social sharing image`
#' \item artimglist `This produces an HTML formatted article list suitable for iframe embedding that includes both image and non-image matching articles, displaying images for articles that include them and a blank square for articles that do not include them`
#' \item artimgonlycollage `This produces an HTML formatted article list suitable for iframe embedding that includes ONLY those articles that include a featured social sharing image and displays each article as a simple thumbnail image linked to the article. Thumbnails automatically tile to fill the available display space by rows`
#' \item output=timelinecsv `CSV output suitable for using with your own alternative visualization tools or ingesting into automated workflows. Supports the same “outputtype” options.`
#' }
#' @param output_timeline_type  \itemize{
#' \item volume `Amount of articles`
#' \item tone `Tone of articles`
#' }
#' @param source_country
#' @param exclude_country
#' @param source_language
#' @param exclude_language
#' @param last_minutes
#' @param max_rows
#' @param absolute_tone_more_than
#' @param absolute_tone_less_than
#' @param tone_less_than
#' @param tone_more_than
#' @param dedeup_results
#' @param sort_by
#' @param use_google_translate
#' @param use_exact_term
#' @param return_message
#'
#' @return
#' @export
#'
#' @examples
#' ft_v1_api(terms = c("J.D. Vance", "Donald Trump"), domains = "dailymail.co.uk")
ft_v1_api <-
  function(terms = NA,
           domains = NA,
           use_exact_term = FALSE,
           output = "artimglist",
           output_timeline_type = "volume",
           source_country = NULL,
           exclude_country = NULL,
           source_language = "English",
           exclude_language = NULL,
           last_minutes = NA,
           max_rows = 1000,
           absolute_tone_more_than = NA,
           absolute_tone_less_than = NA,
           tone_less_than = NA,
           tone_more_than = NA,
           dedeup_results = T,
           sort_by = 'date',
           use_google_translate = TRUE,
           return_message = TRUE) {
    tbl_urls <-
      ft_v1_api_urls(
        terms = terms,
        domains = domains,
        use_exact_term = use_exact_term,
        output = output,
        output_timeline_type = output_timeline_type,
        source_country = source_country,
        exclude_country = exclude_country,
        source_language = source_language,
        exclude_language = exclude_language,
        last_minutes = last_minutes,
        max_rows = max_rows,
        absolute_tone_more_than = absolute_tone_more_than,
        absolute_tone_less_than = absolute_tone_less_than,
        tone_less_than = tone_less_than,
        tone_more_than = tone_more_than,
        dedeup_results = dedeup_results,
        sort_by = sort_by,
        use_google_translate = use_google_translate
      )

    dat <- parse_v1_ft_urls(urls = tbl_urls$url_gdelt_v1_api, return_message = return_message)

    dat <-
      tbl_urls |>
      left_join(dat, by = "url_gdelt_v1_api") |>
      select(-url_gdelt_v1_api, everything())

    dat
  }