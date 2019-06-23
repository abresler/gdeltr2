

# utilities ---------------------------------------------------------------

.remove_full_na_column <-
  function(data) {
    data <-
      data %>%
      dplyr::select(which(colMeans(is.na(.)) < 1))
    return(data)
  }



# trelliscope -------------------------------------------------------------

.check_for_trelliscope_js <-
  function() {
    missing <-
      installed.packages() %>% dplyr::as_tibble() %>%
      dplyr::filter(Package == 'trelliscopejs') %>%
      nrow() == 0
    if (missing) {
      devtools::install_github("hafen/trelliscopejs")
    }
  }


# parse -------------------------------------------------------------------


.parse_source <-
  function(source = "netsdaily.com - writedate('06/02/2016 12:00 UTC'); (English / United States)") {
    source_df <-
      tibble(source) %>%
      tidyr::separate(source,
                      sep = '\\ - ',
                      into = c('source', 'date.language')) %>%
      tidyr::separate(date.language,
                      sep = '\\;',
                      into = c('date', 'language')) %>%
      mutate(
        language = language %>% str_replace('\\(', '') %>% str_replace('\\)', '')  %>% str_replace('\\ /', ','),
        date = date %>% gsub('\\writedate', '', .) %>% str_replace_all('\\(', '') %>% str_replace_all('\\)', '') %>%
          str_replace_all("\\'", '')
      ) %>%
      mutate(dateTime = date %>% lubridate::mdy_hm() %>% lubridate::with_tz(Sys.timezone())) %>%
      mutate(date = dateTime %>% as.Date()) %>%
      tidyr::separate(language,
                      into = c('language', 'country'),
                      sep = '\\, ') %>%
      suppressWarnings() %>%
      suppressMessages()

    source_df <-
      source_df %>%
      mutate_if(is.character,
                str_trim) %>%
      suppressWarnings() %>%
      suppressMessages()

    source_df
  }


# terms -------------------------------------------------------------------
.get_data_ft_api_term <-
  function(term = "'Donald Trump'",
           domain = NA,
           dedeup_results = T,
           restrict_to_usa = F,
           last_minutes = NA,
           max_rows = 1000,
           only_english = T,
           return_image_url = F,
           tone_less_than = NA,
           tone_more_than = NA,
           sort_by = 'date',
           return_message = T) {
    url_base <-
      'https://api.gdeltproject.org/api/v1/search_ftxtsearch/search_ftxtsearch?query='

    if (term %>% is.na()) {
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
    }

    if (term %>% is.na() & !domain %>% is.na()) {
      term_word <-
        domain
    }

    if (!domain %>% is.na()) {
      domain_slug <-
        '%20domain:' %>%
        paste0(domain %>% URLencode())
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
      if (tone_more_than >= 100) {
        stop("Tone can't be over 100")
      }
      tone_more_slug <-
        '%20tonemorethan:' %>%
        paste0(tone_more_than)
    } else {
      tone_more_slug <-
        ''
    }

    if (!tone_less_than %>% is.na()) {
      if (tone_less_than >= 100) {
        stop("Tone can't be under 100")
      }
      tone_less_slug <-
        '%20tonelessthan:' %>%
        paste0(tone_less_than)
    } else {
      tone_less_slug <-
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
        sort_term = c('date', 'relevence', 'tone.ascending', 'tone.descending'),
        sort_slug = c('date', 'rel', 'toneasc', 'tonedesc')
      )

    if (sort_by %in% sort_df$sort_term == F) {
      stop("Sorry sort terms can only be\n" %>%
             paste0(paste0(sort_df$sort_term, collapse = '\n')))
    }

    slug_sort <-
      sort_df %>%
      dplyr::filter(sort_term == sort_by) %>%
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

    if (dedeup_results) {
      dup_slug <-
        '&dropdup=true'
    } else {
      dup_slug <-
        ''
    }

    if (return_image_url) {
      image_slug <-
        '&output=artimglist'
    } else {
      image_slug <-
        '&output=artlist'
    }

    url <-
      url_base %>%
      paste0(term_slug,
             image_slug,
             dup_slug,
             last_minute_slug,
             slug_sort,
             max_row_slug)

    page.has.content <-
      url %>%
      httr::GET()

    page_size_df <-
      page.has.content$headers  %>%
      flatten_df()

    if (!page.has.content$status_code == 200) {
      stop("Seaerch has no data")
    } else {
      if ('content-length' %in% names(page_size_df)) {
        page_size_df <-
          page_size_df %>%
          mutate(`content-length` = `content-length` %>% as.numeric)

        if (page_size_df$`content-length` <= 41) {
          stop("This search has no data")
        }
      }
    }

    page <-
      url %>%
      xml2::read_html()

    if (page %>%
        rvest::html_nodes(xpath = '//b') %>%
        rvest::html_text() %>%
        str_trim() %>% length == 0) {
      stop("This search has no data")
    }

    titleArticle <-
      page %>%
      rvest::html_nodes(xpath = '//b') %>%
      rvest::html_text() %>%
      str_trim()

    if (return_image_url) {
      url.source <-
        page %>%
        rvest::html_nodes(xpath = '//a') %>%
        rvest::html_attr('href') %>%
        .[c(T, F)]
    } else {
      url.source <-
        page %>%
        rvest::html_nodes(xpath = '//a') %>%
        rvest::html_attr('href')
    }

    url.source <-
      url.source %>%
      str_split('\\javascript:window.open') %>%
      flatten_chr %>%
      str_replace_all('\\);', '')

    url.source <-
      url.source[!url.source == '']

    if (!return_image_url) {
      url.source <-
        seq_along(url.source) %>%
        map_chr(function(x) {
          char_url <-
            url.source[x] %>% nchar()

          url.source[x] %>% substr(start = 3, stop = char_url - 1)
        })
    }

    sources <-
      page %>%
      rvest::html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "sourceinfo", " " ))]') %>%
      rvest::html_text()
    wrong_length <-
      url.source %>% length() > sources %>% length()
    if (wrong_length) {
      url.source <-
        url.source[seq_along(sources)]
    }
    .parse_source_safe <-
      purrr::possibly(.parse_source, tibble())
    url_df <-
      tibble(
        term,
        titleArticle,
        urlArticle = url.source,
        dateTimeData = Sys.time(),
        urlSearch = url
      ) %>%
      mutate(idRow = 1:n())

    df_sources <-
      sources %>%
      .parse_source_safe() %>%
      mutate(idRow = 1:n())

    if (df_sources %>% nrow() > 0) {
      url_df <-
        url_df %>%
        left_join(df_sources) %>%
        suppressMessages() %>%
        suppressWarnings()
    }

    url_df <-
      url_df %>%
      dplyr::select(-idRow)


    if (return_image_url) {
      urlThumbnail <-
        page %>%
        rvest::html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "thumbimg", " " ))]') %>%
        rvest::html_attr('src')

      urlThumbnail[urlThumbnail == ''] <-
        NA

      url_df <-
        url_df %>%
        mutate(urlThumbnail)

    }

    if (!domain %>% is.na()) {
      url_df <-
        url_df %>%
        mutate(domainSearch = domain) %>%
        dplyr::select(term, domainSearch, everything())
    }

    if (!tone_more_than %>% is.na()) {
      url_df <-
        url_df %>%
        mutate(tone_more_than)
    }

    if (!tone_less_than %>% is.na()) {
      url_df <-
        url_df  %>%
        mutate(tone_less_than)
    }

    if (only_english) {
      url_df <-
        url_df %>%
        dplyr::filter(language == 'English')
    }

    url_df <-
      url_df %>%
      mutate(domainArticle = urltools::domain(urlArticle) %>% str_replace_all('www.', '')) %>%
      dplyr::select(term:urlArticle, domainArticle, everything()) %>%
      dplyr::rename(
        dateTimeArticle = dateTime,
        dateArticle = date,
        countryArticle = country,
        languageArticle = language,
        sourceArticle = source
      )

    if (term %>% is.na()) {
      url_df <-
        url_df %>%
        dplyr::select(-term)
    }

    if (restrict_to_usa) {
      url_df <-
        url_df %>%
        dplyr::filter(countryArticle == 'United States')
    }

    if (return_message) {
      "You got " %>%
        paste0(url_df %>% nrow(), ' urls for ', term_word, ' at ', Sys.time()) %>%
        cat(fill = T)
    }

    return(url_df)

  }
get_data_ft_v1_api_terms <-
  function(terms = c('"Kevin Durant"', '"Stephen Curry"'),
           visualize_results = TRUE,
           trelliscope_parameters = list(
             path = NULL,
             rows = 1,
             columns = 2,
             id_columns = NULL
           ),
           domain = NA,
           dedeup_results = TRUE,
           restrict_to_usa = F,
           only_english = F,
           return_image_url = TRUE,
           last_minutes = NA,
           max_rows = 1000000,
           sort_by = 'date',
           nest_data = FALSE,
           return_message = TRUE) {
    var_matrix <-
      expand.grid(
        term = terms,
        domain = domain,
        restrict_to_usa = restrict_to_usa,
        only_english = only_english,
        return_image_url = return_image_url,
        last_minutes = last_minutes,
        max_rows = max_rows,
        stringsAsFactors = F
      ) %>%
      as_tibble() %>%
      suppressWarnings()
    .get_data_ft_api_term_safe <-
      purrr::possibly(.get_data_ft_api_term, tibble())
    all_data <-
      1:nrow(var_matrix) %>%
      future_map_dfr(function(x) {
        .get_data_ft_api_term_safe(
          term = var_matrix$term[x],
          domain = var_matrix$domain[x],
          return_image_url = var_matrix$return_image_url[x],
          last_minutes = var_matrix$last_minutes[x],
          max_rows = var_matrix$max_rows[x],
          sort_by = sort_by,
          restrict_to_usa = var_matrix$restrict_to_usa[x],
          only_english = var_matrix$only_english[x],
          dedeup_results = dedeup_results
        )
      }) %>%
      arrange(desc(dateTimeArticle))

    if (visualize_results) {
      .check_for_trelliscope_js()
      title <-
        list("GDELT Term Search for ", Sys.Date()) %>%
        purrr::reduce(paste0)

      df_parameters <- trelliscope_parameters %>% flatten_df()

      if (!df_parameters %>% tibble::has_name('id_columns')) {
        id_columns <-
          c('dateTimeArticle',
            'domainArticle',
            "term",
            "titleArticle",
            "urlArticle")
      } else {
        id_columns <- df_parameters$id_columns
      }

      if (!df_parameters %>% tibble::has_name('rows')) {
        rows <-
          1
      } else {
        rows <- df_parameters$rows
      }

      if (!df_parameters %>% tibble::has_name('columns')) {
        columns <-
          2
      } else {
        columns <- df_parameters$columns
      }

      has_path <-
        df_parameters %>% tibble::has_name('path')

      all_data <-
        all_data %>%
        mutate(
          idArticle = 1:n(),
          panel = trelliscopejs::img_panel(urlThumbnail),
          urlArticle = trelliscopejs::cog_href(urlArticle)
        ) %>%
        select(idArticle, everything()) %>%
        arrange(idArticle) %>%
        mutate_at(c('dateTimeArticle', 'dateArticle'),
                  funs(. %>% as.character()))
      if (has_path) {
        path_loc <-
          df_parameters$path
        viz <-
          all_data %>%
          trelliscopejs::trelliscope(
            name = title,
            nrow = rows,
            ncol = columns,
            path = path_loc,
            state = list(labels = c(id_columns))
          )
        return(viz)
      }
      viz <-
        all_data %>%
        trelliscopejs::trelliscope(
          name = title,
          nrow = rows,
          ncol = columns,
          state = list(labels = c(id_columns))
        )
      return(viz)

    }

    if (nest_data) {
      all_data <-
        all_data %>%
        nest_(nest_cols =
                all_data %>% dplyr::select(-one_of(c('term'))) %>% names,
              key_col = 'data')
    }

    return(all_data)

  }


# domains -----------------------------------------------------------------
get_data_ft_v1_api_domains <-
  function(domains = c('washingtonpost.com', 'nytimes.com'),
           visualize_results = TRUE,
           trelliscope_parameters = list(
             path = NULL,
             rows = 1,
             columns = 2,
             id_columns = NULL
           ),
           use_exact_domains = F,
           term = NA,
           return_image_url = T,
           last_minutes = NA,
           max_rows = 1000,
           sort_by = 'date',
           restrict_to_usa = F,
           dedeup_results = T,
           only_english = F,
           nest_data = F,
           return_message = T) {
    .get_data_ft_api_term_safe <-
      purrr::possibly(.get_data_ft_api_term, tibble())

    var_matrix <-
      expand.grid(
        term = term,
        domain = domains,
        restrict_to_usa = restrict_to_usa,
        only_english = only_english,
        return_image_url = return_image_url,
        last_minutes = last_minutes,
        max_rows = max_rows,
        stringsAsFactors = F
      ) %>%
      as_tibble() %>%
      suppressWarnings()

    all_data <-
      seq_len(var_matrix %>% nrow()) %>%
      future_map_dfr(
        function(x)
          .get_data_ft_api_term_safe(
            term = var_matrix$term[x],
            domain = var_matrix$domain[x],
            return_image_url = var_matrix$return_image_url[x],
            last_minutes = var_matrix$last_minutes[x],
            max_rows = var_matrix$max_rows[x],
            sort_by = sort_by,
            restrict_to_usa = var_matrix$restrict_to_usa[x],
            only_english = var_matrix$only_english[x],
            dedeup_results = dedeup_results
          ) %>%
          suppressWarnings()
      ) %>%
      arrange(desc(dateTimeArticle)) %>%
      suppressWarnings()

    all_data <- all_data %>%
      mutate_if(is.character,
                str_trim)

    if (term %>% is.na()) {
      all_data <-
        all_data %>%
        dplyr::select(-term)
    }

    if (use_exact_domains) {
      all_data <-
        all_data %>%
        dplyr::filter(domainArticle %in% domains)
    }

    if (visualize_results) {
      .check_for_trelliscope_js()

      title <-
        list("GDELT Domain Search at ", Sys.Date()) %>%
        purrr::reduce(paste0)

      df_parameters <- trelliscope_parameters %>% flatten_df()

      if (!df_parameters %>% tibble::has_name('id_columns')) {
        id_columns <-
          c('dateTimeArticle',
            "domainSearch",
            "titleArticle",
            "urlArticle")
      } else {
        id_columns <- df_parameters$id_columns
      }

      if (!df_parameters %>% tibble::has_name('rows')) {
        rows <-
          1
      } else {
        rows <- df_parameters$rows
      }

      if (!df_parameters %>% has_name('columns')) {
        columns <-
          2
      } else {
        columns <- df_parameters$columns
      }

      has_path <-
        df_parameters %>% has_name('path')

      all_data <-
        all_data %>%
        mutate(
          idArticle = 1:n(),
          panel = trelliscopejs::img_panel(urlThumbnail),
          urlArticle = trelliscopejs::cog_href(urlArticle)
        ) %>%
        select(idArticle, everything()) %>%
        arrange(idArticle) %>%
        mutate_at(c('dateArticle', 'dateTimeArticle'),
                  funs(. %>% as.character()))

      if (has_path) {
        path_loc <-
          df_parameters$path
        viz <-
          all_data %>%
          trelliscopejs::trelliscope(
            name = title,
            nrow = rows,
            ncol = columns,
            path = path_loc,
            state = list(labels = c(id_columns))
          )
        return(viz)
      }
      viz <-
        all_data %>%
        trelliscopejs::trelliscope(
          name = title,
          nrow = rows,
          ncol = columns,
          state = list(labels = c(id_columns))
        )


      return(viz)

    }

    if (nest_data) {
      all_data <-
        all_data %>%
        nest_(nest_cols =
                all_data %>% dplyr::select(-one_of(c('domainSearch'))) %>% names,
              key_col = 'data')
    }

    return(all_data)

  }

# sentiment ---------------------------------------------------------------


.get_data_sentiment_ft_api <- function(term = 'Clinton',
                                       domain = NA,
                                       last_minutes = NA,
                                       is_tone = T,
                                       tone_less_than = NA,
                                       tone_more_than = NA,
                                       source_language = NA,
                                       sort_by = 'date',
                                       dedeup_results = T,
                                       return_message = T) {
  url_base <-
    'https://api.gdeltproject.org/api/v1/search_ftxtsearch/search_ftxtsearch?query='

  if (term %>% is.na()) {
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
  }

  if (term %>% is.na() & !domain %>% is.na()) {
    term_word <-
      domain
  }

  if (!domain %>% is.na()) {
    domain_slug <-
      '%20domain:' %>%
      paste0(domain)
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
    if (tone_more_than >= 100) {
      stop("Tone can't be over 100")
    }
    tone_more_slug <-
      '%20tonemorethan:' %>%
      paste0(tone_more_than)
  } else {
    tone_more_slug <-
      ''
  }

  if (!tone_less_than %>% is.na()) {
    if (tone_less_than >= 100) {
      stop("Tone can't be under 100")
    }
    tone_less_slug <-
      '%20tonelessthan:' %>%
      paste0(tone_less_than)
  } else {
    tone_less_slug <-
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
      sort_term = c('date', 'relevence', 'tone.ascending', 'tone.descending'),
      sort_slug = c('date', 'rel', 'toneasc', 'tonedesc')
    )

  if (sort_by %in% sort_df$sort_term == F) {
    stop("Sorry sort terms can only be\n" %>%
           paste0(paste0(sort_df$sort_term, collapse = '\n')))
  }

  slug_sort <-
    sort_df %>%
    dplyr::filter(sort_term == sort_by) %>%
    .$sort_slug

  slug_sort <-
    '&sort=:' %>%
    paste0(slug_sort)



  if (dedeup_results) {
    dup_slug <-
      '&dropdup=true'
  } else {
    dup_slug <-
      ''
  }
  if (is_tone) {
    value_name <-
      'valueTone'
    output_slug <-
      '&output=timelinecsv&outputtype=tone'
  } else {
    output_slug <-
      '&output=timelinecsv&outputtype=volume'
    value_name <-
      'value.volume'
  }

  url <-
    url_base %>%
    paste0(term_slug,
           dup_slug,
           slug_sort,
           output_slug)

  page.has.content <-
    url %>%
    httr::GET()

  page_size_df <-
    page.has.content$headers  %>%
    flatten_df() %>%
    mutate(`content-length` = `content-length` %>% as.numeric)

  if (page_size_df$`content-length` <= 41) {
    stop("This search has no data")
  }

  sentiment_data <-
    url %>%
    readr::read_csv() %>%
    mutate(term, url, dateTimeData = Sys.time()) %>%
    dplyr::select(term, everything()) %>%
    suppressMessages()


  names(sentiment_data)[2:3] <-
    c('dateTime.url', 'dateTime_human.url')

  names(sentiment_data)[4] <-
    value_name

  sentiment_data <-
    sentiment_data  %>%
    mutate(
      dateTimeSentiment = dateTime_human.url %>% lubridate::mdy_hms(tz = 'UTC') %>%  lubridate::with_tz(Sys.timezone()),
      dateSentiment = dateTime_human.url %>% lubridate::mdy_hms(tz = 'UTC') %>% as.Date()
    ) %>%
    dplyr::select(-c(dateTime.url, dateTime_human.url)) %>%
    dplyr::select(term, dateTimeSentiment, dateSentiment, everything()) %>%
    dplyr::rename(urlSearch = url, dateTimeData = dateTimeData)

  if (!domain %>% is.na()) {
    sentiment_data <-
      sentiment_data  %>%
      mutate(domainSearch = domain) %>%
      dplyr::select(term, domainSearch, everything())
  }

  if (!tone_less_than %>% is.na()) {
    sentiment_data <-
      sentiment_data %>%
      mutate(tone_less_than)
  }

  if (!tone_more_than %>% is.na()) {
    sentiment_data <-
      sentiment_data %>%
      mutate(tone_more_than)
  }

  if (term %>% is.na()) {
    sentiment_data <-
      sentiment_data %>%
      dplyr::select(-term)
  }

  if (return_message) {
    "You got " %>%
      paste0(sentiment_data %>% nrow(),
             ' words for ',
             term_word,
             ' at ',
             Sys.time()) %>%
      cat(fill = T)
  }

  return(sentiment_data)

}
#' Get V1 sentiment from specified domains
#'
#' @param domains vector of domain names
#' @param visualization if `TRUE` returns a ggplot2 visualization
#' @param term specific term
#' @param last_minutes how long
#' @param is_tone use tone
#' @param tone_less_than tone minimum
#' @param tone_more_than tone maximum
#' @param source_language vector search language
#' @param sort_by sort by
#' @param dedeup_results if `TRUE` dedupes results
#' @param nest_data if `TRUE` returns nested data frame
#' @param return_message if `TRUE` returns a messaage
#'
#' @return a `tibble` or a ggplot visualiation
#' @export
#' @import dplyr jsonlite ggthemes ggplot2
#' @examples
#' get_data_sentiment_ft_api_domains(domains = c("foxnews.com", "cnn.com", "washingtonpost.com"), term = "Donald Trump", visualization = T)
#'
get_data_sentiment_ft_api_domains <-
  function(domains = c('nytimes.com', 'washingtonpost.com'),
           visualization = TRUE,
           term = NA,
           last_minutes = NA,
           is_tone = T,
           tone_less_than = NA,
           tone_more_than = NA,
           source_language = NA,
           sort_by = 'date',
           dedeup_results = T,
           nest_data = F,
           return_message = T) {
    get_data_sentiment_ft_api_safe <-
      purrr::possibly(get_data_sentiment_ft_api, tibble())

    var_matrix <-
      expand.grid(
        term = term,
        domain = domains,
        is_tone = is_tone,
        last_minutes = last_minutes,
        tone_less_than = tone_less_than,
        tone_more_than = tone_more_than,
        source_language = source_language,
        stringsAsFactors = F
      ) %>%
      as_tibble %>%
      suppressWarnings()

    all_data <-
      seq_len(var_matrix %>% nrow()) %>%
      future_map_dfr(
        function(x)
          get_data_sentiment_ft_api_safe(
            term = var_matrix$term[x],
            domain = var_matrix$domain[x],
            last_minutes = var_matrix$last_minutes[x],
            is_tone = var_matrix$is_tone[x],
            tone_less_than = var_matrix$tone_less_than[x],
            tone_more_than = var_matrix$tone_more_than[x],
            source_language = var_matrix$source_language[x],
            sort_by = sort_by,
            dedeup_results = dedeup_results,
            return_message = return_message
          ) %>%
          suppressMessages
      )
    is_visalization <-
      !purrr::is_null(visualization)
    if (is_visalization) {
      viz <-
        all_data %>%
        ggplot(aes(x = dateTimeSentiment, y = valueTone)) +
        geom_line(aes(color = domainSearch)) +
        scale_y_continuous(limits = c(-7, 7)) +
        facet_wrap( ~ domainSearch, scales = "free") +
        hrbrthemes::theme_ipsum(grid = "XY") +
        scale_x_datetime(expand = c(0, 0)) +
        theme(legend.position = "none") +
        labs(
          x = NULL,
          y = "Tone",
          title = list("GDELT V1 API Domain Sentiment Analysis as of ", Sys.Date()) %>% purrr::reduce(paste0),
          caption = "Data from GDELT via gdeltr2"
        )

      if (domains %>% length() <= 8) {
        viz <-
          viz +
          ggthemes::scale_color_colorblind(guide = guide_legend(title = ""))
      } else {
        manual_colors <-
          RColorBrewer::brewer.pal(12, "Paired")
        over_12 <-
          domains %>% length() > 12
        if (over_12) {
          more_colors <-
            domains %>% length() - 12
          add_colors <-
            grDevices::colors() %>% sample(more_colors)
          manual_colors <-
            c(manual_colors, add_colors)
        }
        viz <-
          viz +
          scale_color_manual(values = manual_colors, guide = guide_legend(title = ""))
      }
      return(viz)
    }

    if (nest_data) {
      all_data <-
        all_data %>%
        nest_(nest_cols =
                all_data %>% dplyr::select(-one_of(c('domainSearch'))) %>% names,
              key_col = 'data')
    }

    return(all_data)

  }
#' Get V1 API sentiment for terms
#'
#' @param terms vector of terms
#' @param visualization if `TRUE` returns a visualization
#' @param domain vector of domain
#' @param last_minutes how long
#' @param is_tone use tone
#' @param tone_less_than tone minimum
#' @param tone_more_than tone maximum
#' @param source_language vector search language
#' @param sort_by sort by
#' @param dedeup_results if `TRUE` dedupes results
#' @param nest_data if `TRUE` returns nested data frame
#' @param return_message if `TRUE` returns a messaage
#'
#' @return a `tibble` or a ggplot visualiation
#' @export
#' @import dplyr jsonlite ggthemes ggplot2
#' @examples
#' get_data_sentiment_ft_api_terms(terms = c("Zika", "Brooklyn Nets"), visualization = T)

get_data_sentiment_ft_api_terms <-
  function(terms = c("Zika", '"Golden State Warriors"'),
           visualization = NULL,
           domain = NA,
           last_minutes = NA,
           is_tone = T,
           tone_less_than = NA,
           tone_more_than = NA,
           source_language = NA,
           sort_by = 'date',
           dedeup_results = T,
           nest_data = F,
           return_message = T) {
    .get_data_sentiment_ft_api_safe <-
      purrr::possibly(.get_data_sentiment_ft_api, tibble())

    var_matrix <-
      expand.grid(
        term = terms,
        domain = domain,
        is_tone = is_tone,
        last_minutes = last_minutes,
        tone_less_than = tone_less_than,
        tone_more_than = tone_more_than,
        source_language = source_language,
        stringsAsFactors = F
      ) %>%
      as_tibble %>%
      suppressWarnings()


    all_data <-
      seq_len(var_matrix %>% nrow()) %>%
      future_map_dfr(
        function(x)
          .get_data_sentiment_ft_api_safe(
            term = var_matrix$term[x],
            domain = var_matrix$domain[x],
            last_minutes = var_matrix$last_minutes[x],
            is_tone = var_matrix$is_tone[x],
            tone_less_than = var_matrix$tone_less_than[x],
            tone_more_than = var_matrix$tone_more_than[x],
            source_language = var_matrix$source_language[x],
            sort_by = sort_by,
            dedeup_results = dedeup_results,
            return_message = return_message
          ) %>%
          suppressMessages
      ) %>%
      suppressWarnings()

    is_visalization <-
      !purrr::is_null(visualization)
    if (is_visalization) {
      viz <-
        all_data %>%
        ggplot(aes(x = dateTimeSentiment, y = valueTone)) +
        geom_line(aes(color = term)) +
        scale_y_continuous(limits = c(-7, 7)) +
        facet_wrap( ~ term, scales = "free") +
        hrbrthemes::theme_ipsum(grid = "XY") +
        scale_x_datetime(expand = c(0, 0)) +
        theme(legend.position = "none") +
        labs(
          x = NULL,
          y = "Tone",
          title = list("GDELT V1 API Term Sentiment Analysis as of ", Sys.Date()) %>% purrr::reduce(paste0),
          caption = "Data from GDELT via gdeltr2"
        )

      if (terms %>% length() <= 8) {
        viz <-
          viz +
          ggthemes::scale_color_colorblind(guide = guide_legend(title = ""))
      } else {
        manual_colors <-
          RColorBrewer::brewer.pal(12, "Paired")
        over_12 <-
          terms %>% length() > 12
        if (over_12) {
          more_colors <-
            terms %>% length() - 12
          add_colors <-
            grDevices::colors() %>% sample(more_colors)
          manual_colors <-
            c(manual_colors, add_colors)
        }
        viz <-
          viz +
          scale_color_manual(values = manual_colors, guide = guide_legend(title = ""))
      }

      return(viz)
    }


    if (nest_data) {
      all_data <-
        all_data %>%
        nest_(nest_cols =
                all_data %>% dplyr::select(-one_of(c('term'))) %>% names,
              key_col = 'data')
    }

    return(all_data)

  }



# instability -------------------------------------------------------------

#' Code book for instability locations
#'
#' @return a \code{tibble}
#' @export
#' @import dplyr tidyr
#' @importFrom readr read_tsv
#' @importFrom purrr set_names
#' @examples
get_codes_stability_locations <-
  function() {
    country_df <-
      'http://data.gdeltproject.org/blog/stability-dashboard-api/GEOLOOKUP-COUNTRY.TXT' %>%
      read_tsv(col_names = F) %>%
      set_names(c('idLocation', 'nameLocation')) %>%
      separate(nameLocation, into = c('NL1', 'NL2'), sep = '\\, ') %>%
      mutate(nameLocation = ifelse(NL2 %>% is.na, NL1, paste(NL2, NL1))) %>%
      dplyr::select(-c(NL2, NL1)) %>%
      suppressWarnings() %>%
      suppressMessages() %>%
      mutate(isCountry = T,
             codeCountry = idLocation)

    place_df <-
      'http://data.gdeltproject.org/blog/stability-dashboard-api/GEOLOOKUP-ADM1.TXT' %>%
      read_tsv(col_names = F) %>%
      set_names(c('idLocation', 'nameLocation')) %>%
      mutate(
        nameLocation = nameLocation %>% str_to_title(),
        nameLocation =  nameLocation %>% str_replace_all("Bahamas, The General, Bahamas, The", "The Bahamas") %>% str_replace_all(
          "Etorofu, Habomai, Kunashiri And Shikotan Islands General, Etorofu, Habomai, Kunashiri And Shikotan Islands",
          "Kuril Islands"
        ) %>% str_replace_all('Serbia And Montenegro General,', 'Serbia and Montenegro') %>% str_replace_all(" Of ", ' of '),
        isCountry = F,
        codeCountry = idLocation %>% substr(1, 2),
        codeLocation = idLocation %>% substr(3, 4),
        idADM1 = idLocation
      ) %>%
      separate(
        nameLocation,
        into = c('placeLocation', 'countryLocation'),
        sep = '\\, ',
        remove = F
      ) %>%
      suppressWarnings() %>%
      suppressMessages()

    location_df <-
      place_df %>%
      bind_rows(country_df) %>%
      arrange(idLocation) %>%
      mutate(countryLocation = if_else(countryLocation %>% is.na, nameLocation, countryLocation))
    gc()
    return(location_df)

  }



.get_data_location_instability_api <-
  function(location_id = 'US',
           variable_name = 'instability',
           day_moving_average = NA,
           time_period = 'daily',
           use_multi_locations = F,
           return_wide = T,
           return_message = T) {
    if (!'location_codes' %>% exists) {
      location_codes <-
        get_codes_stability_locations()

      assign(x = 'location_codes',
             eval(location_codes),
             env = .GlobalEnv)
    }

    if (!location_id %in% location_codes$idLocation) {
      stop("Sorry " %>% paste0(location_id, ' is not a valid location'))
    }

    var_df <-
      tibble(
        idVar = c('instability', 'conflict', 'protest', 'tone', 'artvolnorm'),
        nameVar = c(
          'instability',
          'conflict',
          'protest',
          'tone',
          'relative mentions'
        )
      )

    if (!variable_name %>% str_to_lower %in% var_df$idVar) {
      stop("Variable names can only be:\n" %>%
             paste0(paste0(var_df$nameVar, collapse = '\n')))
    }

    if (!time_period %>% str_to_lower() %in% c('daily',
                                               '15min',
                                               '15minutes',
                                               '15 minutes',
                                               '15 minute periods')) {
      stop("Time period can only be daily or 15 minute periods")
    }

    id_var <-
      var_df %>% dplyr::filter(nameVar == variable_name) %>% .$idVar

    var_slug <-
      '&VAR=' %>% paste0(id_var)

    if (time_period == "daily") {
      period_slug <-
        "&TIMERES=day"
      period_name <-
        'dateData'
    } else {
      period_slug <-
        "&TIMERES=15min"
      period_name <-
        'datetimeData'
    }

    output_slug <-
      "&OUTPUT=csv"

    if (use_multi_locations  &
        (!location_id %>% nchar == 2)) {
      use_multi_locations <-
        F
      "You entered detailed location which cannot have multi locations\nthis analysis will still run though" %>%
        message
    }

    if (use_multi_locations) {
      mode_slug <-
        "&MODE=multi"
    } else {
      mode_slug <-
        ''
    }

    if (day_moving_average %>% is.na()) {
      ma_slug <-
        ''
    } else {
      if (!day_moving_average %in% 1:5) {
        stop("Sorry moving averages can only be 1 through 5 day")
      } else {
        ma_slug <-
          "&SMOOTH=" %>% paste0(day_moving_average)
      }

    }

    base_url <-
      'http://api.gdeltproject.org/api/v1/dash_stabilitytimeline/dash_stabilitytimeline?LOC='

    data_url <-
      base_url %>% paste0(location_id,
                          var_slug,
                          output_slug,
                          period_slug,
                          ma_slug,
                          mode_slug)

    if (!use_multi_locations) {
      data <-
        data_url %>%
        read_csv() %>%
        suppressMessages() %>%
        set_names(c(period_name, 'value')) %>%
        mutate(
          item = id_var,
          idLocation = location_id,
          typePeriod = time_period,
          dayMovingAverage = day_moving_average
        ) %>%
        dplyr::select(1,
                      idLocation,
                      typePeriod,
                      dayMovingAverage,
                      item,
                      value)
    } else {
      data <-
        data_url %>%
        read_csv() %>%
        suppressMessages() %>%
        gather(idLocation, value, -Date) %>%
        mutate(
          item = id_var,
          typePeriod = time_period,
          dayMovingAverage = day_moving_average
        ) %>%
        dplyr::select(Date, idLocation, typePeriod, dayMovingAverage,
                      item, value) %>%
        suppressMessages()

      names(data)[1] <-
        period_name
    }

    data <-
      data %>%
      left_join(
        location_codes %>%
          dplyr::select(
            idLocation,
            codeCountry,
            codeLocation,
            nameLocation,
            countryLocation
          )
      ) %>%
      dplyr::select(
        1:5,
        nameLocation,
        countryLocation,
        codeCountry,
        codeLocation,
        nameLocation,
        countryLocation,
        everything()
      ) %>%
      suppressMessages()

    if (use_multi_locations) {
      data <-
        data %>%
        dplyr::filter(!codeLocation == "00")
    }

    if (period_name == "dateData") {
      data <-
        data %>%
        mutate(dateData = dateData %>% lubridate::ymd())
    } else {
      data <-
        data %>%
        mutate(datetimeData = datetimeData %>% ymd_hms())
    }

    data <-
      data %>%
      .remove_full_na_column()

    if (return_wide) {
      data <-
        data %>%
        spread(item, value)
    }

    if (return_message) {
      locations <-
        data$nameLocation %>% unique() %>% paste0(collapse = '\n')
      min_data <- data %>% pull(dateData) %>% min()
      max_data <-   data %>% pull(dateData) %>% max()


      glue::glue("You got GDELT {variable_name} during {min_data}-{max_data} for {locations}") %>% cat(fill = T)
    }

    return(data)

  }


#' Returns instability data for given locations
#'
#' @param location_ids Specify the location IDs
#' @param random_locations Number of random location IDs to add
#' @param variable_names Specify variables they can include: \itemize{
#' \item \code{instability}
#' \item \code{conflict}
#' \item \code{protest}
#' \item \code{tone}
#' \item \code{artvolnorm}: Relative mentions
#' }
#' @param days_moving_average Specify day moving average, NA is unsmoothed
#' @param time_periods Specified time period
#' \itemize{
#' \item \code{daily}
#' \item \code{15min}
#' }
#' @param use_multi_locations if \code{TRUE} returns all cities in a select country
#' @param return_wide if \code{TRUE} returns a wide data frame
#' @param nest_data if \code{TRUE} returns a nested data frame
#' @param return_message if \code{TRUE} returns a location
#' @param visualize  if `TRUE` returns a ggplot2 visualization
#'
#' @return if \code{visualize} a ggplot visualization else a \code{tibble}
#' @export
#' @import tidyr stringr rvest purrr readr dplyr ggplot2 ggthemes
#' @importFrom magrittr extract2
#' @importFrom grDevices colors
#' @examples
#' get_data_locations_instability_api(location_ids = c('US', 'IS', "TU"), random_locations = NULL, variable_names = c('instability', 'conflict', 'tone', 'protest', 'artvolnorm'), visualization = T, days_moving_average = NA, time_periods = 'daily', use_multi_locations = F, return_wide = T, nest_data = F, return_message = T)
get_data_locations_instability_api <-
  function(location_ids = c('US', 'IS', "TU"),
           random_locations = NULL,
           variable_names = c('instability', 'conflict', 'tone', 'protest', 'artvolnorm'),
           days_moving_average = NA,
           time_periods = 'daily',
           use_multi_locations = F,
           return_wide = T,
           visualize = F,
           nest_data = F,
           return_message = T) {
    .get_data_location_instability_api_safe <-
      purrr::possibly(.get_data_location_instability_api, tibble())

    if (location_ids %>% purrr::is_null()) {
      location_ids <-
        c()
    }

    if (!random_locations %>% purrr::is_null()) {
      random_locs <-
        get_codes_stability_locations() %>%
        mutate(ncharLoc = nchar(idLocation)) %>%
        filter(ncharLoc == 2) %>%
        .$idLocation %>% sample(random_locations)
      location_ids <-
        c(location_ids, random_locs)
    }

    var_matrix <-
      expand.grid(
        id_location = location_ids,
        variable_name = variable_names,
        day_moving_average = days_moving_average,
        time_period = time_periods,
        use_multi_locations = use_multi_locations,
        stringsAsFactors = F
      ) %>%
      as_tibble

    all_data <-
      seq_len(var_matrix %>% nrow()) %>%
      future_map_dfr((function(x) {
        .get_data_location_instability_api_safe(
          location_id = var_matrix$id_location[x],
          variable_name = var_matrix$variable_name[x],
          day_moving_average = var_matrix$day_moving_average[x],
          time_period = var_matrix$time_period[x],
          use_multi_locations = var_matrix$use_multi_locations[x],
          return_wide = F,
          return_message = return_message
        ) %>%
          suppressWarnings()
      }))

    if (visualize) {
      viz <-
        all_data %>%
        arrange(dateData, value) %>%
        ggplot(aes(x = dateData, y = value)) +
        geom_line(aes(color = item)) +
        scale_y_continuous(limits = c(-7, 7)) +
        facet_wrap( ~ nameLocation, scales = "free") +
        hrbrthemes::theme_ipsum(grid = "XY") +
        scale_x_date(expand = c(0, 0)) +
        theme(legend.position = "bottom") +
        labs(
          x = NULL,
          y = "Tone",
          title = list("GDELT Country Stability ", Sys.Date()) %>% purrr::reduce(paste0),
          caption = "Data from GDELT via gdeltr2"
        )
      return(viz)
    }

    all_data
  }


# Trending terms ----------------------------------------------------------



#' Gets most recent terms
#'
#' @param sort_data
#'
#' @return
#' @export
#' @import tidyr stringr rvest purrr dplyr
#' @importFrom readr read_csv
#' @importFrom magrittr extract2
#' @examples
#' get_data_ft_trending_terms()
get_data_ft_trending_terms <-
  function(sort_data = T) {
    data <-
      'http://live.gdeltproject.org/autocomplete_last15.csv' %>%
      read_csv() %>%
      set_names('nameTerm') %>%
      suppressMessages()

    data <-
      data %>%
      mutate(
        isGDELTTag = nameTerm %>%  grepl("[[:upper:]]+$|\\_", .),
        datetimeData = Sys.time()
      )

    if (sort_data) {
      data <-
        data %>%
        arrange(nameTerm)
    }

    return(data)
  }


# new television api ------------------------------------------------------

# http://television.gdeltproject.org/cgi-bin/iatv_ftxtsearch/iatv_ftxtsearch?primary_keyword=Amy+Shumer&context_keywords=&filter_network=CNN&filter_timespan=ALL&filter_timespan_custom_start=&filter_timespan_custom_end=&filter_displayas=PERCENT&filter_combineseparate=SEPARATE&filter_outputtype=JSON
# http://television.gdeltproject.org/cgi-bin/iatv_ftxtsearch/iatv_ftxtsearch?primary_keyword=campaign&context_keywords=&filter_network=CNN&filter_timespan=ALL&filter_displayas=PERCENT&filter_combineseparate=SEPARATE&filter_outputtype=DISPLAY



# GEO V2 ------------------------------------------------------------------

.read_codebook <-
  function(url = "http://data.gdeltproject.org/api/v2/guides/LOOKUP-GKGTHEMES.TXT" ,
           id_name = 'idGKGTheme') {
    data <-
      url %>%
      read_tsv(col_names = FALSE) %>%
      purrr::set_names(c(id_name, 'value')) %>%
      suppressWarnings() %>%
      suppressMessages()
    data
  }

#' GDELT Geo API code book
#'
#' @param code_book selected codebook \itemize{
#' \item GKG: GKG themes
#' \item ADM: ADM codes
#' \item imagetags: Image tags
#' \item imageweb: imageweb tags
#' \item languages: language of text
#' \item countries: country
#' }
#'
#' @return
#' @export
#' @import readr dplyr stringr purrr tibble readr
#' @examples
#' get_gdelt_codebook_geo_api(code_book = 'imagetags')
get_gdelt_codebook_geo_api <-
  function(code_book = 'adm') {
    df_codebooks <-
      tibble(
        nameCodeBook = c(
          'gkg',
          'adm',
          'imagetags',
          'imageweb',
          'languages',
          'countries'
        ),
        nameId = c(
          'idGKGTheme',
          'idADM',
          'idImageTag',
          'idImageWeb',
          'idLanguage',
          'idCountry'
        ),
        urlCodeBook = c(
          "http://data.gdeltproject.org/api/v2/guides/LOOKUP-GKGTHEMES.TXT",
          'http://data.gdeltproject.org/api/v2/guides/LOOKUP-ADM1S.TXT',
          'http://data.gdeltproject.org/api/v2/guides/LOOKUP-IMAGETAGS.TXT',
          'http://data.gdeltproject.org/api/v2/guides/LOOKUP-IMAGEWEBTAGS.TXT',
          'http://data.gdeltproject.org/api/v2/guides/LOOKUP-LANGUAGES.TXT',
          'http://data.gdeltproject.org/api/v2/guides/LOOKUP-COUNTRIES.TXT'
        )
      )
    code_book <- code_book %>% str_to_lower()
    codes <- df_codebooks$nameCodeBook %>% str_to_lower()
    if (!code_book %in% (codes)) {
      stop(str_c('Codebooks can only be:\n', str_c(codes, collapse = '\n')))
    }

    df_section <-
      df_codebooks %>%
      filter(nameCodeBook == code_book)

    data <-
      df_section$urlCodeBook %>%
      .read_codebook(id_name = df_section$nameId) %>%
      mutate(nameCodebook = code_book %>% str_to_upper()) %>%
      select(nameCodebook, everything())
    data
  }


.generate_slug <-
  function(parameter = 'geore',
           sep = '=',
           value_options = 0:2,
           value = NULL) {
    if (value %>% is_null()) {
      return('')
    }
    slug <-
      str_c('&', parameter, sep, value, sep = '')

    if (!value %>% str_to_lower() %in% value_options) {
      return('')
    }
    slug
  }



.parse_query <-
  function(query_parameters) {
    df_query_params <-
      tibble(
        nameFunction = c(
          'term',
          'domain',
          'image_face_tone',
          'image_num_faces',
          'image_ocr',
          'image_tag',
          'image_web_count',
          'image_web_tag',
          'location_name',
          'location_adm1',
          'location_country',
          'near',
          'source_country',
          'source_language',
          'gkg_theme',
          'tone',
          'tone_absolute_value'
        ) ,
        nameSlug = c(
          '',
          'Domain',
          'ImageFaceTone' ,
          'ImageNumFaces',
          'ImageOCRMeta',
          'ImageTag',
          'ImageWebCount',
          'ImageWebTag',
          'Location',
          'LocationADM1',
          'LocationCC',
          "Near",
          'SourceCountry',
          'SourceLang',
          'Theme',
          "Tone",
          'ToneAbs'
        ) %>% stringr::str_to_lower(),
        typeSep = c(
          '',
          ':',
          '',
          '',
          ':',
          ':',
          '',
          ':',
          ':',
          ':',
          ":",
          ":",
          ':',
          ':',
          ":",
          '',
          ''
        )
      )

    df_call <-
      seq_along(query_parameters) %>%
      future_map_dfr(function(x) {
        function_param <-
          names(query_parameters[x])
        value <-
          query_parameters[x][[1]]

        if (value %>% purrr::is_null()) {
          return(tibble())
        }

        if (value %>% is.na()) {
          value <- NULL
        }

        if (value %>% purrr::is_null()) {
          return(tibble())
        }
        value <- value %>% curl::curl_escape()
        has_or <-
          value %>% length() > 1
        is_quoted <-
          function_param %in% c('image_tag' , 'image_web_tag', 'image_ocr')
        if (is_quoted) {
          value <-
            str_c('%22', value, '%22')
        }

        param <-
          df_query_params %>%
          filter(nameFunction == function_param) %>%
          unite(param, nameSlug, typeSep, sep = '') %>%
          .$param

        if (has_or) {
          value

          values <- glue::glue("{param}{value}")
          values <- values %>% str_c(collapse = "%20OR%20")


          value <-
            str_c(value %>% str_c(collapse = "%20OR%20"), ")")
          param <-
            str_c('(', param, sep = '')
          df_call <-
            tibble(nameCall = glue::glue("({values})"))
          return(df_call)
        }

        tibble(nameCall = str_c(param, value, collapse = ''))

      })
    df_call$nameCall
  }

#' Generate a GDELT GEO V2 API query
#'
#' This function will generate a V2 GEO api query and either open
#' the URL in the browser for exploration or download the GEOJSON data
#' for users to explore in R
#'
#' @param query_parameters list of query parameters \itemize{
#'  \item term: search team
#'  \item domain: web domain
#'  \item image_face_tone: image face tone
#'  \item image_num_face: number of faces
#'  \item image_ocr: OCR to find
#'  \item image_tag: image tagcode
#'  \item image_web_tag: image web code
#'  \item image_web_count: image count
#'  \item location_name: name of the location
#'  \item location_adm1: adm code
#'  \item location_country: country
#'  \item near: specified distances
#'  \item source_country: source country
#'  \item source_language: source language
#'  \item gkg_theme: GKG theme
#'  \item tone: numeric tone
#'  \item tone_absolute_value:
#' }
#' @param mode visualization mode \itemize{
#' \item PointData
#' \item ImagePointData
#' \item PointHeatmap
#' \item ImagePointheatmap
#' \item PointAnimation
#' \item ImagePointAnimation
#' \item Country
#' \item ImageCountry
#' \item SourceCountry
#' \item ImageSourceCountry
#' \item ADM1
#' \item ImageADM1
#' }
#' @param format format of the output \itemize{
#' \item html: HTML
#' \item ImageHTML: image html
#' \item GeoJSON: geoJSON
#' \item ImageGeoJSON: geoJSON with images
#' \item Imagehtmlshow: shows overlay-ed pictures on map
#' }
#' @param timespan time span 15 to 1440 minutes
#' @param max_points maximum number of points
#' @param geore geore level \itemize{
#' \item 0: All locations
#' \item 1: Excludes country mentions
#' \item 2: Only landmarks
#' }
#' @param sort how to sort the data \itemize{
#' \item date: by date
#' \item toneDesc: Tone descending
#' \item toneAsc: Tone ascending
#' }
#' @param browse_url if \code{TRUE} open url in browser
#' @references \href{http://blog.gdeltproject.org/gdelt-geo-2-0-api-debuts/}{GDELT GEO API}
#'
#' @return
#' @export
#' @import purrr glue readr stringr dplyr curl tibble tidyr httr
#' @examples
#' generate_geo_query(query_parameters = list(term = "Brooklyn Nets"))
generate_geo_query <-
  function(query_parameters = list(
    term = NULL,
    domain = NULL,
    gkg_theme = NULL,
    image_face_tone = NULL,
    image_num_faces = NULL,
    image_ocr = NULL,
    image_tag = NULL,
    image_web_tag = NULL,
    image_web_count = NULL,
    location_name = NULL,
    location_adm1 = NULL,
    location_country = NULL,
    near = NULL,
    source_country = NULL,
    source_language = NULL,
    tone = NULL,
    tone_absolute_value = NULL
  ),
  mode = 'adm1',
  format = 'ImageHTML',
  timespan = NULL,
  max_points = NULL,
  geore = NULL,
  sort =  NULL,
  browse_url = TRUE) {
    base <-
      "https://api.gdeltproject.org/api/v2/geo/geo?query="

    query_slug <-
      .parse_query(query_parameters = query_parameters)

    if (query_slug %>% length() > 1) {
      qs <- query_slug %>% str_c(collapse  = "%20OR%20")
      query_slug <- glue::glue("({qs})")
    }

    mode_options <-
      c(
        'PointData' ,
        'ImagePointData',
        'PointHeatmap',
        'ImagePointheatmap',
        'PointAnimation',
        'ImagePointAnimation',
        'Country',
        'ImageCountry',
        'SourceCountry',
        'ImageSourceCountry',
        'ADM1',
        'ImageADM1'
      ) %>% str_to_lower()

    mode <-
      mode %>% str_to_lower()

    mode_slug <-
      .generate_slug(
        parameter = 'mode',
        sep = '=',
        value = mode,
        value_options = mode_options
      )

    if (!mode_slug == '' &&
        mode %in% c("pointheatmap",
                    "imagepointheatmap",
                    "pointanimation",
                    "imagepointanimation")) {
      format <-
        'geojson'
    }

    format_options <-
      c('imagehtmlshow',
        'html',
        'ImageHTML',
        'GeoJSON',
        'ImageGeoJSON') %>% str_to_lower()
    format_slug <-
      .generate_slug(
        parameter = 'format',
        sep = '=',
        value = format %>% str_to_lower(),
        value_options = format_options
      )

    timespan_options <- 15:1440
    timespan_slug <-
      .generate_slug(
        parameter = 'timespan',
        sep = '=',
        value = timespan,
        value_options = timespan_options
      )

    geore_options <- 0:2
    geore_slug <-
      .generate_slug(
        parameter = 'geore',
        sep = '=',
        value = geore,
        value_options = geore_options
      )
    sort_options <-
      c('date', 'ToneDesc', 'ToneAsc') %>% str_to_lower()

    sort_slug <-
      .generate_slug(
        parameter = 'sort',
        sep = '=',
        value = sort,
        value_options = sort_options
      )

    maxpoint_slug = ''

    url_api <-
      glue(
        "{base}",
        "{query_slug}",
        '{mode_slug}',
        "{format_slug}",
        "{timespan_slug}",
        '{maxpoint_slug}',
        "{geore_slug}",
        "{sort_slug}"
      ) %>%
      str_to_lower()

    if (browse_url) {
      url_api %>% httr::BROWSE()
    }
    data <-
      tibble(urlAPI = url_api)
    is_geo <-
      format %>% str_detect("json")
    if (is_geo) {
      df_geo <-
        url_api %>%
        jsonlite::fromJSON(simplifyDataFrame = TRUE, flatten = TRUE) %>%
        .$features %>%
        as_tibble()
      df_geo <-
        df_geo %>%
        mutate(idRow = 1:n())
      df_lat_lon <-
        seq_along(df_geo$geometry.coordinates) %>%
        future_map_dfr(function(x) {
          tibble(
            item = c('longitudeArticle', 'latitudeArticle'),
            value = df_geo$geometry.coordinates[[x]]
          ) %>%
            spread(item, value) %>%
            mutate(idRow = x) %>%
            dplyr::select(idRow, everything())
        })

      df_geo <-
        df_geo %>%
        select(-c(geometry.coordinates)) %>%
        purrr::set_names(
          c(
            'typeMap',
            'nameProperty',
            'countProperty',
            'propertiesShareImage',
            'htmlArticle',
            'typeGeometry',
            'idRow'
          )
        ) %>%
        left_join(df_lat_lon) %>%
        suppressMessages()
      data <-
        data %>%
        mutate(dataGeoJSON = list(df_geo))
    }

    data
  }



# FT Viz ------------------------------------------------------------------

# utils -------------------------------------------------------------------

.get_trelliscope_id_columns <-
  function(data,
           id_columns = list(
             is_except = TRUE,
             columns = c("idPanel", "data", "plot"),
             regex = NULL
           )) {
    column_id_names <- c()
    has_columns <-
      id_columns %>% names() %>% str_detect("columns") %>% sum(na.rm = TRUE) > 0
    if (has_columns) {
      columns <- id_columns$columns
      is_except <- id_columns$is_except
      if (is_except) {
        name_columns <- data %>% dplyr::select(-one_of(columns)) %>% names()
      } else {
        name_columns <-
          data %>% dplyr::select(one_of(columns)) %>% names()
      }
      column_id_names <-
        column_id_names %>% append(name_columns)
    }
    has_regex <-
      id_columns[id_columns %>% names() %>% str_detect("regex")] %>% flatten_dbl() %>% length() > 0
    if (has_regex) {
      regex <- id_columns$regex
      regex_columns <-
        data %>% dplyr::select(dplyr::matches(regex)) %>% names()
      column_id_names <-
        column_id_names %>% append(regex_columns)
    }
    column_id_names
  }

.get_mode_types <- function() {
  tribble(
    ~ modeSearch,
    ~ packageVisualization,
    ~ typeVisualization,
    ~ hasTooltip,
    ~ nameXAxis,
    ~ nameYAxis,
    ~ typeXAxis,
    "ArtList",
    "image",
    "trelliscope",
    FALSE,
    NA,
    NA,
    NA,
    "ImageCollage",
    "highcharter",
    'network',
    TRUE,
    '',
    '',
    'linear',
    "ImageCollageInfo",
    "image",
    "trelliscope",
    FALSE,
    NA,
    NA,
    NA,
    "ImageCollageShare",
    "highcharter",
    'network',
    TRUE,
    '',
    '',
    NA,
    "TimelineVol",
    "highcharter",
    'line',
    FALSE,
    'datetimeData',
    'scoreVolume',
    'datetime',
    "TimelineVolInfo",
    "highcharter",
    'line',
    TRUE,
    'datetimeData',
    'scoreVolume',
    'datetime',
    "TimelineTone",
    "highcharter",
    'line',
    FALSE,
    'datetimeData',
    'scoreTone',
    'datetime',
    "TimelineLang",
    "highcharter",
    'line',
    FALSE,
    'datetimeData',
    'scoreVolume',
    'datetime',
    "TimelineSourceCountry",
    "highcharter",
    'line',
    FALSE,
    'datetimeData',
    'scoreVolume',
    'datetime',
    "ToneChart",
    "highcharter",
    "column",
    TRUE,
    'idBIN',
    'countArticles',
    'linear',
    "WordCloudEnglish",
    "wordcloud",
    "wordcloud",
    FALSE,
    NA,
    NA,
    NA,
    "WordCloudNative",
    "wordcloud",
    "wordcloud",
    FALSE,
    NA,
    NA,
    NA,
    "WordCloudTheme",
    "wordcloud",
    "wordcloud",
    FALSE,
    NA,
    NA,
    NA,
    "WordCloudImageTags",
    "wordcloud",
    "wordcloud",
    FALSE,
    NA,
    NA,
    NA,
    "WordCloudImageWebTags",
    "wordcloud",
    "wordcloud",
    FALSE,
    NA,
    NA,
    NA
  )
}

.check_column_name <- function(data, column = "regex") {
  data %>% tibble::has_name(column)
}

# wordcloud ---------------------------------------------------------------

.plot_wordcloud <-
  function(data,
           size = .5,
           ellipticity = .25,
           widget_size = c(300, 300)) {
    df <-
      data %>%
      dplyr::rename(word = labelTime,
                    freq = countValue) %>%
      dplyr::select(word, freq)
    wc <-
      df %>%
      data.frame() %>%
      wordcloud2::wordcloud2(size = size,
                             ellipticity = ellipticity,
                             widgetsize = widget_size)
    wc
  }


# trelliscope -------------------------------------------------------------
.build_folder <-
  function(path = "Desktop/abresler.github.io/trelliscopes/jinkie/otr/kaute") {
    oldwd <- getwd()
    setwd("~")

    folder_exists <-
      dir.exists(paths = path)

    if (folder_exists) {
      setwd(oldwd)
      return(invisible())
    }

    parts <- path %>% str_split("/") %>% flatten_chr()

    seq_along(parts) %>%
      future_map(function(x) {
        if (x == 1) {
          directory <- parts[x]
          if (!dir.exists(directory)) {
            dir.create(directory)
          }
          return(invisible())
        }
        directory <- parts[1:x] %>% str_c(collapse = '/')
        if (!dir.exists(directory)) {
          dir.create(directory)
        }
        return(invisible())
      })

    setwd(oldwd)
    return(invisible())
  }

.munge_for_trelliscope <-
  function(data ,
           remove_columns = c('countMaximumRecords',
                              'urlGDELTV2FTAPI',
                              'isOR',
                              'urlArticleMobile'),
           group_columns = list(regex = "Search",
                                columns = NULL,
                                exclude = NULL)) {
    data <-
      data %>%
      mutate_if(is.logical,
                as.character)

    if (!remove_columns %>% purrr::is_null()) {
      data <-
        data %>%
        dplyr::select(-one_of(remove_columns)) %>%
        suppressWarnings()
    }

    has_grouping <-
      !group_columns %>% purrr::is_null()


    if (has_grouping) {
      df_group <-
        group_columns

      nest_names <- c()

      has_regex <-
        df_group$regex %>% length() > 0

      has_column <-
        df_group$columns %>% length() > 0

      has_exclude <-
        df_group$exclude %>% length() > 0

      if (has_regex) {
        regex <-
          df_group$regex

        nest_regex <-
          data %>% dplyr::select(dplyr::matches(regex)) %>% names()
        nest_names <- nest_names %>% append(nest_regex)
      }

      if (has_column) {
        columns <-
          df_group$column
        nest_columns <-
          data %>% dplyr::select(one_of(columns)) %>% names()

        nest_names <-
          nest_names %>% append(nest_columns)
      }

      if (has_exclude) {
        exclude_names <- df_group$exclude
        nest_names <-
          nest_names[!nest_names %>% str_detect(exclude_names)]
      }

      nest_names <-
        nest_names %>%
        rlang::syms()


      data <-
        data %>%
        dplyr::group_by(!!!nest_names) %>%
        tidyr::nest()

      data <-
        data %>%
        mutate(idPanel = 1:n()) %>%
        dplyr::select(idPanel, everything())

    } else {
      data <-
        data %>%
        mutate(idPanel = 1:n())
    }

    data
  }

#' Plot wordcloud trelliscope
#'
#' @param data
#' @param remove_columns
#' @param group_columns
#' @param id_columns
#' @param word_size
#' @param widget_size
#' @param trelliscope_parameters
#'
#' @return
#' @export
#' @import tidyr wordcloud2 dplyr purrr anytime trelliscopejs rlang readr stringr
#' @examples
plot_wc_trelliscope <-
  function(data,
           remove_columns = c('countMaximumRecords',
                              'urlGDELTV2FTAPI',
                              'isOR',
                              'urlArticleMobile'),
           group_columns = list(regex = "Search",
                                columns = NULL,
                                exclude = NULL),
           id_columns = list(
             is_except = TRUE,
             columns = c("idPanel", "data", "plot"),
             regex = NULL
           ),
           word_size = .65,
           widget_size = c(500, 500),
           trelliscope_parameters = list(rows = 1,
                                         columns = 3,
                                         path = NULL)) {
    data <-
      data %>%
      .munge_for_trelliscope(remove_columns = remove_columns,
                             group_columns = group_columns)


    data <-
      data %>%
      mutate(plot = map_plot(
        data,
        ~ .plot_wordcloud(
          data = .x,
          size = word_size,
          widget_size = widget_size
        )
      ))


    column_ids <-
      data %>%
      .get_trelliscope_id_columns(id_columns = id_columns)

    title <-
      glue::glue("GDELT V2 Full Text API World Cloud Trelliscope")

    df_trelliscope_params <-
      trelliscope_parameters %>% flatten_df()

    has_path <-
      df_trelliscope_params %>% tibble::has_name("path")
    if (has_path) {
      path_loc <- df_trelliscope_params$path

      .build_folder(path = path_loc) %>%
        suppressWarnings()

      viz <-
        data %>%
        trelliscopejs::trelliscope(
          name = title,
          nrow = df_trelliscope_params$rows,
          ncol = df_trelliscope_params$columns,
          state = list(labels = c(column_ids)),
          path = path_loc
        )
      return(viz)
    }

    data %>%
      trelliscope(
        name = title,
        nrow = df_trelliscope_params$rows,
        ncol = df_trelliscope_params$columns,
        state = list(labels = c(column_ids))
      )
  }


# highchart ---------------------------------------------------------------


.add_hc_group <-
  function(data) {
    gather_cols <-
      data %>%
      dplyr::select(-one_of(c("modeSearch", "periodtimeSearch"))) %>%
      dplyr::select(dplyr::matches("Search")) %>% names()

    df_keys <-
      data %>%
      gather_('search', 'term', gather_cols, na.rm = TRUE) %>%
      unite(itemSearch, 'search', 'term', sep = ": ") %>%
      mutate(idRow = 1:n()) %>%
      dplyr::select(idRow, itemSearch)

    data <-
      data %>%
      mutate(idRow = 1:n()) %>%
      left_join(df_keys) %>%
      dplyr::select(itemSearch, everything()) %>%
      tidyr::separate(
        col = 'itemSearch',
        into = c('itemQuery', 'valueQuery'),
        sep = '\\: '
      ) %>%
      suppressMessages() %>%
      dplyr::select(-idRow)

    data
  }

.plot_ft_v2_highchart <-
  function(data,
           search_mode = 'TimelineVolInfo',
           period_time_search = "12 Weeks",
           include_title = TRUE) {
    data <-
      data %>%
      dplyr::mutate(typeChart = search_mode) %>%
      dplyr::select(-one_of(c(
        'countMaximumRecords', 'urlGDELTV2FTAPI', 'isOR'
      ))) %>%
      suppressMessages() %>%
      suppressWarnings()

    chart_type <-
      search_mode %>% str_to_lower()

    df_chart_info <-
      .get_mode_types() %>%
      mutate(modeSearch = modeSearch %>% str_to_lower()) %>%
      filter(modeSearch == chart_type)

    if (data %>% tibble::has_name("periodtimeSearch")) {
      data <-
        data %>%
        dplyr::rename(periodChart = periodtimeSearch)
    }

    if (data %>% tibble::has_name("datetimeStartSearch")) {
      data <-
        data %>%
        dplyr::rename(datetimeStart = datetimeStartSearch)
    }

    if (data %>% tibble::has_name("datetimeSearchEnd")) {
      data <-
        data %>%
        dplyr::rename(datetimeEnd = datetimeSearchEnd)
    }

    gather_cols <-
      data %>%
      dplyr::select(-dplyr::matches("modeSearch")) %>%
      dplyr::select(dplyr::matches("Search")) %>%
      suppressWarnings() %>%
      names()

    data <-
      data %>%
      gather_('search', 'term', gather_cols, na.rm = TRUE) %>%
      unite(itemSearch, 'search', 'term', sep = ": ") %>%
      dplyr::select(itemSearch, everything())

    if (data %>% tibble::has_name("urlArticle")) {
      data <-
        data %>%
        mutate(domainArticle = urlArticle %>% urltools::domain())

      data <-
        data %>%
        group_by(titleArticle, urlArticle) %>%
        mutate(idArticle = 1:n()) %>%
        ungroup() %>%
        filter(idArticle == min(idArticle)) %>%
        dplyr::select(-idArticle)
    }

    if (chart_type %>% str_detect("timeline")) {
      data <-
        data %>%
        group_by(itemSearch) %>%
        arrange((datetimeData)) %>%
        mutate(
          xAxis = datetime_to_timestamp(datetimeData),
          xAxisName = as.character(datetimeData),
          yAxis = value
        ) %>%
        distinct() %>%
        ungroup()
    }

    if (chart_type %>% str_detect("tonechart")) {
      data <-
        data %>%
        mutate(xAxis = idBIN,
               yAxis = countValue,
               xAxisName = idBIN)
    }


    if (df_chart_info$hasTooltip) {
      data <-
        data %>%
        mutate(
          htmlArticle = glue::glue(
            "<li><a href = '{urlArticle}' target = '_blank'> {titleArticle}</a></li>"
          )
        )

      data <-
        data %>%
        group_by(xAxis, itemSearch, typeChart, periodChart) %>%
        # summarise(
        #  yAxis = max(yAxis, na.rm = TRUE),
        #  htmlArticles = htmlArticle %>% str_c(collapse = '')
        # ) %>%
        # ungroup() %>%
        separate(
          itemSearch,
          into = c('item', 'value'),
          sep = ": ",
          remove = FALSE
        )

      y_axis_name <- df_chart_info$nameYAxis
      x_axis_name <- df_chart_info$nameXAxis

      if (x_axis_name %>% str_detect("date")) {
        data <- data %>%
          mutate(xAxisV = anytime::anytime(xAxis / 1000) %>% as.character())
      } else {
        data <- data %>%
          mutate(xAxisV = xAxis)
      }
      data <-
        data %>%
        mutate(
          htmlTooltip = glue::glue(
            "<div><p><strong>{item}:</strong> {value}</p><p><strong>{y_axis_name}:</strong> {yAxis}</p><p><strong>{x_axis_name}:</strong> {xAxisV}</p><span><ul>{htmlArticle}</ul></span></div>"
          ) %>% as.character()
        )
    }

    x_axis_name <- df_chart_info$nameXAxis
    y_axis_name <- df_chart_info$nameYAxis
    chart_type <- df_chart_info$typeVisualization
    type_x_axis <- df_chart_info$typeXAxis
    has_tool_tip <-  df_chart_info$hasTooltip

    if (data %>% tibble::has_name("periodChart")) {
      period_time_slug <-
        data$periodChart %>% unique()

      period_slug <-
        glue::glue("Over the Last {period_time_slug %>% str_to_title()}")
    } else{
      period_slug <- ""
    }

    if (data %>% tibble::has_name("datetimeStart")) {
      start <-
        data$datetimeStart %>% unique()

      end <-
        data$datetimeEnd %>% unique()

      date_slug <-
        glue::glue("from {start} to {end}")
    } else{
      date_slug <- ""
    }

    type <-
      data$typeChart %>% unique()

    viz <-
      data %>%
      hchart(chart_type, hcaes(x = xAxis, y = yAxis, group = itemSearch)) %>%
      hc_add_theme(hc_theme_gridlight()) %>%
      hc_xAxis(
        type = type_x_axis,
        gridLineWidth = 0,
        lineWidth = 1,
        lineColor = "black",
        tickWidth = 1,
        tickAmount = 15,
        tickColor = "black",
        title = list(text = x_axis_name, style = list(color = "black")),
        labels = list(style = list(color = "black"))
      ) %>%
      hc_yAxis(
        opposite = FALSE,
        minorTickInterval = "auto",
        minorGridLineDashStyle = "dot",
        showFirstLabel = FALSE,
        showLastLabel = FALSE,
        tickAmount = 8,
        gridLineWidth = 0,
        title = list(text = y_axis_name, style = list(color = "black")),
        labels = list(format = "{value}")
      ) %>%
      hc_legend(align = "left",
                verticalAlign = "bottom",
                layout = "horizontal") %>%
      hc_credits(
        enabled = TRUE,
        text = glue::glue("Data from GDELT Project via gdeltr2") %>% as.character(),
        href = "http://gdeltproject.org"
      ) %>%
      hc_exporting(enabled = TRUE) %>%
      suppressWarnings() %>%
      hc_chart(zoomType = "xy")

    if (include_title) {
      more_than_1 <-
        data %>% select(itemSearch) %>% distinct() %>% nrow() > 1

      if (more_than_1) {
        title <-
          glue::glue("<h4>GDELT FT API V2 {type} Visualization {period_slug} {date_slug}</h4>") %>%
          as.character() %>%
          str_trim() %>%
          str_replace_all('\\ </h4>', '</h4>')
      } else {
        item <- data$item %>% unique()
        value <- data$value %>% unique()
        title <-
          glue::glue(
            "<h4>GDELT FT API V2 {type} Visualization for {item} of {value} {period_slug} {date_slug}</h4>"
          ) %>%
          as.character() %>%
          str_trim() %>%
          str_replace_all('\\ </h4>', '</h4>')
      }

      viz <-
        viz %>%
        hc_title(text = title,
                 useHTML = TRUE) %>%
        hc_subtitle(
          text = glue::glue("<em>Data as of {Sys.time()}</em>") %>% as.character(),
          useHTML = TRUE
        )

    }

    if (chart_type == "line") {
      viz <-
        viz %>%
        hc_plotOptions(series = list(
          marker = list(enabled = FALSE),
          cursor = "pointer",
          states = list(hover = list(enabled = FALSE)),
          stickyTracking = TRUE
        ))
    }

    if (has_tool_tip) {
      tt <-
        tags$table("{point.htmlTooltip}")


      viz <-
        viz %>%
        hc_tooltip(
          pointFormat = "{point.htmlArticle}",
          headerFormat = "",
          useHTML = TRUE,
          crosshairs = F,
          shared = T,
          borderWidth = 1,
          hideDelay = 0,
          followPointer = FALSE,
          enabled = T,
          followTouchMove = TRUE,
          style = list(pointerEvents = "auto")
        ) %>%

        hc_chart(events = list(
          load = JS(
            "function(){ this.myTooltip = new Highcharts.Tooltip(this, this.options.tooltip);}"
          )
        )) %>%
        hc_plotOptions(
          series = list(
            allowPointSelect = TRUE,
            marker = list(enabled = FALSE),
            cursor = "pointer",
            events = list(
              click = JS(
                "function(evt) {
                this.chart.myTooltip.options.enabled = true;
                this.chart.myTooltip.refresh(evt.point, evt)
    }"
              ),
              mouseOut = JS(
                "function() {
          this.chart.myTooltip.hide();
          this.chart.myTooltip.options.enabled = false;

  }"
              )
            ),
            stickyTracking = T
          )
        )

    } else {
      viz <-
        viz %>%
        hc_tooltip(
          useHTML = TRUE,
          crosshairs = TRUE,
          shared = TRUE,
          borderWidth = 2,
          style = list(pointerEvents = "auto")
        )
    }
    viz
  }

#' Plot a highchart trelliscope
#'
#' @param data
#' @param remove_columns
#' @param group_columns
#' @param id_columns
#' @param include_title
#' @param trelliscope_parameters
#'
#' @return
#' @export
#' @import tidyr highcharter dplyr purrr anytime trelliscopejs rlang readr tibble
#' @examples
plot_hc_trelliscope <-
  function(data,
           remove_columns = c('countMaximumRecords',
                              'urlGDELTV2FTAPI',
                              'isOR',
                              'urlArticleMobile'),
           group_columns = list(
             regex = NULL,
             columns = c('itemQuery', 'valueQuery', 'periodtimeSearch'),
             exclude = NULL
           ),
           id_columns = list(
             is_except = FALSE,
             columns = c('itemQuery', 'valueQuery', "periodtimeSearch",  "termSearch"),
             regex = NULL
           ),
           include_title = TRUE,
           trelliscope_parameters = list(rows = 1,
                                         columns = 3,
                                         path = NULL)) {
    search_mode <-
      data %>%
      pull(modeSearch) %>% unique() %>% .[[1]]

    data <-
      data %>%
      .add_hc_group()

    data <-
      data %>%
      .munge_for_trelliscope(remove_columns = remove_columns,
                             group_columns = group_columns)
    data <-
      data %>%
      mutate(plot = map_plot(
        data,
        ~ .plot_ft_v2_highchart(
          data = .x,
          search_mode = search_mode,
          include_title = include_title
        )
      )) %>%
      suppressWarnings()


    column_ids <-
      data %>%
      .get_trelliscope_id_columns(id_columns = id_columns) %>%
      suppressWarnings() %>%
      suppressMessages()

    search_mode <-
      data$data[[1]]$modeSearch %>% unique()

    title <-
      glue::glue("GDELT Full Text API {search_mode} Trelliscope")

    df_trelliscope_params <-
      trelliscope_parameters %>% flatten_df()

    has_path <-
      df_trelliscope_params %>% tibble::has_name("path")

    if (data %>% ncol() <= 4) {
      state_value <- NULL
    }  else {
      state_value <- list(labels = c(column_ids))
    }
    if (has_path) {
      path_loc <- df_trelliscope_params$path

      .build_folder(path = path_loc) %>%
        suppressWarnings()

      viz <-
        data %>%
        trelliscopejs::trelliscope(
          name = title,
          nrow = df_trelliscope_params$rows,
          ncol = df_trelliscope_params$columns,
          state = state_value,
          path = path_loc
        )
      return(viz)
    }

    viz <-
      data %>%
      trelliscope(
        name = title,
        nrow = df_trelliscope_params$rows,
        ncol = df_trelliscope_params$columns,
        state = state_value
      )

    viz
  }

# utils -------------------------------------------------------------------
.generate_ymd_hms <- function(date) {
  if (date %>% stringr::str_detect(":")) {
    date <- date %>% lubridate::ymd_hms() %>% force_tz()
    return(date)
  }
  glue::glue("{date} 12:00:00") %>% as.character() %>% lubridate::ymd_hms() %>% lubridate::force_tz()
}


#' Generates sequences of dates
#'
#' @param start_date date to start
#' if \code{NULL} defaults to 3 months ago
#' @param end_date date to end
#' if \code{NULL} defaults to system time
#' @param time_interval time interval sequences
#' \itemize{
#' \item months
#' \item weeks
#' \item hours
#' \item minutess
#' }
#'
#' @return a `vector` of date times
#' @export
#' @import glue dplyr lubridate stringr purrr
#' @examples
#' generate_dates(start_date = Sys.Date()  -365, end_date = Sys.Date(), time_interval = "weeks")
generate_dates <-
  function(start_date = NULL,
           end_date = NULL,
           time_interval = "days") {
    if (time_interval %>% purrr::is_null()) {
      time_interval <- 'hours'
    }
    time_interval <- time_interval %>% str_to_lower()
    period_types <-
      c("months", "weeks", 'days', 'hours', 'minutes', "year")
    if (!time_interval %in% period_types) {
      stop(
        glue::glue(
          "Sorry period types can on only be {str_c(period_types, collapse = ', ')}"
        ) %>% as.character()
      )
    }

    now <- Sys.time()
    earliest <- now - lubridate::dweeks(12)
    if (end_date %>% purrr::is_null()) {
      end_date <- now
    } else {
      end_date <- end_date %>% .generate_ymd_hms()
    }
    if (start_date %>% purrr::is_null()) {
      start_date <- earliest
    } else {
      start_date <-
        start_date %>% .generate_ymd_hms()
    }

    all_dates <-
      seq(start_date, end_date, time_interval)

    end_seq <- length(all_dates) - 1

    dates <-
      1:end_seq %>%
      map_chr(function(x) {
        start <-
          all_dates[[x]] %>% as.character()
        end <-
          (all_dates[[x + 1]] - lubridate::dseconds(1)) %>% as.character()
        str_c(start, end, sep = " - ")
      })
    dates
  }


.get_trelliscope_id_columns <-
  function(data,
           id_columns = list(
             is_except = TRUE,
             columns = c("idPanel", "data", "plot"),
             regex = NULL
           )) {
    column_id_names <- c()
    has_columns <-
      id_columns %>% names() %>% str_detect("columns") %>% sum(na.rm = TRUE) > 0 &&
      id_columns$columns %>% length() > 0
    if (has_columns) {
      columns <- id_columns$columns
      is_except <- id_columns$is_except
      if (is_except) {
        name_columns <- data %>% dplyr::select(-one_of(columns)) %>% names()
      } else {
        name_columns <-
          data %>% dplyr::select(one_of(columns)) %>% names()
      }
      column_id_names <-
        column_id_names %>% append(name_columns)
    }
    has_regex <-
      id_columns %>% names() %>% str_detect("regex") %>% sum(na.rm = TRUE) > 0 &&
      id_columns$regex %>% length() > 0
    if (has_regex) {
      regex <- id_columns$regex
      regex_columns <-
        data %>% dplyr::select(dplyr::matches(regex)) %>% names()
      column_id_names <-
        column_id_names %>% append(regex_columns)
    }
    column_id_names
  }

.get_mode_types <-
  function() {
    df <- tribble(
      ~ modeSearch,
      ~ packageVisualization,
      ~ typeVisualization,
      ~ hasTooltip,
      ~ nameXAxis,
      ~ nameYAxis,
      ~ typeXAxis,
      "ArtList",
      "image",
      "trelliscope",
      FALSE,
      NA,
      NA,
      NA,
      "ImageCollage",
      "highcharter",
      'network',
      TRUE,
      '',
      '',
      'linear',
      "ImageCollageInfo",
      "image",
      "trelliscope",
      FALSE,
      NA,
      NA,
      NA,
      "ImageCollageShare",
      "highcharter",
      'network',
      TRUE,
      '',
      '',
      NA,
      "TimelineVol",
      "highcharter",
      'line',
      FALSE,
      'datetimeData',
      'scoreVolume',
      'datetime',
      "TimelineVolInfo",
      "highcharter",
      'line',
      TRUE,
      'datetimeData',
      'scoreVolume',
      'datetime',
      "TimelineTone",
      "highcharter",
      'line',
      FALSE,
      'datetimeData',
      'scoreTone',
      'datetime',
      "TimelineLang",
      "highcharter",
      'line',
      FALSE,
      'datetimeData',
      'scoreVolume',
      'datetime',
      "TimelineSourceCountry",
      "highcharter",
      'line',
      FALSE,
      'datetimeData',
      'scoreVolume',
      'datetime',
      "ToneChart",
      "highcharter",
      "column",
      TRUE,
      'idBIN',
      'countArticles',
      'linear',
      "WordCloudEnglish",
      "wordcloud",
      "wordcloud",
      FALSE,
      NA,
      NA,
      NA,
      "WordCloudNative",
      "wordcloud",
      "wordcloud",
      FALSE,
      NA,
      NA,
      NA,
      "WordCloudTheme",
      "wordcloud",
      "wordcloud",
      FALSE,
      NA,
      NA,
      NA,
      "WordCloudImageTags",
      "wordcloud",
      "wordcloud",
      FALSE,
      NA,
      NA,
      NA,
      "WordCloudImageWebTags",
      "wordcloud",
      "wordcloud",
      FALSE,
      NA,
      NA,
      NA
    )

    df
  }

.check_column_name <- function(data, column = "regex") {
  data %>% tibble::has_name(column)
}

.odd_expand <-
  function(data,
           column_name = 'timespan',
           column_values = c('24 hours', "2 weeks")) {
    if (column_values %>% length() == 0) {
      data <-
        data %>%
        mutate(!!column_name := (!!NA))
      return(data)
    }

    only_1 <- column_values %>% length() == 1

    if (only_1) {
      data <-
        data %>%
        mutate(!!column_name := (!!column_values))
      return(data)
    }

    col_order <- names(data)

    df <- tibble(item = column_values)

    names(df)[[1]] <-
      column_name

    df <-
      df %>%
      mutate(data = list(data)) %>%
      tidyr::unnest()

    columns <-
      c(names(df)[names(df) %in% col_order],
        names(df)[!names(df) %in% col_order])

    df %>%
      dplyr::select(one_of(columns))
  }

# wordcloud ---------------------------------------------------------------

.plot_wordcloud <-
  function(data,
           size = .5,
           ellipticity = .25,
           widget_size = c(300, 300)) {
    df <-
      data %>%
      dplyr::rename(word = labelTime,
                    freq = countValue) %>%
      dplyr::select(word, freq)
    wc <-
      df %>%
      data.frame() %>%
      wordcloud2::wordcloud2(size = size,
                             ellipticity = ellipticity,
                             widgetsize = widget_size)
    wc
  }


# trelliscope -------------------------------------------------------------
.build_folder <-
  function(path = "Desktop/abresler.github.io/trelliscopes/wc_test") {
    oldwd <- getwd()
    setwd("~")
    new_wd <- getwd()
    folder_exists <-
      dir.exists(paths = path)

    if (folder_exists) {
      setwd(oldwd)
      return(invisible())
    }

    levels <- path %>% stringr::str_count("/")

    level_parts <-
      path %>% stringr::str_split('/') %>% purrr::flatten_chr()

    level_parts <- level_parts[!level_parts == '']

    parent_dir <-
      level_parts[1:(levels)] %>% str_c(collapse = '/')

    folder_name <-
      level_parts[level_parts %>% length()]
    if (!dir.exists(paths = parent_dir)) {
      dir.create(parent_dir)
    }
    setwd(oldwd)

    return(invisible())
  }

.munge_for_trelliscope <-
  function(data ,
           remove_columns = c('countMaximumRecords',
                              'urlGDELTV2FTAPI',
                              'isOR',
                              'urlArticleMobile'),
           group_columns = list(regex = "Search",
                                columns = NULL,
                                exclude = NULL)) {
    if (!remove_columns %>% purrr::is_null()) {
      data <-
        data %>%
        dplyr::select(-dplyr::one_of(remove_columns)) %>%
        suppressWarnings()
    }

    has_grouping <-
      !group_columns %>% purrr::is_null()


    if (has_grouping) {
      df_group <-
        group_columns

      nest_names <- c()

      has_regex <-
        df_group$regex %>% length() > 0

      has_column <-
        df_group$columns %>% length() > 0

      has_exclude <-
        df_group$exclude %>% length() > 0

      if (has_regex) {
        regex <-
          df_group$regex

        nest_regex <-
          data %>% dplyr::select(dplyr::matches(regex)) %>% names()
        nest_names <- nest_names %>% append(nest_regex)
      }

      if (has_column) {
        columns <-
          df_group$column
        nest_columns <-
          data %>% dplyr::select(one_of(columns)) %>% names()

        nest_names <-
          nest_names %>% append(nest_columns)
      }

      if (has_exclude) {
        exclude_names <- df_group$exclude
        nest_names <-
          nest_names[!nest_names %>% str_detect(exclude_names)]
      }

      nest_names <-
        nest_names %>%
        rlang::syms()


      data <-
        data %>%
        dplyr::group_by(!!!nest_names) %>%
        tidyr::nest()

      data <-
        data %>%
        mutate(idPanel = 1:n()) %>%
        dplyr::select(idPanel, everything())

    } else {
      data <-
        data %>%
        mutate(idPanel = 1:n())
    }

    data
  }

#' Plot wordcloud trelliscope
#'
#' @param data
#' @param remove_columns
#' @param group_columns
#' @param id_columns
#' @param word_size
#' @param widget_size
#' @param trelliscope_parameters
#'
#' @return
#' @export
#' @import tidyr wordcloud2 dplyr purrr anytime trelliscopejs rlang readr stringr
#' @examples
plot_wc_trelliscope <-
  function(data,
           remove_columns = c('countMaximumRecords',
                              'urlGDELTV2FTAPI',
                              'isOR',
                              'urlArticleMobile'),
           group_columns = list(regex = "Search",
                                columns = NULL,
                                exclude = NULL),
           id_columns = list(
             is_except = TRUE,
             columns = c("idPanel", "data", "plot"),
             regex = NULL
           ),
           word_size = .65,
           widget_size = c(500, 500),
           trelliscope_parameters = list(rows = 1,
                                         columns = 3,
                                         path = NULL)) {
    data <-
      data %>%
      .munge_for_trelliscope(remove_columns = remove_columns,
                             group_columns = group_columns)


    data <-
      data %>%
      mutate(plot = map_plot(
        data,
        ~ .plot_wordcloud(
          data = .x,
          size = word_size,
          widget_size = widget_size
        )
      ))


    column_ids <-
      data %>%
      .get_trelliscope_id_columns(id_columns = id_columns)
    mode_names <-
      data$modeSearch %>% unique() %>% str_c(collapse = ', ')
    title <-
      glue::glue("GDELT Full Text API {mode_names} Trelliscope")

    df_trelliscope_params <-
      trelliscope_parameters %>% flatten_df()

    has_path <-
      df_trelliscope_params %>% tibble::has_name("path")

    if (has_path) {
      path_loc <- df_trelliscope_params$path

      .build_folder(path = path_loc) %>%
        suppressWarnings()

      viz <-
        data %>%
        trelliscopejs::trelliscope(
          name = title,
          nrow = df_trelliscope_params$rows,
          ncol = df_trelliscope_params$columns,
          state = list(labels = c(column_ids)),
          path = path_loc
        )
      return(viz)
    }

    data %>%
      trelliscope(
        name = title,
        nrow = df_trelliscope_params$rows,
        ncol = df_trelliscope_params$columns,
        state = list(labels = c(column_ids))
      )
  }

plot_panel_trelliscope <-
  function(data,
           remove_columns = c('countMaximumRecords',
                              'urlGDELTV2FTAPI',
                              'isOR',
                              'urlArticleMobile'),
           group_columns = NULL,
           id_columns = list(
             is_except = FALSE,
             columns = c(
               'datetimeArticle',
               'domainArticle',
               "titleArticle",
               "urlArticle"
             ),
             regex = NULL
           ),
           image_column = 'urlImage',
           link_column = 'urlArticle' ,
           trelliscope_parameters = list(rows = 1,
                                         columns = 3,
                                         path = NULL)) {
    if (image_column %>% purrr::is_null()) {
      data <-
        data %>%
        mutate(urlImage = NA)
    }

    data <-
      data %>%
      .munge_for_trelliscope(remove_columns = remove_columns,
                             group_columns = group_columns) %>%
      suppressWarnings()

    if (!data %>% tibble::has_name('urlImage')) {
      names(data)[names(data) %>% str_detect(image_column)] <-
        'urlImage'
    }

    if (!data %>% tibble::has_name('urlArticle')) {
      names(data)[names(data) %>% str_detect(link_column)] <-
        'urlArticle'
    }

    if (data %>% tibble::has_name("datetimeArticle")) {
      data <-
        data %>%
        arrange(desc(datetimeArticle))

      data <-
        data %>%
        mutate(dateArticle = anytime::anydate(datetimeArticle))

      data <-
        data %>%
        mutate_at(data %>% dplyr::select(dplyr::matches("^date")) %>% names(),
                  list(. %>% as.character()))

    }

    data <-
      data %>%
      dplyr::select(-one_of('idPanel')) %>%
      suppressWarnings() %>%
      suppressMessages()

    data <-
      data %>%
      mutate(
        idArticle = 1:n(),
        panel = trelliscopejs::img_panel(urlImage),
        urlArticle = trelliscopejs::cog_href(
          urlArticle,
          default_label = TRUE,
          default_active = TRUE
        )
      ) %>%
      select(idArticle, everything()) %>%
      arrange(idArticle)


    column_ids <-
      data %>%
      .get_trelliscope_id_columns(id_columns = id_columns) %>%
      suppressWarnings()

    title <-
      glue::glue("GDELT Full Text API Image Trelliscope") %>%
      as.character()

    df_trelliscope_params <-
      trelliscope_parameters %>% flatten_df()

    has_path <-
      df_trelliscope_params %>% tibble::has_name("path")

    if (has_path) {
      path_loc <- df_trelliscope_params$path

      .build_folder(path = path_loc) %>%
        suppressWarnings()

      viz <-
        data %>%
        mutate(urlImage = trelliscopejs::img_panel(urlImage)) %>%
        trelliscopejs::trelliscope(
          name = title,
          nrow = df_trelliscope_params$rows,
          ncol = df_trelliscope_params$columns,
          state = list(
            labels = c(column_ids),
            sort = list(sort_spec("idArticle"))
          ),
          path = path_loc
        )
      return(viz)
    }

    data %>%
      mutate(urlImage = trelliscopejs::img_panel(urlImage)) %>%
      trelliscopejs::trelliscope(
        name = title,
        nrow = df_trelliscope_params$rows,
        ncol = df_trelliscope_params$columns,
        state = list(
          labels = c(column_ids),
          sort = list(sort_spec("idArticle"))
        )
      )

  }

plot_trelliscope <-
  function(data,
           trelliscope_type = 'image',
           remove_columns = c('countMaximumRecords',
                              'urlGDELTV2FTAPI',
                              'isOR',
                              'urlArticleMobile'),
           group_columns = NULL,
           path = NULL,
           rows = 1,
           columns = 2,
           ...) {
    is_image <-
      trelliscope_type %>% str_to_lower() == 'image'

    if (is_image) {
      if (!'image_column' %>% exists()) {
        image_column  <-
          'urlImage'

      }

      if (!'link_column' %>% exists()) {
        link_column <-
          'urlArticle'
      }

      id_columns <- list(
        is_except = FALSE,
        columns = c(
          "termSearch",
          'datetimeArticle',
          'domainArticle',
          "titleArticle",
          "urlArticle"
        ),
        regex = NULL
      )

      viz <-
        plot_panel_trelliscope(
          data = data ,
          remove_columns = remove_columns,
          group_columns = group_columns,
          id_columns = id_columns,
          image_column = image_column,
          link_column = link_column,
          trelliscope_parameters = list(
            rows = rows,
            columns = columns,
            path = path
          )
        )
      return(viz)
    }

    is_word_cloud <-
      trelliscope_type %>% str_to_lower() %>% str_detect('wordcloud')

    if (is_word_cloud) {
      if (group_columns %>% purrr::is_null()) {
        group_columns <-
          list(regex = "Search",
               columns = NULL,
               exclude = NULL)
      }
      if (!'word_size' %>% exists()) {
        word_size <- .65

      }

      if (!'widget_size' %>% exists()) {
        widget_size = c(500, 500)
      }

      id_columns <- list(
        is_except = FALSE,
        columns = c("modeSearch", "periodtimeSearch", "termSearch"),
        regex = NULL
      )


      viz <-
        plot_wc_trelliscope(
          data = data ,
          remove_columns = remove_columns,
          group_columns = group_columns,
          id_columns = id_columns,
          word_size = word_size,
          widget_size = widget_size,
          trelliscope_parameters = list(
            rows = rows,
            columns = columns,
            path = path
          )
        )
      return(viz)
    }

    is_high_chart <-
      trelliscope_type %>% str_to_lower() %>% str_detect('highchart')

    if (is_high_chart) {
      if (!'group_columns' %>% exists()) {
        group_columns <- NULL
      }
      if (group_columns %>% purrr::is_null()) {
        group_columns = list(
          regex = NULL,
          columns = c('itemQuery', 'valueQuery', 'periodtimeSearch'),
          exclude = NULL
        )
      }
      id_columns <-
        list(
          is_except = FALSE,
          columns = c(
            'itemQuery',
            'valueQuery',
            "periodtimeSearch",
            "termSearch"
          ),
          regex = NULL
        )

      if (!'include_title' %>% exists()) {
        include_title <- FALSE
      }


      viz <-
        plot_hc_trelliscope(
          data = data ,
          remove_columns = remove_columns,
          group_columns = group_columns,
          id_columns = id_columns,
          include_title = include_title,
          trelliscope_parameters = list(
            rows = rows,
            columns = columns,
            path = path
          )
        )
      return(viz)
    }
  }


#' Plot list of trelliscope
#'
#' @param data a data frame
#' @param group_columns columns for grouped data frame
#' @param trelliscope_parameters list of parameters to pass along to trelliscope \itemize{
#' \item path: if not \code{NULL} the path to save the trelliscope
#' \item rows: rows for trelliscope
#' \item columns: columns for trelliscope
#' \item id_columns: initial columns
#' }
#'
#' @return
#' @export
#'
#' @examples
plot_trelliscopes <-
  function(data,
           group_columns = NULL,
           trelliscope_parameters = list(path = NULL,
                                         rows = 1,
                                         columns = 2)) {
    df_mode_type <-
      .get_mode_types() %>%
      dplyr::select(modeSearch, packageVisualization)

    all_data <-
      data %>%
      mutate(modeSearch = modeSearch %>% str_to_upper()) %>%
      left_join(df_mode_type %>% mutate(modeSearch = modeSearch %>% str_to_upper())) %>%
      dplyr::select(packageVisualization, modeSearch, everything()) %>%
      suppressMessages()

    all_data <-
      all_data %>%
      nest(.key = data, -packageVisualization)

    if (names(trelliscope_parameters) %>% str_detect("rows") %>% sum(na.rm = TRUE) > 0) {
      row_no <- trelliscope_parameters$rows
    } else {
      row_no <- 1
    }

    if (names(trelliscope_parameters) %>% str_detect("columns") %>% sum(na.rm = TRUE) > 0) {
      col_no <- trelliscope_parameters$columns
    } else {
      col_no <- 2
    }

    if (names(trelliscope_parameters) %>% str_detect("path") %>% sum(na.rm = TRUE) > 0) {
      path <- trelliscope_parameters$path
    } else {
      path <- NULL
    }

    if (!group_columns %>% purrr::is_null()) {
      group <- group_columns
    } else {
      group <- NULL
    }

    df_viz <-
      1:nrow(all_data) %>%
      map_dfr(function(x) {
        df_row <- all_data %>% slice(x)
        trelliscope_type <-
          df_row$packageVisualization

        data <-
          df_row %>% dplyr::select(-packageVisualization) %>%
          tidyr::unnest()

        viz <-
          plot_trelliscope(
            data = data,
            trelliscope_type = trelliscope_type,
            columns = col_no,
            rows = row_no,
            group_columns = group,
            path = path
          )

        ts_name <-
          str_c("trelliscope",
                trelliscope_type %>% str_to_title())

        assign(x = ts_name,
               eval(viz),
               envir = .GlobalEnv)

        glue::glue("Trelliscope assigned to {ts_name}
                   in your global environment") %>%
          cat(fill = T)

        tibble(
          idTrelliscope = x,
          typeTrelliscope = trelliscope_type,
          vizTrelliscope = list(viz)
        )
      })

    if (df_viz %>% nrow() == 1) {
      return(df_viz$vizTrelliscope[[1]])
    }

    df_viz
  }


codebook_trelliscope <-
  function(data,
           code_book = "gkg",
           path = NULL) {
    data <-
      data %>%
      .munge_for_trelliscope(remove_columns = NULL, group_columns = NULL) %>%
      mutate_if(is.logical, as.character) %>%
      mutate(urlImage = "http://gdeltproject.org/imgs/gdelt-events-nasa-night-lights.jpg")

    if (code_book %>% str_to_lower() == "gkg") {
      parameter <- 'theme'
      data <-
        data %>%
        mutate(
          urlNews1D = glue::glue(
            'https://api.gdeltproject.org/api/v2/doc/doc?query={parameter}:{idGKGTheme}%20sourcelang:english&mode=artlist&timespan=24h&maxrecords=250&sort=datedesc'
          ),
          urlNews1M = glue::glue(
            'https://api.gdeltproject.org/api/v2/doc/doc?query={parameter}:{idGKGTheme}%20sourcelang:english&mode=artlist&timespan=1m&maxrecords=250&sort=datedesc'
          ),
          urlNews3M = glue::glue(
            'https://api.gdeltproject.org/api/v2/doc/doc?query={parameter}:{idGKGTheme}%20sourcelang:english&mode=artlist&timespan=12w&maxrecords=250&sort=datedesc'
          ),
          urlTimelineVol = glue::glue(
            'https://api.gdeltproject.org/api/v2/doc/doc?query={parameter}:{idGKGTheme}%20sourcelang:english&mode=timelinevolinfo&timespan=12w&maxrecords=250&sort=datedesc'
          ),
          urlSentiment = glue::glue(
            'https://api.gdeltproject.org/api/v2/doc/doc?query={parameter}:{idGKGTheme}%20sourcelang:english&mode=ToneChart&timespan=12w&maxrecords=250&sort=datedesc'
          )
        )
    }

    if (code_book %>% str_to_lower() == "imageweb") {
      parameter <- 'imagewebtag'
      data <-
        data %>%
        mutate(
          urlNews1D = glue::glue(
            'https://api.gdeltproject.org/api/v2/doc/doc?query={parameter}:"{idImageWeb}"%20sourcelang:english&mode=artlist&timespan=24h&maxrecords=250&sort=datedesc'
          ),
          urlNews1M = glue::glue(
            'https://api.gdeltproject.org/api/v2/doc/doc?query={parameter}:"{idImageWeb}"%20sourcelang:english&mode=artlist&timespan=1m&maxrecords=250&sort=datedesc'
          ),
          urlNews3M = glue::glue(
            'https://api.gdeltproject.org/api/v2/doc/doc?query={parameter}:"{idImageWeb}"%20sourcelang:english&mode=artlist&timespan=12w&maxrecords=250&sort=datedesc'
          ),
          urlTimelineVol = glue::glue(
            'https://api.gdeltproject.org/api/v2/doc/doc?query={parameter}:"{idImageWeb}"%20sourcelang:english&mode=timelinevolinfo&timespan=12w&maxrecords=250&sort=datedesc'
          ),
          urlSentiment = glue::glue(
            'https://api.gdeltproject.org/api/v2/doc/doc?query={parameter}:"{idImageWeb}"%20sourcelang:english&mode=ToneChart&timespan=12w&maxrecords=250&sort=datedesc'
          )
        )
    }

    if (code_book %>% str_to_lower() == "imagetags") {
      parameter <- 'imagetag'
      data <-
        data %>%
        mutate(
          urlNews1D = glue::glue(
            'https://api.gdeltproject.org/api/v2/doc/doc?query={parameter}:"{idImageTag}"%20sourcelang:english&mode=artlist&timespan=24h&maxrecords=250&sort=datedesc'
          ),
          urlNews1M = glue::glue(
            'https://api.gdeltproject.org/api/v2/doc/doc?query={parameter}:"{idImageTag}"%20sourcelang:english&mode=artlist&timespan=1m&maxrecords=250&sort=datedesc'
          ),
          urlNews3M = glue::glue(
            'httpss://api.gdeltproject.org/api/v2/doc/doc?query={parameter}:"{idImageTag}"%20sourcelang:english&mode=artlist&timespan=12w&maxrecords=250&sort=datedesc'
          ),
          urlTimelineVol = glue::glue(
            'https://api.gdeltproject.org/api/v2/doc/doc?query={parameter}:"{idImageTag}"%20sourcelang:english&mode=timelinevolinfo&timespan=12w&maxrecords=250&sort=datedesc'
          ),
          urlSentiment = glue::glue(
            'https://api.gdeltproject.org/api/v2/doc/doc?query={parameter}:"{idImageTag}"%20sourcelang:english&mode=ToneChart&timespan=12w&maxrecords=250&sort=datedesc'
          )
        )
    }


    data <-
      data %>%
      mutate(
        urlImage = "https://blog.gdeltproject.org/wp-content/uploads/2015-gdelt-api.png",
        idPanel = 1:n(),
        panel = trelliscopejs::img_panel(urlImage)
      ) %>%
      mutate_at(
        data %>% dplyr::select(dplyr::matches("^url")) %>% dplyr::select(-urlImage) %>% names(),
        funs(
          trelliscopejs::cog_href(., default_label = TRUE, default_active = TRUE)
        )
      )

    id_columns <-
      data %>% dplyr::select(dplyr::matches("^id|^url")) %>% dplyr::select(-c(urlImage, idPanel, dplyr::matches("idWBCode"))) %>% names()

    if (id_columns %>% length() == 1) {
      id_columns <- c('nameCodebook', id_columns)
    }

    title <-
      glue::glue("GDELT V2 API {parameter %>% str_to_upper()} CODES")

    if (!path %>% purrr::is_null()) {
      viz <-
        data %>%
        trelliscopejs::trelliscope(
          name = title,
          nrow = 1,
          ncol = 3,
          path = path,
          state = list(
            labels = c(id_columns),
            sort = list(sort_spec("idPanel"))
          )
        )
    } else {
      viz <-
        data %>%
        trelliscopejs::trelliscope(
          name = title,
          nrow = 1,
          ncol = 3,
          state = list(
            labels = c(id_columns),
            sort = list(sort_spec("idPanel"))
          )
        )
    }
    viz
  }

# highchart ---------------------------------------------------------------


.add_hc_group <-
  function(data) {
    gather_cols <-
      data %>%
      dplyr::select(-one_of(c("modeSearch", "periodtimeSearch"))) %>%
      dplyr::select(dplyr::matches("Search")) %>% names()

    df_keys <-
      data %>%
      gather_('search', 'term', gather_cols, na.rm = TRUE) %>%
      unite(itemSearch, 'search', 'term', sep = ": ") %>%
      mutate(idRow = 1:n()) %>%
      dplyr::select(idRow, itemSearch)

    data <-
      data %>%
      mutate(idRow = 1:n()) %>%
      left_join(df_keys) %>%
      dplyr::select(itemSearch, everything()) %>%
      tidyr::separate(
        col = 'itemSearch',
        into = c('itemQuery', 'valueQuery'),
        sep = '\\: '
      ) %>%
      suppressMessages() %>%
      dplyr::select(-idRow)

    data
  }

#' Title
#'
#' @param data
#' @param search_mode
#' @param include_title
#'
#' @return
#' @export
#'
#' @examples
.plot_ft_v2_highchart <-
  function(data,
           search_mode = 'TimelineVolInfo',
           include_title = TRUE) {
    data <-
      data %>%
      dplyr::mutate(typeChart = search_mode) %>%
      dplyr::select(-one_of(c(
        'countMaximumRecords', 'urlGDELTV2FTAPI', 'isOR'
      ))) %>%
      suppressMessages() %>%
      suppressWarnings()

    chart_type <-
      search_mode %>% str_to_lower()

    df_chart_info <-
      .get_mode_types() %>%
      mutate(modeSearch = modeSearch %>% str_to_lower()) %>%
      filter(modeSearch == chart_type)

    if (data %>% tibble::has_name("periodtimeSearch")) {
      data <-
        data %>%
        dplyr::rename(periodChart = periodtimeSearch)
    }

    if (data %>% tibble::has_name("datetimeStartSearch")) {
      data <-
        data %>%
        dplyr::rename(datetimeStart = datetimeStartSearch)
    }

    if (data %>% tibble::has_name("datetimeSearchEnd")) {
      data <-
        data %>%
        dplyr::rename(datetimeEnd = datetimeSearchEnd)
    }

    gather_cols <-
      data %>%
      dplyr::select(-dplyr::matches("modeSearch")) %>%
      dplyr::select(dplyr::matches("Search")) %>%
      suppressWarnings() %>%
      names()

    data <-
      data %>%
      gather_('search', 'term', gather_cols, na.rm = TRUE) %>%
      unite(itemSearch, 'search', 'term', sep = ": ") %>%
      dplyr::select(itemSearch, everything())

    if (data %>% tibble::has_name("urlArticle")) {
      data <-
        data %>%
        mutate(domainArticle = urlArticle %>% urltools::domain())

      data <-
        data %>%
        group_by(titleArticle, urlArticle) %>%
        mutate(idArticle = 1:n()) %>%
        ungroup() %>%
        filter(idArticle == min(idArticle)) %>%
        dplyr::select(-idArticle)
    }

    if (chart_type %>% str_detect("timeline")) {
      data <-
        data %>%
        group_by(itemSearch) %>%
        arrange((datetimeData)) %>%
        mutate(
          xAxis = datetime_to_timestamp(datetimeData),
          xAxisName = as.character(datetimeData),
          yAxis = value
        ) %>%
        distinct() %>%
        ungroup()
    }

    if (chart_type %>% str_detect("tonechart")) {
      data <-
        data %>%
        mutate(xAxis = idBIN,
               yAxis = countValue,
               xAxisName = idBIN)
    }


    if (df_chart_info$hasTooltip) {
      data <-
        data %>%
        mutate(
          htmlArticle = glue::glue(
            "<li><a href = '{urlArticle}' target = '_blank'> {titleArticle}</a></li>"
          )
        )

      data <-
        data %>%
        group_by(xAxis, itemSearch, typeChart, periodChart) %>%
        dplyr::transmute(
          yAxis = max(yAxis, na.rm = TRUE),
          htmlArticles = htmlArticle %>% str_c(collapse = '')
        ) %>%
        ungroup() %>%
        separate(
          itemSearch,
          into = c('item', 'value'),
          sep = ": ",
          remove = FALSE
        )

      y_axis_name <- df_chart_info$nameYAxis
      x_axis_name <- df_chart_info$nameXAxis

      if (x_axis_name %>% str_detect("date")) {
        data <- data %>%
          mutate(xAxisV = anytime::anytime(xAxis / 1000) %>% as.character())
      } else {
        data <- data %>%
          mutate(xAxisV = xAxis)
      }
      data <-
        data %>%
        mutate(
          htmlTooltip = glue::glue(
            "<div><p><strong>{item}:</strong> {value}</p><p><strong>{y_axis_name}:</strong> {yAxis}</p><p><strong>{x_axis_name}:</strong> {xAxisV}</p><span><ul>{htmlArticles}</ul></span></div>"
          ) %>% as.character()
        )
    }

    x_axis_name <- df_chart_info$nameXAxis
    y_axis_name <- df_chart_info$nameYAxis
    chart_type <- df_chart_info$typeVisualization
    type_x_axis <- df_chart_info$typeXAxis
    has_tool_tip <-  df_chart_info$hasTooltip

    if (data %>% tibble::has_name("periodChart")) {
      period_time_slug <-
        data$periodChart %>% unique()

      period_slug <-
        glue::glue("Over the Last {period_time_slug %>% str_to_title()}")
    } else{
      period_slug <- ""
    }

    if (data %>% tibble::has_name("datetimeStart")) {
      start <-
        data$datetimeStart %>% unique()

      end <-
        data$datetimeEnd %>% unique()

      date_slug <-
        glue::glue("from {start} to {end}")
    } else{
      date_slug <- ""
    }

    type <-
      data$typeChart %>% unique()
    leg_500_opts <- list(enabled = FALSE)
    leg_900_opts <-
      list(align = "right",
           verticalAlign = "middle",
           layout = "vertical")
    options(scipen = 999999)
    viz <-
      data %>%
      hchart(chart_type, hcaes(x = xAxis, y = yAxis, group = itemSearch)) %>%
      hc_add_theme(hc_theme_elementary()) %>%
      hc_xAxis(
        type = type_x_axis,
        gridLineWidth = 0,
        lineWidth = 1,
        lineColor = "black",
        tickWidth = 1,
        tickAmount = 15,
        tickColor = "black",
        title = list(text = x_axis_name, style = list(color = "black")),
        labels = list(style = list(color = "black"))
      ) %>%
      hc_yAxis(
        tickAmount = 8,
        lineColor = "transparent",
        minorGridLineWidth = 0,
        gridLineColor = "transparent",
        title = list(text = y_axis_name, style = list(color = "black")),
        labels = list(format = "{value}")
      ) %>%
      hc_legend(align = "left",
                verticalAlign = "bottom",
                layout = "horizontal") %>%
      hc_credits(
        enabled = TRUE,
        text = glue::glue("Data from GDELT Project via gdeltr2") %>% as.character(),
        href = "http://gdeltproject.org"
      ) %>%
      #hc_responsive(
      # rules = list(
      #  # remove legend if there is no much space
      # list(
      #  condition = list(maxWidth  = 500),
      # chartOptions = list(legend = leg_500_opts)
      #),
      # put legend on the right when there is much space
      #list(
      # condition = list(minWidth  = 900),
      #chartOptions = list(legend = leg_900_opts)
    #)
    #)
    #) %>%
    hc_exporting(enabled = TRUE) %>%
      hc_chart(zoomType = "x") %>%
      suppressWarnings()

    if (include_title) {
      more_than_1 <-
        data %>% select(itemSearch) %>% distinct() %>% nrow() > 1

      if (more_than_1) {
        title <-
          glue::glue("<h4>GDELT FT API V2 {type} Visualization {period_slug} {date_slug}</h4>") %>%
          as.character() %>%
          str_trim() %>%
          str_replace_all('\\ </h4>', '</h4>')
      } else {
        item <- data$item %>% unique()
        value <- data$value %>% unique()
        title <-
          glue::glue(
            "<h4>GDELT FT API V2 {type} Visualization for {item} of {value} {period_slug} {date_slug}</h4>"
          ) %>%
          as.character() %>%
          str_trim() %>%
          str_replace_all('\\ </h4>', '</h4>')
      }

      viz <-
        viz %>%
        hc_title(text = title,
                 useHTML = TRUE) %>%
        hc_subtitle(
          text = glue::glue("<em>Data as of {Sys.time()}</em>") %>% as.character(),
          useHTML = TRUE
        )

    }

    if (chart_type == "line") {
      viz <-
        viz %>%
        hc_tooltip() %>%
        hc_plotOptions(
          series = list(
            allowPointSelect = TRUE,
            marker = list(enabled = FALSE),
            cursor = "pointer",
            states = list(hover = list(enabled = TRUE)),
            stickyTracking = TRUE
          )
        )
    }

    if (has_tool_tip) {
      viz <-
        viz %>%
        hc_tooltip(
          pointFormat = "{point.htmlArticles}",
          headerFormat = "",
          useHTML = TRUE,
          crosshairs = F,
          shared = T,
          borderWidth = 1,
          hideDelay = 50,
          followPointer = FALSE,
          enabled = T,
          followTouchMove = TRUE,
          style = list(pointerEvents = "auto")
        ) %>%
        hc_chart(events = list(
          load = JS(
            "function(){ this.myTooltip = new Highcharts.Tooltip(this, this.options.tooltip);}"
          )
        ),
        showAxes = TRUE) %>%
        hc_plotOptions(
          series = list(
            allowPointSelect = TRUE,
            marker = list(enabled = FALSE),
            cursor = "pointer",
            events = list(
              click = JS(
                "function(evt) {
                         this.chart.myTooltip.refresh(evt.point, evt);}"
              ),
              mouseOut = JS("function() {this.chart.myTooltip.hide();}")
            ),
            stickyTracking = FALSE
          )
        )

    } else {
      viz <-
        viz %>%
        hc_tooltip(
          useHTML = TRUE,
          crosshairs = TRUE,
          shared = TRUE,
          borderWidth = 2,
          style = list(pointerEvents = "auto")
        )
    }
    viz
  }

#' Plot a highchart trelliscope
#'
#' @param data
#' @param remove_columns
#' @param group_columns
#' @param id_columns
#' @param include_title
#' @param trelliscope_parameters
#'
#' @return
#' @export
#' @import tidyr highcharter dplyr purrr anytime trelliscopejs rlang readr
#' @examples
plot_hc_trelliscope <-
  function(data,
           remove_columns = c('countMaximumRecords',
                              'urlGDELTV2FTAPI',
                              'isOR',
                              'urlArticleMobile'),
           group_columns = list(
             regex = NULL,
             columns = c('itemQuery', 'valueQuery', "periodtimeSearch"),
             exclude = NULL
           ),
           id_columns = list(
             is_except = FALSE,
             columns = c('itemQuery', 'valueQuery', "periodtimeSearch",  "termSearch"),
             regex = NULL
           ),
           include_title = TRUE,
           trelliscope_parameters = list(rows = 1,
                                         columns = 3,
                                         path = NULL)) {
    search_mode <-
      data %>% pull(modeSearch) %>% unique() %>% .[[1]]

    data <-
      data %>%
      .add_hc_group()

    if (data %>% tibble::has_name("periodtimeSearch")) {
      data <-
        data %>%
        dplyr::rename(periodChart = periodtimeSearch)

    }

    data <-
      data %>%
      .munge_for_trelliscope(remove_columns = remove_columns,
                             group_columns = group_columns)
    data <-
      data %>%
      mutate(plot = map_plot(
        data,
        ~ .plot_ft_v2_highchart(
          data = .x,
          search_mode = search_mode,
          include_title = include_title
        )
      )) %>%
      suppressWarnings()


    column_ids <-
      data %>%
      .get_trelliscope_id_columns(id_columns = id_columns) %>%
      suppressWarnings() %>%
      suppressMessages()

    search_mode <- data$data[[1]]$modeSearch %>% unique()

    title <-
      glue::glue("GDELT Full Text API {search_mode} Trelliscope")

    df_trelliscope_params <-
      trelliscope_parameters %>% flatten_df()

    has_path <-
      df_trelliscope_params %>% tibble::has_name("path")

    if (data %>% ncol() <= 3) {
      state_value <- NULL
    }  else {
      state_value <- list(labels = c(column_ids))
    }
    if (has_path) {
      path_loc <- df_trelliscope_params$path

      .build_folder(path = path_loc) %>%
        suppressWarnings()

      viz <-
        data %>%
        trelliscopejs::trelliscope(
          name = title,
          nrow = df_trelliscope_params$rows,
          ncol = df_trelliscope_params$columns,
          state = state_value,
          path = path_loc
        )
      return(viz)
    }

    viz <-
      data %>%
      trelliscope(
        name = title,
        nrow = df_trelliscope_params$rows,
        ncol = df_trelliscope_params$columns,
        state = state_value
      )

    viz
  }


# FreeText V2 -------------------------------------------------------------
.parse_timespan <- function(timespan = "24 hours") {
  if (timespan %>% purrr::is_null()) {
    df <-
      tibble(period_time = NA,
             period_timeframe = NA)
    return(df)
  }

  period_time <-
    timespan %>% readr::parse_number()
  period_timeframe <-
    timespan %>% str_split('\\ ') %>% flatten_chr() %>% str_to_lower() %>% {
      .[2]
    }
  tibble(period_time, period_timeframe)
}

.parse_datetimes <-
  function(dates = NULL) {
    if (dates %>% purrr::is_null()) {
      return(invisible())
    }

    if (dates %>% length() == 2) {
      datetime_start <- dates[1] %>% lubridate::ymd_hms()
      datetime_end <- dates[2]  %>% lubridate::ymd_hms()
      data <-
        tibble(datetime_start,
               datetime_end)
      return(data)
    }

    no_sep <-
      !dates %>% str_detect("\\ - ")

    if (no_sep) {
      stop("No separator identified")
    }

    dates <-
      dates %>% str_split('\\ - ') %>%
      purrr::flatten_chr()

    tibble(datetime_start = dates[[1]],
           datetime_end = dates[[2]])
  }

.munge_gkg_themes <- function(data) {
  data <-
    data %>%
    mutate(
      code2 = idGKGTheme %>% str_to_lower(),
      isWBCode = ifelse(code2 %>% str_detect("wb_"), T, F),
      isEconomicEvent = ifelse(code2 %>% str_detect("econ_"), T, F),
      isSocialEvent = ifelse(code2 %>% str_detect("soc"), T, F),
      isTaxEvent = ifelse(code2 %>% str_detect("tax_"), T, F),
      isSocialEvent = ifelse(code2 %>% str_detect("soc_"), T, F),
      isMilitaryEvent = ifelse(code2 %>% str_detect("military|mil_"), T, F),
      isGovernmentEvent = ifelse(code2 %>% str_detect("gov_|government"), T, F),
      isMedicalEvent = ifelse(code2 %>% str_detect("med_|medical"), T, F),
      isAgressionAct = ifelse(code2 %>% str_detect("act_"), T, F),
      isMediaEvent = ifelse(code2 %>% str_detect("media_|_media"), T, F),
      isEmergencyEvent = ifelse(code2 %>% str_detect("emerg_"), T, F),
      isMovement = ifelse(code2 %>% str_detect("movement_"), T, F),
      isCriminalEvent = ifelse(code2 %>% str_detect("crime|crm_"), T, F)
    ) %>%
    dplyr::select(-code2)

  wb_codes <-
    data %>%
    dplyr::filter(isWBCode)

  wb_codes <-
    wb_codes %>%
    mutate(idGKGTheme = idGKGTheme %>% sub('\\_', '\\.', .)) %>%
    separate(
      idGKGTheme,
      into = c('idDictionary', 'nameWBCode'),
      remove = F,
      sep = '\\.'
    ) %>%
    mutate(nameWBCode = nameWBCode %>% sub('\\_', '\\.', .)) %>%
    separate(
      nameWBCode,
      into = c('idWBCode', 'nameWBCode'),
      remove = T,
      sep = '\\.'
    ) %>%
    mutate(
      idWBCode = idWBCode %>% as.numeric,
      nameWBCode = nameWBCode %>% str_replace_all('\\_', ' ') %>% str_to_lower
    ) %>%
    dplyr::select(-idDictionary)

  non_wb <-
    data %>%
    dplyr::filter(!isWBCode)

  data <-
    non_wb %>%
    bind_rows(wb_codes)

  data <-
    data %>%
    mutate(idGKGTheme = idGKGTheme %>% str_replace_all('WB.', '\\WB_')) %>%
    dplyr::select(isWBCode, idGKGTheme, idWBCode, nameWBCode, everything())

  data
}

# http://blog.gdeltproject.org/gdelt-doc-2-0-api-debuts/

#' GDELT Full Text API 2.0 Codebooks
#'
#' @param code_book code book selection \itemize{
#' \item gkg: Global knowledge graph codes
#' \item imagetags: Image tag data store
#' \item imageweb: Image web data store
#' \item languages: Languages
#' \item countries: Countries with country code
#'
#' }
#' @param visualize_trelliscope if \code{TRUE} returns an
#' interactive trelliscope
#' @param path file path where you want to save the trelliscope if \code{TRUE}
#'
#' @return
#' @export
#' @import dplyr stringr purrr glue readr
#' @examples
#' get_gdelt_codebook_ft_api(code_book = "gkg")
#' get_gdelt_codebook_ft_api(code_book = "imagetags")
#' get_gdelt_codebook_ft_api(code_book = "imageweb")
#' get_gdelt_codebook_ft_api(code_book = "languages")
get_gdelt_codebook_ft_api <-
  function(code_book = 'gkg',
           visualize_trelliscope = FALSE,
           path = NULL) {
    df_codebooks <-
      tibble(
        nameCodeBook = c('gkg',
                         'imagetags',
                         'imageweb',
                         'languages',
                         'countries'),
        nameId = c(
          'idGKGTheme',
          'idImageTag',
          'idImageWeb',
          'idLanguage',
          'idCountry'
        ),
        urlCodeBook = c(
          "http://data.gdeltproject.org/api/v2/guides/LOOKUP-GKGTHEMES.TXT",
          'http://data.gdeltproject.org/api/v2/guides/LOOKUP-IMAGETAGS.TXT',
          'http://data.gdeltproject.org/api/v2/guides/LOOKUP-IMAGEWEBTAGS.TXT',
          'http://data.gdeltproject.org/api/v2/guides/LOOKUP-LANGUAGES.TXT',
          'http://data.gdeltproject.org/api/v2/guides/LOOKUP-COUNTRIES.TXT'
        )
      )
    code_book <- code_book %>% str_to_lower()
    codes <- df_codebooks$nameCodeBook %>% str_to_lower()
    if (!code_book %in% (codes)) {
      stop(str_c('Codebooks can only be:\n', str_c(codes, collapse = '\n')))
    }

    df_section <-
      df_codebooks %>%
      filter(nameCodeBook == code_book)

    data <-
      df_section$urlCodeBook %>%
      .read_codebook(id_name = df_section$nameId) %>%
      mutate(nameCodebook = code_book %>% str_to_upper()) %>%
      select(nameCodebook, everything()) %>%
      suppressMessages()

    if (code_book %>% str_to_lower() == "gkg") {
      data <- data %>% .munge_gkg_themes() %>% arrange(desc(idGKGTheme))
    }

    if (!visualize_trelliscope) {
      return(data)
    }

    if (visualize_trelliscope) {
      viz <-
        codebook_trelliscope(
          data = data,
          code_book = code_book %>% str_to_lower(),
          path = path
        )
      return(viz)
    } else {
      return(data)
    }
  }



.generate_free_text_api <-
  function(query_parameters = list(
    term = '"Brooklyn Nets"',
    domain = NULL,
    image_face_tone = NULL,
    image_num_faces = NULL,
    image_ocr = NULL,
    image_tag = NULL,
    image_web_tag = NULL,
    image_web_count = NULL,
    source_country = NULL,
    gkg_theme = NULL,
    tone = NULL,
    tone_absolute_value = NULL
  ),
  source_language = "English",
  use_or = FALSE,
  mode = 'ArtList',
  #ArtList, ImageCollage,  ImageCollageInfo, ImageCollageShare, TimelineVol, TimelineVolInfo, TimelineTone, TimelineLang, TimelineSourceCountry, ToneChart, WordCloudEnglish, WordCloudNative, WordCloudTheme, WordCloudImageTags, WordCloudImageWebTags
  format = 'JSON',
  # HTML, CSV, JSON, JSONP, RSS, RSS archive,
  timespan = "24 hours",
  dates = NULL,
  maximum_records = 250,
  translate = NULL,
  timeline_smooth = 5,
  sort_by = 'DateDesc') {
  base <- "https://api.gdeltproject.org/api/v2/doc/doc?query="

  for (i in seq_along(query_parameters)) {
    query_parameters[query_parameters[i] %>% names()] %>% is.na()
  }
  query_slug <-
    .parse_query(query_parameters = query_parameters)

  if (query_slug %>% length() > 1) {
    if (use_or) {
      qs <-
        query_slug %>% str_c(collapse  = "%20OR%20")
      query_slug <- glue::glue("({qs})")
    } else {
      query_slug <-
        query_slug %>% str_c(collapse  = "%20") %>% URLencode()
    }
  }

  if (source_language %>% length() > 0) {
    source_language_slug <-
      glue::glue("&sourcelang:{source_language}")
  } else {
    source_language_slug <- ""
  }

  mode_options <-
    c(
      "ArtList",
      "ImageCollage",
      "ImageCollageInfo",
      "ImageCollageShare",
      "TimelineVol",
      "TimelineVolInfo",
      "TimelineTone",
      "TimelineLang",
      "TimelineSourceCountry",
      "ToneChart",
      "WordCloudImageTags",
      "WordCloudImageWebTags"
    ) %>% str_to_lower()

  mode <-
    mode %>% str_to_lower()

  mode_slug <-
    .generate_slug(
      parameter = 'mode',
      sep = '=',
      value = mode,
      value_options = mode_options
    )

  format_options <-
    c("HTML", "CSV", "JSON", "JSONP", "RSS", "RSS archive", "") %>%
    str_to_lower()

  format_slug <-
    .generate_slug(
      parameter = 'format',
      sep = '=',
      value = format %>% str_to_lower(),
      value_options = format_options
    )

  if (timespan %>% length() > 0) {
    df_timespan <-
      timespan %>% .parse_timespan()

    period <- df_timespan$period_time

    metric <- df_timespan$period_timeframe %>% str_to_lower()

    id_metric <-
      case_when(
        metric %>% str_detect("minutes") ~ '',
        metric %>% str_detect("hour") ~ 'h',
        metric %>% str_detect("day") ~ 'd',
        metric %>% str_detect("week") ~ 'w',
        metric %>% str_detect("month") ~ 'm',
        metric %>% str_detect("year") ~ "y"
      )

    timespan_slug <-
      glue::glue("&timespan={period}{id_metric}")

  } else {
    timespan_slug <- ''
  }

  if (dates %>% length() > 0) {
    dates_df <-
      dates %>% .parse_datetimes()

    start_time <-
      dates_df$datetime_start %>%
      as.character() %>% str_replace_all('\\ |\\:|\\-', '')

    end_time <-
      dates_df$datetime_end %>%
      as.character() %>% str_replace_all('\\ |\\:|\\-', '')

    datetime_slug <-
      glue::glue('&startdatetime={start_time}&enddatetime={end_time}')
  } else {
    datetime_slug <- ''
  }

  sort_options <-
    c('DateDesc', 'DateAsc', 'DateAsc',
      'ToneAsc') %>%
    str_to_lower()

  sort_slug <-
    .generate_slug(
      parameter = 'sort',
      sep = '=',
      value = sort_by,
      value_options = sort_options
    )

  max_slug <-
    .generate_slug(
      parameter = 'maxrecords',
      sep = '=',
      value = maximum_records,
      value_options = 1:250
    )


  url_api <-
    glue(
      "{base}",
      "{query_slug}",
      "{source_language_slug}",
      "{mode_slug}",
      "{format_slug}",
      "{timespan_slug}",
      "{datetime_slug}",
      "{max_slug}",
      "{sort_slug}"
    ) %>%
    str_to_lower()

  url_api
  }


.get_gdelt_ft_api_names <-
  function() {
    df_name <-
      tibble(
        nameGDELT = c(
          "bin",
          "count",
          "date",
          "domain",
          "imageurl",
          "imagewebcount",
          "imageweburls",
          "label",
          "language",
          "seendate",
          "series",
          "socialimage",
          "sourcearticleurl",
          "sourcecountry",
          "timelapsesec",
          "title",
          "url",
          "url_mobile",
          "value"
        ),
        nameActual = c(
          "idBIN",
          "countValue",
          "datetimeData",
          "domainArticle",
          "urlImage",
          "countImageUses",
          "urlsImageWeb",
          "labelTime",
          "languageArticle",
          "datetimeArticle",
          "nameSeries",
          "urlImage",
          "urlArticle",
          "countryArticle",
          "countTimelapseSeconds",
          "titleArticle",
          "urlArticle",
          "urlArticleMobile",
          "value"
        )

      )
    df_name
  }

.parse_json_api_2 <-
  function(url = "http://api.gdeltproject.org/api/v2/doc/doc?query=domain:netsdaily.com&timespan=1m&maxrecords=250&format=json") {
    data <-
      url %>%
      jsonlite::fromJSON(simplifyDataFrame = TRUE)


    data <-
      data[[names(data)]] %>%
      as_tibble()

    if (data %>% tibble::has_name("imageweburls")) {
      df_urls <-
        seq_along(data$imageweburls) %>%
        future_map_dfr(function(x) {
          value <- data$imageweburls[[x]]

          if (value %>% length() == 0) {
            return(tibble(idRow = x,
                          imageweburls = NA))
          }
          values <- value %>% str_c(collapse = ', ')
          tibble(idRow = x, imageweburls = values)
        })

      data <-
        data %>%
        mutate(idRow = 1:n()) %>%
        dplyr::select(-imageweburls) %>%
        left_join(df_urls) %>%
        suppressMessages() %>%
        dplyr::select(-idRow)
    }

    if (data %>% tibble::has_name("series")) {
      data <-
        data %>%
        tidyr::unnest()
    }

    if (data %>% tibble::has_name("toparts")) {
      data <-
        data %>%
        unnest()
    }
    df_name <-
      .get_gdelt_ft_api_names()

    actual_names <-
      names(data) %>%
      map_chr(function(x) {
        df_name %>%
          filter(nameGDELT == x) %>%
          pull(nameActual)
      })

    data <-
      data %>%
      purrr::set_names(actual_names)

    data <-
      data %>%
      mutate_if(is.character,
                funs(ifelse(. == '', NA, .)))

    has_datetime <-
      data %>% names() %>% str_detect("^datetime[A-Z]") %>% sum(na.rm = TRUE) > 0

    if (has_datetime) {
      data <-
        data %>%
        mutate_at(
          .vars = data %>% dplyr::select(dplyr::matches("^datetime[A-Z]")) %>% names(),
          funs(
            . %>% lubridate::ymd_hms() %>% lubridate::with_tz(Sys.timezone())
          )
        )
    }

    data
  }

.generate_v2_url_df <-
  function(term = NULL,
           domain = NULL,
           image_face_tone = NULL,
           image_num_faces = NULL,
           image_ocr = NULL,
           image_tag = NULL,
           image_web_tag = NULL,
           image_web_count = NULL,
           source_country = NULL,
           source_language = "English",
           gkg_theme = NULL,
           tone = NULL,
           tone_absolute_value = NULL,
           use_or = FALSE,
           mode = 'ArtList',
           #ArtList, ImageCollage,  ImageCollageInfo, ImageCollageShare, TimelineVol, TimelineVolInfo, TimelineTone, TimelineLang, TimelineSourceCountry, ToneChart, WordCloudEnglish, WordCloudNative, WordCloudTheme, WordCloudImageTags, WordCloudImageWebTags
           format = "JSON",
           quote_terms = F,
           timespan = "24 hours",
           dates = NULL,
           maximum_records = 250,
           translate = NULL,
           timeline_smooth = 5,
           sort_by = 'DateDesc',
           nest_data = FALSE,
           return_message = TRUE) {
    if (!gkg_theme %>% purrr::is_null()) {
      if (gkg_theme %>% is.na()) {
        gkg_theme <- NULL
      }
    }


    if (!dates %>% purrr::is_null()) {
      timespan <- NULL
    }

    if (length(term) > 0 && quote_terms) {
      term <-
        glue::glue('"{term}"') %>% as.character()
    }

    query_params <-
      list(
        term = term,
        domain = domain,
        image_face_tone = image_face_tone,
        image_num_faces = image_num_faces,
        image_ocr = image_ocr,
        image_tag = image_tag,
        image_web_tag = image_web_tag,
        image_web_count = image_web_count,
        source_country = source_country,
        gkg_theme = gkg_theme,
        tone = tone,
        tone_absolute_value = tone_absolute_value
      )

    for (i in seq_along(query_params)) {
      query_params[query_params[i] %>% names()] %>% is.na()
    }


    if (query_params %>% flatten_df() %>% ncol() == 0) {
      stop("Please enter query parameters")
    }

    df_query_params <-
      query_params %>%
      flatten_df() %>%
      dplyr::select(which(colMeans(is.na(.)) < 1))

    df_search <-
      df_query_params %>%
      tidyr::gather(item, value) %>%
      mutate(
        item = item %>% str_replace_all('\\_', ''),
        item = str_c(item, "Search", sep = '')
      ) %>%
      spread(item, value) %>%
      mutate(
        isOR = use_or,
        modeSearch = mode,
        countMaximumRecords = maximum_records
      )

    if (mode %>% str_to_lower() %>% str_detect("timeline")) {
      df_search <-
        df_search %>%
        mutate(timelineSmoothPeriod = timeline_smooth)
    }

    url <-
      .generate_free_text_api(
        query_parameters = query_params,
        use_or = use_or,
        mode = mode,
        format = format,
        timespan = timespan,
        dates = dates,
        source_language = source_language,
        maximum_records = maximum_records,
        translate = translate,
        timeline_smooth = timeline_smooth,
        sort_by = sort_by
      )

    url <-
      url %>% str_replace_all("\\ ", "%20")


    df_search <-
      df_search %>%
      mutate(urlGDELTV2FTAPI = url)


    if (timespan %>% length() > 0) {
      df_search <-
        df_search %>%
        mutate(periodtimeSearch = timespan)
    }

    if (!dates %>% purrr::is_null()) {
      datetimes <- dates %>% str_split("\\ - ") %>% flatten_chr()

      df_search <-
        df_search %>%
        mutate(datetimeStartSearch = datetimes[[1]],
               datetimeEndSearch = datetimes[[2]])
    }

    df_search

  }

.parse_v2_urls <-
  function(urls = c(
    "http://api.gdeltproject.org/api/v2/doc/doc?query=brooklyn%20nets%20sourcelang:english&mode=artlist&format=json&timespan=12w&maxrecords=250&sort=datedesc",
    "http://api.gdeltproject.org/api/v2/doc/doc?query=domain:netsdaily.com%20sourcelang:english&mode=artlist&format=json&timespan=12w&maxrecords=250&sort=datedesc",
    "http://api.gdeltproject.org/api/v2/doc/doc?query=theme:econ_bitcoin%20sourcelang:english&mode=artlist&format=json&timespan=12w&maxrecords=250&sort=datedesc"
  ),
  return_message = TRUE) {
    df <-
      tibble()

    .parse_json_api_2_safe <-
      purrr::possibly(.parse_json_api_2, tibble())

    success <- function(res) {
      if (return_message) {
        list("Parsing: ", res$url, "\n") %>% purrr::reduce(paste0) %>% cat(fill = T)
      }

      data <-
        res$url %>%
        .parse_json_api_2_safe() %>%
        mutate(urlGDELTV2FTAPI = res$url)

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


.query_gdelt_ft_v2_api <-
  function(terms = "Brooklyn Nets",
           domains = "netsdaily.com",
           images_face_tone = NA,
           images_num_faces = NA,
           images_ocr = NA,
           images_tag = NA,
           images_web_tag = NA,
           images_web_count = NA,
           source_languages = "English",
           source_countries = NA,
           gkg_themes = NA,
           tone = NA,
           tone_absolute_value = NA,
           use_or = FALSE,
           modes = 'ArtList',
           #ArtList, ImageCollage,  ImageCollageInfo, ImageCollageShare, TimelineVol, TimelineVolInfo, TimelineTone, TimelineLang, TimelineSourceCountry, ToneChart, WordCloudEnglish, WordCloudNative, WordCloudTheme, WordCloudImageTags, WordCloudImageWebTags
           timespans = "12 weeks",
           dates = NA,
           quote_terms = F,
           maximum_records = 250,
           translate = NULL,
           timeline_smooth = 5,
           sort_by = 'DateDesc',
           nest_data = FALSE,
           return_message = TRUE) {
    df_terms <-
      list(
        tibble(term = terms),
        tibble(domain = domains),
        tibble(image_ocr = images_ocr),
        tibble(image_face_tone = images_face_tone),
        tibble(image_num_faces = images_num_faces),
        tibble(image_web_tag = images_web_tag),
        tibble(image_tag = images_tag),
        tibble(gkg_theme = gkg_themes),
        tibble(image_web_count = images_web_count)
      ) %>%
      purrr::reduce(bind_rows) %>%
      distinct()

    if (!dates %>% is.na()) {
      timespans <- NA
    }

    col_count <- ncol(df_terms)

    df_terms <-
      df_terms %>% purrrlyr::by_row(is.na) %>% mutate(pctNA = (.out %>% map_dbl(sum)) / col_count) %>%
      filter(pctNA < 1) %>%
      dplyr::select(-c(.out, pctNA))

    df_terms <-
      df_terms %>%
      .odd_expand(column_name = "timespan", column_values = timespans)

    df_terms <-
      df_terms %>%
      .odd_expand(column_name = "dates", column_values = dates)


    df_terms <-
      df_terms %>%
      .odd_expand(column_name = "source_language", column_values = source_languages)


    df_terms <-
      df_terms %>%
      .odd_expand(column_name = "source_country", column_values = source_countries)

    df_terms <-
      df_terms %>%
      .odd_expand(column_name = "mode", column_values = modes)

    .generate_v2_url_df_safe <-
      purrr::possibly(.generate_v2_url_df, tibble())

    all_url_df <-
      1:nrow(df_terms) %>%
      future_map_dfr(function(x) {
        df_row <-
          df_terms %>%
          slice(x)
        if (df_row$dates %>% is.na()) {
          date <- NULL
        } else {
          date <- df_row$dates
        }

        .generate_v2_url_df(
          term = df_row$term,
          domain = df_row$domain,
          image_face_tone = df_row$image_face_tone,
          image_num_faces = df_row$image_num_faces,
          image_ocr = df_row$image_ocr,
          image_tag = df_row$image_tag,
          image_web_tag = df_row$image_web_tag,
          image_web_count = df_row$image_web_count,
          source_country = df_row$source_country,
          source_language = df_row$source_language,
          quote_terms = quote_terms,
          gkg_theme = df_row$gkg_theme,
          tone = tone,
          tone_absolute_value = tone_absolute_value,
          use_or = use_or,
          mode = df_row$mode,
          dates = date,
          timespan = df_row$timespan,
          maximum_records = maximum_records,
          translate = translate,
          timeline_smooth = timeline_smooth,
          sort_by = sort_by,
          nest_data = nest_data,
          return_message = return_message
        )

      }) %>%
      dplyr::select(which(colMeans(is.na(.)) < 1)) %>%
      distinct()

    search_params <-
      all_url_df %>% dplyr::select(dplyr::matches("Search")) %>% names()

    search_params <- c('modeSearch', search_params) %>% unique()

    all_url_df <-
      all_url_df %>%
      dplyr::select(one_of(search_params), everything())
    .parse_v2_urls_safe <-
      purrr::possibly(.parse_v2_urls, tibble())

    all_data <-
      all_url_df$urlGDELTV2FTAPI %>%
      .parse_v2_urls_safe(return_message = return_message)

    all_data <-
      all_data %>% tidyr::nest(-urlGDELTV2FTAPI)

    names(all_data)[[2]] <-
      'dataSearch'

    all_data <-
      all_url_df %>%
      left_join(all_data) %>%
      suppressMessages()

    all_data <-
      all_data %>%
      mutate(countRows = dataSearch %>% map_dbl(length)) %>%
      filter(countRows > 0) %>%
      dplyr::select(-countRows)


    if (!nest_data) {
      all_data <-
        all_data %>%
        tidyr::unnest()
    }

    all_data
  }

#' Query GDELT V2 Full Text API
#'
#' Queries GDELT's full text API for
#' user specified terms, machine learned items,
#' web domains and more.  Users can return raw data
#' or a Trelliscope of visual results.  Data is a available
#' in rolling three month periods.
#'
#' @param terms a vector of terms
#' @param domains a vector of webdomains
#' @param images_face_tone vector of facial tone scores
#' @param images_num_faces vector of face count
#' @param images_ocr vector of words to search for OCR'd text
#' @param images_tag vector of image tags from the image tag code book.
#' use \code{get_gdelt_codebook_ft_api(code_book = "imagetag"))} for options
#' @param images_web_tag vector of image tags from the image web tag code book.
#' use \code{get_gdelt_codebook_ft_api(code_book = "imageweb"))}
#' @param images_web_count numeric vector of number of times photo appeared
#' @param source_countries character source countries
#' #' see \code{get_gdelt_codebook_ft_api(code_book = "countries")} for options
#' @param gkg_themes global knowledge graph theme
#' #' use \code{get_gdelt_codebook_ft_api(code_book = "gkg"))} for options
#' @param tone numeric tone - default (NA)
#' @param tone_absolute_value numeric tone absolute value (default NA)
#' @param use_or if \code{TRUE} chains multiple items using and or statement
#' else chained by and
#' @param modes API search mode \itemize{
#' \item  ArtList - data frame of articles for specified terms/domains/webtags/imagewebtags and OCR'd text (default)
#' \item ImageCollage - data frame of images from for specified terms/domains/webtags/imagewebtags and OCR'd text
#' \item ImageCollageInfo - data frame of images from the article for specified terms/domains/webtags/imagewebtags and OCR'd text, includes information on age of the photo and number of uses
#' \item ImageCollageShare - data frame of images shared on social media from the article for specified terms/domains/webtags/imagewebtags and OCR'd text, includes information on age of the photo and number of uses
#' \item TimelineVol - Timeline of of article volume for specified terms/domains/webtags/imagewebtags and OCR'd text
#' \item TimelineVolInfo - Timeline of of article volume with article information for specified terms/domains/webtags/imagewebtags and OCR'd text
#' \item TimelineTone - Timeline of of article tone for specified terms/domains/webtags/imagewebtags and OCR'd text
#' \item TimelineLang - Timeline of of article language for specified terms/domains/webtags/imagewebtags and OCR'd text
#' \item TimelineSourceCountry - Timeline of of article sourcelanguage for specified terms/domains/webtags/imagewebtags and OCR'd text
#' \item ToneChart - histogram of binned counts by tone for specified terms/domains/webtags/imagewebtags and OCR'd text
#' \item WordCloudNative - word cloud of native text for specified specified terms/domains/webtags/imagewebtags and OCR'd text
#' \item WordCloudImageTags - word cloud of resolved imagetags for specified terms/domains/webtags/imagewebtags and OCR'd text
#' \item WordCloudImageWebTags - word cloud of resolved image web tags for specified terms/domains/webtags/imagewebtags and OCR'd text
#' }
#' @param timespans character vector of the time frame - no more than 52 weeks -
#' default is 24 hours
#' acceptable periods include: \itemize{
#' \item hours (default)
#' \item minutes
#' \item weeks
#' \item months
#' \item years
#' }
#' @param dates vector of dates in YMD HMS format, separated by \code{ - }
#' you can use the \code{generate_dates()} function to generate a vector of
#' default \code{NULL}
#' @param maximum_records Number between 1 and 250
#' @param source_languages  source languages
#' @param translate if \code{TRUE} translates
#' @param timeline_smooth if \code{mode} is a timeline
#' @param sort_by sorting method \itemize{
#' \item DateDesc - descending by date (default)
#' \item DateAsc - ascending by date
#' \item ToneDesc - descending tone
#' }
#' @param visualize_results if \code{TRUE} returns a visualization
#' @param trelliscope_parameters list of parameters to pass along to trelliscope \itemize{
#' \item path: if not \code{NULL} the path to save the trelliscope
#' \item rows: rows for trelliscope
#' \item columns: columns for trelliscope
#' \item id_columns: initial columns
#' }
#' @param nest_data if \code{TRUE} returns a nested \code{tibble()}
#' @param return_message if \code{TRUE} returns a message
#' @param ... - additional parameters
#'
#' @import tidyr dplyr rlang highcharter trelliscopejs anytime lubridate purrr purrrlyr tibble glue stringr jsonlite
#' @return a \code{tibble} or a form of visualization
#' @export
#'
#' @examples
#' terms <- c('"Brooklyn Nets"', '"Donovan Mitchell"', 'Blackston Real Estate', '"Tom Brady"')
#' web_sites <- c("realdeal.com", "netsdaily.com", "wsj.com", "archdaily.com", "alphr.com")
#' get_data_ft_v2_api(terms = terms, domains = web_sites, timespans = "28 Weeks")

get_data_ft_v2_api <-
  function(terms = NA,
           domains = NA,
           images_face_tone = NA,
           images_num_faces = NA,
           images_ocr = NA,
           images_tag = NA,
           images_web_tag = NA,
           images_web_count = NA,
           source_countries = NA,
           source_languages = "English",
           gkg_themes = NA,
           tone = NA,
           tone_absolute_value = NA,
           use_or = FALSE,
           modes = 'ArtList',
           #ArtList, ImageCollage,  ImageCollageInfo, ImageCollageShare, TimelineVol, TimelineVolInfo, TimelineTone, TimelineLang, TimelineSourceCountry, ToneChart, WordCloudEnglish, WordCloudNative, WordCloudTheme, WordCloudImageTags, WordCloudImageWebTags
           timespans = c("24 hours"),
           dates = NA,
           quote_terms = F,
           maximum_records = 250,
           translate = NULL,
           timeline_smooth = 5,
           sort_by = 'DateDesc',
           visualize_results = TRUE,
           trelliscope_parameters = list(
             path = NULL,
             rows = 1,
             columns = 2,
             id_columns = NULL,
             group_columns = NULL
           ),
           nest_data = FALSE,
           return_message = TRUE,
           ...) {
    if (terms %>% purrr::is_null()) {
      terms <- NA
    }

    if (domains %>% purrr::is_null()) {
      domains <- NA
    }

    search_options <- c(
      terms,
      domains,
      images_face_tone,
      images_num_faces,
      images_web_count,
      images_web_tag,
      gkg_themes
    )

    is_not_artlist <-
      !modes %>% str_to_lower() %>% str_detect('artlist') %>%
      sum(na.rm = TRUE) > 0
    if (search_options[!search_options %>% is.na()] %>% length() <= 1 &&
        visualize_results && is_not_artlist) {
      stop(
        "Need more than 1 search parameter to create a trelliscope please add a term, webdomain, theme, imagetag, imagewebtag or anyother V2 api search parameter"
      )
    }

    .query_gdelt_ft_v2_api_safe <-
      purrr::possibly(.query_gdelt_ft_v2_api, tibble())

    all_data <-
      .query_gdelt_ft_v2_api_safe(
        terms = terms,
        domains = domains,
        images_face_tone = images_face_tone,
        images_num_faces = images_num_faces,
        images_ocr = images_ocr,
        images_tag = images_tag,
        images_web_tag = images_web_tag,
        quote_terms = quote_terms,
        images_web_count = images_web_count,
        source_countries = source_countries,
        source_languages = source_languages,
        gkg_themes = gkg_themes,
        tone = tone,
        tone_absolute_value = tone_absolute_value,
        use_or = use_or,
        modes = modes,
        timespans = timespans,
        dates = dates,
        maximum_records = maximum_records,
        translate = translate,
        timeline_smooth = timeline_smooth,
        sort_by = sort_by,
        nest_data = nest_data,
        return_message = return_message
      )

    if (all_data %>% has_name("datetimeArticle")) {
      all_data <-
        all_data %>%
        arrange(desc(datetimeArticle))
    }

    if (!visualize_results) {
      return(all_data)
    }

    if (dates %>% purrr::is_null()) {
      dates <- NA
    }

    if (!dates %>% is.na() %>% suppressWarnings()) {
      timespans <- NA
    }

    all_data <-
      all_data %>%
      dplyr::select(-one_of(c("isOR", "countMaximumRecords"))) %>%
      suppressMessages()

    all_data %>%
      plot_trelliscopes(trelliscope_parameters = trelliscope_parameters)
  }




# bundle ------------------------------------------------------------------

#' Generate Trelliscope Bundle
#'
#' Generates a bundle of trelliscopes for publishing to the web
#'
#' @param include_image_panel if \code{TRUE} includes ArtList
#' @param include_timeline_info  if \code{TRUE} includes TimelineInfo
#' @param include_timeline_tone if \code{TRUE} includes Timeline tone
#' @param include_sentiment_bin if \code{TRUE} includes sentiment bin
#' @param wordcloud_modes Wordcloud items \itemize{
#' \item NA - none
#' \item WordCloudNative - word cloud of native text for specified specified terms/domains/webtags/imagewebtags and OCR'd text
#' \item WordCloudImageTags - word cloud of resolved imagetags for specified terms/domains/webtags/imagewebtags and OCR'd text
#' \item WordCloudImageWebTags - word cloud of resolved image web tags for specified terms/domains/webtags/imagewebtags and OCR'd text

#' }
#' @param terms a vector of terms
#' @param domains a vector of webdomains
#' @param images_face_tone vector of facial tone scores
#' @param images_num_faces vector of face count
#' @param images_ocr vector of words to search for OCR'd text
#' @param images_tag vector of image tags from the image tag code book.
#' use \code{get_gdelt_codebook_ft_api(code_book = "imagetag"))} for options
#' @param images_web_tag vector of image tags from the image web tag code book.
#' use \code{get_gdelt_codebook_ft_api(code_book = "imageweb"))}
#' @param images_web_count numeric vector of number of times photo appeared
#' @param source_countries character source countries
#' #' see \code{get_gdelt_codebook_ft_api(code_book = "countries")} for options
#' @param gkg_themes global knowledge graph theme
#' #' use \code{get_gdelt_codebook_ft_api(code_book = "gkg"))} for options
#' @param tone numeric tone - default (NA)
#' @param tone_absolute_value numeric tone absolute value (default NA)
#' @param timespans character vector of the time frame - no more than 12 weeks -
#' default is 24 hours
#' acceptable periods include: \itemize{
#' \item hours (default)
#' \item minutes
#' \item weeks
#' \item months
#' }

#' @param dates vector of dates in YMD HMS format, separated by \code{ - }
#' you can use the \code{generate_dates()} function to generate a vector of
#' default \code{NULL}
#' @param maximum_records Number between 1 and 250
#' @param translate
#' @param timeline_smooth if \code{mode} is a time-line
#' @param sort_by sorting method \itemize{
#' \item DateDesc - descending by date (default)
#' \item DateAsc - ascending by date
#' \item ToneDesc - descending tone
#' }
#' @param wordcloud_timespans timespans for wordcloud
#' @param base_path base path to save the trelliscopes
#' @param rows rows of
#' @param columns
#' @param id_columns
#' @param group_columns
#' @param return_message
#'
#' @return
#' @export
#' @import tidyr dplyr rlang highcharter trelliscopejs anytime lubridate purrr purrrlyr tibble glue stringr jsonlite
#' @examples
generate_trelliscope_bundle <-
  function(include_image_panel = TRUE,
           include_timeline_info = TRUE,
           include_timeline_tone = FALSE,
           include_sentiment_bin = TRUE,
           wordcloud_modes = c("WordCloudImageTags",
                               "WordCloudImageWebTags"),
           terms = NA,
           domains = NA,
           gkg_themes = NA,
           images_ocr = NA,
           images_tag = NA,
           images_web_tag = NA,
           images_face_tone = NA,
           images_num_faces = NA,
           images_web_count = NA,
           timespans = c("24 hours"),
           wordcloud_timespans  = c("1 month"),
           dates = NA,
           source_countries = NA,
           tone = NA,
           tone_absolute_value = NA,
           maximum_records = 250,
           translate = NULL,
           timeline_smooth = 5,
           sort_by = 'DateDesc',
           base_path = NULL,
           rows = 1,
           columns = 2,
           id_columns = NULL,
           group_columns = NULL,
           return_message = TRUE) {
    if (base_path %>% purrr::is_null()) {
      stop("Please enter a path to save the trelliscopes")
    }

    if (base_path %>% is.na()) {
      stop("Please enter a path to save the trelliscopes")
    }
    oldwd <- getwd()
    setwd("~")
    .build_folder(path = base_path)
    data <- tibble()

    if (include_image_panel) {
      path <-
        glue::glue("{base_path}/image_panel") %>% as.character() %>% str_replace_all('//', '/')

      trelliscopeImage <-
        get_data_ft_v2_api(
          terms = terms,
          domains = domains,
          images_face_tone = images_face_tone,
          images_num_faces = images_num_faces,
          images_ocr = images_ocr,
          images_tag = images_tag,
          images_web_tag = images_web_tag,
          images_web_count = images_web_count,
          source_countries = source_countries,
          gkg_themes = gkg_themes,
          tone = tone,
          tone_absolute_value = tone_absolute_value,
          use_or = FALSE,
          modes = "ArtList",
          timespans = timespans,
          dates = dates,
          maximum_records = maximum_records,
          translate = translate,
          timeline_smooth = timeline_smooth,
          sort_by = sort_by,
          nest_data = FALSE,
          return_message = TRUE,
          visualize_results = TRUE,
          trelliscope_parameters = list(
            path = path,
            rows = rows,
            columns = columns,
            id_columns  = id_columns,
            group_columns = group_columns
          )
        )

      data <-
        data %>%
        bind_rows(tibble(
          nameTrelliscope = "Image",
          dataTrelliscope = list(trelliscopeImage)
        ))
    }

    if (include_sentiment_bin) {
      path <-
        glue::glue("{base_path}/sentiment_bin") %>% as.character() %>% str_replace_all('//', '/')
      trelliscopeBIN <-
        get_data_ft_v2_api(
          terms = terms,
          domains = domains,
          images_face_tone = images_face_tone,
          images_num_faces = images_num_faces,
          images_ocr = images_ocr,
          images_tag = images_tag,
          images_web_tag = images_web_tag,
          images_web_count = images_web_count,
          source_countries = source_countries,
          gkg_themes = gkg_themes,
          tone = tone,
          tone_absolute_value = tone_absolute_value,
          use_or = FALSE,
          modes = "ToneChart",
          timespans = timespans,
          dates = dates,
          maximum_records = maximum_records,
          translate = translate,
          timeline_smooth = timeline_smooth,
          sort_by = sort_by,
          nest_data = FALSE,
          return_message = TRUE,
          visualize_results = TRUE,
          trelliscope_parameters = list(
            path = path,
            rows = rows,
            columns = columns
          )
        )

      data <-
        data %>%
        bind_rows(tibble(
          nameTrelliscope = "BIN",
          dataTrelliscope = list(trelliscopeBIN)
        ))

      glue::glue("\n
                 Saved Sentiment Panel Trelliscope to {path}
                 \n") %>% cat(fill = T)
    }

    if (include_timeline_info) {
      path <-
        glue::glue("{base_path}/timeline_info") %>% as.character() %>% str_replace_all('//', '/')

      trelliscopeTimeline <-
        gdeltr2::get_data_ft_v2_api(
          terms = terms,
          domains = domains,
          images_face_tone = images_face_tone,
          images_num_faces = images_num_faces,
          images_ocr = images_ocr,
          images_tag = images_tag,
          images_web_tag = images_web_tag,
          images_web_count = images_web_count,
          source_countries = source_countries,
          gkg_themes = gkg_themes,
          tone = tone,
          tone_absolute_value = tone_absolute_value,
          modes = "timelinevolinfo",
          timespans = "52 Weeks",
          dates = dates,
          maximum_records = maximum_records,
          translate = translate,
          timeline_smooth = timeline_smooth,
          sort_by = sort_by,
          visualize_results = T,
          trelliscope_parameters = list(
            path = path,
            rows = 1,
            columns = 1
          )
        )

      data <-
        data %>%
        bind_rows(tibble(
          nameTrelliscope = "Timelineinfo",
          dataTrelliscope = list(trelliscopeTimeline)
        ))


      glue::glue("\n
                 Saved Timline Info Panel Trelliscope to {path}
                 \n") %>% cat(fill = T)
    }

    if (include_timeline_tone) {
      path <-
        glue::glue("{base_path}/timeline_tone") %>% as.character() %>% str_replace_all('//', '/')
      trelliscopeTimelinetone <-
        get_data_ft_v2_api(
          terms = terms,
          domains = domains,
          images_face_tone = images_face_tone,
          images_num_faces = images_num_faces,
          images_ocr = images_ocr,
          images_tag = images_tag,
          images_web_tag = images_web_tag,
          images_web_count = images_web_count,
          source_countries = source_countries,
          gkg_themes = gkg_themes,
          tone = tone,
          tone_absolute_value = tone_absolute_value,
          modes = "TimelineTone",
          timespans = "52 Weeks",
          dates = dates,
          maximum_records = maximum_records,
          translate = translate,
          timeline_smooth = timeline_smooth,
          sort_by = sort_by,
          nest_data = FALSE,
          return_message = TRUE,
          visualize_results = TRUE,
          trelliscope_parameters = list(
            path = path,
            rows = rows,
            id_columns = id_columns,
            group_columns = group_columns,
            columns = columns
          )
        )

      data <-
        data %>%
        bind_rows(tibble(
          nameTrelliscope = "TimelineTone",
          dataTrelliscope = list(trelliscopeTimelinetone)
        ))

      glue::glue("\n
                 Saved Timline Tone Panel Trelliscope to {path}
                 \n") %>% cat(fill = T)
    }

    has_wordclouds <-
      (wordcloud_modes %>% length()) > 0 &
      (!wordcloud_modes %>% is.na() %>% sum(na.rm = TRUE)  > 0)

    if (has_wordclouds) {
      path <-
        glue::glue("{base_path}/wordclouds") %>% as.character() %>% str_replace_all('//', '/')
      trelliscopeWordcloud <-
        get_data_ft_v2_api(
          terms = terms,
          domains = domains,
          images_face_tone = images_face_tone,
          images_num_faces = images_num_faces,
          images_ocr = images_ocr,
          images_tag = images_tag,
          images_web_tag = images_web_tag,
          images_web_count = images_web_count,
          source_countries = source_countries,
          gkg_themes = gkg_themes,
          tone = tone,
          tone_absolute_value = tone_absolute_value,
          modes = wordcloud_modes,
          timespans = wordcloud_timespans,
          dates = dates,
          maximum_records = maximum_records,
          translate = translate,
          timeline_smooth = timeline_smooth,
          sort_by = sort_by,
          return_message = TRUE,
          visualize_results = TRUE,
          trelliscope_parameters = list(
            path = path,
            rows = 1,
            id_columns = id_columns,
            group_columns = group_columns,
            columns = 1
          )
        )

      data <-
        data %>%
        bind_rows(tibble(
          nameTrelliscope = "Wordcloud",
          dataTrelliscope = list(trelliscopeWordcloud)
        ))

    }
    data$dataTrelliscope %>% walk(print)
    setwd(oldwd)
    return(data)
  }



# domains -----------------------------------------------------------------

#' GDELT Outlets by Country
#'
#' GDELT news outlets as of May 2018
#'
#' @return a \code{tibble}
#' @export
#'
#' @examples
#' dictionary_country_domains()
dictionary_country_domains <- function() {
  data <-
    "http://data.gdeltproject.org/blog/2018-news-outlets-by-country-may2018-update/MASTER-GDELTDOMAINSBYCOUNTRY-MAY2018.TXT" %>%
    read_tsv(col_names = F)

  data %>%
    purrr::set_names(c("domainSite", "slugCountry", "nameCountry"))
}


#' GDELT V2 API URL Df
#'
#' Creates data frame of gdeltv2 API queries.
#'
#' @param terms
#' @param domains
#' @param image_face_tone
#' @param image_num_faces
#' @param image_ocr
#' @param image_tag
#' @param image_web_tag
#' @param image_web_count
#' @param source_country
#' @param source_language
#' @param gkg_theme
#' @param tone
#' @param tone_absolute_value
#' @param use_or
#' @param mode
#' @param timespan
#' @param format
#' @param dates
#' @param maximum_records
#' @param translate
#' @param timeline_smooth
#' @param sort_by
#' @param return_message
#' @param quote_terms
#'
#' @return
#' @export
#'
#' @examples
v2_api_url_df <-
  function(terms = NA,
           domains = NA,
           image_face_tone = NA,
           image_num_faces = NA,
           image_ocr = NA,
           image_tag = NA,
           image_web_tag = NA,
           image_web_count = NA,
           source_country = NA,
           source_language = "English",
           gkg_theme = NA,
           tone = NA,
           tone_absolute_value = NA,
           use_or = FALSE,
           mode = 'ArtGallery',
           #ArtList, ImageCollage,  ImageCollageInfo, ImageCollageShare, TimelineVol, TimelineVolInfo, TimelineTone, TimelineLang, TimelineSourceCountry, ToneChart, WordCloudEnglish, WordCloudNative, WordCloudTheme, WordCloudImageTags, WordCloudImageWebTags
           timespan = "104 weeks",
           quote_terms = F,
           format = "html",
           dates = NULL,
           maximum_records = 250,
           translate = NULL,
           timeline_smooth = 5,
           sort_by = 'DateDesc',
           return_message = TRUE) {
    df_grid <- expand.grid(term = terms,
                           domain = domains,
                           image_face_tone = image_face_tone,
                           image_num_faces = image_num_faces,
                           image_ocr = image_ocr,
                           image_tag = image_tag,
                           image_web_tag = image_web_tag,
                           tone = tone,
                           tone_absolute_value = tone,
                           source_language = source_language,
                           gkg_theme = gkg_theme,
                           image_web_count = image_web_count,
                           source_country = source_country,
                           stringsAsFactors = F) %>%
      as_tibble()
    all_data <-
      1:nrow(df_grid) %>%
      map_dfr(function(x) {
        df_row <- df_grid %>% slice(x)
        if (!is.na(df_row$term)) {
          term_slug <- df_row$term
        } else {
          term_slug <- NULL
        }

        if (!is.na(df_row$domain)) {
          domain_slug <- df_row$domain
        } else {
          domain_slug <- NULL
        }

        if (!is.na(df_row$image_face_tone)) {
          image_face_tone_slug <- df_row$image_face_tone
        } else {
          image_face_tone_slug <- NULL
        }

        if (!is.na(df_row$image_num_faces)) {
          image_num_faces_slug <- df_row$image_num_faces
        } else {
          image_num_faces_slug <- NULL
        }

        if (!is.na(df_row$image_ocr)) {
          image_ocr_slug <- df_row$image_ocr
        } else {
          image_ocr_slug <- NULL
        }

        if (!is.na(df_row$image_tag)) {
          image_tag_slug <- df_row$image_tag
        } else {
          image_tag_slug <- NULL
        }

        if (!is.na(df_row$image_web_tag)) {
          image_web_tag_slug <- df_row$image_web_tag
        } else {
          image_web_tag_slug <- NULL
        }

        if (!is.na(df_row$image_web_count)) {
          image_web_count_slug <- df_row$image_web_count
        } else {
          image_web_count_slug <- NULL
        }

        if (!is.na(df_row$source_country)) {
          source_country_slug <- df_row$source_country
        } else {
          source_country_slug <- NULL
        }

        if (!is.na(df_row$gkg_theme)) {
          gkg_theme_slug <- df_row$gkg_theme
        } else {
          gkg_theme_slug <- NULL
        }

        if (!is.na(df_row$tone)) {
          tone_slug <- df_row$tone
        } else {
          tone_slug <- NULL
        }

        if (!is.na(df_row$tone_absolute_value)) {
          tone_absolute_value_slug <- df_row$tone_absolute_value
        } else {
          tone_absolute_value_slug <- NULL
        }

        if (!is.na(df_row$source_language)) {
          source_language_slug <- df_row$source_language
        } else {
          source_language_slug <- NULL
        }

        .generate_v2_url_df(
          term = term_slug,
          domain = domain_slug,
          image_face_tone = image_face_tone_slug,
          image_num_faces = image_num_faces_slug,
          image_ocr = image_ocr_slug,
          image_tag = image_tag_slug,
          image_web_tag = image_web_count_slug,
          image_web_count = image_web_count_slug,
          source_country = source_country_slug,
          source_language = source_language_slug,
          gkg_theme = gkg_theme_slug,
          tone = tone_slug,
          tone_absolute_value = tone_absolute_value_slug,
          use_or = use_or,
          mode = mode,
          timespan = timespan,
          dates = dates,
          maximum_records = maximum_records,
          translate = translate,
          timeline_smooth = timeline_smooth,
          sort_by = sort_by,
          nest_data = FALSE,
          format = format,
          return_message = return_message

        )
      })

    all_data %>%
      mutate(urlGDELTV2FTAPI = glue("{urlGDELTV2FTAPI}&mode={mode}") %>% as.character())
  }