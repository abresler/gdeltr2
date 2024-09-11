# https://blog.gdeltproject.org/https-now-available-for-selected-gdelt-apis-and-services/

.generate_v2_url <-
  function(term = NULL,
           domain = NULL,
           domain_exact = NULL,
           image_face_tone = NULL,
           image_number_faces = NULL,
           image_ocr_meta = NULL,
           image_tag = NULL,
           image_web_count = NULL,
           image_web_tag = NULL,
           near_term = NULL,
           near_length = 20,
           repeat_term = NULL,
           repeat_count = 3,
           source_language = "english",
           source_country = "United States",
           theme_gkg = NULL,
           tone = NULL,
           tone_absolute = NULL,
           mode = "ArtList",
           format = "csv",
           timespan = NULL,
           date_resolution = NULL,
           maximum_records = 250,
           sort_variable = "DateDesc",
           timeline_smooth = NULL,
           start_date = NULL,
           end_date = NULL,
           timezone_adjust = NULL,
           time_zoom = NULL,
           return_message = FALSE
  ) {
    base_url <- "https://api.gdeltproject.org/api/v2/doc/doc?query="

    ## Query

    if (length(term) > 0) {
      term_slug <- collapse_terms(terms = term, encode = TRUE)

      if (length(term) > 1) {
        term_slug <- glue("({term_slug})")
      }


    } else {
      term_slug <- ""
    }


    # Context



    if (length(domain) > 0) {
      domain_slug <-
        generate_gdelt_api_parameters_values(parameter = "domain", values = domain)
    } else {
      domain_slug <- NULL
    }

    if (length(domain_exact) > 0) {
      domain_exact_slug <-
        generate_gdelt_api_parameters_values(parameter = "domainis", values = domain_exact)
    } else {
      domain_exact_slug <- NULL
    }

    if (length(image_face_tone) > 0) {
      image_face_tone_slug <-
        generate_gdelt_api_parameters_values(parameter = "imagefacetone", values = image_face_tone)
    } else {
      image_face_tone_slug <- NULL
    }

    if (length(image_number_faces) > 0) {
      image_number_faces_slug <-
        generate_gdelt_api_parameters_values(parameter = "imagenumberfaces", values = image_number_faces)
    } else {
      image_number_faces_slug <- NULL
    }

    if (length(image_ocr_meta) > 0) {
      image_ocr_meta_slug <-
        generate_gdelt_api_parameters_values(parameter = "imageocrmeta", values = image_ocr_meta)
    } else {
      image_ocr_meta_slug <- NULL
    }

    if (length(image_tag) > 0) {
      image_tag_slug <-
        generate_gdelt_api_parameters_values(parameter = "imagetag", values = image_tag)
    } else {
      image_tag_slug <- NULL
    }

    if (length(image_web_count) > 0) {
      image_web_count_slug <-
        generate_gdelt_api_parameters_values(parameter = "imagewebcount", values = image_web_count)
    } else {
      image_web_count_slug <- NULL
    }

    if (length(image_web_tag) > 0) {
      image_web_count_slug <-
        generate_gdelt_api_parameters_values(parameter = "imagewebtag", values = image_web_count)
    } else {
      image_web_tag_slug <- NULL
    }

    if (length(near_term) > 0) {
      near_param <- glue::glue("near{near_length}")
      near_term <- glue::glue("\"{near_term}\"")

      near_slug <-
        generate_gdelt_api_parameters_values(parameter = near_param, values = near_term)
    } else {
      near_slug  <- NULL
    }

    if (length(repeat_term) > 0) {
      repeat_param <- glue::glue("near{repeat_count}")
      repeat_term <- glue::glue("\"{repeat_term}\"")

      repeat_slug <-
        generate_gdelt_api_parameters_values(parameter = repeat_param, values = near_term)
    } else {
      repeat_slug  <- NULL
    }

    if (length(source_language) > 0) {
      source_language_slug <-
        generate_gdelt_api_parameters_values(parameter = "sourcelang", values = source_language)
    } else {
      source_language_slug <- NULL
    }

    if (length(source_country) > 0) {
      source_clean <- source_country |> str_remove_all("\\ |\\+\\|-") |> str_to_lower()
      source_country_slug <-
        generate_gdelt_api_parameters_values(parameter = "sourcecountry", values = source_clean)
    } else {
      source_country_slug <- NULL
    }

    if (length(theme_gkg) > 0) {
      theme_gkg_slug <-
        generate_gdelt_api_parameters_values(parameter = "theme", values = theme_gkg)
    } else {
      theme_gkg_slug <- NULL
    }

    if (length(tone) > 0) {
      tone_slug <-
        generate_gdelt_api_parameters_values(parameter = "tone", values = tone)
    } else {
      tone_slug <- NULL
    }
    if (length(tone_absolute) > 0) {
      tone_absolute_slug <-
        generate_gdelt_api_parameters_values(parameter = "tone", values = tone)
    } else {
      tone_absolute_slug <- NULL
    }



    query_url <-
      str_c(
        base_url,
        term_slug,
        domain_slug,
        domain_exact_slug,
        image_face_tone_slug,
        image_number_faces_slug,
        image_tag_slug,
        image_web_count_slug,
        image_web_tag_slug,
        near_slug,
        repeat_slug,
        source_language_slug,
        source_country_slug,
        theme_gkg_slug,
        tone_slug,
        tone_absolute_slug,
        sep  = "%20%"
      )
    query_params <- query_url |> str_remove_all(base_url) |> str_remove_all("^\\?query=") |> url_decode() |> str_remove_all("\\%") |> str_squish()

    ## Mode

    mode_slug <-
      str_c("mode=", mode)

    # Format

    is_html_format <- format |> str_detect("html")

    format_slug <-
      str_c("format=", format)



    # DATERES

    if (length(date_resolution) > 0) {
      date_res_slug <- str_c("dateres=", date_resolution)
    } else {
      date_res_slug <- NULL
    }



    # MAXRECORDS

    if (str_to_lower(mode) |> str_detect("wordcloud")) {
      maximum_records <- min(1000, maximum_records)
    }

    max_records_slug <- str_c("maxrecords=", maximum_records)

    # SORT

    if (length(sort_variable) > 0) {
      sort_slug <-
        str_c("sort=", sort_variable)
    } else {
      sort_slug <- NULL
    }


    # STARTDATETIME/ENDDATETIME

    if (length(start_date) > 0) {
      start_date <- start_date |> str_replace_all("\\-", "")
      start_date <-
        str_c(start_date, "120000")
      start_date_slug <- str_c("startdatetime=", start_date)
    } else {
      start_date_slug <- NULL
    }

    if (length(end_date) > 0) {
      end_date <- end_date |> str_replace_all("\\-", "")
      end_date <-
        str_c(end_date, "120000")
      end_date_slug <- str_c("enddatetime=", end_date)
    } else {
      end_date_slug <- NULL
    }



    # TIMELINESMOOTH

    if (length(timeline_smooth) > 0) {
      timeline_smooth_slug <- str_c("timelinesmooth=", timeline_smooth)
    } else {
      timeline_smooth_slug <- NULL
    }

    # TIMESPAN

    if (length(timespan) > 0) {
      timespan <-
        str_remove_all(timespan, "\\-|\\ ")
      timespan_slug <-
        str_c("timespan=", timespan)
    }  else {
      timespan_slug <- NULL
    }

    # TIMEZONEADJ

    if (length(timezone_adjust) > 0) {
      timezone_adjust_slug <- str_c("timezoneadj=", timezone_adjust)
    } else {
      timezone_adjust_slug <- NULL
    }

    if (length(time_zoom) &
        is_html_format) {
      time_zoom_slug <- str_c("timezoom=", time_zoom)
    } else {
      time_zoom_slug <- NULL
    }


    # TIMEZOOM

    url_gdelt_tv_api <- str_c(
      query_url,
      mode_slug,
      format_slug,
      comb_slug,
      data_norm_slug,
      date_res_slug,
      last24_slug,
      max_records_slug,
      start_date_slug,
      end_date_slug,
      timeline_smooth_slug,
      timespan_slug,
      time_zoom_slug,
      sep = "&"
    )

    if (return_message) {
      cli::cli_text(
        "V2 TV API URL: {.url {url_gdelt_tv_api}}\n\n"
      )
    }

    term <- glue::glue("{term |> str_c(collapse='|')}") |> as.character()

    if (length(markets) > 0) {
      market <- glue::glue("{markets |> str_c(collapse='|')}") |> as.character()

    } else {
      market <- NULL
    }

    if (length(stations) > 0) {
      station <- glue::glue("{stations |> str_c(collapse='|')}") |> as.character()
    } else {
      station <- NULL
    }

    if (length(contexts) > 0) {
      context <- glue::glue("{contexts |> str_c(collapse='|')}") |> as.character()
    } else {
      context <- NULL
    }

    if (length(shows) > 0) {
      show <- glue::glue("{shows |> str_c(collapse='|')}") |> as.character()
    } else {
      show <- NULL
    }



    tibble(
      term,
      market,
      station,
      context,
      show,
      timespan,
      value_parameter = data_normalization_parameter,
      mode,
      query_params,
      format,
      url_gdelt_tv_api
    )
  }