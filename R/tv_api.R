# utils -------------------------------------------------------------------


.munge_hours <- function(data) {
  hours_24 <- 0:23

  # Convert to 12-hour format with AM/PM
  hours_12 <-
    sprintf("%02d:%02d %s",
            (hours_24 %% 12),
            # Convert to 12-hour format
            0,
            # Minutes
            ifelse(hours_24 < 12, "AM", "PM"))  # AM/PM designation

  hours_12[[1]] <- "12:00 AM"
  # Adjust for 12 AM and 12 PM

  names(data[[5]])[2:25] <- hours_12
  names(data[[5]])

}

#' Collapse Set of Terms Across Or
#'
#' @param terms Vector of terms
#' @param encode If `TRUE` encodes for URL
#'
#' @return
#' @export
#'
#' @examples
#' collapse_terms(terms = c("trump", "biden"), encode = TRUE)
collapse_terms <-
  function(terms = NULL, encode = FALSE) {
    if (length(terms) == 0) {
      stop("Please enter terms")
    }
    out <- terms |> str_c(collapse = " OR ")

    if (encode) {
      out <- URLencode(out)
    }
    out
  }

#' Generate API Query Parameters Values
#'
#' @param parameter Name of the Parameter
#' @param values Vector of Values
#' @param use_exact_quote If `TRUE` uses exact quote
#'
#' @return
#' @export
#'
#' @examples
#' generate_gdelt_api_parameters_values(parameter = "station", values = c("CNN", "FOXNEWS", "MSNBC", "WNBC"))
#' generate_gdelt_api_parameters_values(parameter = "show", values = c("Hardball With Chris Matthews"),   use_exact_quote = TRUE)
generate_gdelt_api_parameters_values <-
  function(parameter = "station",
           values = NULL,
           use_exact_quote = FALSE) {
    if (length(values) == 0) {
      stop("Enter parameterers")
    }

    if (use_exact_quote) {
      values <- glue::glue("\"{values}\"")
    }

    if (length(values) == 1) {
      out <- glue::glue("{parameter}:{values}") |> URLencode() |> as.character()
      return(out)
    }

    out <-
      glue::glue("{parameter}:{values}")
    out <- out |> str_c(collapse = " OR ")

    out <- glue::glue("({out})") |> URLencode() |> as.character()

    out
  }



#' V2 TV API Modes
#'
#' @return
#' @export
#'
#' @examples
dictionary_v2_tv_api_modes <-
  function() {
    modes <- c("ClipGallery",
      "ShowChart", "StationChart", "StationDetails", "TimelineVol",
      "TimelineVolHeatmap", "TimelineVolStream", "TimelineVolNorm",
      "TrendingTopics", "WordCloud")

    modes

  }

# parsers ------------------------------------------------------------------

.remove_parentheses <- function(input_string) {
  return(gsub("\\([^)]*\\)", "", input_string))
}

.parse_v2_tv_api_url <-
  function(url, return_message = TRUE, nest_data = FALSE) {
    url <- str_to_lower(url)
    if (return_message) {
      cli::cli_text(
        "Parsing: {.url {url}}\n\n"
      )
    }
    is_json <- url |> str_detect("format=json")
    is_csv <-
      url |> str_detect("format=csv")
    if (is_json) {
      .read_gdelt_api_json_safe <-
        purrr::possibly(.read_gdelt_api_json, tibble())
      data <- .read_gdelt_api_json_safe(url, url_type = "url_gdelt_tv_api")
    }

    if (is_csv) {
      .read_gdelt_api_csv_safe <-
        purrr::possibly(.read_gdelt_api_csv, tibble())
      data <- .read_gdelt_api_csv_safe(url, url_type = "url_gdelt_tv_api")
    }


    if (data |> hasName("value")) {
      is_pct <- url |> str_detect("datanorm=percent")
      if (is_pct) {
        data <- data |>
          rename(pct_estimated_coverage = value)
      }

      if (!is_pct) {
        data <- data |>
          rename(count_mentions = value)
      }
    }

    if (nest_data) {
      data <-
        data |>
        nest(-url_gdelt_tv_api) |>
        rename(data_gdelt_tv_api = data)
    }

    data
  }

#' Parse V2 TV API URL
#'
#' @param urls Vector of URLS
#' @param nest_data If `TRUE` nests data
#' @param return_message if `TRUE` returns message
#'
#' @return
#' @export
#'
#' @examples
parse_v2_tv_api_urls <-
  function(urls = NULL,
           nest_data = FALSE,
           return_message = T) {
    if (length(urls) ==0) {
      stop("Enter v2 TV API urls")
    }

    df <-
      tibble()

    .parse_v2_tv_api_url_safe <-
      possibly(.parse_v2_tv_api_url, tibble())

    success <- function(res) {
      url <-
        res$url


      data <-
        .parse_v2_tv_api_url_safe(url = url, return_message = return_message, nest_data = nest_data)

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


# https://blog.gdeltproject.org/gdelt-2-0-television-api-debuts/




# api ---------------------------------------------------------------------


# https://blog.gdeltproject.org/gdelt-2-0-television-api-debuts/

.generate_tv_url <-
  function(term = NULL,
           networks = NULL,
           stations = NULL,
           contexts = NULL,
           markets = NULL,
           shows = NULL,
           mode = "ClipGallery",
           format = "csv",
           use_combined_stations = FALSE,
           data_normalization_parameter = "percent",
           date_resolution = NULL,
           include_last_24_hours = FALSE,
           maximum_records = 3000,
           sort_variable = "DateDesc",
           timeline_smooth = NULL,
           start_date = NULL,
           end_date = NULL,
           timespan = NULL,
           timezone_adjust = NULL,
           time_zoom = NULL,
           return_message = FALSE
           ) {
    base_url <- "https://api.gdeltproject.org/api/v2/tv/tv?query="

    if (length(stations) == 0 & length(networks) == 0) {
      markets <- "National"
    }
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

    if (length(contexts) > 0) {
      context_slug <-
        generate_gdelt_api_parameters_values(parameter = "context", values = contexts)
    } else {
      context_slug <- NULL
    }

    # Market

    if (length(markets) > 0) {
      market_slug <-
        generate_gdelt_api_parameters_values(
          parameter = "market",
          values = markets,
          use_exact_quote = TRUE
        )
    } else {
      market_slug <- NULL
    }

    # Network

    if (length(networks) > 0) {
      network_slug <-
        generate_gdelt_api_parameters_values(
          parameter = "network",
          values = networks,
          use_exact_quote = FALSE
        )
    } else {
      network_slug <- NULL
    }

    # Show
    if (length(shows) > 0) {
      show_slug <-
        generate_gdelt_api_parameters_values(
          parameter = "show",
          values = shows,
          use_exact_quote = FALSE
        )
    } else {
      show_slug <- NULL
    }

    # Station

    if (length(stations) > 0) {
      station_slug <-
        generate_gdelt_api_parameters_values(
          parameter = "station",
          values = stations,
          use_exact_quote = FALSE
        )
    } else {
      station_slug <- NULL
    }



    query_url <-
      str_c(
        base_url,
        term_slug,
        market_slug,
        context_slug,
        network_slug,
        show_slug,
        station_slug,
        sep  = "%20%"
      )
    query_params <- query_url |> str_remove_all("https://api.gdeltproject.org/api/v2/tv/tv?") |> str_remove_all("^\\?query=") |> url_decode() |> str_remove_all("\\%") |> str_squish()

    ## Mode

    mode_slug <-
      str_c("mode=", mode)

    # Format

    is_html_format <- format |> str_detect("html")

    format_slug <-
      str_c("format=", format)

    # DATACOMB

    comb_slug <-  case_when(use_combined_stations ~ "datacomb=combined",
                            TRUE ~ "datacomb=sep")


    # DATANORM

    data_norm_slug <-
      str_c("datanorm=", data_normalization_parameter)

    # DATERES

    if (length(date_resolution) > 0) {
      date_res_slug <- str_c("dateres=", date_resolution)
    } else {
      date_res_slug <- NULL
    }

    # LAST24

    if (include_last_24_hours) {
      last24_slug <- "last24=yes"
    } else {
      last24_slug <- NULL
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

#'  GDELT V2 TV API Data and URLS
#'
#' Access data from  \href{https://blog.gdeltproject.org/gdelt-2-0-television-api-debuts/}{GDELT Television V2 API}
#'
#' @param terms Vector of terms
#' @param parse_data If `TRUE` parses data
#' @param networks Vector of networks
#' @param stations Vector of stations
#' @param contexts Vector of contexts
#' @param markets Vector of markets
#' @param shows Vector of shows
#' @param modes \itemize{
#' \item `ClipGallery` (default) This displays up to the top 50 most relevant clips matching your search. Each returned clip includes the name of the source show and station, the time the clip aired, a thumbnail, the actual text of the snippet and a link to view the full one minute clip on the Internet Archive's website. This allows you to see what kinds of clips are matching and view the full clip to gain context on your search results. In HTML output, this mode displays a "high design" visual layout suitable for creating magazine-style collages of matching coverage. When embedded as an iframe, the API uses the same postMessage resize model as the DOC 2.0 API.
#' \item `ShowChart` This determines what percent of your search results were from each specific television show and displays the top 10 shows
#' \item `StationChart` This compares how many results your search generates from each of the selected stations over the selected time period, allowing you to assess the relative attention each is paying to the topic. Using the DATANORM parameter you can control whether this reports results as raw clip counts or as normalized percentages of all coverage (the most robust way of comparing stations). Note that in HTML mode, you can use the button at the top right of the display to save it as a static image or save its underlying data.
#' \item `TimelineVol` This tracks how many results your search generates by day/hour over the selected time period, allowing you to assess the relative attention each is paying to the topic and how that attention has varied over time. Using the DATANORM parameter you can control whether this reports results as raw clip counts or as normalized percentages of all coverage (the most robust way of comparing stations). By default, the timeline will not display the most recent 24 hours, since those results are still being generated (it can take up to 2-12 hours for a show to be processed by the Internet Archive and ready for analysis), but you can include those if needed via the LAST24 option. You can also smooth the timeline using the TIMELINESMOOTH option and combine all selected stations into a single time series using the DATACOMB option. Note that in HTML mode, you can toggle the station legend using the button that appears at the top right of the display or export the timeline as a static image or save its underlying data.
#' \item `TimelineVolHeatmap` Displays an hourly timeline of total airtime seconds matching the query, rapidly pinpointing hourly trends, where the X axis is days and Y axis is hours from 0 to 23. Each cell is color-coded from white (0) to dark blue (maximum value). Note that this visualization is very computationally expensive and thus may take several seconds or longer to return
#' \item `TimelineVolStream` Same as "TimelineVol" but displays as a streamgraph rather than a line-based timeline
#' \item `TimelineVolNorm` This displays the total airtime (in terms of 15 second clips) monitored from each of the stations in your query. It must be combined with a valid query, since it displays the airtime for the stations queried in the search. This mode can be used to identify brief monitoring outages or for advanced normalization, since it reports the total amount of clips monitored overall from each station in each day/hour.
#' \item `WordCloud` This mode returns the top words that appear most frequently in clips matching your search. It takes the 200 most relevant clips that match your search and displays a word cloud of up to the top 200 most frequent words that appeared in those clips (common stop words are automatically removed). This is a powerful way of understanding the topics and words dominating the relevant coverage and suggesting additional contextual search terms to narrow or evolve your search. Note that if there are too few matching clips for your query, the word cloud may be blank. Note that in HTML mode, you can use the options at the bottom right of the display to save it as a static image or save its underlying data
#' }
#' @param formats This controls what file format the results are displayed in. Not all formats are available for all modes.  To assist with website embedding, the CORS ACAO header for all output of the API is set to the wildcard "*", permitting universal embedding. \itemize{
#' \item `HTML` This is the default mode and returns a browser-based visualization or display. Some displays, such as word clouds, are static images, some, like the timeline modes, result in interactive clickable visualizations, and some result in simple HTML lists of images or articles. The specific output varies by mode, but all are intended to be displayed directly in the browser in a user-friendly intuitive display and are designed to be easily embedded in any page via an iframe.
#' \item `CSV` This returns the requested data in comma-delimited (CSV) format. The specific set of columns varies based on the requested output mode. Note that since some modes return multilingual content, the CSV is encoded as UTF8 and includes the UTF8 BOM to work around Microsoft Excel limitations
#' \item `JSON` This returns the requested data in UTF8 encoded JSON. The specific fields varies by output mode.
#' \item `RSS` This is only available in ClipGallery mode and returns the results in RSS 2.0 format
#' }
#' @param use_combined_stations If `TRUE` both timeline and station chart modes separate out each matching station's data to make it possible to compare the relative attention paid to a given topic by each station. Sometimes, however, the interest is in overall media attention, rather than specific per-station differences in that coverage. Setting this parameter to "combined" will collapse all matching data into a single "Combined" synthetic station and make it much easier to understand macro-level patterns
#' @param data_normalization_parameter Most media researchers are accustomed to working with raw result counts, such as the absolute number of 15 second clips that matched a given query. Such raw counts, while useful quick "sanity checks", are inappropriate for production analysis, especially looking over time or across stations. Raw clip counts reflect the absolute number of clips that matched a given query. However, one station may devote more of its airtime to commercials or non-news entertainment programming (which are largely excluded by the TV Explorer) and thus render such comparisons meaningless. Instead, by default the API normalizes timeline and station chart modes by dividing the number of matching clips in each time interval by the total number of all monitored clips from that station or set of stations over that time interval. For example, if searching for "trump" on CNN over the past 2 years in timeline mode, each day will be displayed as the percent of all 15 second clips monitored from CNN that day that contained the word "trump" somewhere in that 15 second interval. This transforms result counts into normalized density measurements that allow you to directly compare different periods of time or stations in a meaningful way. We strongly recommend that users use only normalized result counts (the default), but for cases where you need raw counts, you can set this parameter to.
#' \itemize{
#' \item `percent` This is the default mode and returns the results as a percentage of all monitored clips from that station or set of stations over that time interval.
#' \item `raw` This returns the results as raw counts of matching clips.
#' }
#' @param date_resolution  By default results are returned in hourly resolution (for <7 day timespans), daily resolution for <3 year timespans and monthly resolution otherwise. You can override these settings and manually set the time resolution to any of the following values. NOTE that this will automatically adjust the STARTDATETIME and ENDDATETIME parameters to their start/stop dates/times at the given date resolution. \itemize{
#' \item `Hour:` This is only available for timespans of 7 or fewer days. Rounds start/stop dates to their nearest full hours.
#' \item `Day`: Rounds start/stop dates to their nearest full day.
#' \item `Week`: Rounds start/stop dates to their nearest full week.
#' \item `Month`: Rounds start/stop dates to their nearest full month.
#' \item `Year`: Rounds start/stop dates to their nearest full year.
#' }
#' @param include_last_24_hours If `TRUE` includes the most recent 24 hours of data in the timeline. By default, the timeline will not display the most recent 24 hours, since those results are still being generated (it can take up to 2-12 hours for a show to be processed by the Internet Archive and ready for analysis), but you can include those if needed via the LAST24 option.
#' @param maximum_records This option only applies to ClipGallery mode. By default 50 clips are displayed in HTML mode. In JSON, JSONP or CSV formats, up to 3,000 clips can be returned
#' @param sort_variable  By default results are sorted by relevance to your query. Sometimes you may wish to sort by date or tone instead.
#' @param timeline_smooth This option is only available in timeline mode and performs moving window smoothing over the specified number of time steps, up to a maximum of 30. Timeline displays can sometimes capture too much of the chaotic noisy information environment that is the television landscape, resulting in jagged displays. Use this option to enable moving average smoothing up to 30 time steps to smooth the results to see the macro-level patterns. Note that since this is a moving window average, peaks will be shifted to the right, up to several days or weeks at the heaviest smoothing levels
#' @param start_date Start date; The earliest available date is July 2, 2009. If you do not specify an ENDDATETIME, the API will search from STARTDATETIME through the present date/time
#' @param end_date End Date;  If you do not specify a STARTDATETIME, the API will search from July 2, 2009 through the specified ENDDATETIMEs
#' @param timespan By default the TV API searches the entirety of the Internet Archive's Television News Archive's holdings, which extend back to July 2009 for some stations. You can narrow this range by using this option to specify the number of months, weeks, days or hours (minimum of 1 hour). The API then only searches airtime within the specified timespan backwards from the present time. If you would instead like to specify the precise start/end time of the search instead of an offset from the present time, you should use the STARTDATETIME/ENDDATETIME parameters
#' \itemize{
#' \item `Hours`. Specify a number followed by "h" or "hours" to provide the timespan in hours.
#' \item  `Days`. Specify a number followed by "d" or "days" to provide the timespan in days.
#'  \item `Weeks`. Specify a number followed by "w" or "weeks" to provide the timespan in weeks.
#'  \item `Months`. Specify a number followed by "m" or "months" to provide the timespan in months.
#'  \item `Years`. Specify a number followed by "y" or "years" to provide the timespan in years.
#'
#' }
#' @param timezone_adjust By default STARTDATETIME, ENDDATETIME and all returned results are interpreted and reported in the UTC timezone. Use this parameter to adjust to any of the following major timezones: -12:00, -11:00, -10:00, -09:30, -09:00, -08:00, -07:00, -06:00, -05:00, -04:00, -03:30, -03:00, -02:30, -02:00, -01:00, +00:00, +01:00, +02:00, +03:00, +03:30, +04:00, +04:30, +05:00, +05:30, +05:45, +06:00, +06:30, +07:00, +08:00, +08:45, +09:00, +09:30, +10:00, +10:30, +11:00, +12:00, +12:45, +13:00, +13:45 or +14:00. Note that UTC offsets are treated as-is over the entire timespan, meaning that the offset is not adjusted to account for periods in which daylight savings is honored.
#' @param time_zoom This option is only available for timeline modes in HTML format output and enables interactive zooming of the timeline using the browser-based visualization. Set to "yes" to enable and set to "no" or do not include the parameter, to disable. By default, the browser-based timeline display allows interactive examination and export of the timeline data, but does not allow the user to rezoom the display to a more narrow time span. If enabled, the user can click-drag horizontally in the graph to select a specific time period. If the visualization is being displayed directly by itself (it is the "parent" page), it will automatically refresh the page to display the revised time span. If the visualization is being embedded in another page via iframe, it will use postMessage to send the new timespan to the parent page with parameters "startdate" and "enddate" in the format needed by the STARTDATETIME and ENDDATETIME API parameters. The parent page can then use these parameters to rewrite the URLs of any API visualizations embedded in the page and reload each of them. This allows the creation of dashboard-like displays that contain multiple TV API visualizations where the user can zoom the timeline graph at the top and have all of the other displays automatically refresh to narrow their coverage to that revised time frame
#' @param return_message If `TRUE` returns a message
#' @param use_exact_term If `TRUE` uses exact term
#'
#' @return
#' @export
#'
#' @examples
#' library(gdeltr2)
#' v2_tv_api(terms = c("Walz", "JD Vance"), formats = c("json"), modes = "TimelineVol")
v2_tv_api <-
  function(terms = NULL,
           use_exact_term = FALSE,
           parse_data = TRUE,
           return_message = TRUE,
           networks = NULL,
           stations = NULL,
           contexts = NULL,
           markets = NULL,
           shows = NULL,
           modes = "ClipGallery",
           formats = "json",
           use_combined_stations = FALSE,
           data_normalization_parameter = "percent",
           date_resolution = NULL,
           include_last_24_hours = FALSE,
           maximum_records = 3000,
           sort_variable = "DateDesc",
           timeline_smooth = NULL,
           start_date = NULL,
           end_date = NULL,
           timespan = NULL,
           timezone_adjust = NULL,
           time_zoom = NULL) {

    if (length(modes) == 0) {
      stop("Enter modes")
    }
    tbl_params <-
      expand.grid(
        term = terms,
        mode = modes,
        format = formats,
        stringsAsFactors = FALSE
      ) |> as_tibble()

    all_data <-
      seq_along(1:nrow(tbl_params)) |>
      map_dfr(function(i) {
        tbl_row <- tbl_params[i, ]
        term <- tbl_row$term
        if (use_exact_term) {
          term <- glue::glue("\"{term}\"")
        }
        format <- tbl_row$format
        mode  <- tbl_row$mode
        .generate_tv_url(
          term = term,
          networks = networks,
          stations = stations,
          contexts = contexts,
          markets = markets,
          shows = shows,
          mode = mode,
          format = format,
          use_combined_stations = use_combined_stations,
          data_normalization_parameter = data_normalization_parameter,
          date_resolution = date_resolution,
          include_last_24_hours = include_last_24_hours,
          maximum_records = maximum_records,
          sort_variable = sort_variable,
          timeline_smooth = timeline_smooth,
          start_date = start_date,
          end_date = end_date,
          timespan = timespan,
          timezone_adjust = timezone_adjust,
          time_zoom = time_zoom,
          return_message = return_message
        )
      })

    if (!parse_data) {
      return(all_data)
    }

    all_data <- all_data |> filter(!format |> str_detect("html"))

     nest_data <- length(modes) > 1

     dat_gdelt <- parse_v2_tv_api_urls(
       urls = all_data$url_gdelt_tv_api,
       nest_data = nest_data,
       return_message = FALSE
     )

     all_data <-
       all_data |>
       mutate(url_gdelt_tv_api = str_to_lower(url_gdelt_tv_api)) |>
       left_join(dat_gdelt, by = "url_gdelt_tv_api")

    all_data

  }




# new television api ------------------------------------------------------

# http://television.gdeltproject.org/cgi-bin/iatv_ftxtsearch/iatv_ftxtsearch?primary_keyword=Amy+Shumer&context_keywords=&filter_network=CNN&filter_timespan=ALL&filter_timespan_custom_start=&filter_timespan_custom_end=&filter_displayas=PERCENT&filter_combineseparate=SEPARATE&filter_outputtype=JSON
# http://television.gdeltproject.org/cgi-bin/iatv_ftxtsearch/iatv_ftxtsearch?primary_keyword=campaign&context_keywords=&filter_network=CNN&filter_timespan=ALL&filter_displayas=PERCENT&filter_combineseparate=SEPARATE&filter_outputtype=DISPLAY



# json --------------------------------------------------------------------

.parse_tv <- function(url = "https://api.gdeltproject.org/api/v2/tv/tv?query=trump%20Network:CBS&mode=timelinevolnorm&format=csv&datanorm=perc&timelinesmooth=0&datacomb=sep&last24=yes&timezoom=yes") {

}

.parse_tv_show_url <-
  function(url = "https://api.gdeltproject.org/api/v2/tv/tv?format=csv&timespan=FULL&last24=yes&query=%22Thomas%20Crooks%22%20(station:CNN%20OR%20station:FOXNEWS%20OR%20station:MSNBC%20OR%20%20station:WNBC%20)%20&mode=showchart") {

  }

#' Widen TV Trend data
#'
#' @param data `tibble`
#'
#' @return
#' @export
#'
#' @examples
widen_tv_trends <-
  function(data) {
    data <-
      data |> group_by(id_station, type) |>
      arrange(topic) |>
      mutate(number_topic = 1:n()) |>
      ungroup()

    data <-
      data |>
      unite(type, type, id_station) |> spread(type, topic) |> janitor::clean_names()

    data
  }

#' Returns Trends from TV API
#'
#' @param include_national_topics if `TRUE` binds national topics
#' @param return_wide If `TRUE` returns data in wide form.
#' @param return_message If `TRUE` returns messages
#'
#' @return
#' @export
#'
#' @examples
#' library(gdeltr2)
#' v2_tv_trends()
#' v2_tv_trends(return_wide = TRUE)
v2_tv_trends <-
  function(include_national_topics = TRUE,
           return_wide = FALSE,
           return_message = TRUE) {
    dat <- "https://api.gdeltproject.org/api/v2/tv/tv?mode=TrendingTopics&format=json" |> fromJSON(simplifyDataFrame = TRUE)
    time_generated <- dat$`DateGenerated:` |> ymd_hms() |> lubridate::with_tz(Sys.timezone())
    tbl_trending <- dat$StationTrendingTopics |> as_tibble() |> unnest() |> mutate(type = "trending")
    tbl_top <-
      dat$StationTopTopics |> as_tibble() |> unnest() |> mutate(type = "top")
    trending_phrases <- dat$OverallTrendingPhrases |> str_to_title()
    trending_topics <- dat$OverallTrendingTopics |> str_to_title()

    if (return_message) {
      tp <- trending_phrases |> sort() |> str_c(collapse = "\n")
      topics <- trending_topics |> sort() |> str_c(collapse = "\n")
      glue::glue(
        '{crayon::red("GDELT Trending National Phrases")} at {crayon::green(time_generated)}:\n{tp}\n\n'
      ) |> cat(fill = TRUE)
      glue::glue(
        '{crayon::red("\n\nGDELT Trending National Topics")} at {crayon::green(time_generated)}:\n{topics}'
      ) |> cat(fill = TRUE, sep = "\n")
    }

    data <-
      list(tbl_trending, tbl_top) |> reduce(bind_rows) |>
      setNames(c("id_station", "topic", "type")) |>
      select(type, everything())

    if (include_national_topics) {
      dat_phrases <- tibble(topic = trending_phrases) |>
        mutate(type = "trending") |>
        mutate(id_station = "National Phrases")

      dat_topic <-
        tibble(topic = trending_topics) |>
        mutate(type = "top") |>
        mutate(id_station = "National Topic")

      data <- list(dat_phrases, dat_topic, data) |> reduce(bind_rows) |>
        select(type, everything())
    }

    if (return_wide) {
      data <- widen_tv_trends(data)
    }

    data

  }


# Inventory dailies -----------------------------------------------------------------



# stations

#' GDELT TV Station Dictionary
#'
#' Returns information about the television
#' stations monitored by GDELT
#'
#' @return a \code{tibble}
#' @export
#' @import dplyr jsonlite lubridate purrr
#' @importFrom tidyr separate
#'
#' @examples
#' dictionary_gdelt_tv_stations()
dictionary_gdelt_tv_stations <-
  memoise::memoise(function(snake_names = FALSE) {
    json_data <-
      "https://api.gdeltproject.org/api/v2/tv/tv?mode=stationdetails&format=json" %>%
      fromJSON(simplifyVector = T)

    data <- json_data$station_details %>%
      as_tibble() |>
      janitor::clean_names()

    data <- .munge_gdelt_names(data = data, snake_names = FALSE)
    data <-
      data %>%
      separate(
        col = "nameStation",
        into = c("nameStation", "slugAffiliate"),
        sep = "\\(",
        extra = "merge",
        fill = "left"
      ) |>
      mutate(slugAffiliate = slugAffiliate %>% str_replace_all("\\)", "")) %>%
      mutate_all(str_trim)

    data <-
      data %>%
      mutate(
        datetimeStartData = datetimeStartData %>% lubridate::ymd_hms() %>% lubridate::with_tz(Sys.timezone()),
        datetimeEndData = datetimeEndData %>% lubridate::ymd_hms() %>% lubridate::with_tz(Sys.timezone()),
        yearDataStart = lubridate::year(datetimeStartData),
        yearDataEnd  = lubridate::year(datetimeEndData),
        isActiveStation =  yearDataEnd == (lubridate::year(Sys.Date()))
      )

    if (snake_names) {
      data <- data |> janitor::clean_names()
    }

    data

  })


.generate_tv_inventory_url <-  function(date = Sys.Date()) {
  if (date > Sys.Date()) {
    stop("Data cannot go into the future")
  }

  if (date < "2009-06-16") {
    stop("Sorry data starts on 2009-06-16")
  }

  date_slug <-
    date %>% lubridate::ymd() %>% str_replace_all("\\-", "")

  base <- "http://data.gdeltproject.org/gdeltv3/iatv/inventory"

  url <-
    glue("{base}/{date_slug}.inventory.csv") %>% as.character()

  tibble(dateData = date, urlGDELTInventory = url)

}

.generate_tv_inventory_urls <-
  function(date_start = c("2018-02-01"),
           date_end = NULL) {
    if (date_start %>% is_null()) {
      stop("Please enter a date")
    }

    if (length(date_end) == 0) {
      data <- .generate_tv_inventory_url(date = date_start)
      return(data)
    }

    dates <- seq(ymd(date_start), ymd(date_end), by = "days")
    generate_tv_inventory_url_safe <-
      possibly(.generate_tv_inventory_url, tibble())

    data <-
      dates |>
      map_dfr(function(date) {
        generate_tv_inventory_url_safe(date = date)
      })

    data


  }


.parse_summary_inventory_data_url <-
  function(url = "http://data.gdeltproject.org/gdeltv3/iatv/inventory/20240805.inventory.csv") {
    data <- data.table::fread(url) |> as_tibble()

    data <-
      data %>%
      set_names(
        c(
          "idShow",
          "urlArchive",
          "idStation",
          "nameShow",
          "countClips",
          "durationShowSeconds",
          "datetimeShowStart",
          "datetimeShowEnd"
        )
      ) %>%
      mutate(urlGDELTInventory = url)

    data
  }

#' Parsse Inventory URLs
#'
#' @param urls vector of GDELT v3 URLSs
#' @param snake_names if `TRUE` snakes names
#' @param return_message if `TRUE` returns message
#'
#' @return
#' @export
#'
#' @examples
#' parse_summary_inventory_data_urls("http://data.gdeltproject.org/gdeltv3/iatv/inventory/20240805.inventory.csv")
parse_summary_inventory_data_urls <-
  function(urls = "http://data.gdeltproject.org/gdeltv3/iatv/inventory/20240805.inventory.csv",
           snake_names = FALSE,
           return_message = T) {
    df <-
      tibble()

    .parse_summary_inventory_data_url_safe <-
      possibly(.parse_summary_inventory_data_url, tibble())

    success <- function(res) {
      url <-
        res$url
      if (return_message) {
        glue("Parsing {url}") %>% cat(fill = T)
      }

      data <-
        url %>%
        .parse_summary_inventory_data_url_safe()

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

    if (snake_names) {
      df <- df |> janitor::clean_names()
    }
    df
  }

#' GDELT TV Show Summary
#'
#' Acquires daily summary data for the television shows
#' that GDELT monitors
#'
#' @param date_start Start date of data, cannot be before June 16th, 2009
#' @param date_end End date of data, cannot exceed current date
#' @param return_message if \code{TRUE} returns a messag
#'
#' @return a \code{tibble}
#' @export
#' @examples
#' tv_summaries(date_start = Sys.Date(), date_end = Sys.Date(), return_message = T)
tv_summaries <-
  function(date_start = Sys.Date() - 1,
           date_end = Sys.Date(),
           snake_names = FALSE,
           return_message = T) {
    df_urls <-
      .generate_tv_inventory_urls(date_start = date_start, date_end = date_end)

    all_data <-
      parse_summary_inventory_data_urls(
        urls = df_urls$urlGDELTInventory,
        return_message = return_message,
        snake_names = FALSE
      )

    all_data <-
      all_data %>%
      left_join(df_urls, by = "urlGDELTInventory") |>
      select(dateData, everything()) |>
      arrange(dateData)


    all_data <-
      all_data %>%
      mutate(
        datetimeShowStart = datetimeShowStart %>% lubridate::with_tz(Sys.timezone()),
        datetimeShowEnd = datetimeShowEnd %>% lubridate::with_tz(Sys.timezone())
      )

    if (snake_names) {
      all_data <- all_data |> janitor::clean_names()
    }


    all_data

  }

#' Distinct TV Shows for GDELT API
#'
#' @param date_start Start Date.  Default is `Sys.Date()-1`
#' @param date_end End Date.  Default is Default is `Sys.Date()`
#'
#' @return
#' @export
#'
#' @examples
dictionary_gdelt_tv_shows <-
  function(date_start = Sys.Date() - 30,
           date_end = Sys.Date()) {
    dat <- tv_summaries(snake_names = TRUE,
                        date_start = date_start,
                        date_end = date_end)

    dat <- dat |> distinct(id_station, name_show) |> arrange(id_station, desc(name_show))

    dat
  }