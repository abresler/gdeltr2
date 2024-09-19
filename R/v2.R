# https://blog.gdeltproject.org/https-now-available-for-selected-gdelt-apis-and-services/


# utils -------------------------------------------------------------------

#' V2 Document API Modes
#'
#' @return
#' @export
#'
#' @examples
dictionary_v2_doc_api_modes <-
  function() {
    modes <- c(
      "ArtList",
      "ArtGallery",
      "ImageCollage",
      "ImageCollageInfo",
      "ImageGallery",
      "ImageCollageShare",
      "TimelineVol",
      "TimelineVolRaw",
      "TimelineVolInfo",
      "TimelineTone",
      "TimelineLang",
      "TimelineSourceCountry",
      "ToneChart",
      "WordCloudImageTags",
      "WordCloudImageWebTags"
    )

    modes

  }

# doc ---------------------------------------------------------------------

# https://blog.gdeltproject.org/gdelt-doc-2-0-api-debuts/
.generate_v2_doc_url <-
  function(term = NULL,
           domain = NULL,
           domain_exact = NULL,
           image_face_tone = NULL,
           image_number_faces = NULL,
           image_ocr_meta = NULL,
           image_tag = NULL,
           image_web_count = NULL,
           image_web_tag = NULL,
           theme_gkg = NULL,
           near_term = NULL,
           near_length = 20,
           repeat_term = NULL,
           repeat_count = 3,
           source_language = "english",
           source_country = "United States",
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
           return_message = FALSE) {
    base_url <- "https://api.gdeltproject.org/api/v2/doc/doc?query="

    ## Query

    if (length(term) > 0) {
      term_slug <- collapse_terms(terms = term, encode = TRUE)

      if (length(term) > 1) {
        term_slug <- glue("({term_slug})")
      }


    } else {
      term_slug <- NULL
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
      image_web_tag_slug <-
        generate_gdelt_api_parameters_values(parameter = "imagewebtag", values = image_web_tag)
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
        image_ocr_meta_slug,
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


    query_params <-
      query_url |>   str_remove("^https://api.gdeltproject.org/api/v2/doc/doc\\?query=") |> url_decode() |> str_remove_all("\\%") |> str_squish() |> str_split("\\&") |> flatten_chr()

    query_params <- query_params[[1]] |> str_replace_all("\\ ", "\\ - ")

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

    url_gdelt_v2_doc_api <- str_c(
      query_url,
      mode_slug,
      format_slug,
      date_res_slug,
      max_records_slug,
      start_date_slug,
      end_date_slug,
      timeline_smooth_slug,
      timespan_slug,
      time_zoom_slug,
      sep = "&"
    )

    if (return_message) {
      cli::cli_text("V2 DOC API URL: {.url {url_gdelt_v2_doc_api}}\n\n")
    }

    term <- glue::glue("{term |> str_c(collapse='|')}") |> as.character()

    if (term == "") {
      term <- NULL
    }



    tibble(
      term,
      domain,
      domain_exact,
      image_face_tone,
      image_number_faces,
      image_ocr_meta,
      image_tag,
      image_web_count,
      image_web_tag,
      near_term,
      timespan,
      mode,
      source_country,
      source_language,
      query_params,
      format,
      url_gdelt_v2_doc_api
    )
  }

.parse_v2_doc_api_url <-
  function(url,
           return_message = TRUE,
           nest_data = FALSE) {
    url <- str_to_lower(url)
    if (return_message) {
      cli::cli_text("Parsing: {.url {url}}\n\n")
    }
    is_json <- url |> str_detect("format=json")
    is_csv <-
      url |> str_detect("format=csv")
    if (is_json) {
      .read_gdelt_api_json_safe <-
        purrr::possibly(.read_gdelt_api_json, tibble())
      data <- .read_gdelt_api_json_safe(url, url_type = "url_gdelt_v2_doc_api")
    }

    if (is_csv) {
      .read_gdelt_api_csv_safe <-
        purrr::possibly(.read_gdelt_api_csv, tibble())
      data <- .read_gdelt_api_csv_safe(url, url_type = "url_gdelt_v2_doc_api")


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

    if (data |> hasName("id_series") &
        data |> hasName("count_mentions")) {
      data <-
        data |> spread(id_series, count_mentions) |> janitor::clean_names()
    }

    if (data |> hasName("id_series") & data |> hasName("value")) {
      data <-
        data |> spread(id_series, value) |> janitor::clean_names()
    }



    if (data |> hasName("url")) {
      data <- data |>
        rename(url_article = url)
    }

    if (data |> hasName("url_mobile_link")) {
      data <-
        data |>
        mutate(url_mobile_link = case_when(url_mobile_link == "" ~ NA_character_, TRUE ~ url_mobile_link))
    }

    if (data |> hasName("title_article")) {
      data <- data |>
        mutate(title_article = title_article |> str_squish())
    }

    if (data |> hasName("datetime_article")) {
      data <- data |>
        mutate(
          datetime_article = datetime_article |> lubridate::ymd_hms() %>% lubridate::with_tz(Sys.timezone())
        )
    }

    data <- data |> mutate(url_gdelt_v2_doc_api = url |> str_to_lower())

    if (nest_data) {
      data <-
        data |>
        nest(-url_gdelt_v2_doc_api) |>
        rename(data_v2_doc_api = data)
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
parse_v2_doc_api_urls <-
  function(urls = NULL,
           nest_data = FALSE,
           return_message = T) {
    if (length(urls) == 0) {
      stop("Enter v2 Documeny API urls")
    }

    df <-
      tibble()

    .parse_v2_doc_api_url_safe <-
      possibly(.parse_v2_doc_api_url, tibble())

    success <- function(res) {
      url <-
        res$url


      data <-
        .parse_v2_doc_api_url_safe(url = url,
                                   return_message = return_message,
                                   nest_data = nest_data)

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



#' GDELT V2 Doc API
#'
#' Interact with the GDELT V2 Document API \href{https://blog.gdeltproject.org/gdelt-doc-2-0-api-debuts/}{Documentation}
#'
#' @param terms This contains your search query and supports keyword and keyphrase searches, OR statements and a variety of advanced operators.
#' @param term_domains Vector of domains isolated to search.
#' @param term_exact_domains Vector of `exact` domains to search
#' @param use_exact_term If `TRUE` quotes terms for exact representations.
#' @param domains Vector of domains.  Returns all coverage from the specified domain. Follow by a colon and the domain name of interest. Search for "domain:cnn.com" to return all coverage from CNN
#' @param domains_exact Vector of exact domains
#' @param images_face_tone Vector of tones.  Searches the average "tone" of human facial emotions in each image. Only human faces that appear large enough in the image to accurately gauge their facial emotion are considered, so large crowd photos where it is difficult to see the emotion of peoples' faces may not be scored accurately. The tone score of an average photograph typically ranges from +2 to -2. To search for photos where visible people appear to be sad, search "imagefacetone<-1.5". Only available in any of the "image" modes
#' @param images_number_faces This searches the total number of foreground human faces in the image.
#' @param images_ocr_meta This searches a combination of the results of OCR performed on the image in 80+ languages (to extract any text found in the image, including background text like storefronts and signage), all metadata embedded in the image file itself (EXIF, etc) and the textual caption provided for the image. To search for images of a specific event, such as "mobile congress" you would use this field, since that information would most likely either be found in signage in the background of the image, provided in the EXIF metadata in the image or listed in the caption under the image. The search parameter for this field must always be enclosed in quote marks, even when searching for a single word like "imageocrmeta:"zika"". Only available in any of the "image" modes.
#' @param image_tags  Every image processed by GDELT is assigned one or more topical tags from a universe of more than 10,000 objects and activities recognized by Google's algorithms. This is the primary and most accurate way of searching global news imagery monitored by GDELT, as these tags represent the ground truth of what is actually depicted in the image itself.
#' @param image_web_counts Every image processed by GDELT is run through the equivalent of a reverse Google Images search that searches the web to see if the image has ever appeared anywhere else on the web that Google has seen. Up to the first 200 web pages where the image has been seen are returned. This operator allows you to screen for popular versus novel images
#' @param image_web_tags Every image processed by GDELT is run through the equivalent of a reverse Google Images search that searches the web to see if the image has ever appeared anywhere else on the web that Google has seen. The system then takes every one of those appearances from across the web and looks at all of the textual captions appearing beside the image and compiles a list of the major topics used to describe the image across the web. This offers tremendous descriptive advantage in that you are essentially "crowdsourcing" the key topics of the image by looking at how it has been described across the web. Values must be enclosed in quote marks. Only available in any of the "image" modes. You can access a list of all tags appearing in at least 100 images (Image WebTag Lookup).
#' @param themes_gkg Searches for any of the GDELT Global Knowledge Graph (GKG) Themes. GKG Themes offer a more powerful way of searching for complex topics, since they can include hundreds or even thousands of different phrases or names under a single heading. To search for coverage of terrorism, use "theme:terror". You can find a list of all themes that have appeared in at least 100 articles over the past two years (GKG Theme Lookup).
#' @param near_terms Allows you to specify a set of keywords that must appear within a given number of words of each other. To use this operator, you specify the word "near", followed by the maximum distance all of the words can appear apart in a given document and still be considered a match, a colon, and then the list of words in quote marks. Phrase matching is not supported at this time, so the list of words is treated as a list of individual words that must all appear together within the given proximity. Note that if the words appear in a document in a different order than specified in the "near" operator, each ordering difference increments the word distance counted by the "near" operator. (Thus, near10:"donald trump" will return documents where "trump" appears within 10 words after "donald", but will also return documents in which "donald" appears within 9 words after "trump".) The distance measure is not precise and can count punctuation and other tokens as "words" as well. It is also important to remember that proximity in a document does not necessarily imply two words are connected semantically each other.
#' @param near_length Vector of lengths to isolate near
#' @param repeat_terms Allows you to specify that a given word must appear at least a certain number of times in a document to be considered a match.
#' @param repeat_count Vector of repeat counts
#' @param source_languages Vector of countries.  Searches for articles originally published in the given language. The GEO API currently only allows you to search the English translations of all coverage, but you can specify that you want to limit your search to articles published in a particular language. Using this operator by itself you can map all of the locations mentioned in a particular language across all topics to see the geographic focus of a given language. Search for "sourcelang:spanish" to return only Spanish language coverage. You can also specify its three-character language code. All 65 machine translated languages are supported
#' @param source_countries Vector of source countries.  Searches for articles published in outlets located in a particular country. This allows you to narrow your scope to the press of a single country. For countries with spaces in their names, type the full name without the spaces (like "sourcecountry:unitedarabemirates" or "sourcecountry:saudiarabia"). You can also use their 2-character FIPS country code
#' @param tone Allows you to filter for only articles above or below a particular tone score (ie more positive or more negative than a certain threshold). To use, specify either a greater than or less than sign and a positive or negative number (either an integer or floating point number). To find fairly positive articles, search for "tone>5" or to search for fairly negative articles, search for "tone<-5".
#' @param tone_absolute The same as "Tone" but ignores the positive/negative sign and lets you simply search for high emotion or low emotion articles, regardless of whether they were happy or sad in tone. Thus, search for "toneabs<1" for fairly neutral articles or search for "toneabs>10" for fairly emotional articles.
#' @param modes This specifies the specific output you would like from the API, ranging from timelines to word clouds to article lists.
#'
#' \itemize{
#' \item `ArtList`  This is the most basic output mode and generates a simple list of news articles that matched the query. In HTML mode articles are displayed in a table with its social sharing image (if available) to its left, the article title, its source country, language and publication date all shown. RSS output format is only available in this mode.
#' \item `ArtGallery` This displays the same information as the ArtList mode, but does so using a high design visual layout suitable for creating magazine-style collages of matching coverage. Only articles containing a social sharing image are included.
#' #' \item `ImageCollageInfo` This yields identical output as the ImageCollage option, but adds four additional pieces of information to each image: 1) the number of times (up to 200) it has been seen before on the open web (via a reverse Google Images search), 2) a list of up to 6 of those web pages elsewhere on the web where the image was found in the past, 3) the date the photograph was captured via in the image's internal metadata (EXIF/etc), and 4) a warning if the image's embedded date metadata suggests the photograph was taken more than 72 hours prior to it appearing in the given article. Using this information you can rapidly triage which of the returned images are heavily-used images and which are novel images that have never been found anywhere on the web before by Google's crawlers. (You can also use the imagewebcount query term above to restrict your search to just images which have appeared a certain number of times.) Only a relatively small percent of news images contain an embedded capture datestamp that documents the date and time the image was taken or created and it is not always accurate, but where available this can offer a powerful indicator that a given image may be older than it appears and for applications that rely on filtering for only novel images (such as crisis mapping image cataloging), this can be used as a signal to perform further verification on an image.
#' \item `ImageGallery` This displays most of the same information as the `ImageCollageInfo` mode (though it does not include the embedded date warning)
#' \item `ImageCollageShare` Instead of returning VGKG-processed images, this mode returns a list of the social sharing images found in the matching news articles. Social sharing images are those specified by an article to be shown as its image when shared via social media sites like Facebook and Twitter. Not all articles include social sharing images and the images may sometimes only be the logo of the news outlet or not representative of the article contents, but in general they offer a reasonable visual summary of the core focus of the article and especially how it will appear when shared across social media platforms.
#' \item `TimelineVol` This is the most basic timeline mode and returns the volume of news coverage that matched your query by day/hour/15 minutes over the search period. Since the total number of news articles published globally varies so much through the course of a day and through the weekend and holiday periods, the API does not return a raw count of matched articles, but instead divides the number of matching articles by the total number of all articles monitored by GDELT in each time step. Thus, the timeline reports volume as a percentage of all global coverage monitored by GDELT. For time spans of less than 72 hours, the timeline uses a time step of 15 minutes to provide maximum temporal resolution, while for time spans from 72 hours to one week it uses an hourly resolution and for time spans of greater than a week it uses a daily resolution. In HTML mode the timeline is displayed as an interactive browser-based visualization.
#' \item `TimelineVolRaw` This is identical to the standard TimelineVol mode, but instead of reporting results as a percent of all online coverage monitored by GDELT, it returns the actual number of distinct articles that matched your query.
#' \item `TimelineVolInfo` This is identical to the main TimelineVol mode, but for each time step it displays the top 10 most relevant articles that were published during that time interval. Thus, if you see a sudden spike in coverage of your topic, you can instantly see what was driving that coverage. In HTML mode a popup is displayed over the timeline as you mouse over it and you can click on any of the articles to view them, while in JSON and CSV mode the article list is output as part of the file
#' \item `TimelineTone` Similar to the main TimelineVol mode, but instead of coverage volume it displays the average tone of all matching coverage, from extremely negative to extremely positive.
#' \item `TimelineLang` Similar to the TimelineVol mode, but instead of showing total coverage volume, it breaks coverage volume down by language so you can see which languages are focusing the most on a topic. Note that the GDELT APIs currently only search the 65 machine translated languages supported by GDELT, so stories trending in unsupported languages will not be displayed in this graph, but will likely be captured by GDELT as they are cross-covered in other languages. With the launch of GDELT3 later this summer, the resolution and utility of this graph will increase dramatically.
#' \item `TimelineSourceCountry` Similar to the TimelineVol mode, but instead of showing total coverage volume, it breaks coverage volume down by source country so you can see which countries are focusing the most on a topic. Note that GDELT attempts to monitor as much media as possible in each country, but smaller countries with less developed media systems will necessarily be less represented than larger countries with massive local press output. With the launch of GDELT3 later this summer, the resolution and utility of this graph will increase dramatically.
#' \item `ToneChart`This is an extremely powerful visualization that creates an emotional histogram showing the tonal distribution of coverage of your query. All coverage matching your query over the search time period is tallied up and binned by tone, from -100 (extremely negative) to +100 (extremely positive). (Though typically the actual range will be from -20 to 20 or less). Articles in the -1 to +1 bin tend to be more neutral or factually-focused, while those on either extreme tend to be emotionally-laden diatribes. Typically most sentiment dashboards display a single number representing the average of all coverage matching the query ala The average tone of Donald Trump coverage in the last week is -7. Such displays are not very informative since its unclear what precisely -7 means in terms of tone and whether that means that most coverage clustered around -7 or whether it means there were a lot of extremely negative and extremely positive coverage that averaged out to -7, but no actual coverage around that tonal range. By displaying tone as a histogram you are able to see the full distributional curve, including whether most coverage clusters around a particular range, whether it has an exponential or bell curve, etc. In HTML mode you can mouse over each bar to see a popup with the top 10 most relevant articles in that tone range and click on any of the headlines to view them.
#' `WordCloudImageTags` This is identical to the WordCloudEnglish mode, but instead of the article text words, this mode takes all of the VGKG-processed images found in the matching articles (or which matched any image query operators) and constructs a histogram of the top topics assigned by Google's deep learning neural network algorithms as part of the Google Cloud Vision API.
#' `WordCloudImageWebTags` This is identical to the WordCloudImageTags mode, but instead of using the tags assigned by Google's deep learning algorithms, it uses the Google knowledge graph topical taxonomy tags assigned by the Google Cloud Vision API's Web Annotations engine. This engine performs a reverse Google Images search on each image to locate all instances where it has been seen on the open web, examines the captions of all of those instances of the image and compiles a list of topical tags that capture the contents of those captions. In this way this field offers a far more powerful and higher resolution understanding of the primary topics and activities depicted in the image, including context that is not visible in the image, but relies on the captions assigned by others, whereas the WordCloudImageTags field displays the output of deep learning algorithms considering the visual contents of the image.
#' }
#' @param formats This controls what file format the results are displayed in. Not all formats are available for all modes.  To assist with website embedding, the CORS ACAO header for all output of the API is set to the wildcard "*", permitting universal embedding
#' \itemize{
#' \item `HTML` This is the default mode and returns a browser-based visualization or display. Some displays, such as word clouds, are static images, some, like the timeline modes, result in interactive clickable visualizations, and some result in simple HTML lists of images or articles. The specific output varies by mode, but all are intended to be displayed directly in the browser in a user-friendly intuitive display and are designed to be easily embedded in any page via an iframe.
#' \item `CSV` This returns the requested data in comma-delimited (CSV) format. The specific set of columns varies based on the requested output mode. Note that since some modes return multilingual content, the CSV is encoded as UTF8 and includes the UTF8 BOM to work around Microsoft Excel limitations handling UTF8 CSV files.
#' \item `RSS` This output format is only available in ArticleList mode and returns the list of matching article URLs and titles in RSS 2.0 format. This makes it possible to display the results using any standard RSS reader. It also makes it seamless for web archives to create tailored archival feeds to preserve news coverage on certain topics or meeting certain criteria.
#' \item `JSON` This returns the requested data in UTF8 encoded JSON. The specific fields varies by output mode.
#' \item `JSONP` This mode is identical to "JSON" mode, but accepts an additional parameter in the API URL "callback=XYZ" (if not present defaults to "callback") and wraps the JSON in that callback to return JSONP compliant JavaScript code.
#' \item `JSONFeed` This output format is only available in ArticleList mode and returns the list of matching article URLs and titles in JSONFeed 1.0 format.
#' }
#' @param timespans By default the DOC API searches the last 3 months of coverage monitored by GDELT. You can narrow this range by using this option to specify the number of months, weeks, days, hours or minutes (minimum of 15 minutes). The API then only searches documents published within the specified timespan backwards from the present time. If you would instead like to specify the precise start/end time of the search instead of an offset from the present time, you should use the STARTDATETIME/ENDDATETIME parameters
#' @param date_resolution These parameters allow you to specify the precise start and end date/times to search, instead of using an offset like with TIMESPAN.
#' @param maximum_records This option only applies to the ArticleList and various ImageCollage modes, it is ignored in all other modes. To conserve system resources, in Article List and the ImageCollage modes, the API only returns up 75 results by default, but this can be increased up to 250 results if desired by using this URL parameter.
#' @param sort_variable By default results are sorted by relevance to your query. Sometimes you may wish to sort by date or tone instead.
#' \itemize{
#' \item `DateDesc` Sorts results by publication date, displaying the most recent articles first
#' \item `DateAsc` Sorts results by publication date, displaying the oldest articles first.
#' \item `ToneDesc` Sorts results by tone, displays the most positive articles first.
#' \item `ToneAsc` Sorts results by tone, displays the most negative articles first
#' \item `HybridRel` This is the default new relevance sorting mode for all searches of content published after 12:01AM September 16, 2018. It uses a combination of the textual relevance of the article and other signals, including the "popularity" of the outlet to rank highly relevant content from well known outlets at top, rather than ranking content exclusively based on its textual relevance, which tends to surface obscure coverage. We will be constantly refining the underlying scoring models over time to yield the best possible results and once we have a final model that performs well in all scenarios we will retroactively apply it to our entire backfile and make it available for all searches. This mode is not currently available for image searches, only textual article searches
#' }
#' @param maximum_records Number of records
#' @param sort_variable By default results are sorted by relevance to your query. Sometimes you may wish to sort by date or tone instead.
#' \itemize{
#' \item `DateDesc` Sorts results by publication date, displaying the most recent articles first
#' \item `DateAsc` Sorts results by publication date, displaying the oldest articles first.
#' \item `ToneDesc` Sorts results by tone, displays the most positive articles first.
#' \item `ToneAsc` Sorts results by tone, displays the most negative articles first
#' \item `HybridRel` This is the default new relevance sorting mode for all searches of content published after 12:01AM September 16, 2018. It uses a combination of the textual relevance of the article and other signals, including the "popularity" of the outlet to rank highly relevant content from well known outlets at top, rather than ranking content exclusively based on its textual relevance, which tends to surface obscure coverage. We will be constantly refining the underlying scoring models over time to yield the best possible results and once we have a final model that performs well in all scenarios we will retroactively apply it to our entire backfile and make it available for all searches. This mode is not currently available for image searches, only textual article searches
#' }
#' @param timeline_smooth This option is only available in the various Timeline modes and performs moving window smoothing over the specified number of time steps, up to a maximum of 30. Due to GDELT's high temporal resolution, timeline displays can sometimes capture too much of the chaotic noisy information environment that is the global news landscape, resulting in jagged displays. Use this option to enable moving average smoothing up to 30 days.  Note that since this is a moving window average, peaks will be shifted to the right, up to several days or weeks at the heaviest smoothing levels.
#' @param start_date Start time YYYYMMDDHHMMSS
#' @param end_date  End time YYYYMMDDHHMMSS
#' @param timezone_adjust Timezone Adjus
#' @param time_zoom This option is only available for timeline modes in HTML format output and enables interactive zooming of the timeline using the browser-based visualization. Set to "yes" to enable and set to "no" or do not include the parameter, to disable. By default, the browser-based timeline display allows interactive examination and export of the timeline data, but does not allow the user to rezoom the display to a more narrow time span. If enabled, the user can click-drag horizontally in the graph to select a specific time period. If the visualization is being displayed directly by itself (it is the "parent" page), it will automatically refresh the page to display the revised time span. If the visualization is being embedded in another page via iframe, it will use postMessage to send the new timespan to the parent page with parameters "startdate" and "enddate" in the format needed by the STARTDATETIME and ENDDATETIME API parameters. The parent page can then use these parameters to rewrite the URLs of any API visualizations embedded in the page and reload each of them. This allows the creation of dashboard-like displays that contain multiple DOC API visualizations where the user can zoom the timeline graph at the top and have all of the other displays automatically refresh to narrow their coverage to that revised time frame.

#' @param parse_data If `TRUE` parse data
#' @param return_message If `TRUE` returns message
#' @param widen_url_parameters if `TRUE` widens URL parameters
#' @param widen_variables If `TRUE` variables to unite for API urls.  Default `c("mode", "timespan", "format")`
#' @param nest_data If `TRUE` nest parsed data
#'
#' @return
#' @export
#'
#' @examples
#' library(gdeltr2)
#' v2_doc_api(terms = c("Donald Trump"))
v2_doc_api <-
  function(terms = NULL,
           term_domains = NULL,
           term_exact_domains = NULL,
           use_exact_term = FALSE,
           domains = NULL,
           domains_exact = NULL,
           images_face_tone = NULL,
           images_number_faces = NULL,
           images_ocr_meta = NULL,
           image_tags = NULL,
           image_web_counts = NULL,
           image_web_tags = NULL,
           themes_gkg = NULL,
           near_terms = NULL,
           near_length = 20,
           repeat_terms = NULL,
           repeat_count = 3,
           source_languages = "english",
           source_countries = "United States",
           tone = NULL,
           tone_absolute = NULL,
           modes = "ArtList",
           formats = "json",
           timespans = NULL,
           date_resolution = NULL,
           maximum_records = 250,
           sort_variable = "DateDesc",
           timeline_smooth = NULL,
           start_date = NULL,
           end_date = NULL,
           timezone_adjust = NULL,
           time_zoom = NULL,
           parse_data = TRUE,
           widen_url_parameters = FALSE,
           widen_variables = c("mode", "timespan", "format"),
           nest_data = FALSE,
           return_message = TRUE) {
    if (length(modes) == 0) {
      stop("Enter modes")
    }

    all_data <- tibble::tibble()

    if (length(terms) > 0) {
      if (use_exact_term) {
        terms <- glue::glue("\"{terms}\"")
      }

      dat_input <-
        expand_grid(
          term = terms,
          term_domain = term_domains,
          term_exact_domain = term_exact_domains,
          timespan = timespans,
          mode = modes,
          format = formats,
          source_country = source_countries,
          source_language = source_languages
        )

      dat_urls <-
        1:nrow(dat_input) |>
        map_dfr(function(x) {
          dat_row <-
            dat_input[x, ]

          row_format <- dat_row[["format"]]
          row_mode <- dat_row[["mode"]]
          row_source_country <- dat_row[["source_country"]]
          row_source_language <- dat_row[["source_language"]]
          if (dat_row |> hasName("term")) {
            row_term <- dat_row[["term"]]
          } else {
            row_term <- NULL
          }

          if (dat_row |> hasName("timespan")) {
            row_timespan <- dat_row[["timespan"]]
          } else {
            row_timespan <- NULL
          }

          if (dat_row |> hasName("term_domain")) {
            row_term_domain <- dat_row[["term_domain"]]
          } else {
            row_term_domain <- NULL
          }

          if (dat_row |> hasName("term_exact_domain")) {
            row_term_exact_domain <- dat_row[["term_exact_domain"]]
          } else {
            row_term_exact_domain <- NULL
          }

          out <- .generate_v2_doc_url(
            term = row_term,
            domain = row_term_domain,
            domain_exact = row_term_exact_domain,
            image_face_tone = images_face_tone,
            image_number_faces = images_number_faces,
            image_ocr_meta = NULL,
            image_tag = NULL,
            image_web_count = NULL,
            image_web_tag = NULL,
            theme_gkg = NULL,
            near_term = NULL,
            near_length = near_length,
            repeat_term = repeat_terms,
            repeat_count = repeat_count,
            source_language = row_source_language,
            source_country = row_source_country,
            tone = tone,
            tone_absolute = tone_absolute,
            mode = row_mode,
            format = row_format,
            timespan = row_timespan,
            date_resolution = date_resolution,
            maximum_records = maximum_records,
            sort_variable = sort_variable,
            timeline_smooth = timeline_smooth,
            start_date = start_date,
            end_date = end_date,
            timezone_adjust = timezone_adjust,
            time_zoom = time_zoom,
            return_message = return_message
          )
        })



      all_data <- all_data |> bind_rows(dat_urls)

      if (nrow(dat_urls) > 0) {
        rm(dat_urls)
      }

    }

    if (length(domains) > 0) {
      dat_input <-
        expand_grid(
          domain = domains,
          timespan = timespans,
          mode = modes,
          format = formats,
          source_country = source_countries,
          source_language = source_languages

        )

      dat_urls <-
        1:nrow(dat_input) |>
        map_dfr(function(x) {
          dat_row <-
            dat_input[x, ]

          row_format <- dat_row[["format"]]
          row_mode <- dat_row[["mode"]]
          row_source_country <- dat_row[["source_country"]]
          row_source_language <- dat_row[["source_language"]]
          if (dat_row |> hasName("domain")) {
            row_domain <- dat_row[["domain"]]
          } else {
            row_domain <- NULL
          }

          if (dat_row |> hasName("timespan")) {
            row_timespan <- dat_row[["timespan"]]
          } else {
            row_timespan <- NULL
          }

          if (dat_row |> hasName("term_domain")) {
            row_term_domain <- dat_row[["term_domain"]]
          } else {
            row_term_domain <- NULL
          }

          if (dat_row |> hasName("term_exact_domain")) {
            row_term_exact_domain <- dat_row[["term_exact_domain"]]
          } else {
            row_term_exact_domain <- NULL
          }

          out <- .generate_v2_doc_url(
            term = NULL,
            domain = row_domain,
            domain_exact = NULL,
            image_face_tone = images_face_tone,
            image_number_faces = images_number_faces,
            image_ocr_meta = NULL,
            image_tag = NULL,
            image_web_count = NULL,
            image_web_tag = NULL,
            theme_gkg = NULL,
            near_term = near_terms,
            near_length = near_length,
            repeat_term = repeat_terms,
            repeat_count = repeat_count,
            source_language = row_source_language,
            source_country = row_source_country,
            tone = tone,
            tone_absolute = tone_absolute,
            mode = row_mode,
            format = row_format,
            timespan = row_timespan,
            date_resolution = date_resolution,
            maximum_records = maximum_records,
            sort_variable = sort_variable,
            timeline_smooth = timeline_smooth,
            start_date = start_date,
            end_date = end_date,
            timezone_adjust = timezone_adjust,
            time_zoom = time_zoom,
            return_message = return_message
          )
        })

      all_data <- all_data |> bind_rows(dat_urls)

      if (nrow(dat_urls) > 0) {
        rm(dat_urls)
      }
    }

    if (length(domains_exact) > 0) {
      dat_input <-
        expand_grid(
          domain_exact = domains_exact,
          timespan = timespans,
          mode = modes,
          format = formats,
          source_country = source_countries,
          source_language = source_languages
        )

      dat_urls <-
        1:nrow(dat_input) |>
        map_dfr(function(x) {
          dat_row <-
            dat_input[x, ]

          row_format <- dat_row[["format"]]
          row_mode <- dat_row[["mode"]]
          row_source_country <- dat_row[["source_country"]]
          row_source_language <- dat_row[["source_language"]]

          if (dat_row |> hasName("domain_exact")) {
            row_domain_exact <- dat_row[["domain_exact"]]
          } else {
            row_domain_exact <- NULL
          }

          if (dat_row |> hasName("timespan")) {
            row_timespan <- dat_row[["timespan"]]
          } else {
            row_timespan <- NULL
          }

          if (dat_row |> hasName("term_domain")) {
            row_term_domain <- dat_row[["term_domain"]]
          } else {
            row_term_domain <- NULL
          }

          if (dat_row |> hasName("term_exact_domain")) {
            row_term_exact_domain <- dat_row[["term_exact_domain"]]
          } else {
            row_term_exact_domain <- NULL
          }

          out <- .generate_v2_doc_url(
            term = NULL,
            domain = NULL,
            domain_exact = row_domain_exact,
            image_face_tone = images_face_tone,
            image_number_faces = images_number_faces,
            image_ocr_meta = NULL,
            image_tag = NULL,
            image_web_count = NULL,
            image_web_tag = NULL,
            theme_gkg = NULL,
            near_term = NULL,
            near_length = near_length,
            repeat_term = repeat_terms,
            repeat_count = repeat_count,
            source_language = row_source_language,
            source_country = row_source_country,
            tone = tone,
            tone_absolute = tone_absolute,
            mode = row_mode,
            format = row_format,
            timespan = row_timespan,
            date_resolution = date_resolution,
            maximum_records = maximum_records,
            sort_variable = sort_variable,
            timeline_smooth = timeline_smooth,
            start_date = start_date,
            end_date = end_date,
            timezone_adjust = timezone_adjust,
            time_zoom = time_zoom,
            return_message = return_message
          )
        })

      all_data <- all_data |> bind_rows(dat_urls)

      if (nrow(dat_urls) > 0) {
        rm(dat_urls)
      }
    }

    if (length(images_ocr_meta) > 0) {
      dat_input <-
        expand_grid(
          image_ocr_meta = images_ocr_meta,
          timespan = timespans,
          mode = modes,
          format = formats,
          source_country = source_countries,
          source_language = source_languages
        )

      dat_urls <-
        1:nrow(dat_input) |>
        map_dfr(function(x) {
          dat_row <-
            dat_input[x, ]

          row_format <- dat_row[["format"]]
          row_mode <- dat_row[["mode"]]
          row_source_country <- dat_row[["source_country"]]
          row_source_language <- dat_row[["source_language"]]

          if (dat_row |> hasName("image_ocr_meta")) {
            row_image_ocr_meta <- dat_row[["image_ocr_meta"]]
          } else {
            row_image_ocr_meta <- NULL
          }

          if (dat_row |> hasName("timespan")) {
            row_timespan <- dat_row[["timespan"]]
          } else {
            row_timespan <- NULL
          }

          if (dat_row |> hasName("term_domain")) {
            row_term_domain <- dat_row[["term_domain"]]
          } else {
            row_term_domain <- NULL
          }

          if (dat_row |> hasName("term_exact_domain")) {
            row_term_exact_domain <- dat_row[["term_exact_domain"]]
          } else {
            row_term_exact_domain <- NULL
          }

          out <- .generate_v2_doc_url(
            term = NULL,
            domain = NULL,
            domain_exact = NULL,
            image_face_tone = images_face_tone,
            image_number_faces = images_number_faces,
            image_ocr_meta = row_image_ocr_meta,
            image_tag = NULL,
            image_web_count = NULL,
            image_web_tag = NULL,
            theme_gkg = NULL,
            near_term = NULL,
            near_length = near_length,
            repeat_term = repeat_terms,
            repeat_count = repeat_count,
            source_language = row_source_language,
            source_country = row_source_country,
            tone = tone,
            tone_absolute = tone_absolute,
            mode = row_mode,
            format = row_format,
            timespan = row_timespan,
            date_resolution = date_resolution,
            maximum_records = maximum_records,
            sort_variable = sort_variable,
            timeline_smooth = timeline_smooth,
            start_date = start_date,
            end_date = end_date,
            timezone_adjust = timezone_adjust,
            time_zoom = time_zoom,
            return_message = return_message
          )
        })

      all_data <- all_data |> bind_rows(dat_urls)

      if (nrow(dat_urls) > 0) {
        rm(dat_urls)
      }
    }

    if (length(image_web_tags) > 0) {
      dat_input <-
        expand_grid(
          image_web_tag = image_web_tags,
          timespan = timespans,
          mode = modes,
          format = formats,
          source_country = source_countries,
          source_language = source_languages
        )

      dat_urls <-
        1:nrow(dat_input) |>
        map_dfr(function(x) {
          dat_row <-
            dat_input[x, ]

          row_format <- dat_row[["format"]]
          row_mode <- dat_row[["mode"]]
          row_source_country <- dat_row[["source_country"]]
          row_source_language <- dat_row[["source_language"]]

          if (dat_row |> hasName("image_web_tag")) {
            row_image_web_tag <- dat_row[["image_web_tag"]]
            row_image_web_tag <-
              glue::glue("\"{row_image_web_tag}\"")

          } else {
            row_image_web_tag <- NULL
          }

          if (dat_row |> hasName("timespan")) {
            row_timespan <- dat_row[["timespan"]]
          } else {
            row_timespan <- NULL
          }

          if (dat_row |> hasName("term_domain")) {
            row_term_domain <- dat_row[["term_domain"]]
          } else {
            row_term_domain <- NULL
          }

          if (dat_row |> hasName("term_exact_domain")) {
            row_term_exact_domain <- dat_row[["term_exact_domain"]]
          } else {
            row_term_exact_domain <- NULL
          }

          out <-
            .generate_v2_doc_url(
              term = NULL,
              domain = NULL,
              domain_exact = NULL,
              image_face_tone = images_face_tone,
              image_number_faces = images_number_faces,
              image_ocr_meta = NULL,
              image_tag = NULL,
              image_web_count = NULL,
              image_web_tag = row_image_web_tag,
              theme_gkg = NULL,
              near_term = NULL,
              near_length = near_length,
              repeat_term = repeat_terms,
              repeat_count = repeat_count,
              source_language = row_source_language,
              source_country = row_source_country,
              tone = tone,
              tone_absolute = tone_absolute,
              mode = row_mode,
              format = row_format,
              timespan = row_timespan,
              date_resolution = date_resolution,
              maximum_records = maximum_records,
              sort_variable = sort_variable,
              timeline_smooth = timeline_smooth,
              start_date = start_date,
              end_date = end_date,
              timezone_adjust = timezone_adjust,
              time_zoom = time_zoom,
              return_message = return_message
            )
        })

      all_data <- all_data |> bind_rows(dat_urls)

      if (nrow(dat_urls) > 0) {
        rm(dat_urls)
      }
    }

    if (length(themes_gkg) > 0) {
      dat_input <-
        expand_grid(
          theme_gkg = themes_gkg,
          timespan = timespans,
          mode = modes,
          format = formats,
          source_country = source_countries,
          source_language = source_languages
        )

      dat_urls <-
        1:nrow(dat_input) |>
        map_dfr(function(x) {
          dat_row <-
            dat_input[x, ]

          row_format <- dat_row[["format"]]
          row_mode <- dat_row[["mode"]]
          row_source_country <- dat_row[["source_country"]]
          row_source_language <- dat_row[["source_language"]]

          if (dat_row |> hasName("theme_gkg")) {
            row_theme_gkg <- dat_row[["theme_gkg"]]
            row_theme_gkg <-
              glue::glue("\"{row_theme_gkg}\"")

          } else {
            row_theme_gkg <- NULL
          }

          if (dat_row |> hasName("timespan")) {
            row_timespan <- dat_row[["timespan"]]
          } else {
            row_timespan <- NULL
          }

          if (dat_row |> hasName("term_domain")) {
            row_term_domain <- dat_row[["term_domain"]]
          } else {
            row_term_domain <- NULL
          }

          if (dat_row |> hasName("term_exact_domain")) {
            row_term_exact_domain <- dat_row[["term_exact_domain"]]
          } else {
            row_term_exact_domain <- NULL
          }

          out <-
            .generate_v2_doc_url(
              term = NULL,
              domain = NULL,
              domain_exact = NULL,
              image_face_tone = images_face_tone,
              image_number_faces = images_number_faces,
              image_ocr_meta = NULL,
              image_tag = NULL,
              image_web_count = NULL,
              image_web_tag = NULL,
              theme_gkg = row_theme_gkg,
              near_term = NULL,
              near_length = near_length,
              repeat_term = repeat_terms,
              repeat_count = repeat_count,
              source_language = row_source_language,
              source_country = row_source_country,
              tone = tone,
              tone_absolute = tone_absolute,
              mode = row_mode,
              format = row_format,
              timespan = row_timespan,
              date_resolution = date_resolution,
              maximum_records = maximum_records,
              sort_variable = sort_variable,
              timeline_smooth = timeline_smooth,
              start_date = start_date,
              end_date = end_date,
              timezone_adjust = timezone_adjust,
              time_zoom = time_zoom,
              return_message = return_message
            )
        })

      all_data <- all_data |> bind_rows(dat_urls)

      if (nrow(dat_urls) > 0) {
        rm(dat_urls)
      }
    }

    if (length(near_terms) > 0) {
      dat_input <-
        expand_grid(
          near_term = near_terms,
          near_length = near_length,
          timespan = timespans,
          mode = modes,
          format = formats,
          source_country = source_countries,
          source_language = source_languages
        )

      dat_urls <-
        1:nrow(dat_input) |>
        map_dfr(function(x) {
          dat_row <-
            dat_input[x, ]

          row_format <- dat_row[["format"]]
          row_mode <- dat_row[["mode"]]
          row_source_country <- dat_row[["source_country"]]
          row_source_language <- dat_row[["source_language"]]

          if (dat_row |> hasName("near_term")) {
            row_near_term <- dat_row[["near_term"]]
            row_near_term <-
              glue::glue("\"{row_near_term}\"")

          } else {
            row_near_term <- NULL
          }

          if (dat_row |> hasName("timespan")) {
            row_timespan <- dat_row[["timespan"]]
          } else {
            row_timespan <- NULL
          }

          if (dat_row |> hasName("term_domain")) {
            row_term_domain <- dat_row[["term_domain"]]
          } else {
            row_term_domain <- NULL
          }

          if (dat_row |> hasName("near_length")) {
            row_near_length <- dat_row[["near_length"]]
          } else {
            row_near_length <- NULL
          }

          if (dat_row |> hasName("term_exact_domain")) {
            row_term_exact_domain <- dat_row[["term_exact_domain"]]
          } else {
            row_term_exact_domain <- NULL
          }

          out <-
            .generate_v2_doc_url(
              term = NULL,
              domain = NULL,
              domain_exact = NULL,
              image_face_tone = images_face_tone,
              image_number_faces = images_number_faces,
              image_ocr_meta = NULL,
              image_tag = NULL,
              image_web_count = NULL,
              image_web_tag = NULL,
              theme_gkg = NULL,
              near_term = row_near_term,
              near_length = row_near_length,
              repeat_term = repeat_terms,
              repeat_count = repeat_count,
              source_language = row_source_language,
              source_country = row_source_country,
              tone = tone,
              tone_absolute = tone_absolute,
              mode = row_mode,
              format = row_format,
              timespan = row_timespan,
              date_resolution = date_resolution,
              maximum_records = maximum_records,
              sort_variable = sort_variable,
              timeline_smooth = timeline_smooth,
              start_date = start_date,
              end_date = end_date,
              timezone_adjust = timezone_adjust,
              time_zoom = time_zoom,
              return_message = return_message
            )
        })

      all_data <- all_data |> bind_rows(dat_urls)

      if (nrow(dat_urls) > 0) {
        rm(dat_urls)
      }
    }

    if (length(repeat_terms) > 0) {
      dat_input <-
        expand_grid(
          repeat_term = repeat_terms,
          repeat_count = repeat_count,
          timespan = timespans,
          mode = modes,
          format = formats,
          source_country = source_countries,
          source_language = source_languages
        )

      dat_urls <-
        1:nrow(dat_input) |>
        map_dfr(function(x) {
          dat_row <-
            dat_input[x, ]

          row_format <- dat_row[["format"]]
          row_mode <- dat_row[["mode"]]
          row_source_country <- dat_row[["source_country"]]
          row_source_language <- dat_row[["source_language"]]

          if (dat_row |> hasName("repeat_term")) {
            row_repeat_term <- dat_row[["repeat_term"]]
            row_repeat_term <-
              glue::glue("\"{row_repeat_term}\"")

          } else {
            row_repeat_term <- NULL
          }

          if (dat_row |> hasName("timespan")) {
            row_timespan <- dat_row[["timespan"]]
          } else {
            row_timespan <- NULL
          }

          if (dat_row |> hasName("term_domain")) {
            row_term_domain <- dat_row[["term_domain"]]
          } else {
            row_term_domain <- NULL
          }

          if (dat_row |> hasName("repeat_count")) {
            row_repeat_count <- dat_row[["repeat_count"]]
          } else {
            row_repeat_count <- NULL
          }

          if (dat_row |> hasName("term_exact_domain")) {
            row_term_exact_domain <- dat_row[["term_exact_domain"]]
          } else {
            row_term_exact_domain <- NULL
          }

          out <-
            .generate_v2_doc_url(
              term = NULL,
              domain = NULL,
              domain_exact = NULL,
              image_face_tone = images_face_tone,
              image_number_faces = images_number_faces,
              image_ocr_meta = NULL,
              image_tag = NULL,
              image_web_count = NULL,
              image_web_tag = NULL,
              theme_gkg = NULL,
              near_term = NULL,
              near_length = NULL,
              repeat_term = row_repeat_term,
              repeat_count = row_repeat_count,
              source_language = row_source_language,
              source_country = row_source_country,
              tone = tone,
              tone_absolute = tone_absolute,
              mode = row_mode,
              format = row_format,
              timespan = row_timespan,
              date_resolution = date_resolution,
              maximum_records = maximum_records,
              sort_variable = sort_variable,
              timeline_smooth = timeline_smooth,
              start_date = start_date,
              end_date = end_date,
              timezone_adjust = timezone_adjust,
              time_zoom = time_zoom,
              return_message = return_message
            )
        })

      all_data <- all_data |> bind_rows(dat_urls)

      if (nrow(dat_urls) > 0) {
        rm(dat_urls)
      }
    }
    if (!parse_data) {
      if (widen_url_parameters) {
        all_data <- widen_gdelt_url_api_parameters(
          data = all_data,
          url_gdelt = "url_gdelt_v2_doc_api",
          widen_variables = widen_variables
        )
      }

      return(all_data)
    }

    if (length(modes) >= 2) {
      nest_data <- T
    }

    out <-
      all_data$url_gdelt_v2_doc_api |>
      parse_v2_doc_api_urls(return_message = return_message, nest_data = T)

    all_data <-
      all_data |>
      mutate(url_gdelt_v2_doc_api = str_to_lower(url_gdelt_v2_doc_api)) |>
      left_join(out, by = "url_gdelt_v2_doc_api")

    if (!nest_data) {
      all_data <- all_data |> unnest()
    }


    all_data
  }


# geo ---------------------------------------------------------------------

# https://blog.gdeltproject.org/gdelt-geo-2-0-api-debuts/
