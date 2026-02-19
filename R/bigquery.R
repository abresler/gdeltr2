# BigQuery integration for GDELT corpus queries via Google BigQuery


# utils -------------------------------------------------------------------

#' Dictionary of Available GDELT BigQuery Tables
#'
#' Returns a tibble describing all GDELT tables available in Google BigQuery,
#' including their full BigQuery paths.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{name_table}{Short identifying name for the table}
#'     \item{description}{Human-readable description of the table's contents}
#'     \item{path_bq}{Full BigQuery path in project:dataset.table format}
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' dictionary_gdelt_bq_tables()
#' }
dictionary_gdelt_bq_tables <- function() {
  tibble::tibble(
    name_table = c(
      "events",
      "eventmentions",
      "gkg",
      "gkg_partitioned",
      "gfg_partitioned",
      "hathitrust_books",
      "iatv_1grams",
      "iatv_2grams"
    ),
    description = c(
      "GDELT 2.0 Events: global events database with CAMEO-coded event records",
      "GDELT 2.0 Event Mentions: individual article mentions of each event",
      "Global Knowledge Graph: thematic, emotional, and geographic metadata per article",
      "Global Knowledge Graph partitioned by date for efficient date-range queries",
      "Global Frontpage Graph partitioned by date: frontpage story tracking",
      "HathiTrust Books: full text from digitized book collections",
      "Internet Archive TV 1-grams: word frequency from television transcripts",
      "Internet Archive TV 2-grams: bigram frequency from television transcripts"
    ),
    path_bq = c(
      "gdelt-bq.gdeltv2.events",
      "gdelt-bq.gdeltv2.eventmentions",
      "gdelt-bq.gdeltv2.gkg",
      "gdelt-bq.gdeltv2.gkg_partitioned",
      "gdelt-bq.gdeltv2.gfg_partitioned",
      "gdelt-bq.hathitrustbooks.all",
      "gdelt-bq.gdeltv2.iatv_1grams",
      "gdelt-bq.gdeltv2.iatv_2grams"
    )
  )
}


# query -------------------------------------------------------------------

#' Execute a SQL Query Against GDELT BigQuery
#'
#' Runs an arbitrary SQL query against the GDELT BigQuery corpus using the
#' bigrquery package. Requires a valid Google Cloud project with BigQuery
#' enabled and appropriate authentication.
#'
#' @param query Character string. A valid BigQuery SQL query.
#' @param project Character string. The billing Google Cloud project ID used
#'   for query costs. Defaults to \code{"gdelt-bq"}.
#' @param max_results Integer. Maximum number of rows to download.
#'   Defaults to \code{10000}.
#' @param return_message Logical. If \code{TRUE}, prints progress messages
#'   via \code{cli}. Defaults to \code{TRUE}.
#'
#' @return A tibble containing the query results, or \code{NULL} invisibly
#'   on error.
#' @export
#'
#' @examples
#' \dontrun{
#' sql <- "
#'   SELECT Actor1CountryCode, COUNT(*) AS n_events
#'   FROM `gdelt-bq.gdeltv2.events`
#'   WHERE Year = 2023
#'   GROUP BY Actor1CountryCode
#'   ORDER BY n_events DESC
#'   LIMIT 20
#' "
#' gdelt_bq_query(query = sql, project = "my-gcp-project")
#' }
gdelt_bq_query <- function(
    query,
    project = "gdelt-bq",
    max_results = 10000,
    return_message = TRUE
) {
  if (!requireNamespace("bigrquery", quietly = TRUE)) {
    stop(
      "Package 'bigrquery' is required for BigQuery queries. ",
      "Install it with: install.packages('bigrquery')",
      call. = FALSE
    )
  }

  if (missing(query) || !nzchar(trimws(query))) {
    stop("'query' must be a non-empty SQL string.", call. = FALSE)
  }

  if (return_message) {
    cli::cli_alert_info("Submitting query to GDELT BigQuery (project: {project}) ...")
  }

  tryCatch(
    {
      bq_table <- bigrquery::bq_project_query(project, query)

      if (return_message) {
        cli::cli_alert_info("Downloading up to {max_results} rows ...")
      }

      result <- bigrquery::bq_table_download(bq_table, max_results = max_results)
      result <- tibble::as_tibble(result)

      if (return_message) {
        cli::cli_alert_info(
          "Query returned {nrow(result)} row{?s} and {ncol(result)} column{?s}."
        )
      }

      result
    },
    error = function(e) {
      cli::cli_alert_danger("BigQuery query failed: {conditionMessage(e)}")
      invisible(NULL)
    }
  )
}


# events ------------------------------------------------------------------

#' Query GDELT 2.0 Events Table via BigQuery
#'
#' Convenience wrapper that queries the \code{gdelt-bq.gdeltv2.events} table
#' with optional date filtering and additional WHERE conditions.
#'
#' @param start_date Character or Date. Start of the date range (inclusive),
#'   in \code{"YYYY-MM-DD"} format. If \code{NULL}, no lower date bound is
#'   applied.
#' @param end_date Character or Date. End of the date range (inclusive),
#'   in \code{"YYYY-MM-DD"} format. If \code{NULL}, no upper date bound is
#'   applied.
#' @param sql_where Character string. An additional SQL WHERE clause fragment
#'   (without the \code{WHERE} keyword) to further filter results. This is
#'   combined with the date filter using \code{AND}. Defaults to \code{NULL}.
#' @param project Character string. The billing Google Cloud project ID.
#'   Defaults to \code{"gdelt-bq"}.
#' @param max_results Integer. Maximum number of rows to download.
#'   Defaults to \code{10000}.
#' @param return_message Logical. If \code{TRUE}, prints progress messages.
#'   Defaults to \code{TRUE}.
#'
#' @return A tibble of GDELT 2.0 event records, or \code{NULL} invisibly
#'   on error.
#' @export
#'
#' @examples
#' \dontrun{
#' # Events involving the United States in 2023
#' gdelt_bq_events(
#'   start_date = "2023-01-01",
#'   end_date   = "2023-12-31",
#'   sql_where  = "Actor1CountryCode = 'USA'",
#'   project    = "my-gcp-project",
#'   max_results = 5000
#' )
#' }
gdelt_bq_events <- function(
    start_date = NULL,
    end_date = NULL,
    sql_where = NULL,
    project = "gdelt-bq",
    max_results = 10000,
    return_message = TRUE
) {
  table_path <- "`gdelt-bq.gdeltv2.events`"

  date_filters <- character(0)

  if (!is.null(start_date)) {
    start_str <- format(as.Date(start_date), "%Y%m%d")
    date_filters <- c(date_filters, glue::glue("SQLDATE >= {start_str}"))
  }

  if (!is.null(end_date)) {
    end_str <- format(as.Date(end_date), "%Y%m%d")
    date_filters <- c(date_filters, glue::glue("SQLDATE <= {end_str}"))
  }

  if (!is.null(sql_where) && nzchar(trimws(sql_where))) {
    date_filters <- c(date_filters, sql_where)
  }

  where_clause <- if (length(date_filters) > 0) {
    paste("WHERE", paste(date_filters, collapse = " AND "))
  } else {
    ""
  }

  query <- glue::glue("SELECT * FROM {table_path} {where_clause} LIMIT {max_results}")

  if (return_message) {
    cli::cli_alert_info("Querying GDELT 2.0 Events table ...")
  }

  gdelt_bq_query(
    query          = query,
    project        = project,
    max_results    = max_results,
    return_message = return_message
  )
}


# gkg ---------------------------------------------------------------------

#' Query GDELT Global Knowledge Graph Table via BigQuery
#'
#' Convenience wrapper that queries the \code{gdelt-bq.gdeltv2.gkg} table
#' with optional date filtering and additional WHERE conditions.
#'
#' @param start_date Character or Date. Start of the date range (inclusive),
#'   in \code{"YYYY-MM-DD"} format. The GKG date column (\code{DATE}) is
#'   stored as an integer in \code{YYYYMMDDHHMMSS} format; filtering is applied
#'   against the leading 8 digits. If \code{NULL}, no lower date bound is
#'   applied.
#' @param end_date Character or Date. End of the date range (inclusive),
#'   in \code{"YYYY-MM-DD"} format. If \code{NULL}, no upper date bound is
#'   applied.
#' @param sql_where Character string. An additional SQL WHERE clause fragment
#'   (without the \code{WHERE} keyword) to further filter results. Defaults
#'   to \code{NULL}.
#' @param project Character string. The billing Google Cloud project ID.
#'   Defaults to \code{"gdelt-bq"}.
#' @param max_results Integer. Maximum number of rows to download.
#'   Defaults to \code{10000}.
#' @param return_message Logical. If \code{TRUE}, prints progress messages.
#'   Defaults to \code{TRUE}.
#'
#' @return A tibble of GDELT GKG records, or \code{NULL} invisibly on error.
#' @export
#'
#' @examples
#' \dontrun{
#' # GKG records mentioning climate from 2023
#' gdelt_bq_gkg(
#'   start_date = "2023-01-01",
#'   end_date   = "2023-03-31",
#'   sql_where  = "THEMES LIKE '%CLIMATE%'",
#'   project    = "my-gcp-project",
#'   max_results = 2000
#' )
#' }
gdelt_bq_gkg <- function(
    start_date = NULL,
    end_date = NULL,
    sql_where = NULL,
    project = "gdelt-bq",
    max_results = 10000,
    return_message = TRUE
) {
  table_path <- "`gdelt-bq.gdeltv2.gkg`"

  date_filters <- character(0)

  if (!is.null(start_date)) {
    start_str <- format(as.Date(start_date), "%Y%m%d")
    date_filters <- c(date_filters, glue::glue("LEFT(CAST(DATE AS STRING), 8) >= '{start_str}'"))
  }

  if (!is.null(end_date)) {
    end_str <- format(as.Date(end_date), "%Y%m%d")
    date_filters <- c(date_filters, glue::glue("LEFT(CAST(DATE AS STRING), 8) <= '{end_str}'"))
  }

  if (!is.null(sql_where) && nzchar(trimws(sql_where))) {
    date_filters <- c(date_filters, sql_where)
  }

  where_clause <- if (length(date_filters) > 0) {
    paste("WHERE", paste(date_filters, collapse = " AND "))
  } else {
    ""
  }

  query <- glue::glue("SELECT * FROM {table_path} {where_clause} LIMIT {max_results}")

  if (return_message) {
    cli::cli_alert_info("Querying GDELT Global Knowledge Graph table ...")
  }

  gdelt_bq_query(
    query          = query,
    project        = project,
    max_results    = max_results,
    return_message = return_message
  )
}
