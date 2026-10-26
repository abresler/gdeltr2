pkg <- "gdeltr2"

test_that("gdeltr2 namespace loads", {
  expect_true(requireNamespace(pkg, quietly = TRUE))
})

test_that("every NAMESPACE export() exists in the namespace", {
  ns <- readLines(system.file("NAMESPACE", package = pkg))
  declared <- grep("^export\\(", ns, value = TRUE)
  declared <- sub("^export\\((.*)\\)$", "\\1", declared)
  declared <- trimws(gsub("\"", "", unlist(strsplit(declared, ","))))
  expect_gt(length(declared), 0)
  missing <- setdiff(declared, getNamespaceExports(pkg))
  expect_equal(missing, character(0))
})

test_that("collapse_terms joins search terms with OR", {
  expect_equal(gdeltr2::collapse_terms("x"), "x")
  expect_equal(gdeltr2::collapse_terms(c("a b", "c", "d")), "a b OR c OR d")
})

test_that("dictionary_v2_doc_api_modes lists the DOC API modes", {
  modes <- gdeltr2::dictionary_v2_doc_api_modes()
  expect_length(modes, 15)
  expect_equal(modes[1:3], c("ArtList", "ArtGallery", "ImageCollage"))
  expect_true("TimelineTone" %in% modes)
})

test_that("parse_gkg_mentioned_article_tone splits the tone field into scores", {
  old <- options(scipen = 0, digits = 7)
  withr::defer(options(old))
  df <- dplyr::tibble(idGKG = "20200101000000-1", tone = "-4.5,1.5,6,7.5,13,0,134")
  long <- suppressWarnings(gdeltr2::parse_gkg_mentioned_article_tone(df, return_wide = FALSE))
  expect_equal(names(long), c("idGKG", "numberItem", "scoreTone"))
  expect_equal(long$scoreTone, c(-4.5, 1.5, 6, 7.5, 13, 0, 134))
  expect_equal(long$numberItem, 0:6)
  wide <- suppressWarnings(gdeltr2::parse_gkg_mentioned_article_tone(df))
  expect_equal(nrow(wide), 1)
  expect_equal(wide$scoreTone, -4.5)
  expect_equal(wide$scoreTone6, 134)
  expect_error(gdeltr2::parse_gkg_mentioned_article_tone(dplyr::tibble(idGKG = "x")), "missing tone")
})

test_that("a live GDELT call returns the most recent GKG log URLs", {
  skip_on_cran()
  skip_if_offline()
  out <- gdeltr2::get_urls_gkg_most_recent_log()
  expect_gt(nrow(out), 0)
})
