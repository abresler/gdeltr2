library(tidyverse)
library(gdeltr2)
ft_v2_api(
  terms = "United States",
  modes = c("WordCloudImageWebTags"),
  visualize_results = F,
  timespans = "10 days"
) %>%
  asbviz::hc_xy(
  name = "label",
  y = "size",
  type = "wordcloud",
  title = "ANTIFA Google Cloud Vision Image Tags",
  theme_name = "538"
)



gdeltr2::ft_v2_api(
  terms = c("ANTIFA", "CCP", "James Mattis", "Donald Trump"),
  modes = c("TimelineVolInfo"),
  visualize_results = F,
  timespans = "13 days",
  source_countries = "US"
) %>%
  rename(tone = value) %>%
  asbviz::hc_xy(
    name = "titleArticle",
    x = "datetimeData",
    y = "tone",
    group = "termSearch",
    title = "Media Tone -- Last 13 Days",
    subtitle = "data via gdeltr2",
    type = "spline",
    link = "urlArticle",
    use_point_select = T,
    color_palette = "pals::kovesi.rainbow_bgyr_35_85_c73",
    override_legend_location = NULL,
    theme_name = "elementary",
  )


gdeltr2::ft_v2_api(
  domains = c("cnn.com", "washingtonpost.com", "nytimes.com"),
  modes = c("TimelineVolInfo"),
  visualize_results = F,
  timespans = "13 days",
  source_countries = "US"
) %>%
  rename(tone = value) %>%
  asbviz::hc_xy(
    name = "titleArticle",
    x = "datetimeData",
    y = "tone",
    group = "domainSearch",
    title = "Media Tone -- Last 13 Days",
    subtitle = "data via gdeltr2",
    type = "spline",
    link = "urlArticle",
    use_point_select = T,
    color_palette = "pals::kovesi.rainbow_bgyr_35_85_c73",
    override_legend_location = NULL,
    theme_name = "elementary",
  )