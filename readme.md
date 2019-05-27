gdeltr2
================

`gdeltr2` — **R’s modern GDELT Project interface**

#### <strong>What is the GDELT Project?</strong>

[The Global Database of Events, Language, and
Tone](http://gdeltproject.org/) **\[GDELT\]** is a non profit whose
initiative is to:

<blockquote>

construct a catalog of human societal-scale behavior and beliefs across
all countries of the world, connecting every person, organization,
location, count, theme, news source, and event across the planet into a
single massive network that captures what’s happening around the world,
what its context is and who’s involved, and how the world is feeling
about it, every single day.

</blockquote>

GDELT was founded in 1994 and it’s data commences in 1979. Over the last
two years the GDELT’s functionality and abilities have grown
exponentially, for example in May 2014 GDELT processed 3,928,926 where
as in May 2016 it processed 6,198,461. GDELT continues to evolve and
integrate advanced machine learning tools including [Google Cloud
Vision](https://cloud.google.com/vision/), a data store that became
available in February
2016.

#### <strong>This package wraps GDELT’s four primary data stores</strong>

  - [The GDELT Events
    Database](http://gdeltproject.org/data.html#rawdatafiles)
    **\[EVENTS\]**: Global Events, 1979 to present.
  - [The GDELT Global Knowledge
    Graph](http://gdeltproject.org/data.html#rawdatafiles) **\[GKG\]** :
    GDELT’s Knowledge Graph, April 2013 to present.
  - [The GDELT Full Text
    API](http://blog.gdeltproject.org/announcing-the-gdelt-full-text-search-api/)
    **\[Full Text API\]**: Full text search for all monitored sources
    within a 24 hour window. Output includes raw data, sentiment, and
    word counts.
  - [The GDELT Visual Knowledge
    Graph](http://blog.gdeltproject.org/gdelt-visual-knowledge-graph-vgkg-v1-0-available/)
    **VGKG**: Google Cloud Vision API output for every indexed piece of
    GKG media.

#### <strong>Why gdeltr2?</strong>

My main motivation for this building package is simple, **GDELT IS
INCREDIBLE\!\!**

Accessing GDELT’s data gold is doable but either difficult or costly.

Currently, anyone proficient in SQL can access the data via [Google Big
Query](https://bigquery.cloud.google.com/dataset/gdelt-bq:gdeltv2?pli=1).
The problem is that even if you want to use SQL, users have to pay above
a certain API call threshold and then you still need another layer of
connectivity to explore the data in R.

Although R has two existing packages that allow users to interact with
portions of GDELT’s data outside of Big Query:

  - [gdeltr](https://github.com/ahalterman/gdeltr)
  - [GDELTtools](https://cran.r-project.org/web/packages/GDELTtools/)

These packages are old, incomplete and difficult to use. It is my hope
that `gdelt2r` allows the R user easy access to GDELT’s data allowing
for faster, more exhilarating data visualizations and analysis\!

#### <strong>PRIOR TO INSTALL</strong>

This package may require the development versions of `devtools` and
`dplyr` so, to be safe, before installation run the following code:

``` r
devtools::install_github("hadley/devtools")
devtools::install_github("hadley/dplyr")
devtools::install_github("hafen/trelliscopejs")
```

#### <strong>Installation</strong>

``` r
devtools::install_github("abresler/gdeltr2")
```

#### <strong>Function Ontology</strong>

The package currently consists of two function families, **data
acquisition** and **data tidying**.

The package’s data acquisition functions begin with `get_urls_` for
acquiring data store log information, `get_codes_` for acquiring code
books and `get_data_` for downloading and reading data.

The data tidying functions begin with `parse_` and they apply to a
number of the features in the **gkg** and **vgkg** data stores that will
get described in further detail farther below.

#### <strong>CAUTION</strong>

  - `gdeltr2` requires an internet connection for any data retrieval
    function
  - The package’s `get_gkg_data` and `get_gdelt_event_` functions are
    extremely bandwidth intensive given the download sizes of these data
    stores.
  - The package is very memory intensive given the unzipped size of the
    `GDELT Event`, `Global Knowledge Graph` and `Visual Knowledge Graph`
    files.

#### <strong>Primary Functions</strong>

  - <strong>Full Text API</strong>
      - `get_data_ft_v2_api()` - retrieves descriptive data from V2 API
        see
        [this](https://asbcllc.com/blog/2017/august/intro_to_programming_with_gdeltr2/index.html)
        blog post for more on how to use this
      - `get_data_ft_trending_terms()` - retrieves trending terms over
        the last 15 minutes. The term can be a GDELT tag, location,
        person, place, or thing.
  - <strong>[GDELT
    Events](http://gdeltproject.org/data.html#documentation)</strong>
      - `get_urls_gdelt_event_log()` - retrieves descriptive data and
        urls for all available GDELT event downloads.
      - `get_data_gdelt_period_event_totals()` - retrieves summary event
        data for a given a period \[monthly, daily, yearly\]; this can
        be grouped by country.
      - `get_data_gdelt_periods_event()` - retrieves GDELT event data
        for a specified periods. Periods are by 4 digit years from 1979
        to 2005, 6 digit year month from January 2006 to March 2013, and
        8 digit year month day code thereafter.
  - <strong>[Global Knowledge
    Graph](http://blog.gdeltproject.org/gdelt-2-0-our-global-world-in-realtime/)</strong>
      - `get_urls_gkg_15_minute_log` - retrieves GKG 15 minute capture
        logs; data begins February 18th, 2015 for the three table types
          - gkg: This is the full gkg data set and contains columns that
            may require further data tidying tying to a **GKG Record
            ID**
          - export: This data replicates the output contained in the
            GDELT event table for processed documents tying to a
            **Global Event ID**
          - mentions: This data contains information surrounding the
            processed events, including sources, tone, location within a
            document and this tying to a **Global Event ID**
      - `get_urls_gkg_daily_summaries` - retrieves daily gkg capture
        logs; data begins in April of 2013.
          - Each day contains a count file and the full gkg output.
      - `get_data_gkg_day_summary()` retrieves GKG daily summary data
        for specified date(s), this captures *count files* by
        `is_count_file = T`
      - `get_data_gkg_days_detailed()` - retrieves GKG data from the
        data cached every 15 minutes for specified date(s) for a given
        table. The table can be one of `c('gkg', 'export', 'mentions')`.
        This function may require significant bandwidth and memory given
        the potential file sizes.
  - <strong>[American Television Knowledge
    Graph](http://blog.gdeltproject.org/announcing-the-american-television-global-knowledge-graph-tv-gkg/)
      - `get_urls_gkg_tv_daily_summaries()` - retrieves available dates
          - `get_data_gkg_tv_days()` - retrieves data for specified
            dates. Note that the data is on a 2 day lag so the most
            recent data is 2 days old.
  - <strong>[Location Sentiment
    API](http://blog.gdeltproject.org/announcing-the-gdelt-stability-dashboard-api-stability-timeline/)</strong>
      - `get_codes_stability_locations()` - retrieves possible locations
      - `get_data_locations_instability_api()` - retrieves instability
        data for a specified location and time period. Variables can be
        `c('instability', 'conflict', 'protest', 'tone', 'relative
        mentions')` Time periods can be `c('daily', '15 minutes')`, for
        `daily` the data is the average per day of the specified
        variable for the last 180 days and for `15 minutes` the data is
        the variable reading every 15 minutes for the last week.
  - <strong>[Visual Global Knowledge
    Graph](http://blog.gdeltproject.org/gdelt-visual-knowledge-graph-vgkg-v1-0-available/)</strong>
      - `get_urls_vgkg()` - retrieves VGKG log urls
      - `get_data_vgkg_dates()` - retrieves VGKG data from the data
        cached every 15 minutes for specified date(s).

#### <strong>Tidying Functions</strong>

Many of the columns in the GKG output are concatenated and require
further parsing for proper analysis. These function tidy those
concatenated columns, note given file sizes the functions may be time
consuming.

### V2 Full Text API

You can refer to
[this](https://asbcllc.com/blog/2017/august/intro_to_programming_with_gdeltr2/index.html)
blog post that discusses how to use this functionality.

#### Global Knowledge Graph

  - `parse_gkg_mentioned_names()` - parses mentioned names
  - `parse_gkg_mentioned_people()` - parses mentioned people
  - `parse_gkg_mentioned_organizations()` - parses mentioned
    organizations
  - `parse_gkg_mentioned_numerics()` - parses mentioned numeric figures
  - `parse_gkg_mentioned_themes()` - parses mentioned themes, ties to
    CAMEO Theme Codes
  - `parse_gkg_mentioned_gcams()` - parses resolved GCAMs ties GCAM code
    book.
  - `parse_gkg_mentioned_dates()` - parses mentioned dates according to
    the GKG scheme
  - `parse_xml_extras()` - parses XML metadata from GKG table

##### Visual Global Knowledge Graph

  - `parse_vgkg_labels()` - parses and labels learned items
  - `parse_vgkg_landmarks()` - parses and geocodes learned landmarks
  - `parse_vgkg_logos()` - parses learned logos
  - `parse_vgkg_safe_search()` - parses safe search likelihoods
  - `parse_vgkg_faces()` - parses learned faces
  - `parse_vgkg_ocr()` - parses OCR’d items
  - `parse_vgkg_languages()` - parses languages

#### <strong>Code Books</strong>

All these the GDELT and GKG datasets contain a whole host of codes that
need resolution to be human readable. The package contains easy access
to these code books to allow for that resolution. These functions
provide access to the code books:

  - `get_codes_gcam()` - retrieves Global Content Analysis Measurement
    **\[GCAM\]** codes
  - `get_codes_cameo_country()` - retrieves Conflict and Mediation Event
    Observations **\[CAMEO\]** country codes
  - `get_codes_cameo_ethnic()` - retrieves cameo ethnic codes
  - `get_codes_cameo_events()` - retrieves cameo event codes
  - `get_codes_gkg_themes()` - retrieves gkg theme codes
  - `get_codes_cameo_type()` - retrieves cameo type codes
  - `get_codes_cameo_religion()` - retrieves cameo religion codes
  - `get_codes_cameo_known_groups()` - retrieves cameo known group codes

#### Coming Soon

  - Vignettes
  - Generic data visualization functions
  - Generic machine learning and data analysis functions
  - [`bigrquery`](https://github.com/rstats-db/bigrquery) integration
  - Third party database mirror

## <strong>EXAMPLES</strong>

``` r
library(gdeltr2)
load_needed_packages(c('dplyr', 'magrittr'))
```

### GDELT Event Data

``` r
events_1989 <-
  get_data_gdelt_periods_event(
    periods = 1989,
    return_message = T
  )
```

### GKG Data

``` r
gkg_summary_count_may_15_16_2014 <-
  get_data_gkg_days_summary(
    dates = c('2014-05-15', '2014-05-16'),
    is_count_file = T,
    return_message = T
  )

gkg_full_june_2_2016 <-
  get_data_gkg_days_detailed(
    dates = c("2016-06-02"),
    table_name = 'gkg',
    return_message = T
  )

gkg_mentions_may_12_2016 <-
  get_data_gkg_days_detailed(
    dates = c("2016-05-12"),
    table_name = 'mentions',
    return_message = T
  )
```

#### GKG Television Data

``` r
gkg_tv_test <- 
  get_data_gkg_tv_days(dates = c("2016-06-17", "2016-06-16"))
```

#### GKG Tidying

``` r
load_needed_packages(c('magrittr'))

gkg_test <- 
  get_data_gkg_days_detailed(only_most_recent = T, table_name = 'gkg')

gkg_sample_df <- 
  gkg_test %>% 
  sample_n(1000)

xml_extra_df <- 
  gkg_sample_df %>% 
  parse_gkg_xml_extras(filter_na = T, return_wide = F)

article_tone <- 
  gkg_sample_df %>% 
  parse_gkg_mentioned_article_tone(filter_na = T, return_wide = T)

gkg_dates <- 
  gkg_sample_df %>% 
  parse_gkg_mentioned_dates(filter_na = T, return_wide = T)

gkg_gcams <- 
  gkg_sample_df %>% 
  parse_gkg_mentioned_gcams(filter_na = T, return_wide = T)

gkg_event_counts <- 
  gkg_sample_df %>% 
  parse_gkg_mentioned_event_counts(filter_na = T, return_wide = T)

gkg_locations <- 
  gkg_sample_df %>% 
  parse_gkg_mentioned_locations(filter_na = T, return_wide = T)

gkg_names <- 
  gkg_sample_df %>% 
  parse_gkg_mentioned_names(filter_na = T, return_wide = T)

gkg_themes <- 
  gkg_sample_df %>% 
  parse_gkg_mentioned_themes(theme_column = 'charLoc',
                                      filter_na = T, return_wide = T)

gkg_numerics <- 
  gkg_sample_df %>% 
  parse_gkg_mentioned_numerics(filter_na = T, return_wide = T)

gkg_orgs <-
  gkg_sample_df %>% 
  parse_gkg_mentioned_organizations(organization_column = 'charLoc', 
                                             filter_na = T, return_wide = T)

gkg_quotes <-
  gkg_sample_df %>% 
  parse_gkg_mentioned_quotes(filter_na = T, return_wide = T)

gkg_people <- 
  gkg_sample_df %>% 
  parse_gkg_mentioned_people(people_column = 'charLoc', filter_na = T, return_wide = T)
```

#### VGKG Tidying

``` r
vgkg_test <- 
  get_data_vgkg_dates(only_most_recent = T)

vgkg_sample <- 
  vgkg_test %>% 
  sample_n(1000)

vgkg_labels <- 
  vgkg_sample %>% 
  parse_vgkg_labels(return_wide = T)

faces_test <- 
  vgkg_sample %>% 
  parse_vgkg_faces(return_wide = T)

landmarks_test <- 
  vgkg_sample %>% 
  parse_vgkg_landmarks(return_wide = F)

logos_test <- 
  vgkg_sample %>% 
  parse_vgkg_logos(return_wide = T)

ocr_test <- 
  vgkg_sample %>% 
  parse_vgkg_ocr(return_wide = F)

search_test <- 
  vgkg_sample %>% 
  parse_vgkg_safe_search(return_wide = F)
```

#### Sentiment API

``` r
location_codes <-
  get_codes_stability_locations()
location_test <-
  get_data_locations_instability_api(
    location_ids = c("US", "IS", "CA", "TU", "CH", "UK", "IR"),
    use_multi_locations = c(T, F),
    variable_names = c('instability', 'tone', 'protest', 'conflict'),
    time_periods = c('daily'),
    nest_data = F,
    days_moving_average = NA,
    return_wide = T,
    return_message = T
  )

location_test %>%
  dplyr::filter(codeLocation %>% is.na()) %>%
  group_by(nameLocation) %>%
  summarise_at(.vars = c('instability', 'tone', 'protest', 'conflict'),
               funs(mean)) %>%
  arrange(desc(instability))
```
