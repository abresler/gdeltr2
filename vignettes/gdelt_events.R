library(gdeltr2)


# read the data -----------------------------------------------------------


setwd("~")
gdeltr2::load_needed_packages(c('magrittr', 'dplyr', 'threejs', 'ggplot2'))
events1983 <-
  get_data_gdelt_periods_event(periods = 1983)

cameo_events <-
  get_codes_cameo_events()

events1983 <-
  events1983 %>%
  left_join(cameo_events %>% dplyr::select(idCAMEOEvent, descriptionCAMEOEvent), by = c("codeEvent" = "idCAMEOEvent"))

top_tone_codes <-
  events1983 %>%
  group_by(codeEvent) %>%
  summarise(avgTone = mean(avgTone, na.rm = T) ) %>%
  arrange(desc(avgTone)) %>%
  slice(1:25) %>%
  .$codeEvent



# eda ---------------------------------------------------------------------
options(scipen = 99999)
events1983 %>%
  glimpse()

events1983 %>%
  dplyr::select(nameQuad) %>%
  ggplot() +
  geom_bar(aes(nameQuad))

c(2,1 ,.5, .01) %>%
  future_map(function (x){
  events1983 %>%
  dplyr::filter(!avgTone %>% is.na()) %>%
  ggplot() +
  geom_histogram(aes(x = avgTone), binwidth = x)
})

events1983 %>%
  dplyr::filter(!avgTone %>% is.na()) %>%
  ggplot() +
  geom_density2d(aes(x = avgTone, y = scoreGoldstein))

events1983 %>%
  dplyr::filter(!avgTone %>% is.na()) %>%
  ggplot() +
  geom_freqpoly(aes(x = avgTone, color = nameQuad), binwidth = 0.2) +
  coord_cartesian(xlim = c(5, 10))

events1983 %>%
  dplyr::filter(codeEvent %in% top_tone_codes) %>%
  dplyr::filter(!descriptionCAMEOEvent %>% is.na) %>%
  ggplot() +
  geom_boxplot(aes(
    x = reorder(descriptionCAMEOEvent, avgTone, FUN = mean),
    y = avgTone
  ), na.rm = T) +
  coord_flip()

ggplot(data = events1983) +
  geom_violin(aes(x = reorder(codeEventBase, avgTone, FUN = mean), y = avgTone), na.rm = T) +
  coord_flip()


events1983 %>%
  ggplot() +
  geom_point(aes(x = avgTone, y = scoreGoldstein))

events1983 %>%
  ggplot() +
  geom_bin2d(aes(x = scoreGoldstein, y = avgTone))

ggplot(data = events1983, aes(y = scoreGoldstein, x = avgTone)) +
  geom_jitter() +
  geom_density2d(h = c(1, 1)) +
  geom_rug(position = "jitter")

# threeJS -----------------------------------------------------------------


count1983 <-
  events1983 %>%
  dplyr::select(latitudeAction, longitudeAction) %>%
  group_by(latitudeAction, longitudeAction) %>%
  count(sort = T) %>%
  ungroup %>%
  mutate(q = n %>%
           cut(breaks = quantile(n, probs = c(0, 0.90, 0.95, 0.99, 1)),
               include.lowest = TRUE) %>%
           as.numeric())

col <-
  c("#0055ff", "#00aaff", "#00ffaa", "#aaff00")[count1983$q]

# bling out the data
globejs(
  img = "http://goo.gl/GVjxJ",
  lat = count1983$latitudeAction,
  long = count1983$longitudeAction,
  val = count1983$q ^ 3,
  # Bar height
  color = col,
  pointsize = 0.5,
  emmisive = "#000000",
  bodycolor = "#000000",
  lightcolor = "#aaaa44"
)
