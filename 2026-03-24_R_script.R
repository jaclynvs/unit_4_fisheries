# 2026-03-24
# JVS

library(tidyverse)
load("data/RAMLDB v4.66/R Data/DBdata[asmt][v4.66].RData")

head(tsmetrics)
head(timeseries)

timeseries_tsmetrics = left_join(
    timeseries,
    tsmetrics,
    by = c("tsid" = "tsunique")
)

dim(timeseries)
dim(timeseries_tsmetrics)
head(timeseries_tsmetrics)

######## END OF 2026-03-18 ##########

head(timeseries_values_views)
head(taxonomy)
glimpse(stock)

fish = timeseries_values_views %>%
  left_join(stock, by = c("stockid", "stocklong")) %>%
  left_join(taxonomy, by = c("tsn", "scientificname")) %>%
  select(stockid, stocklong, year, TCbest, tsn, scientificname, commonname, region, FisheryType, taxGroup)

dim(fish)
dim(timeseries_values_views)
glimpse(fish)

fish %>% arrange(desc(TCbest))

ggplot() +
  geom_line(data = fish, aes(x = year, y = TCbest, color =stockid)) +
  theme(legend.position = "none")

ggplot() +
  geom_line(data = fish %>% filter(TCbest > 3e6),
     aes(x = year, y = TCbest, color = stocklong))

fish %>%
  filter(scientificname == "Gadus morhua") %>%
  distinct(region)

cod_can = fish %>%
  filter(scientificname == "Gadus morhua",
        region == "Canada East Coast",
        !is.na(TCbest))
head(cod_can)

ggplot(data = cod_can) +
  geom_line(aes(x= year, y = TCbest, color = stocklong)) +
  theme_bw() +
  ylab("Total catch in MT")

# add all cod stocks in E Canada together

cod_can_total = cod_can %>%
    group_by(year) %>%
    summarize(total_catch = sum(TCbest))
head(cod_can_total)

ggplot(data = cod_can_total) +
  geom_line(aes(x = year, y = total_catch))

#### play with the cummulative functions in dplyr

dat = c(1, 3, 6, 2, 3, 9, -1)
dat_max = cummax(dat)
dat_sum = cumsum(dat)
test_cum = data.frame(dat, dat_max, dat_sum)
test_cum

# using Boris Worm (2006) stock collapse definition: has cod collapses?

head(cod_can_total)
cod_collapse = cod_can_total %>%
  mutate(historical_max_catch = cummax(total_catch),
        collapse = total_catch <= 0.1 * historical_max_catch)
head(cod_collapse)
tail(cod_collapse)

cod_collapse_year = cod_collapse %>%
  filter(collapse == T) %>%
  summarize(year = min(year)) %>%
  pull()     # pull vector out of the data frame/ tibble
class(cod_collapse_year)

ggplot() +
  geom_line(data = cod_collapse, aes(x = year, y = total_catch, color = collapse)) +
  geom_vline(xintercept = cod_collapse_year)

# apply collapse to full data set

collapse = fish %>%
  filter(!is.na(TCbest)) %>%
  group_by(stockid) %>%
  mutate(historical_max_catch = cummax(TCbest),
        current_collapse = TCbest <= 0.1 * historical_max_catch,
        collapsed_yet = cumsum(current_collapse) > 0) %>%
  ungroup()

glimpse(collapse)

collapse_year = collapse %>%
  group_by(stockid, stocklong, region) %>%
  filter(collapsed_yet == T) %>%
  summarize(first_collapse_year = min(year)) %>%
  ungroup()

head(collapse_year)

# time series

n_stocks = length(unique(collapse$stockid))
n_stocks

collapse_ts = collapse_year %>%
  group_by(first_collapse_year) %>%
  summarize(n = n()) %>%
  mutate(cum_first_collapse_year = cumsum(n),
        ratio_collapsed_yet = cum_first_collapse_year / n_stocks)

head(collapse_ts)

ggplot(data = collapse_ts) +
  geom_line(aes(x = first_collapse_year, y = ratio_collapsed_yet))
