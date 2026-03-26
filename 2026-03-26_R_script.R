# 2026-03-26
# JVS

library(tidyverse)
load("data/RAMLDB v4.66/R Data/DBdata[asmt][v4.66].RData")

source("build_collapse_table.R")

glimpse(collapse)

# has a given stock ever collapsed

model_data = collapse %>%
  group_by(stockid, stocklong) %>%
  summarize(ever_collapsed = any(current_collapse)) %>%
  ungroup() %>%
  left_join(metadata, by = c("stockid", "stocklong")) %>%
  mutate(FisheryType = as.factor(FisheryType))

glimpse(model_data)

model_l = glm(data = model_data, ever_collapsed ~ FisheryType, family = "binomial")
summary(model_l)

newdata = model_data %>% distinct(FisheryType)
newdata

model_l_predict = predict(model_l, newdata = newdata, type = "response", se.fit = T)
collapse_fishery_type_predictions = cbind(newdata, model_l_predict)
collapse_fishery_type_predictions

ggplot(data = collapse_fishery_type_predictions) +
  geom_bar(aes(x = FisheryType, y = fit, fill = FisheryType), stat = "identity", show.legend = F) +
  geom_errorbar(aes(x = FisheryType, ymin = fit - se.fit, ymax = fit + se.fit), width = 0.2) +
  coord_flip()

# poisson model
glimpse(tsmetrics)
tsmetrics %>% filter(tsshort == "BdivBmgtpref")
tsmetrics %>% filter(tsshort == "UdivUmsypref")

u_summary = timeseries_values_views %>%
  filter(!is.na(BdivBmgtpref),
        !is.na(UdivUmsypref)) %>%
  group_by(stockid, stocklong) %>%
  summarize(yrs_data = n(),
            ratio_yrs_overfished = sum((UdivUmsypref > 1)/yrs_data),
            ratio_yrs_low_stock = sum((BdivBmgtpref < 1)/yrs_data)
            ) %>%
  select(-yrs_data) %>%
  ungroup() %>%
  left_join(metadata %>% select(stockid, FisheryType))

glimpse(u_summary)

# join it with the collapse table

collapse_summary = collapse %>%
  group_by(stockid, stocklong) %>%
  summarize(yrs_data = n(),
            yrs_collapsed = sum(current_collapse)) %>%
  ungroup() %>%
  inner_join(u_summary, by = c("stockid", "stocklong"))

glimpse(collapse_summary)

hist(collapse_summary$yrs_collapsed)
table(collapse_summary$yrs_collapsed)

collapse_summary_zero_trunc = collapse_summary %>% filter(yrs_collapsed > 0)
table(collapse_summary_zero_trunc$yrs_collapsed)

model_p = glm(yrs_collapsed ~ FisheryType, 
              offset(log(yrs_data)),
              data = collapse_summary_zero_trunc,
              family = "poisson")
summary(model_p)

install.packages("AER")
AER::dispersiontest(model_p)

model_qp = glm(yrs_collapsed ~ FisheryType, 
              offset(log(yrs_data)),
              data = collapse_summary_zero_trunc,
              family = "quasipoisson")
summary(model_qp)

median_ratio_yrs_low_stock = median(collapse_summary_zero_trunc$ratio_yrs_low_stock)
FisheryType = collapse_summary_zero_trunc %>% distinct(FisheryType) %>% pull

newdata = expand.grid(FisheryType = FisheryType,
                      ratio_yrs_low_stock = median_ratio_yrs_low_stock,
                      ratio_yrs_overfished = seq(0, 1, by = 0.1))
newdata