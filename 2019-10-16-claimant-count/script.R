## Claimant count ##

# Source: Department for Work and Pensions
# URL: https://www.nomisweb.co.uk/datasets/ucjsa
# Licence: Open Government Licence 3.0

library(tidyverse) ; library(scales) ; library(zoo) ; library(ggsci)
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data ---------------------------
df <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_162_1.data.csv?geography=989862149...989862169&date=latest&gender=0&age=10...20&measure=1&measures=20100&select=date_name,geography_name,geography_code,gender_name,age_name,measure_name,measures_name,obs_value,obs_status_name") %>%
  select(period = DATE_NAME, area_code = GEOGRAPHY_CODE, area_name = GEOGRAPHY_NAME, ageband = AGE_NAME, n = OBS_VALUE) %>%
  mutate(period = as.Date(paste("01", period), format = "%d %b %Y"),
         ageband = case_when(
           ageband %in% c("Aged 16-17", "Aged 18-24") ~ "16-24",
           ageband %in% c("Aged 25-29","Aged 30-34") ~ "25-34",
           ageband %in% c("Aged 35-39", "Aged 40-44") ~ "35-44",
           ageband %in% c("Aged 45-49", "Aged 50-54") ~ "45-54",
           ageband %in% c("Aged 55-59", "Aged 60-64") ~ "55-64",
           TRUE ~ "65+"
         )) %>% 
  group_by(period, ageband, area_code, area_name) %>% 
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  group_by(period, area_code, area_name) %>%
  mutate(ageband = fct_rev(ageband),
         total = sum(n), percent = n/total,
         indicator = "Claimant count by ward",
         measure = "Count",
         unit = "Persons") %>% 
  select(area_code, area_name, indicator, period, measure, unit, everything())


# plot data ---------------------------
ggplot(df, aes(x = area_name, y = percent, width = total, fill = ageband)) +
  geom_col(position = "stack", colour = NA) +
  facet_grid(~fct_reorder(area_name,total, .desc = T), scales = "free_x", space = "free_x") +
  scale_y_continuous(expand = c(0.005, 0.005), breaks = c(0, 0.5, 1), labels = percent) +
  scale_fill_startrek(alpha = 0.6) +
  labs(x = NULL, y = NULL, 
       title = "Age profile of Trafford's Universal Credit and JSA claimants",
       subtitle = as.yearmon(unique(df$period)),
       caption = "Source: Department for Work and Pensions | @traffordDataLab",
       fill = NULL) +
  theme_lab() +
  theme(
    panel.spacing.x = unit(0.01, "npc"),
    panel.grid.major = element_blank(),
    strip.text.x = element_blank(),
    legend.position = "right",
    axis.text.x = element_text(angle = 90, hjust = 1))

# write data ---------------------------
write_csv(df, "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300, scale = 1)
