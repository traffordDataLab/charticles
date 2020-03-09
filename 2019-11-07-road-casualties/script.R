## Road casualties 2005-2018 ##

# Source: Transport for Greater Manchester (TfGM)
# Publisher URL: https://data.gov.uk/dataset/25170a92-0736-4090-baea-bf6add82d118/gm-road-casualty-accidents-full-stats19-data
# Licence: OS "Free to Use Data" Licence

# 2020-03-09 Relating to GitHub issue #1:
#   Function geom_waffle() is available in v1.0.1 of the waffle package, current version on CRAN is 0.7.0
#   To install the latest version use: install.packages("waffle", repos = "https://cinc.rud.is")
#   See: https://git.sr.ht/~hrbrmstr/waffle for more information.
#   URLs for the data will need updating each year as TfGM don't seem to keep the previous versions.

library(tidyverse) ; library(lubridate) ; library(waffle) ; library(ggthemes)
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data ---------------------------
accident <- read_csv("http://odata.tfgm.com/opendata/downloads/STATS19AccData20052019.csv") 
casualty <- read_csv("http://odata.tfgm.com/opendata/downloads/STATS19CasData20052019.csv")

# tidy data  ---------------------------
df <- left_join(casualty, accident, by = "Accident Index") %>% 
  filter(LocalAuthority == 112) %>% 
  mutate(date = dmy(OutputDate),
         year = year(date),
         group = case_when(
           CasTypeCode == 0 ~ "Pedestrian",
           CasTypeCode == 1 ~ "Pedal Cycle",
           CasTypeCode %in% c(2, 3, 4, 5, 23, 96, 97) ~ "Powered 2 Wheeler",
           CasTypeCode == 9 ~ "Car",
           CasTypeCode == 11 ~ "Bus or Coach",
           CasTypeCode %in% c(19, 20, 21, 98) ~ "Goods Vehicle",
           CasTypeCode %in% c(8, 10, 14, 15, 16, 17, 18) ~ "Other",
           TRUE ~ "NA"),
         group = factor(group),
         casualty_severity = fct_recode(as.factor(CasualtySeverity), 
                                        "Fatal" = "1", "Serious" = "2", "Slight" = "3")) %>% 
  select(AREFNO = `Accident Index`, year, group, casualty_severity) %>% 
  filter(year == 2018, casualty_severity != "Slight") %>% 
  count(year, group) %>% 
  mutate(area_code = "E08000009",
         area_name = "Trafford",
         indicator = "KSIs by road user type",
         measure = "Count",
         unit = "Casualties") %>% 
  select(area_code, area_name, indicator, period = year, measure, unit, value = n, group)

# plot data ---------------------------
ggplot(df, aes(fill = group, values = value)) +
  geom_waffle(color = "#FFFFFF", size = 0.25, n_rows = 5, flip = TRUE) +
  facet_wrap(~group, nrow = 1, strip.position = "bottom") +
  scale_x_discrete() + 
  scale_fill_tableau(name = NULL) +
  coord_equal() +
  labs(x = NULL, y = NULL,
       title = "KSIs by road user type",
       subtitle = "Trafford, 2018",
       caption = "Source: TfGM | @traffordDataLab") +
  theme_lab() +
  theme(panel.grid = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(margin = margin(t = 30)),
        strip.text = element_text(size = 10, hjust = 0.5),
        legend.position = "none")

# write data ---------------------------
write_csv(df, "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300, scale = 1)
