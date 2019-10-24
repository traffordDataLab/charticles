# Travel to work #

# Source: QS701EW, 2011 Census
# URL: https://www.nomisweb.co.uk/census/2011/qs701ew
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(treemapify) ; library(ggsci) ; library(scales)
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data ---------------------------
df <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_568_1.data.csv?date=latest&geography=1946157089&rural_urban=0&cell=1...11&measures=20100&select=date_name,geography_name,geography_code,rural_urban_name,cell_name,measures_name,obs_value,obs_status_name") %>% 
  select(area_code = GEOGRAPHY_CODE, area_name = GEOGRAPHY_NAME, OBS_VALUE, CELL_NAME) %>% 
  mutate(group = case_when(
    CELL_NAME %in% c("Bus, minibus or coach", "Train", "Underground, metro, light rail, tram")  ~ "Public transport",
    CELL_NAME == "Bicycle" ~ "Bike",
    CELL_NAME == "On foot" ~ "Walking",
    CELL_NAME %in% c("Driving a car or van", "Passenger in a car or van") ~ "Car or van",
    TRUE ~ CELL_NAME)
    ) %>% 
  group_by(area_code, area_name, group) %>% 
  summarise(n = sum(OBS_VALUE)) %>% 
  mutate(value = n/sum(n),
         indicator = "Commuting journeys by mode of transport",
         period = "2011-03-27",
         measure = "Proportion",
         unit = "Persons") %>% 
  filter(group != "Other") %>% 
  select(area_code, area_name, indicator, period, measure, unit, value, group)

# plot data ---------------------------
temp <- mutate(df, 
               info = str_c(group, ": ",  percent(value)))

ggplot(temp,
       aes(area = value, fill = fct_reorder(group, value, .desc = TRUE), subgroup = group, label = info)) +
  geom_treemap(colour = "#212121") +
  geom_treemap_text(colour = "#FFFFFF", place = "bottomleft", reflow = TRUE, 
                    padding.x = grid::unit(1.5, "mm"),
                    padding.y = grid::unit(2, "mm"),
                    size = 14) +
  scale_fill_d3() +
  labs(x = NULL, y = NULL, 
       title = "Commuting journeys by mode of transport",
       subtitle = "Trafford, 2011",
       caption = "Source: 2011 Census | @traffordDataLab") +
  theme_lab() +
  theme(legend.position = "none") 

# write data ---------------------------
write_csv(df, "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300, scale = 1)

