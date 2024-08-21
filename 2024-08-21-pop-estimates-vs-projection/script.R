# Title # Population estimates vs projections

# Source: ONS
# URL: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/estimatesofthepopulationforenglandandwales
# URL: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/datasets/localauthoritiesinenglandtable2
# Licence: Open Government Licence v3.0

# load libraries ---------------------------
library(tidyverse) ; library(ggpol)
# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data ---------------------------
midYear <- read_csv("https://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=1778385132&date=latest&gender=0...2&c_age=1,3...18,210&measures=20100") %>%
  select(date = DATE_NAME, age = C_AGE_NAME, sex = GENDER_NAME, value = OBS_VALUE) %>%
  mutate(indicator = "Mid-year population estimate")

projections <- read_csv("https://www.nomisweb.co.uk/api/v01/dataset/NM_2006_1.data.csv?geography=1820327969&projected_year=2023&gender=0...2&c_age=1,3...18,210&measures=20100") %>%
  select(date = PROJECTED_YEAR_CODE, age = C_AGE_NAME, sex = GENDER_NAME, value = OBS_VALUE) %>%
  mutate(indicator = "2018 based population projection")

# tidy data ---------------------------
df <- midYear %>%
  bind_rows(projections) %>%
  mutate(age = str_replace(age,"Aged |Age ", ""), age = str_replace(age," - ", "-")) %>% 
  mutate(area_code = "E08000009", area_name = "Trafford", 
  measure = "Count", period = "2023-06-01") %>%
  select(area_code, area_name, period, indicator,age, sex, measure, value)

temp <- df %>%
  filter(sex != "Total") %>%
  mutate(value = case_when(sex == "Female" ~ value * -1, TRUE ~ value)) %>%
  mutate(sex = factor(sex, levels = c("Female", "Male"))) %>%
  bind_rows(data.frame(area_name="None", age = "85+", value = -10000, sex = "Female")) %>%
  bind_rows(data.frame(area_name="None", age = "85+", value = 10000, sex = "Male")) %>%
  mutate(age = factor(age, levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49",  "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+"))) 

# plot data ---------------------------
ggplot() +
  geom_col(data = filter(temp, indicator == "Mid-year population estimate"), 
           aes(x = age, y = value, fill = sex), alpha = 0.6, width = 0.9)+ 
  geom_step(data = filter(temp, indicator == "2018 based population projection"), 
            aes(x = age, y = value, group = sex, colour = sex), stat = "identity", size = 1, direction = "mid", alpha = 0.8) +
  geom_point(data = temp %>% filter(area_name == "None"), aes(x = age, y = value), fill = "transparent", color="transparent") +
  scale_fill_manual(values = c("#762a83", "#088F8F")) +
  scale_colour_manual(values = c("#762a83", "#088F8F")) +
  facet_share(~sex, dir = "h", scales = "free",reverse_num = TRUE) +
  coord_flip() +
  labs(x = NULL, y = NULL, 
       title = "Population pyramid compared to projected population",
       subtitle = "Trafford, 2023",
       caption = "Projected population based on 2018 (line), Mid-year 2023 (bar)\nSource: ONS | @traffordDataLab") +
  theme(
    plot.title = element_text(size=14, face = "bold", hjust = 0, colour = "#757575"),
    plot.subtitle = element_text(size=12, hjust = , colour = "#757575"),
    plot.caption = element_text(size = 8, hjust = 1, margin = margin(t = 15)),
    plot.title.position = "plot",
    axis.text.y = element_text(hjust = 1, size=8),
    axis.text.x = element_text(size=8),
    axis.line.y = element_line(colour = "#000000"),
    panel.background = element_blank(),
    plot.background = element_rect(fill = 'white'),
    panel.border = element_blank(),
    panel.grid.major.x = element_line(size = 0.3, color = "#cbcbcb"),
    legend.position = "none",
    axis.ticks.x=element_line(size=0.1, color = "transparent"),
    axis.ticks.y=element_line(size=0.1, color = "transparent"),
    axis.line = element_blank(),
    strip.background = element_rect(fill = "transparent", colour = NA),
    strip.text = element_text(size = 9, face = "bold", hjust = .5))

# write data ---------------------------
write_csv(df, "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300, scale = 1)

