## Old-age dependency ratio ##

# load libraries ---------------------------
library(tidyverse) ; library(readxl) ; library(ggplot2)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data ---------------------------
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/wardlevelmidyearpopulationestimatesexperimental/mid2017sape20dt8/sape20dt8mid2017ward2017syoaestimatesunformatted1.zip"
download.file(url, dest = "sape20dt8mid2017ward2017syoaestimatesunformatted1.zip")
unzip("sape20dt8mid2017ward2017syoaestimatesunformatted1.zip", exdir = ".")
file.remove("sape20dt8mid2017ward2017syoaestimatesunformatted1.zip")

raw <- read_excel("SAPE20DT8-mid-2017-ward-2017-syoa-estimates-unformatted.xls", sheet = 4, skip = 3) %>% 
  filter(`Local Authority` == 'Trafford') %>% 
  select(-c(`Local Authority`, `All Ages`)) %>% 
  rename(area_code = `Ward Code 1`, area_name = `Ward Name 1`, `90` = `90+`) %>% 
  select(area_code, area_name, everything()) %>%
  gather(age, n, -c(area_name, area_code)) %>% 
  mutate(age = as.integer(age))

# tidy data ---------------------------
df <- raw %>%
  mutate(ageband = cut(age,
                       breaks = c(16,65,120),
                       labels = c("working_age","state_pension_age"),
                       right = FALSE)) %>% 
  filter(!is.na(ageband)) %>%
  group_by(area_code, area_name, ageband) %>% 
  summarise(n = sum(n)) %>% 
  group_by(area_code, area_name) %>% 
  select(area_code, area_name, ageband, n) %>%
  spread(ageband, n) %>%
  mutate(ratio = round((state_pension_age/working_age)*100, 0)) %>% 
  arrange(desc(ratio)) %>%
  ungroup() %>%
  mutate(area_name = factor(area_name, levels = area_name))

# plot data ---------------------------
ggplot(df, aes(x = ratio, y = area_name, ratio)) +
  geom_segment(aes(x = 0, y = area_name, xend = ratio, yend = area_name), color = "#f0f0f0") +
  geom_point(colour = "#fc6721", size = 7) +
  geom_text(aes(label = ratio, fontface = "bold"), color = "white", size = 4) +
  labs(x = NULL, y = NULL,
       title = "Old-age dependency ratios in Trafford's wards, 2017",
       subtitle = "Number of people aged 65 and over as % of working age population",
       caption = "Source: Office for National Statistics  |  @traffordDataLab") +
  theme_lab() +
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(hjust = 0.12),
        plot.subtitle = element_text(hjust = 0.10),
        axis.text.y = element_text(family = "Open Sans", face = "plain", size = 12, hjust = 0))

# write data ---------------------------
write_csv(df, "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300, scale = 1)
