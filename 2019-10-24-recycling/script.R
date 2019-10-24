# Recycling #

# Source: Department for Environment, Food & Rural Affairs
# URL: https://www.gov.uk/government/statistical-data-sets/env18-local-authority-collected-waste-annual-results-tables
# Licence: Open Government Licence

library(tidyverse) ; library(httr) ; library(readxl) ; library(scales)
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

tmp <- tempfile(fileext = ".xls")
GET(url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/766014/LA_and_Regional_Spreadsheet_201718_rev2.xlsx",
    write_disk(tmp))

england <- read_xlsx(tmp, sheet = 8, skip = 3) %>% 
  filter(Region == "Total England") %>% 
  mutate(area_code = "E92000001", 
         area_name = "England") %>% 
  select(area_code, area_name,
         period = Year, 
         value = `Percentage of household waste sent for reuse, recycling or composting (Ex NI192)`)

df <- read_xlsx(tmp, sheet = 8, skip = 3) %>% 
  filter(`ONS code` %in% c("E50000005", "E08000009")) %>% 
  mutate(area_code = case_when(`ONS code`== "E50000005" ~ "E47000001", TRUE ~ `ONS code`),
         area_name = case_when(area_code == "E47000001" ~ "Greater Manchester", 
                               area_code == "E08000009" ~ "Trafford")) %>% 
  select(area_code, 
         area_name,
         period = Year, 
         value = `Percentage of household waste sent for reuse, recycling or composting (Ex NI192)`) %>% 
  bind_rows(england) %>% 
  mutate(indicator = "Reuse, recycling or composting of household waste",
         measure = "Percentage",
         unit = "Waste") %>% 
  select(area_code, area_name, period, indicator, measure, unit, value) %>% 
  arrange(period)

rm(england)

# plot data ---------------------------
ggplot(filter(df, period %in% c("2010-11", "2017-18"))) + 
  geom_line(aes(x = as.factor(period), y = value, group = area_name, colour = area_name), size = 2,
            show.legend = FALSE) + 
  geom_point(aes(x = as.factor(period), y = value, group = area_name, colour = area_name), size = 5) + 
  geom_text(data = filter(df, period == "2010-11"), aes(period, y = value, label = percent(value, accuracy = 1)), nudge_x = -0.06) +
  geom_text(data = filter(df, period == "2017-18"), aes(period, y = value, label = percent(value, accuracy = 1)), nudge_x = 0.06) +
  scale_colour_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575"),
                      guide = guide_legend(reverse = TRUE)) +
  labs(x = NULL, y = NULL,
       title = "Household waste sent for recycling",
       subtitle = "", caption = "Source: DEFRA | @traffordDataLab",
       colour = NULL) +
  theme_lab() +
  theme(panel.grid.major.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.76),
        axis.text.x = element_text(face = "bold"),
        axis.text.y = element_blank())

# write data ---------------------------
write_csv(df, "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300, scale = 1)
