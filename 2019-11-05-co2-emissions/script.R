# CO₂ emissions #

# Source: Department for Business, Energy & Industrial Strategy
# URL: https://www.gov.uk/government/statistics/uk-local-authority-and-regional-carbon-dioxide-emissions-national-statistics-2005-to-2017
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(httr) ; library(readxl) ; library(lubridate) ; library(ggrepel)
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data ---------------------------
url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/812142/2005-17_UK_local_and_regional_CO2_emissions_tables.xlsx"
GET(url, write_disk(tmp <- tempfile(fileext = ".xlsx")))

gm <- read_excel(tmp, sheet = 2, skip = 1) %>% 
  filter(LAD14NM %in% c("Bolton","Bury","Manchester","Oldham","Rochdale","Salford","Stockport","Tameside","Trafford","Wigan")) %>% 
  select(area_code = LAD14CD,
         area_name = LAD14NM,
         period = Year, 
         `Domestic` = `Domestic Total`,
         `Industry and commercial` = `Industry and Commercial Total`,
         `Transport` = `Transport Total`,
         `All sectors` = `Grand Total`) %>% 
  gather(group, value, -area_code, -area_name, -period) %>%
  group_by(period, group) %>% 
  summarise(value = sum(value)) %>% 
  mutate(area_code = "E47000001",
         area_name = "Greater Manchester") %>% 
  select(area_code, area_name, period, group, value)

df <- read_excel(tmp, sheet = 2, skip = 1) %>% 
  filter(LAD14NM %in% c("Trafford", "England Total")) %>% 
  select(area_code = LAD14CD,
         area_name = LAD14NM,
         period = Year, 
         `Domestic` = `Domestic Total`,
         `Industry and commercial` = `Industry and Commercial Total`,
         `Transport` = `Transport Total`,
         `All sectors` = `Grand Total`) %>% 
  mutate(area_name = case_when(area_name == "England Total" ~ "England", TRUE ~ area_name)) %>% 
  gather(group, value, -area_code, -area_name, -period) %>% 
  bind_rows(gm) %>% 
  ungroup() %>% 
  mutate(indicator = "CO₂ emissions",
         period = ymd(str_c(period, "01-01", sep = "-")),
         measure = "CO₂",
         unit = "CO₂ (kt)") %>% 
  select(area_code, area_name, 
         indicator, period, measure, unit, value, group) %>% 
  arrange(period, area_name)

# plot data ---------------------------
temp <- df %>% 
  filter(group == "Industry and commercial") %>% 
  group_by(area_name) %>%
  mutate(index = round(100 * value / value[1], 0),
         change = index-100)

ggplot(temp, aes(period, index, colour = area_name, fill = area_name, group = area_name)) +
  geom_hline(yintercept = 0, size = 1, colour = "#212121", linetype = "solid") +
  geom_hline(aes(yintercept = 100), colour = "#212121") +
  geom_line(size = 1.5) +
  geom_point(data = filter(temp, period == "2017-01-01"), size = 4) +
  scale_x_date(expand = c(0.005, 20),
               date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(limits = c(0, 130), expand = c(0.005, 0.005),
                     breaks = c(0, 25, 50, 75, 100, 125),
                     labels = c("-100%", "-75%", "-50%", "-25%", "0%", "+25%"),
                     sec.axis = sec_axis(~ ., breaks = pull(filter(temp, period == "2017-01-01"), index),
                                         labels = paste0(pull(filter(temp, period == "2017-01-01"), change), "%"))) +
  scale_colour_manual(values = c("Trafford" = "#E58606", "Greater Manchester" = "#5D69B1", "England" = "#52BCA3")) +
  scale_fill_manual(values = c("Trafford" = "#E58606", "Greater Manchester" = "#5D69B1", "England" = "#52BCA3")) +
  labs(x = NULL, y = NULL,
       title = "Change in carbon dioxide emissions since 2005",
       subtitle = "Industry and commercial sector, 2005-2017",
       caption = "Source: BEIS",
       colour = NULL
  ) +
  theme_lab() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_text(hjust = 0),
        legend.position = "none")

# write data ---------------------------
write_csv(df, "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300, scale = 1)

# analyse data ---------------------------
filter(df, period == "2017-01-01",
       area_name == "England",
       group != "All sectors") %>%
  mutate(percent = round(value/sum(value)*100,1)) %>% View()
