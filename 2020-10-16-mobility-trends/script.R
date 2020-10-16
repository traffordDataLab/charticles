# Mobility trends in Trafford #

# Source: Google COVID-19 Community Mobility Trends
# URL: https://www.google.com/covid19/mobility
# Licence: Google's Terms of Service

library(tidyverse) ; library(scales) ; library(ggtext)

# load data ---------------------------
url <- "https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip"
download.file(url, dest = "Region_Mobility_Report_CSVs.zip")
unzip("Region_Mobility_Report_CSVs.zip")
file.remove("Region_Mobility_Report_CSVs.zip")

df <- read_csv("2020_GB_Region_Mobility_Report.csv") %>% 
  filter(iso_3166_2_code == "GB-TRF") %>% 
  mutate(area_name = "Trafford") %>% 
  select(area_name, date, 
         retail_and_recreation_percent_change_from_baseline, 
         grocery_and_pharmacy_percent_change_from_baseline,
         parks_percent_change_from_baseline) %>% 
  pivot_longer(-c(area_name, date), names_to = "place", values_to = "value") %>% 
  mutate(value = value/100,
         group = case_when(
           place == "retail_and_recreation_percent_change_from_baseline" ~ "Retail & recreation",
           place == "grocery_and_pharmacy_percent_change_from_baseline" ~ "Supermarket & pharmacy",
           place == "parks_percent_change_from_baseline" ~ "Parks*"
         ),
         group = fct_relevel(group, "Retail & recreation", "Supermarket & pharmacy", "Parks*"),
         indicator = "Google community mobility trends",
         measure = "Percentage change from baseline",
         unit = "Visits and length of stay") %>% 
  select(area_name, indicator, date, measure, unit, value, group) %>% 
  # smooth out with 7-day rolling average
  group_by(group) %>% 
  arrange(date) %>% 
  mutate(ma = rollmean(value, 7, align = "right", fill = NA))
  
# plot data ---------------------------
ggplot(df, aes(x = date, y = ma)) +
  geom_hline(yintercept = 0, size = 0.5, colour = "#333333", linetype = "dotted") +
  geom_line(aes(col = group), size = 1, show.legend = FALSE) +
  scale_colour_manual(values = c("Retail & recreation" = "#C1A13E", "Supermarket & pharmacy" = "#5D641E", "Parks*" = "#BAD0C4")) +
  scale_x_date(breaks = c(min(df$date), median(df$date), max(df$date)), date_labels = "%d %b") +
  scale_y_continuous(expand = c(0.005, 0.005), labels = percent) +
  facet_wrap(~group, nrow = 1) +
  labs(x = NULL, y = NULL,
       title = "Mobility trends in Trafford",
       subtitle = paste0("<span style = 'color:#757575;'>Percentage change compared with baseline, 7-day rolling average</span>"),
       caption = "Source: Google COVID-19 Community Mobility Reports | @traffordDataLab",
       tag = "* The data doesn't meet quality and privacy thresholds",
       colour = NULL) +
  theme_minimal() +
  theme(
    plot.margin = unit(c(0,1,0,1), "cm"),
    panel.spacing = unit(2, "lines"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_markdown(margin = margin(b = 20)), 
    plot.caption = element_text(size = 10, color = "grey50", margin = margin(t = 30)),
    strip.text = element_text(size = 12, colour = "#757575", face = "bold", hjust = 0),
    plot.tag.position = c(0.01, 0.06),
    plot.tag = element_text(size = 9, colour = "#757575", hjust = 0),
    axis.ticks.x = element_line(colour = 'black', size = 0.5),
    aspect.ratio = 0.5
  )

# write data ---------------------------
write_csv(select(df, -ma), "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300, scale = 1)

