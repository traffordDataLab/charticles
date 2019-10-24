# Employment rate #

# Source: Annual Population Survey, Office for National Statistics
# URL: https://www.nomisweb.co.uk/query/construct/summary.asp?mode=construct&version=0&dataset=17
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(scales)
source("https://trafforddatalab.github.io/assets/theme/ggplot2/theme_lab.R")

# read data ---------------------------
df <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_17_5.data.csv?geography=E92000001,E11000001,E08000009&date=latestMINUS36,latestMINUS32,latestMINUS28,latestMINUS24,latestMINUS20,latestMINUS16,latestMINUS12,latestMINUS8,latestMINUS4,latest&variable=45&measures=20599,21001,21002,21003&select=date_name,geography_name,geography_code,variable_name,measures_name,obs_value") %>% 
  filter(MEASURES_NAME == "Variable") %>%
  mutate(area_name = case_when(GEOGRAPHY_NAME == "Greater Manchester (Met County)" ~ "Greater Manchester", TRUE ~ GEOGRAPHY_NAME),
         period = str_sub(DATE_NAME, start = 10),
         period = as.Date(paste("01", period), format = "%d %b %Y"),
         indicator = "Employment rate - aged 16-64",
         measure = "Percentage",
         unit = "Persons") %>% 
  select(area_code = GEOGRAPHY_CODE,
         area_name,
         indicator, period, measure, unit,
         value = OBS_VALUE)

# plot data ---------------------------
df %>% 
  mutate(area_name = factor(area_name, levels = c("Trafford", "Greater Manchester", "England"), ordered = TRUE)) %>% 
  ggplot(aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
  geom_line(size = 1.1) +
  geom_point(shape = 21, size = 2.5, colour = "white") +
  geom_hline(yintercept = 0, size = 1, colour = "#A6A6A6") +
  scale_x_date(breaks = seq(min(df$period), max(df$period),  by = "1 year"), labels = date_format("%b-%Y")) +
  scale_y_continuous(expand = c(0.005, 0.005), limits = c(0, 100)) +
  scale_colour_manual(values = c("Trafford" = "#00AFBB", 
                                 "Greater Manchester" = "#E7B800", 
                                 "England" = "#757575")) +
  scale_fill_manual(values = c("Trafford" = "#00AFBB", 
                                 "Greater Manchester" = "#E7B800", 
                                 "England" = "#757575")) +
  labs(
    title = "Employment rate",
    subtitle = NULL,
    caption = "Source: Annual Population Survey | @traffordDataLab",
    x = "12 months ending",
    y = "Percentage",
    colour = NULL
  ) +
  theme_lab() +
  theme(axis.text.x = element_text(angle = 90)) +
  guides(fill = FALSE)

# write data ---------------------------
write_csv(df, "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png",  dpi = 300, scale = 1)
