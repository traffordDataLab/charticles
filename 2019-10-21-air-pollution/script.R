# Air pollution #

# Source: Trafford Council / Ricardo EE
# URL: https://www.airqualityengland.co.uk/site/latest?site_id=TRF2
# Licence: Open Government Licence 3.0

library(tidyverse) ; library(scales) ; library(lubridate) ; library(rvest)
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data ---------------------------
site_id <- "TRF2"
start_date <- as.Date(Sys.time()) %m-% months(12)
end_date <- Sys.Date()

url <- paste0("http://www.airqualityengland.co.uk/local-authority/data.php?site_id=", site_id, "&parameter_id%5B%5D=GE10&f_query_id=920788&data=%3C%3Fphp+print+htmlentities%28%24data%29%3B+%3F%3E&f_date_started=", start_date, "&f_date_ended=", end_date, "&la_id=368&action=download&submit=Download+Data")
df <- read_html(url) %>% 
  html_node("a.b_xls.valignt") %>% 
  html_attr('href') %>% 
  read_csv(skip = 5) %>% 
  mutate(period = as.Date(`End Date`, format = "%d/%m/%Y"),
         hour = hour(strptime(`End Time`, format = "%H:%M:%S")),
         value = as.double(PM10)) %>% 
  select(period:value) %>% 
  group_by(period) %>% 
  summarise(mean = mean(value, na.rm = TRUE)) %>% 
  arrange(period) %>% 
  mutate(site = "Trafford A56",
         indicator = "PM10",
         measure = "24-hour mean",
         unit = "µg/m3") %>% 
  select(site, indicator, period, measure, unit, everything())

# plot data ---------------------------
ggplot(df, aes(x = period, y = mean)) +
  geom_line(colour = "#b2e303", size = 1) +
  geom_hline(aes(yintercept = 50), color = "#000000", size = 1) +
  annotate(geom = "text", x = Sys.Date()-50, y = 50, 
           label = "Legal limit (35 exceedances allowed)", fontface = "bold", vjust = 2) +
  scale_x_date(breaks = df$period, date_labels = "%b", date_breaks = "1 month") +
  scale_y_continuous(limits = c(0, max(readings$mean))) +
  labs(title = expression(bold(paste("24-hour mean ", PM[10], " concentrations"))),
       subtitle = paste0(sum(df$mean >= 50, na.rm = TRUE), " exceedances at Trafford A56 over last 12 months"),
       caption = "Source: Trafford Council / Ricardo EE",
       x = "",
       y = "µg/m³") +
  theme_lab()

# write data ---------------------------
write_csv(df, "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300, scale = 1)
