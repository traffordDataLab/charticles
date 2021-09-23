# NO2 at Trafford A56

# Source: Ricardo EE
# URL: https://www.airqualityengland.co.uk/
# Licence: Open Government Licence v3.0

# load libraries ---------------------------
library(openair) ; library(tidyverse) ; library(lubridate) ; library(rvest)

# load data ---------------------------

# Function to download data
airqualityengland <- function(site_id, start_date, end_date, pollutant) {
  
  if(pollutant=="PM10"){poll="GE10"}else{poll=pollutant}
  
  url <- paste0("https://www.airqualityengland.co.uk/site/data.php?site_id=", site_id, "&parameter_id%5B%5D=" ,poll , "&f_query_id=1818812&data=%3C%3Fphp+print+htmlentities%28%24data%29%3B+%3F%3E&f_date_started=", start_date, "&f_date_ended=", end_date, "&la_id=368&action=download&submit=Download+Data")

  readings <- read_html(url) %>%
    html_node("a.b_xls.valignt") %>%
    html_attr('href') %>%
    read_csv(skip = 5) %>%
    mutate(`End Date` = as.Date(`End Date`, format = "%d/%m/%Y"),
           `End Time` = gsub("NA", "24:00:00", `End Time`),
           date = as.POSIXct(paste(`End Date`, `End Time`), format = "%Y-%m-%d %H:%M:%S"),
           pollutant = pollutant,
           value = as.double(get(pollutant))) %>%
    select(date,pollutant,value) %>% 
    arrange(date) %>%
    mutate(station=site_id) %>%
    filter(!is.na(date))
  
  return(readings)
  
}

#Site Id for Trafford A56
site_id <- "TRF2" 

start_date1 <- "2020-09-01"
end_date1 <-  "2021-08-31"

readings1 <- airqualityengland(site_id, start_date1, end_date1, "NO2")

pollution_frame <- readings1 %>%
  mutate(category = "Previous 12 months up to August 2021") 

start_date1 <- "2019-09-01"
end_date1 <-  "2020-08-31"

readings2 <- airqualityengland(site_id, start_date1, end_date1, "NO2")

pollution_frame <- readings2 %>%
  mutate(category="Previous 12 months up to August 2020") %>%
  rbind(pollution_frame)

start_date1 <- "2018-09-01"
end_date1 <-  "2019-08-31"

readings3 <- airqualityengland(site_id, start_date1, end_date1, "NO2")

pollution_frame <- readings3 %>%
  mutate(category="Previous 12 months up to August 2019") %>%
  rbind(pollution_frame)

#tidy data

month_mean <- pollution_frame %>%
  mutate(period = as.Date(paste0(year(date),"-",month(date),"-01"))) %>%
  group_by(category,period) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(site="Trafford A56",
         indicator="NO2",
         measure="monthly mean",
         unit="µg/m3",
         value=round(value,1)) 

# plot data ---------------------------
ggplot(month_mean,aes(period,value,group=category)) +
  geom_line(size = 2, color = "grey") +
  geom_point(aes(color = -value),size = 10) +
  geom_text(aes(label=round(value,0)),color = "white") +
  facet_wrap(~category, ncol = 1, scales = "free_x") +
  scale_y_continuous(limits = c(0,50)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.015),
        plot.subtitle = element_text(margin=margin(0,0,20,0), hjust = 0.015),
        legend.position = "none",
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 10, face = "bold", hjust = 0)) +
  labs(title = expression(bold(paste("Monthly mean ", NO[2], " concentrations"))),
       subtitle = "Trafford A56, August 2021 to September 2018",
       caption = "Source: airqualityengland.co.uk  |  @traffordDataLab",
       x = "",
       y = "µg/m³") 

# write data ---------------------------
write_csv(month_mean %>% select(period, site, indicator, measure, unit, value), "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300, scale = 1)

