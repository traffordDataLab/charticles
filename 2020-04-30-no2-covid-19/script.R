# NO2 pollution levels during COVID-19 lockdown #

# Source: Trafford Council / Ricardo EE
# URL: https://www.airqualityengland.co.uk/site/latest?site_id=TRF2
# Licence: Open Government Licence 3.0


library(tidyverse) ; library(lubridate) ; library(rvest) ; library(svglite)
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data ---------------------------

site_id <- "TRF2"
end_date <- Sys.Date()

# 1-hour mean NO2 concentrations from Feb 2020 
url <- paste0("http://www.airqualityengland.co.uk/local-authority/data.php?site_id=", site_id, "&parameter_id%5B%5D=NO2&f_query_id=920788&data=%3C%3Fphp+print+htmlentities%28%24data%29%3B+%3F%3E&f_date_started=2020-01-29&f_date_ended=",end_date,"&la_id=368&action=download&submit=Download+Data")

readings2020 <- read_html(url) %>% 
  html_node("a.b_xls.valignt") %>% 
  html_attr('href') %>% 
  read_csv(skip = 5) %>%
  mutate(date = as.Date(`End Date`, format = "%d/%m/%Y"),
         no2 = as.double(NO2)) 

# 1-hour mean NO2 concentrations 2019
url <- paste0("http://www.airqualityengland.co.uk/local-authority/data.php?site_id=", site_id, "&parameter_id%5B%5D=NO2&f_query_id=920788&data=%3C%3Fphp+print+htmlentities%28%24data%29%3B+%3F%3E&f_date_started=2019-01-29&f_date_ended=",as.Date(Sys.time()) %m-% months(12),"&la_id=368&action=download&submit=Download+Data")

readings2019 <- read_html(url) %>% 
  html_node("a.b_xls.valignt") %>% 
  html_attr('href') %>% 
  read_csv(skip = 5) %>%
  mutate(date = as.Date(`End Date`, format = "%d/%m/%Y"),
         no2 = as.double(NO2)) 


# 1-hour mean NO2 concentrations 2018
url <- paste0("http://www.airqualityengland.co.uk/local-authority/data.php?site_id=", site_id, "&parameter_id%5B%5D=NO2&f_query_id=920788&data=%3C%3Fphp+print+htmlentities%28%24data%29%3B+%3F%3E&f_date_started=2018-01-29&f_date_ended=",as.Date(Sys.time()) %m-% months(24),"&la_id=368&action=download&submit=Download+Data")

readings2018 <- read_html(url) %>% 
  html_node("a.b_xls.valignt") %>% 
  html_attr('href') %>% 
  read_csv(skip = 5) %>%
  mutate(date = as.Date(`End Date`, format = "%d/%m/%Y"),
         no2 = as.double(NO2)) 

df <- bind_rows(readings2018, readings2019, readings2020) %>%
  select(date,no2) %>%
  arrange(date) %>%
  group_by(date) %>%
  summarise(no2=mean(no2,na.rm = TRUE)) %>%
  filter(!is.na(date)) %>%
  mutate(year=factor(year(date))) %>%
  group_by(year, week = week(date)) %>%
  filter(n() > 6) %>%
  mutate(date=date[1]) %>%
  mutate(no2=mean(no2,na.rm = TRUE)) %>%
  unique() %>%
  ungroup() %>%
  mutate(site="Trafford A56",
         indicator="NO2",
         measure="weakly mean",
         unit="µg/m3",
         period=date,
         value=round(no2,1)) %>%
select(site:value)

# plot data ---------------------------
temp <- df %>%
  mutate(year=factor(year(period))) %>%
  mutate(period=update(period, year = 1))

ggplot(temp, aes(x = period, y = value, colour = year, group=year)) +
  geom_line(size = 1)+
  geom_vline(aes(xintercept = as.Date("0001-03-23")), color = "#212121", size = 1) +
  scale_colour_manual(values = c("2018" = "#52BCA3", "2019" = "#51757A", "2020" = "#814047")) +
  annotate(geom = "text", x = as.Date("0001-03-23")+8, y = 60, 
           label = "First day of UK lockdown", fontface = "bold", vjust = -0.5) +
  scale_y_continuous(limits = c(0, 60)) +
  labs(title = expression(bold(paste("Weekly mean ", NO[2], " concentrations during COVID-19 lockdown"))),
       subtitle = "Trafford A56 Site, 2018-2020",
       caption = "Source: airqualityengland.co.uk  |  @traffordDataLab",
       x = "",
       y = "µg/m³",
       colour = NULL) +
  theme_lab() +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "top")

# write data ---------------------------
write_csv(df, "data.csv")
ggsave("plot.png", dpi = 300, scale = 1)
ggsave("plot.svg", dpi = 300, scale = 1)
