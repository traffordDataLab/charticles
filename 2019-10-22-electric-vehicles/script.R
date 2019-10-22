# Electric vehicles #

# Source: Department for Transport and Driver and Vehicle Licensing Agency 
# URL: https://www.gov.uk/government/statistical-data-sets/all-vehicles-veh01
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(httr);  library(readODS)
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# read data ---------------------------
tmp <- tempfile(fileext = ".ods")
GET(url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/794447/veh0131.ods",
    write_disk(tmp))

df <- read_ods(tmp, skip = 6)  %>%  
  filter(`Region/Local Authority` == "Trafford") %>% 
  rename(area_code = `ONS LA Code`, area_name = `Region/Local Authority`) %>% 
  gather(period, value, -area_code, -area_name) %>% 
  mutate(quarter = 
           case_when(
             str_detect(period, "Q1") ~ "01-01",
             str_detect(period, "Q2") ~ "04-01",
             str_detect(period, "Q3") ~ "07-01",
             str_detect(period, "Q4") ~ "10-01"),
         period = parse_number(period),
         value = as.numeric(na_if(value, "c"))) %>% 
  unite(period, c("period", "quarter"), sep = "-") %>% 
  mutate(indicator = "Licensed electric vehicles",
         period = as.Date(period, format = "%Y-%m-%d"),
         measure = "Count",
         unit = "Vehicles") %>% 
  select(area_code, area_name, indicator, period, measure, unit, value)

# plot data ---------------------------
ggplot(df, aes(x = period, y = value)) +
  geom_line(colour = "#2DAC58", size = 1) +
  geom_area(fill = "#2DAC58") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_y_continuous(expand = c(0.005, 0.005)) +
  labs(x = NULL, y = NULL,
       title = "Number of licensed electric vehicles in Trafford",
       subtitle = NULL,
       caption = "Source: DfT and DVLA") +
  theme_lab() 

# write data ---------------------------
write_csv(df, "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300, scale = 1)
