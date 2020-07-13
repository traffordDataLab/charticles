# Claimant rate during COVID-19 outbreak

# Source: ONS
# URL: https://www.nomisweb.co.uk/sources/cc
# Licence: Open Government Licence v3.0

# load libraries ---------------------------
library(tidyverse) ; library(zoo) ; library(forcats)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data ---------------------------
raw <- read_csv("https://www.nomisweb.co.uk/api/v01/dataset/NM_162_1.data.csv?geography=1660945005...1660945019,1660945021,1660945020,1660945022...1660945025,1853882369,1811939363,2092957697&date=latestMINUS2-latest&gender=0&age=0&measure=2&measures=20100") 


# tidy data ---------------------------
df <- raw %>%
  select(area_code = GEOGRAPHY_CODE,
         area_name = GEOGRAPHY_NAME,
         indicator = MEASURE_NAME,
         period = DATE_NAME,
         value = OBS_VALUE) %>%
  mutate(period = factor(as.yearmon(period, "%b %Y"), levels = c("Mar 2020","Apr 2020","May 2020")),
         area_name = fct_reorder(factor(area_name), ifelse(period == "May 2020", value, NA), na.rm = TRUE),
         area_name = fct_relevel(factor(area_name), "United Kingdom", "Greater Manchester","Trafford"),
         measure = "percent",
         unit = "persons")

# plot data ---------------------------

ggplot(df, aes(value,area_name)) +
  geom_line(color = "#f0f0f0", size=5) +
  geom_point(aes(color = period), size = 5) +
  scale_x_continuous(labels = function(x){ paste0(x, "%") }, limits = c(0, 15)) +
  scale_colour_manual(values = c( "#fd8d3c","#f03b20", "#bd0026")) +
  labs(x = "percentage of residents aged 16 or over", y = NULL,
       title = "Claimant Rate during COVID-19 outbreak", 
       subtitle = "Trafford Wards, March 2020 to May 2020", 
       caption = "Source: ONS  |  @traffordDataLab")+
  theme_lab() +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_text(hjust = 1),
        legend.position = "bottom", 
        legend.title = element_blank())

# write data ---------------------------
write_csv(df, "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300, scale = 1)

