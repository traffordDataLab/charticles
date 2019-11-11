# Alcohol related mortality #

# Source: Office for National Statistics
# URL: https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(fingertipsR) ; library(scales)
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data ---------------------------
gm <- fingertips_data(IndicatorID = 91382, AreaTypeID = 101, ParentAreaTypeID = 126, rank = TRUE) %>% 
  filter(AreaCode == "E47000001", Sex == "Persons") %>% 
  mutate(AreaName = str_replace(AreaName, "CA-Greater Manchester", "Greater Manchester"))

df <- fingertips_data(IndicatorID = 91382, AreaTypeID = 101, rank = TRUE) %>% 
  filter(AreaType %in% c("England", "District & UA (pre 4/19)"), Sex == "Persons") %>% 
  bind_rows(gm) %>% 
  select(area_code = AreaCode,
         area_name = AreaName,
         period = Timeperiod,
         value = Value,
         significance = ComparedtoEnglandvalueorpercentiles,
         LCI = LowerCI95.0limit ,
         UCI = UpperCI95.0limit) %>%
  mutate(indicator = "Alcohol related mortality",
         measure = "Age standardised rate per 100,000",
         unit = "Persons",
         value = round(value, 1)) %>% 
  select(-significance, everything()) %>% 
  filter(period == "2017",
         area_name %in% c("England", "Greater Manchester", "Bolton","Bury","Manchester","Oldham","Rochdale","Salford","Stockport","Tameside","Trafford","Wigan"),
         !is.na(value)) %>% 
  mutate(area_name = fct_reorder(factor(area_name), value),
         area_name = fct_relevel(factor(area_name), "England", "Greater Manchester"))

# plot data ---------------------------
ggplot(df, aes(x = area_name, y = value)) +
  geom_col(aes(fill = significance)) +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.2) +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  scale_y_continuous(expand = c(0.005, 0.005) , limits = c(0,80)) +
  scale_fill_manual(values = c("Not compared" = "#dddddd",
                               "Similar" = "#FFC000",
                               "Worse" = "#C00000"),
                    breaks = c("Similar", "Worse")) +
  coord_flip() +
  labs(title = "Deaths from alcohol-related conditions",
       subtitle = "Greater Manchester district authorities, 2017",
       caption = "Source: Office for National Statistics",
       x = NULL, y = "Directly standardised rate - per 100,000",
       fill = "Compared with England") +
  theme_lab() +
  theme(panel.grid.major.y = element_blank(),
        plot.caption = element_text(margin = margin(t = 30)),
        axis.text.y = element_text(hjust = 0),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9))

# write data ---------------------------
write_csv(df, "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300, scale = 1)

