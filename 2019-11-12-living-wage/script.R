# Living wage #

# Source: Annual Survey of Hours and Earnings, ONS
# URL: https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/adhocs/009211annualsurveyofhoursandearningsasheestimatesofthenumberandproportionofemployeejobswithhourlypaybelowthelivingwagebyworkgeographylocalauthorityandparliamentaryconstituencyukapril2017andapril2018
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(readxl)
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data ---------------------------
url <- "https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/adhocs/009211annualsurveyofhoursandearningsasheestimatesofthenumberandproportionofemployeejobswithhourlypaybelowthelivingwagebyworkgeographylocalauthorityandparliamentaryconstituencyukapril2017andapril2018/20172018livingwagebyworkgeographyv2.zip"
download.file(url, dest = "20172018livingwagebyworkgeographyv2.zip")
unzip("20172018livingwagebyworkgeographyv2.zip", exdir = ".")
file.remove("20172018livingwagebyworkgeographyv2.zip")

df_2017 <- read_xls("Work Geography LW Table 7.1a   lpmgx 2017.xls", sheet = 2, skip = 4) %>% 
  filter(Description %in% c("England", "Greater Manchester Met County", "Bolton", "Bury", "Manchester", "Oldham", "Rochdale", "Salford", "Stockport", "Tameside", "Trafford", "Wigan")) %>% 
  mutate(period = 2017) %>% 
  select(area_code = 2, area_name = 1, period, value = 4)

df_2018 <- read_xls("Work Geography LW Table 7.1a   lpmgx 2018.xls", sheet = 2, skip = 4) %>% 
  filter(Description %in% c("England", "Greater Manchester Met County", "Bolton", "Bury", "Manchester", "Oldham", "Rochdale", "Salford", "Stockport", "Tameside", "Trafford", "Wigan")) %>% 
  mutate(period = 2018) %>% 
  select(area_code = 2, area_name = 1, period, value = 4)

df <- bind_rows(df_2017, df_2018) %>% 
  mutate(area_name = case_when(area_name == "Greater Manchester Met County" ~ "Greater Manchester", TRUE ~ area_name),
         indicator = "Employees earning below the Real Living Wage",
         measure = "Percentage",
         unit = "Employees",
         value = round(as.numeric(value), 1)) %>% 
  select(area_code, area_name, indicator, period, measure, unit, value) %>% 
  mutate(area_name = fct_reorder(factor(area_name), value),
         area_name = fct_relevel(factor(area_name), "England", "Greater Manchester"))

# plot data ---------------------------
ggplot() +
  geom_segment(
    data = select(df, area_name, value, period) %>% 
      group_by(area_name) %>% 
      summarise(start = range(value)[1], end = range(value)[2]) %>% 
      ungroup(),
    aes(x = start, xend = end, y = area_name, yend = area_name), 
    color = "#e3e2e1", size = 2) +
  geom_point(
    data = select(df, area_name, period, value),
    aes(x = value, y = area_name, color = factor(period)),
    size = 4) +
  scale_x_continuous(expand = c(0.005, 0.005), labels = function(x){ paste0(x, "%") }) +
  scale_color_manual(values = c("#d2fbd4", "#559c9e"),
                     labels = c("2017", "2018")) +
  labs(x = NULL, y = NULL, 
       title = "Employee jobs below the living wage", 
       subtitle = "Greater Manchester district authorities, 2017 to 2018", 
       caption = "Source: Office of National Statistics | @traffordDataLab", 
       colour = NULL) +
  theme_lab() +
  theme(panel.grid.major.x = element_line(size = 0.5, color = "#cbcbcb"),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(face = "bold", hjust = 0),
        legend.position = "right",
        legend.text = element_text(size = 10)) +
  expand_limits(x = c(0, 35))

# write data ---------------------------
write_csv(df, "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300, scale = 1)
