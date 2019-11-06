# Childhood obesity, 2015/16 to 2017/18 #

# Source: National Child Measurement Programme
# URL: https://www.gov.uk/government/statistics/child-obesity-and-excess-weight-small-area-level-data
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(httr) ; library(readODS) ; library(janitor) ; library(fingertipsR)
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data ---------------------------
url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/789000/NCMP_data_Ward_update_2019.ods"
GET(url, write_disk(tmp <- tempfile(fileext = ".ods")))

wards <- read_ods(tmp, sheet = 5, skip = 3) %>% 
  clean_names() %>% 
  filter(la_name == "Trafford") %>% 
  select(area_code = ward_code,
         area_name = ward_name,
         value = percent_8, 
         LCI = lci_8, 
         UCI = uci_8)

benchmark <- fingertips_data(IndicatorID = 93107) %>% 
  filter(AreaName %in% c("Trafford", "England")) %>% 
  select(area_code = AreaCode,
         area_name = AreaName,
         value = Value,
         LCI = LowerCI95.0limit,
         UCI = UpperCI95.0limit)

df <- bind_rows(wards, benchmark) %>% 
  mutate(area_name = fct_reorder(factor(area_name), value),
         area_name = fct_relevel(factor(area_name), "England", "Trafford"),
         indicator = "Obesity prevalence in Year 6 children",
         period = "2015/16 to 2017/18",
         measure = "Proportion",
         unit = "Children",
         group = "Year 6") %>% 
  select(area_code, area_name, indicator, period, measure, unit, value, LCI, UCI, group)

# plot data ---------------------------
ggplot(df, aes(x = area_name, y = value)) +
  geom_col(fill = "#ca562c") +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.2) +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  scale_y_continuous(expand = c(0.005, 0.005),
                     labels = function(x){ paste0(x, "%") }) +
  labs(x = NULL, y = NULL,
       title = "Obesity prevalence amongst Year 6 children",
       subtitle = "Trafford's wards, 2015/16 to 2017/18",
       caption = "Source: National Child Measurement Programme | @traffordDataLab") +
  theme_lab() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none")

# write data ---------------------------
write_csv(df, "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300, scale = 1)
