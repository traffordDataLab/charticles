# Domestic Renewable Heat Incentive #

# Source: Ofgem
# URL: https://www.gov.uk/government/collections/renewable-heat-incentive-statistics
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(httr) ; library(readxl)
source("https://trafforddatalab.github.io/assets/theme/ggplot2/theme_lab.R")

# read data ---------------------------
url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/839469/RHI_monthly_official_stats_tables_sept_19_final.xlsx"
GET(url, write_disk(tmp <- tempfile(fileext = ".xlsx")))

df <- read_xlsx(tmp, sheet = 20, skip = 4) %>% 
  filter(`Area Codes` %in% c("E08000009", "E08000001", "E08000002", "E08000003", "E08000004", "E08000005", "E08000006", "E08000007", "E08000008", "E08000010")) %>% 
  select(area_code = `Area Codes`,
         area_name = `Area names`,
         value = `Number of accredited installations`) %>% 
  mutate(period = "2011-11 to 2019-09",
         value = case_when(value %in% c("#", "^") ~ "NA", TRUE ~ value),
         value = as.integer(value),
         indicator = "Domestic Renewable Heat Incentive",
         measure = "Count",
         unit = "Accredited applications") %>% 
  select(area_code, area_name, indicator, period, measure, unit, value)

# plot data ---------------------------
ggplot(df, aes(x = fct_reorder(area_name, value), y = value)) +
  geom_col(fill = "#a9c093") +
  geom_text(aes(y = 0, label = paste0(area_name, ": ", value, " ")), 
            hjust = 1, size = 4, colour = "#212121") +
  labs(x = NULL, y = NULL,
       title = "Domestic Renewable Heat Incentive accreditations",
       subtitle = "Greater Manchester, 2011-11 to 2019-09",
       caption = "Source: Ofgem | @traffordDataLab",
       fill = NULL) +
  theme_lab() +
  theme(panel.grid.major.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text = element_blank()) +
  coord_polar(theta = "y") +
  ylim(0, 500) 

# write data ---------------------------
write_csv(df, "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300, scale = 1)
