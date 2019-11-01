# Relative deprivation #

# Source: English Indices of Deprivation 2019, MHCLG
# Publisher URL: https://www.gov.uk/government/statistics/announcements/english-indices-of-deprivation-2019
# Licence: Open Government Licence 3.0

library(tidyverse) ; library(janitor) ; library(httr) ; library(readxl) 
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data ---------------------------
tmp <- tempfile(fileext = ".xlsx")
GET(url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833970/File_1_-_IMD2019_Index_of_Multiple_Deprivation.xlsx",
    write_disk(tmp))

df <- read_xlsx(tmp, sheet = 2) %>% 
  clean_names() %>% 
  filter(local_authority_district_name_2019 == "Trafford") %>% 
  select(area_code = lsoa_code_2011,
         area_name = lsoa_name_2011,
         value = index_of_multiple_deprivation_imd_rank) %>% 
  mutate(period = "2019",
         indicator = "Index of Multiple Deprivation",
         measure = "Rank",
         unit = "Index") %>% 
  select(area_code, area_name, indicator, period, measure, unit, value)

# plot data ---------------------------
ggplot() +
  annotate("rect", xmin = 0, xmax = 3284, ymin = 0, ymax = 1, 
           alpha = 0.5, fill = "#1380A1") +
  annotate("rect", xmin = 3284, xmax = 32844, ymin = 0, ymax = 1, 
           alpha = 0.5, fill = "#dddddd") +
  geom_segment(data = df, aes(x = value, xend = value, y = 0, yend = 1), 
               colour = "#212121", size = 0.25, alpha = 1) +
  scale_x_continuous(limits = c(0, 32844), expand = c(0.005, 0.005), 
                     breaks = c(3284, 6569, 9853, 13138, 16422, 19706, 22991, 26275, 29560, 32844),
                     labels = c(1:10)) +
  scale_y_continuous(expand = c(0.005, 0.005)) +
  labs(x = NULL, y = NULL, 
       title = "Index of Multiple Deprivation", 
       subtitle = "Trafford, 2019", 
       caption = "Source: English Indices of Deprivation 2019, MHCLG",
       tag = "Each line represents an LSOA") +
  theme_lab() +
  theme(plot.margin = unit(c(17,0.5,0.5,0.5), "cm"),
        panel.grid.major = element_blank(),
        axis.text.x = element_text(size = 8, face = "bold"),
        axis.text.y = element_blank(),
        plot.tag.position = c(0.01, 0.1),
        plot.tag = element_text(size = 9, colour = "#757575", hjust = 0),
        legend.position = "none")

# write data ---------------------------
write_csv(df, "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300, scale = 1)

