## Fuel poverty ##

# Source: Department for Business, Energy & Industrial Strategy
# URL: https://www.gov.uk/government/statistics/sub-regional-fuel-poverty-data-2019
# Licence: Open Government Licence 3.0

library(tidyverse) ; library(httr) ; library(readxl)
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data ---------------------------
tmp <- tempfile(fileext = ".xlsx")
GET(url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/808294/Fuel_poverty_sub-regional_tables_2019.xlsx",
    write_disk(tmp))

df <- read_xlsx(tmp, sheet = 6, range = "A3:H32847") %>% 
  filter(`LA Name` == "Trafford") %>% 
  mutate(indicator = "Proportion of households in fuel poverty",
         period = "2017",
         measure = "Proportion",
         unit = "Households") %>% 
  select(lsoa11cd = `LSOA Code`, lsoa11nm = `LSOA Name`,
         area_code = `LA Code`, area_name = `LA Name`,
         indicator, period, measure, unit,
         value = `Proportion of households fuel poor (%)`) 

sf <- left_join(st_read(paste0("https://ons-inspire.esriuk.com/arcgis/rest/services/Census_Boundaries/Lower_Super_Output_Areas_December_2011_Boundaries/MapServer/2/query?where=UPPER(lsoa11nm)%20like%20'%25", URLencode(toupper("Trafford"), reserved = TRUE), "%25'&outFields=lsoa11cd&outSR=4326&f=geojson")),
                df, by = "lsoa11cd")

# plot data ---------------------------
ggplot(sf) + 
  geom_sf(aes(fill = value), color = "#FFFFFF", size = 0.5, alpha = 0.8) +
  scale_fill_viridis(discrete = F, 
                     label = function(x) paste0(x, "%"),
                     direction = -1,
                     guide = guide_colourbar(
                       direction = "vertical",
                       barwidth = unit(3, units = "mm"),
                       title.position = 'top',
                       title.vjust = 1)) +
  labs(x = NULL, y = NULL,
       title = "Proportion of households in fuel poverty",
       subtitle = "Trafford, 2017",
       caption = "Source: BEIS \n Contains Ordnance Survey data © Crown copyright and database right 2019",
       fill = NULL) +
  coord_sf(datum = NA) +
  theme_lab() +
  theme(plot.title = element_text(size = 16),
        legend.position = "right",
        legend.text = element_text(size = 10))

# write data ---------------------------
write_csv(df, "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300, scale = 1)
