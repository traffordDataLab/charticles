# Median house prices #

# Source: ONS
# Publisher URL: https://www.ons.gov.uk/peoplepopulationandcommunity/housing/datasets/medianpricepaidbywardhpssadataset37
# Licence: Open Government Licence

library(tidyverse) ; library(jsonlite) ; library(sf) ; library(readxl) ; library(viridis) ; library(scales) ; library(ggrepel)
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data ---------------------------

# codes for Trafford's electoral wards
# https://geoportal.statistics.gov.uk/
codes <- fromJSON(paste0("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/WD18_LAD18_UK_LU/FeatureServer/0/query?where=LAD18NM%20like%20'%25", URLencode(toupper("Trafford"), reserved = TRUE), "%25'&outFields=WD18CD,LAD18NM&outSR=4326&f=json"), flatten = TRUE) %>% 
  pluck("features") %>% 
  as_tibble() %>% 
  distinct(attributes.WD18CD) %>% 
  pull(attributes.WD18CD)

# median house prices
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/housing/datasets/medianpricepaidbywardhpssadataset37/yearendingdecember2018/hpssadataset37medianpricepaidbyward.zip"
download.file(url, dest = "hpssadataset37medianpricepaidbyward.zip")
unzip("hpssadataset37medianpricepaidbyward.zip", exdir = ".")
file.remove("hpssadataset37medianpricepaidbyward.zip")

df <- read_xls("HPSSA Dataset 37 - Median price paid by ward.xls", sheet = 6, skip = 4) %>% 
  select(area_code = `Ward code`, value = `Year ending Dec 2018`) %>% 
  filter(area_code %in% codes) %>% 
  mutate(value = as.integer(value))

# join median house prices to Trafford's electoral wards
sf <- st_read(paste0("https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/Wards_December_2018_Boundaries_V3/MapServer/2/query?where=", 
                        URLencode(paste0("wd18cd IN (", paste(shQuote(codes), collapse = ", "), ")")), 
                        "&outFields=wd18cd,wd18nm&outSR=4326&f=geojson")) %>% 
  rename(area_code = wd18cd, area_name = wd18nm) %>% 
  left_join(df, by = "area_code") %>% 
  mutate(indicator = "Median house price by ward",
         period = "2018",
         measure = "Price",
         unit = "Sterling") %>% 
  select(-value, everything()) %>% 
  arrange(area_name)

# plot data ---------------------------
ggplot(sf) + 
  geom_sf(aes(fill = value), color = "#FFFFFF", size = 0.5, alpha = 0.8) +
  geom_text_repel(data = filter(sf, area_name == "Bucklow-St Martins"),
                  aes(label = paste0(area_name, "\n£", comma(value)),  
                      geometry = geometry),
                  stat = "sf_coordinates",
                  min.segment.length = 0,
                  nudge_x = -0.08, nudge_y = 0.01,
                  arrow = arrow(length = unit(0.02, "npc"))) +
  geom_text_repel(data = filter(sf, area_name == "Bowdon"),
                  aes(label = paste0(area_name, "\n£", comma(value)),  
                      geometry = geometry),
                  stat = "sf_coordinates",
                  min.segment.length = 0,
                  nudge_x = -0.08, nudge_y = -0.01,
                  arrow = arrow(length = unit(0.02, "npc"))) +
  scale_fill_viridis(discrete = F, 
                     label = function(x) paste0("£", comma(x)),
                     direction = -1,
                     guide = guide_colourbar(
                       direction = "horizontal",
                       barheight = unit(4, units = "mm"),
                       barwidth = unit(80, units = "mm"),
                       title.position = 'left',
                       title.vjust = 1)) +
  labs(x = NULL, y = NULL,
       title = "Median house prices in Trafford's wards, 2018",
       subtitle = NULL,
       caption = "Source: Office for National Statistics \n Contains Ordnance Survey data © Crown copyright and database right 2019",
       fill = NULL) +
  coord_sf(datum = NA) +
  theme_lab() +
  theme(legend.position = c(0.2, 0.02),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)) 

# write data ---------------------------
write_csv(st_set_geometry(sf, NULL), "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300, scale = 1)
