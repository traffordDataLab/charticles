# Green belt #

# Source: MHCLG
# Publisher URL: https://data.gov.uk/dataset/d7fcc345-6028-4266-836c-1d7cc6b034c5/english-local-authority-green-belt-dataset
# Licence: Open Government Licence

library(tidyverse) ; library(sf)
source("https://trafforddatalab.github.io/assets/theme/ggplot2/theme_lab.R")

# load data ---------------------------
url <- "http://maps.communities.gov.uk/geoserver/dclg_inspire/ows?service=WFS&version=2.0.0&request=GetFeature&typeName=dclg_inspire:Local_Authority_Greenbelt_boundaries_2018-19&outputFormat=shape-zip&srsName=EPSG:4326"
download.file(url, dest = "Local_Authority_Greenbelt_boundaries_2018-19.zip")
unzip("Local_Authority_Greenbelt_boundaries_2018-19.zip")
file.remove("Local_Authority_Greenbelt_boundaries_2018-19.zip")

green_belt <- st_read("Local_Authority_Greenbelt_boundaries_2018-19.shp") %>% 
  filter(str_detect(LA_Name, "Trafford")) %>% 
  st_transform(4326) %>% 
  mutate(period = "2018-19",
         indicator = "Designated green belt land",
         measure = "Area",
         unit = "Hectares") %>% 
  select(area_code = ONS_Code,
         area_name = LA_Name,
         period, indicator, measure, unit,
         value = Area_ha)

# Local authority district boundary
# Source: ONS, Open Geography Portal
la <- st_read(paste0("https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/Local_Authority_Districts_December_2018_Boundaries_UK_BGC/MapServer/0/query?where=UPPER(lad18nm)%20like%20'%25", URLencode(toupper("Trafford"), reserved = TRUE), "%25'&outFields=lad18cd,lad18nm,long,lat&outSR=4326&f=geojson"), quiet = TRUE) %>% 
  select(area_code = lad18cd, area_name = lad18nm, lon = long, lat)

# Built-up Areas (December 2011)
# Source: ONS, Open Geography Portal
built_up_areas <- st_read("https://opendata.arcgis.com/datasets/f6684981be23404e83321077306fa837_0.geojson") %>% 
  st_transform(4326) %>% 
  st_intersection(la)

# plot data ---------------------------
ggplot() + 
  geom_sf(data = green_belt, aes(fill = "Green belt"), colour = NA, show.legend = T) +
  geom_sf(data = la, fill = NA, color = "#212121", size = 0.5) +
  geom_sf(data = built_up_areas, aes(fill = "Built-up areas (December 2011)"), colour = NA) +
  scale_fill_manual(values = c("Green belt" = "#A3AF4D", "Built-up areas (December 2011)" = "#EEB23D"),
                    guide = guide_legend(reverse = TRUE)) +
  labs(x = NULL, y = NULL, 
       title = "Extent of Green Belt designated land",
       subtitle = "Trafford, as of 31 March 2019",
       caption = "Contains Ordnance Survey data © Crown copyright and database right 2019\nSource: MHCLG | @traffordDataLab",
       fill = NULL) +
  coord_sf(datum = NA) +
  theme_lab() +
  theme(legend.position = c(0.23, 0.95))

# write data ---------------------------
st_write(green_belt, "data.geojson")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300, scale = 1)
