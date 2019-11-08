# Living alone #

# Source: Table DC1109EW, Census 2011
# Publisher URL: https://www.nomisweb.co.uk/census/2011/dc1109ew
# Licence: OS "Free to Use Data" Licence

library(tidyverse) ; library(sf) ; library(scales)
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data ---------------------------
df <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_748_1.data.csv?date=latest&geography=1140857093...1140857113&c_sex=0&c_age=5&c_hhchuk11=0,1&measures=20100&select=date_name,geography_name,geography_code,c_sex_name,c_age_name,c_hhchuk11_name,measures_name,obs_value,obs_status_name") %>% 
  select(area_code = GEOGRAPHY_CODE, 
         area_name = GEOGRAPHY_NAME,
         period = DATE_NAME,
         value = OBS_VALUE,
         group = C_HHCHUK11_NAME) %>% 
  spread(group, value) %>% 
  mutate(value = `One person household: Total`/`All categories: Household composition`,
         indicator = "Residents aged 50 and over living alone",
         measure = "Proportion",
         unit = "Persons") %>% 
  select(area_code, area_name, indicator, period, measure, unit, value)

# Census Merged Wards

# Source: ONS, Open Geography Portal
# Publisher URL: https://geoportal.statistics.gov.uk/datasets/census-merged-wards-december-2011-generalised-clipped-boundaries-in-england-and-wales
# Licence: https://www.ons.gov.uk/methodology/geography/licences

wards <- st_read("https://ons-inspire.esriuk.com/arcgis/rest/services/Census_Boundaries/Census_Merged_Wards_December_2011_Boundaries/MapServer/1/query?where=UPPER(lad11nm)%20like%20'%25TRAFFORD%25'&outFields=lad11nm,cmwd11nm,cmwd11cd&outSR=4326&f=geojson") %>% 
  select(area_code = cmwd11cd)

sf <- left_join(wards, df, by = "area_code") %>% 
  mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         lat = map_dbl(geometry, ~st_centroid(.x)[[2]]))

# plot data ---------------------------
ggplot(sf) +   
  geom_sf(fill = "#FFFFFF") +   
  geom_point(aes(lon, lat, size = value), shape = 21, fill = "#6c2167", colour = "#FFFFFF", alpha = 0.7) +   
  geom_text(aes(lon, lat, label = paste0(round(value*100,0), "%")), size = 3.6, colour = "#FFFFFF") +   
  scale_size_continuous(range = c(10, 20), label = percent_format(accuracy = 1),
                        breaks = pretty_breaks(3)) +
  labs(x = NULL, y = NULL,
       title = "Residents aged 50 and over living alone",
       subtitle = "Trafford, 2011",
       caption = "Contains Ordnance Survey data © Crown copyright and database right 2019\nSource: Census 2011 | @traffordDataLab",
       size = NULL) +
  coord_sf(datum = NA) +
  theme_lab() +
  theme(legend.position = "none")

# write data ---------------------------
write_csv(df, "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300, scale = 1)

# analyse data ---------------------------
gm <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_748_1.data.csv?date=latest&geography=1946157081...1946157090&c_sex=0&c_age=5&c_hhchuk11=0,1&measures=20100&select=date_name,geography_name,geography_code,c_sex_name,c_age_name,c_hhchuk11_name,measures_name,obs_value,obs_status_name") %>% 
  select(area_code = GEOGRAPHY_CODE, 
         area_name = GEOGRAPHY_NAME,
         period = DATE_NAME,
         value = OBS_VALUE,
         group = C_HHCHUK11_NAME) %>% 
  spread(group, value) %>% 
  summarise(value = sum(`One person household: Total`)/sum(`All categories: Household composition`))