# Green flags #

# Source: Ministry of Housing, Communities & Local Government / Keep Britain Tidy
# URL: http://www.greenflagaward.org.uk
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(httr) ; library(jsonlite) ; library(purrr) ; library(sf)
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data ---------------------------
bdy <- st_read("https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/Local_Authority_Districts_December_2018_Boundaries_UK_BGC/MapServer/0/query?where=lad18nm%20IN%20(%27Bolton%27,%27Bury%27,%27Manchester%27,%27Oldham%27,%27Rochdale%27,%27Salford%27,%27Stockport%27,%27Tameside%27,%27Trafford%27,%27Wigan%27)&outFields=lad18cd,lad18nm&outSR=4326&f=geojson") %>% 
  select(area_code = lad18cd, area_name = lad18nm)

r <- POST("http://www.greenflagaward.org.uk/umbraco/surface/parks/getparks/")
df <- content(r, as = "text", encoding = "UTF-8") %>% 
  fromJSON(flatten = TRUE) %>% 
  purrr::pluck("Parks") %>% 
  as_tibble() %>% 
  filter(WonGreenFlagAward == TRUE) %>% 
  mutate(name = str_trim(Title),
         url = str_c("http://www.greenflagaward.org.uk/park-summary/?park=", ID),
         lon = as.numeric(as.character(Longitude)),
         lat = as.numeric(as.character(Latitude))) %>% 
  select(name, url, lon, lat) %>% 
  arrange(name) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_join(bdy, join = st_within) %>% 
  filter(!is.na(area_code)) %>% 
  select(area_code, area_name, name, url) %>% 
  cbind(st_coordinates(.)) %>%
  rename(lon = X, lat = Y) %>% 
  st_set_geometry(NULL)

# plot data ---------------------------
ggplot(data = bdy) +
  geom_sf(fill = "#E7E7E7", color = "#C4C4C4") +
  geom_point(data = df, aes(x = lon, y = lat), size = 4, 
             shape = 21, fill = "#7EA52E", colour = "#FFFFFF") +
  labs(x = NULL, y = NULL, 
       title = "Green Flag Awards",
       subtitle = "Greater Manchester district authorities, 2019",
       caption = "Source: MHCLG | @traffordDataLab\nContains Ordnance Survey data © Crown copyright and database right 2019",
       fill = NULL) +
  coord_sf(datum = NA) +
  theme_lab() 

# write data ---------------------------
write_csv(df, "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300, scale = 1)

