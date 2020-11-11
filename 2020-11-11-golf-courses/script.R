# Golf courses #

# load libraries ---------------------------
library(tidyverse) ; library(sf) ; library(units) ; library(tmap)

# load data ---------------------------

# Name of local authority district
id <- "Trafford"

# OS Open Greenspace
# Source: Ordnance Survey
# URL: https://www.ordnancesurvey.co.uk/business-government/products/open-map-greenspace
# Licence: OGL 3.0
url <- "https://api.os.uk/downloads/v1/products/OpenGreenspace/downloads?area=GB&format=ESRI%C2%AE+Shapefile&redirect"
download.file(url, dest = "opgrsp_essh_gb")
unzip("opgrsp_essh_gb", exdir = ".")
file.remove("opgrsp_essh_gb")
greenspace <- read_sf("OS Open Greenspace (ESRI Shape File) GB/data/GB_GreenspaceSite.shp")

# Electoral ward GSS codes
# Source: ONS Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/
# Licence: OGL 3.0
lookup <- read_csv("https://opendata.arcgis.com/datasets/e169bb50944747cd83dcfb4dd66555b1_0.csv") %>% 
  filter(LAD19NM == id) %>% 
  distinct(WD19CD) %>% 
  pull()

# Electoral ward boundaries
# Source: ONS Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/wards-december-2019-boundaries-ew-bfc
# Licence: OGL 3.0
wards <- st_read(paste0("https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/Wards_December_2019_Boundaries_EW_BFC/MapServer/0/query?where=", 
                        URLencode(paste0("wd19cd IN (", paste(shQuote(lookup), collapse = ", "), ")")), 
                        "&outFields=wd19cd,wd19nm&outSR=27700&f=geojson")) %>% 
  # calculate area (sq/m) of each electoral ward
  mutate(area_m2 = as.numeric(set_units(st_area(.), m^2))) %>%
  select(area_code = wd19cd, area_name = wd19nm, area_m2)
  
# tidy data ---------------------------
sf <- greenspace %>%
  # filter by golf course
  filter(function. == "Golf Course") %>% 
  # clip polygons to electoral ward boundary
  st_intersection(wards) %>%
  # calculate area (sq/m) of golf courses
  mutate(golf_course_m2 = as.numeric(set_units(st_area(.), m^2))) %>%
  group_by(area_code, area_name, area_m2) %>%
  # calculate total area (sq/m) of golf courses by electoral ward
  summarise(golf_course_m2 = sum(golf_course_m2)) %>%
  ungroup() %>%
  # calculate percentage of electoral ward covered by golf courses
  mutate(percent_golf_course = round((golf_course_m2/area_m2)*100, 3),
         area_m2 = round(area_m2,0),
         golf_course_m2 = round(golf_course_m2,0)) %>%
  select(area_code, area_name, area_m2, golf_course_m2, percent_golf_course)

# plot data ---------------------------
tm <- tm_shape(filter(wards, area_name %in% sf$area_name), is.master = TRUE) +
  tm_borders() +
  tm_fill("#E8E5D8") +
  tm_facets(by = "area_name", nrow = 2) +
  tm_shape(sf) +
  tm_polygons(col = "#0a8116", lwd = 0.5) +
  tm_facets(by = "area_name", nrow = 2) +
  tm_layout(main.title = "Golf courses in Trafford's wards",
            main.title.position = "center",
            main.title.size = 1.5,
            main.title.color = "#212121",
            panel.label.height = 2,
            panel.label.size = 1.5, 
            panel.label.bg.color = "#FFFFFF") +
  tmap_style("gray")

tm

# write data ---------------------------
write_csv(st_drop_geometry(sf), "data.csv")
tmap_save(tm, "plot.png", height = 7)
tmap_save(tm, "plot.svg", height = 7)
