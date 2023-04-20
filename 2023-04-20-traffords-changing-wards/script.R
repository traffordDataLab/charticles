# Changing Boundaries - Trafford's new electoral ward boundaries for 2023
# 2023-04-13

# Source: OS Open Geography Portal, Trafford Council Planning Department.
# URL: https://geoportal.statistics.gov.uk/, https://www.trafford.gov.uk
# Licence: Open Government Licence v3.0

# load libraries ---------------------------
library(tidyverse) ; library(sf); library(nngeo); library(ggrepel)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")


# load data ---------------------------
## At the time of writing Trafford's current wards have been unchanged from 2004. The latest version of them available on the OS Open Geography Portal are from 2022: https://geoportal.statistics.gov.uk/datasets/ons::wards-december-2022-boundaries-gb-bfc/api
## API parameters specifying the spatial rectangular area containing Trafford
api_geommetry_envelope <- "&geometryType=esriGeometryEnvelope&geometry=%7B%22spatialReference%22%3A%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D%2C%22xmin%22%3A-278455.35481123265%2C%22ymin%22%3A7047642.057770884%2C%22xmax%22%3A-244823.0623658004%2C%22ymax%22%3A7073592.428873666%2C%22type%22%3A%22esriGeometryEnvelope%22%7D"

wards_prev <- st_read(paste0("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Wards_December_2022_Boundaries_GB_BFC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson", api_geommetry_envelope)) %>% 
    filter(LAD22NM == "Trafford") %>%
    select(area_code = WD22CD, area_name = WD22NM, lat = LAT, lon = LONG) %>% 
    st_as_sf(crs = 4326, coords = c("long", "lat"))

## The latest version of Trafford's wards come into effect from 04 May 2023 and have been provided by the Trafford Council Planning department. These will be available from the OS Open Geography Portal in time
wards_new <- st_read("https://www.trafforddatalab.io/spatial_data/ward/2023/trafford_ward_full_resolution.geojson")


# tidy data ---------------------------
localities_prev <- wards_prev %>%
    # Assign the wards into their relevant locality names and collapse the internal boundaries into a single area for each
    mutate(area_name = case_when(
        area_name %in% c("Ashton upon Mersey", "Brooklands", "Priory", "St Mary\'s", "Sale Moor") ~ "Central",
        area_name %in% c("Clifford", "Gorse Hill", "Longford", "Stretford") ~ "North",
        area_name %in% c("Altrincham", "Bowdon", "Broadheath", "Hale Barns", "Hale Central", "Timperley", "Village") ~ "South",
        area_name %in% c("Bucklow-St Martins", "Davyhulme East", "Davyhulme West", "Flixton", "Urmston") ~ "West")) %>% 
    group_by(area_name) %>% 
    summarise() %>%
    
    # Calculate and store the coordinates of each locality's centroid as a "lat" and "lon" property
    mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
           lat = map_dbl(geometry, ~st_centroid(.x)[[2]]))


localities_new <- wards_new %>% 
    # Assign the wards into their relevant locality names and collapse the internal boundaries into a single area for each
    mutate(area_name = case_when(
        area_name %in% c("Ashton upon Mersey", "Brooklands", "Manor", "Sale Central", "Sale Moor") ~ "Central",
        area_name %in% c("Gorse Hill & Cornbrook", "Longford", "Lostock & Barton", "Old Trafford", "Stretford & Humphrey Park") ~ "North",
        area_name %in% c("Altrincham", "Bowdon", "Broadheath", "Hale", "Hale Barns & Timperley South", "Timperley Central", "Timperley North") ~ "South",
        area_name %in% c("Bucklow-St Martins", "Davyhulme", "Flixton", "Urmston") ~ "West")) %>% 
    group_by(area_name) %>% 
    summarise() %>%
    
    # Calculate and store the coordinates of each locality's centroid as a "lat" and "lon" property
    mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
           lat = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>%
    
    # Some "artifacts" (small polygons) seem to be present within the Central and South localities - probably loose ends when creating the ward boundaries. This removes them    
    st_remove_holes()


# plot data ---------------------------
plot_map <- function(wards, localities, map_subtitle) {
    ggplot() +
        geom_sf(data = wards, fill = "#cce3aa", colour = "#757575", alpha = 1, linewidth = 0.1) +
        geom_sf(data = localities, fill = "transparent", colour = "#ffffff", linewidth = 1.2) +
        geom_text(data = wards,
                  mapping = aes(x = lon, y = lat, label = str_wrap(area_name, width = 15), group = area_name),
                  color = "#212121", size = 2.75) +
        geom_text_repel(data = localities, 
                        mapping = aes(x = lon, y = lat, label = area_name),
                        colour = "#ffffff", bg.color = "#636363", bg.r = 0.08, size = 5, fontface = "bold", point.size = NA, nudge_y = -0.0023) +
        labs(x = NULL, y = NULL, title = "Trafford wards and localities", subtitle = map_subtitle,
             caption = "Contains OS data © Crown copyright and database right (2023)\nTrafford Council | @traffordDataLab") +
        coord_sf(datum = NA) +
        theme_void(base_family = "Roboto") +
        theme(plot.title = element_text(size = 20, face = "bold"),
              plot.subtitle = element_text(size = 14),
              axis.text = element_blank(),
              legend.position = "none",
              plot.margin = unit(c(0.25,0,0.25,0), "cm"))
}

# plot wards and localities from 2004 - 2022
plot_prev <- plot_map(wards_prev, localities_prev, "2004 - 2022")

# plot wards and localities from 2023-05-04 onwards
plot_new <- plot_map(wards_new, localities_new, "From 04 May 2023")

# NOTE: view plots if required, however when saving don't have the plot in the viewer window as this will negatively affect the output
#plot_prev
#plot_new

# write data ---------------------------
ggsave("plot_2004-2022.svg", plot = plot_prev, dpi = 320, scale = 1, units = "px")
ggsave("plot_2004-2022.png", plot = plot_prev, dpi = 320, scale = 1, units = "px")

ggsave("plot_2023.svg", plot = plot_new, dpi = 320, scale = 1, units = "px")
ggsave("plot_2023.png", plot = plot_new, dpi = 320, scale = 1, units = "px")
