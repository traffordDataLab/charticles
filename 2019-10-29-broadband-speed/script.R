# Broadband speed #

# Source: Connected Nations, Ofcom
# URL: https://www.ofcom.org.uk/research-and-data/multi-sector-research/infrastructure-research/connected-nations-2018/data-downloads
# Licence: Open Government Licence 3.0

# Method: Median of mean postcode centroid download speed.

library(tidyverse); library(sf) ; library(jsonlite) ; library(purrr)
source("https://trafforddatalab.github.io/assets/theme/ggplot2/theme_lab.R")

# load data ---------------------------
url <- "https://www.ofcom.org.uk/__data/assets/file/0011/131042/201809_fixed_pc_r03.zip"
download.file(url, dest = "201809_fixed_pc_r03.zip")
unzip("201809_fixed_pc_r03.zip", exdir = ".")
file.remove("201809_fixed_pc_r03.zip")

# Retrieve Trafford's postcodes
# Source: ONS Open Geography Portal
postcodes <- read_csv("http://geoportal.statistics.gov.uk/datasets/75edec484c5d49bcadd4893c0ebca0ff_0.csv") %>%
  filter(oslaua == "E08000009") %>%
  select(postcode_space = pcds, lon = long, lat)

# Retrieve Trafford's electoral ward boundaries
# Source: ONS Open Geography Portal
codes <- fromJSON(paste0("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/WD18_LAD18_UK_LU/FeatureServer/0/query?where=LAD18NM%20like%20'%25", URLencode(toupper("Trafford"), reserved = TRUE), "%25'&outFields=WD18CD,LAD18NM&outSR=4326&f=json"), flatten = TRUE) %>% 
  pluck("features") %>% 
  as_tibble() %>% 
  distinct(attributes.WD18CD) %>% 
  pull(attributes.WD18CD)

wards <- st_read(paste0("https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/Wards_December_2018_Boundaries_V3/MapServer/2/query?where=", 
                        URLencode(paste0("wd18cd IN (", paste(shQuote(codes), collapse = ", "), ")")), 
                        "&outFields=wd18cd,wd18nm&outSR=4326&f=geojson")) %>% 
  select(area_code = wd18cd, area_name = wd18nm)

# Tidy data ---------------------------
df <- bind_rows(read_csv("201809_fixed_pc_coverage_r01.csv"),
                read_csv("201805_fixed_pc_performance_r03.csv")) %>%
  left_join(., postcodes, by = "postcode_space") %>%
  filter(!is.na(lon)) %>%
  st_as_sf(crs = 4326, coords = c("lon", "lat")) %>%
  st_join(wards) %>%
  mutate(mean_download_speed = as.double(`Average download speed (Mbit/s)`)) %>%
  filter(mean_download_speed != "N/A") %>%
  group_by(area_code, area_name) %>%
  summarize(value = round(median(mean_download_speed, na.rm = TRUE), 1)) %>%
  st_set_geometry(value = NULL) %>%
  mutate(period = "2018-05",
         indicator = "Average download speed",
         measure = "Bandwidth",
         unit = "Mbps") %>%
  select(area_code, area_name, indicator, period, measure, unit, value)

# plot data ---------------------------
ggplot(df, aes(x = reorder(area_name, value), y = value)) +
  geom_col(fill = "#756bb1") +
  geom_text(aes(x = area_name, y = value, label = value),
            colour = "#FFFFFF", size = 5, hjust = 1.1, vjust = 0.5) +
  geom_hline(yintercept = 0, size = 1, colour = "#212121") +
  scale_y_continuous(expand = c(0.005, 0.005)) +
  coord_flip() +
  labs(title = "Average broadband speed",
       subtitle = "Trafford, May/June 2018",
       caption = "Source: Ofcom | @traffordDataLab",
       x = NULL,
       y = "Bandwidth (Mbit/s)",
       fill = NULL) +
  theme_lab() +
  theme(panel.grid.major.y = element_blank(),
        axis.title.x = element_text(hjust = 0.01),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12, hjust = 0))

# write data ---------------------------
write_csv(df, "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300, scale = 1)
