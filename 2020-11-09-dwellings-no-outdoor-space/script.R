# Dwellings with no outdoor space #

library(tidyverse) ; library(httr) ; library(readxl) ; library(sf) ; library(classInt) ; library(scales) ; library(ggrepel)

# load data ---------------------------

# MSOA names
# Source: House of Commons Library
# URL: https://visual.parliament.uk/msoanames
# Licence: OGL 3.0

lookup <- read_csv("https://visual.parliament.uk/msoanames/static/MSOA-Names-1.5.0.csv") %>% 
  select(msoa11cd, msoa11hclnm)

# MSOA boundaries
# Source: Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/middle-layer-super-output-areas-december-2011-boundaries-ew-bgc
# Licence: OGL 3.0

msoa <- st_read("https://opendata.arcgis.com/datasets/1e6f162967de4f3da92040761e8464d8_0.geojson") %>% 
  filter(str_detect(MSOA11NM, "Trafford")) %>%
  # extract centroid coordinates
  mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         lat = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>% 
  left_join(lookup, by = c("MSOA11CD" = "msoa11cd")) %>% 
  select(msoa11cd = MSOA11CD, msoa11hclnm, lon, lat)

# Access to gardens and public green space in Great Britain
# Source: Office for National Statistics
# URL: https://www.ons.gov.uk/economy/environmentalaccounts/datasets/accesstogardensandpublicgreenspaceingreatbritain
# Licence: OGL 3.0

tmp <- tempfile(fileext = ".xlsx")
GET(url = "https://www.ons.gov.uk/file?uri=%2feconomy%2fenvironmentalaccounts%2fdatasets%2faccesstogardensandpublicgreenspaceingreatbritain%2faccesstogardenspacegreatbritainapril2020/osprivateoutdoorspacereferencetables.xlsx",
    write_disk(tmp))

df <- read_xlsx(tmp, sheet = 5) %>% 
  filter(`LAD name` == "Trafford") %>% 
  mutate(percent_flats_no_garden = 1-as.numeric(...19),
         percent_dwellings_no_garden = 1-as.numeric(...25),
         median_home_garden_size = as.numeric(...14)) %>% 
  select(msoa11cd = `MSOA code`, percent_flats_no_garden, percent_dwellings_no_garden, median_home_garden_size) 

sf <- left_join(msoa, df, by = "msoa11cd")

# plot data ---------------------------

classes <- classIntervals(sf$percent_dwellings_no_garden, n = 5, style = "jenks")
sf <- mutate(sf, interval = factor(cut(percent_dwellings_no_garden, classes$brks, include.lowest = T), labels = c(
  paste0(round(classes$brks[1]*100,0), "-", percent(classes$brks[2])),
  paste0(round(classes$brks[2]*100,0), "-", percent(classes$brks[3])),
  paste0(round(classes$brks[3]*100,0), "-", percent(classes$brks[4])),
  paste0(round(classes$brks[4]*100,0), "-", percent(classes$brks[5])),
  paste0(round(classes$brks[5]*100,0), "-", percent(classes$brks[6]))
)))

ggplot(data = sf) +
  geom_sf(aes(fill = factor(interval)), alpha = 0.8, colour = "#bdbdbd", size = 0.2) +
  geom_sf(data = msoa, fill = NA, alpha = 1, colour = "#212121",  size = 0.4) +
  geom_label_repel(aes(x = lon, y = lat, label = str_wrap(msoa11hclnm, width = 15)), 
                   colour = "#000000", fill = "#FFFFFF", alpha = 0.75, size = 2.5, fontface = "bold",
                   min.segment.length = 0, segment.color = "#212121", segment.size = 0.1) +
  scale_fill_brewer(palette = "Greens", direction = -1,
                    guide = guide_legend(
                      reverse = TRUE,
                      keyheight = unit(6, units = "mm"), 
                      keywidth = unit(12, units = "mm"), 
                      nrow = 5)) +
  labs(x = NULL, y = NULL,
       title = "Percentage of households with no garden ",
       subtitle = "Trafford, April 2020", 
       caption = "Source: Office for National Statistics\nContains OS data © Crown copyright and database right (2020)",
       fill = NULL) +
  theme_void() +
  theme(plot.margin = unit(rep(0.5, 4), "cm"),
        plot.title.position = "plot",
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 16, margin = margin(t = 5)),
        plot.caption = element_text(size = 12, colour = "grey60", margin = margin(t = 20, b = -10)),
        legend.position = c(0.1, 0.8), 
        legend.text = element_text(size = 10))

# write data ---------------------------
write_csv(select(st_set_geometry(sf, NULL),-c(lon,lat,interval)), "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300, scale = 1)

