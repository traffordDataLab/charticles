# Internet engagement #

# load libraries -------------------------
library(tidyverse) ; library(sf) ; library(ggsci) ; library(ggtext) ; library(shadowtext)

# read data -------------------------

# 2018 Internet User Classification
# Source: Consumer Data Research Centre
# URL: https://data.cdrc.ac.uk/dataset/internet-user-classification
# Licence: Open Government Licence
df <- read_csv("data/iuc2018.csv") %>% 
  select(area_code = LSOA11_CD, group = GRP_LABEL) %>% 
  mutate(group = factor(group,
                        levels = c(
                          "e-Cultural Creators",
                          "e-Professionals",
                          "e-Veterans",
                          "Youthful Urban Fringe",
                          "e-Rational Utilitarians",
                          "e-Mainstream",
                          "Passive and Uncommitted Users",
                          "Digital Seniors",
                          "Settled Offline Communities",
                          "e-Withdrawn"), ordered = TRUE))

# LSOAs in Trafford
# Source: ONS Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/lower-layer-super-output-areas-december-2011-generalised-clipped-boundaries-in-england-and-wales
# Licence: OGL v3.0
lsoa <- st_read("https://ons-inspire.esriuk.com/arcgis/rest/services/Census_Boundaries/Lower_Super_Output_Areas_December_2011_Boundaries/MapServer/2/query?where=UPPER(lsoa11nm)%20like%20'%25TRAFFORD%25'&outFields=lsoa11cd,lsoa11nm&outSR=4326&f=geojson") %>% 
  select(area_code = lsoa11cd, area_name = lsoa11nm)

# Wards in Trafford
# Source: ONS Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/wards-december-2018-generalised-clipped-boundaries-uk
# Licence: OGL v3.0
wards <- st_read("https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/Wards_December_2018_Boundaries_V3/MapServer/2/query?where=wd18cd%20IN%20('E05000819',%20'E05000820',%20'E05000821',%20'E05000822',%20'E05000823',%20'E05000824',%20'E05000825',%20'E05000826',%20'E05000827',%20'E05000828',%20'E05000829',%20'E05000830',%20'E05000831',%20'E05000832',%20'E05000833',%20'E05000834',%20'E05000835',%20'E05000836',%20'E05000837',%20'E05000838',%20'E05000839')&outFields=*&outSR=4326&f=geojson") %>% 
  select(area_code = wd18cd, area_name = wd18nm, long, lat)

# transform data -------------------------
sf <- left_join(lsoa, df, by = "area_code") 

# plot data -------------------------
map <- ggplot() + 
  geom_sf(data = sf, aes(fill = group), colour = "transparent", show.legend = FALSE) +
  geom_sf(data = wards, fill = "transparent", color = "#000000", lwd = 0.1) +
  geom_shadowtext(data = wards, aes(x = long, y = lat, label = str_wrap(area_name, width = 15)), colour = "#FFFFFF", family = "Open Sans", fontface = "bold", size = 2, bg.colour = "#212121", nudge_y = 0.002) +
  scale_fill_locuszoom() +
  labs(title = "Neigbourhood differences in Internet engagement",
       subtitle = paste0("<span style = 'color:#757575;'>Number of LSOAs, Trafford, 2018</span>"),
       caption = "Contains Ordnance Survey data © Crown copyright and database right 2021\nSource: 2018 Internet User Classification") +
  coord_sf(datum = NA) +
  theme_void() +
  theme(plot.margin = unit(rep(0.5, 4), "cm"),
        plot.title.position = "plot",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_markdown(size = 12, margin = margin(b = 20)),
        plot.caption = element_text(colour = "grey60", margin = margin(t = 20, b = -10)))
  
chart <- count(sf, group) %>% 
  ggplot(aes(x = fct_rev(group), y = n, fill = group)) +
  geom_col(show.legend = FALSE) +
  geom_label(aes(label = n),
             hjust = 1, vjust = 0.5, colour = "#FFFFFF", fill = NA, label.size = NA, size = 2.5) +
  scale_fill_locuszoom() +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(colour = "#bdbdbd"),
        axis.text.x = element_blank())

ggplot() + 
  theme_void() +
  annotation_custom(grob = ggplotGrob(map),
                    xmin = -0.2, xmax = 0.8,
                    ymin = 0, ymax = 1) +
  annotation_custom(grob = ggplotGrob(chart),
                    xmin = 0.55, xmax = 1,
                    ymin = 0.2, ymax = 0.5)


# write data -------------------------
write_csv(st_set_geometry(sf, NULL), "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300)
