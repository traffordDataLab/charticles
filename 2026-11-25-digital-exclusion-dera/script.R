# Digital Exclusion DERA index #

# Source: NHS England
# URL: https://www.england.nhs.uk/about/equality/equality-hub/national-healthcare-inequalities-improvement-programme/the-digital-exclusion-risk-atlas/
# Licence:

# load libraries ---------------------------
library(tidyverse) ; library(httr) ; library(sf) ;  library(ggrepel)
library(RColorBrewer)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data ---------------------------

#Export Data from https://experience.arcgis.com/experience/e990bec3d90f4a0dbb15e56dd58da5e7/page/Home for the local authority for all deciles and save it locally

raw <- read_csv("GI3904_DERA_Data_-1072918754886891936.csv")

lsoa <- st_read("https://www.trafforddatalab.io/spatial_data/lsoa/2021/trafford_lsoa_generalised.geojson")

wards <- st_read("https://www.trafforddatalab.io/spatial_data/ward/2023/trafford_ward_generalised.geojson")

# Mid-2024 population estimates
# Source: Nomis / ONS
# URL: https://www.nomisweb.co.uk/datasets/pestsyoala

lsoa_population <- read_csv(paste0("https://www.nomisweb.co.uk/api/v01/dataset/NM_2014_1.data.csv?geography=",paste(c(lsoa$area_code), collapse = ','),"&date=latest&gender=0&c_age=200&measures=20100"))

# tidy data ---------------------------
df <- raw %>%
  select(area_code = `LSOA21CD`, `DERA decile` = DERA_NTL_DECILE )

lsoa_digital <- lsoa %>%
  left_join(df) %>%
  mutate(`DERA score decile` = factor(`DERA decile`, levels = df %>% select(`DERA decile`) %>% unique() %>% pull()))


DERA_decile_pop <- lsoa_population %>% select(area_code = GEOGRAPHY_CODE, population = OBS_VALUE) %>%
  left_join(df, by = "area_code") %>%
  group_by(`DERA decile`) %>%
  summarise(pop = sum(population)) %>%
  mutate(value = round((pop/sum(pop))*100,1),
         indicator = "Population percent by DERA score decile",
         area_code = "E08000009",
         area_name = "Trafford",
         period = "2026",
         `DERA score decile` = factor(`DERA decile`, levels = `DERA decile`)) %>%
  select(area_code, area_name, period, indicator, `DERA score decile`, value)

ward_centers <- st_as_sf(wards %>% st_drop_geometry(), coords = c("lon", "lat"), crs = 4326)

# plot data ---------------------------

map <- 
  ggplot() + 
  geom_sf(data = lsoa_digital, aes(fill = `DERA score decile`), color = "#FFFFFF", size = 0, alpha = 0.9) +
  geom_sf(data = wards, fill = "transparent", color = "#8c8c8c", lwd = 0.8) + 
  geom_text_repel(data = ward_centers, 
                  aes(geometry = geometry, label = str_wrap(area_name, width = 15), group = area_name),
                  bg.color = "white", bg.r = 0.15 ,
                  color = "#212121", size  = 2.5, fontface = "bold",
                  box.padding = 0, point.padding = 0,
                  stat = "sf_coordinates" ) +
  scale_fill_brewer(palette = "YlGnBu",
                    label = function(x) paste0(x, ""),
                    direction = -1) +
  labs(x = NULL, y = NULL,
       title = "Digital Exclusion Risk Index from DERA",
       subtitle = "Trafford LSOAs, 2026",
       fill = NULL) +
  coord_sf(datum = NA) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.position = "none",
        legend.text = element_text(size = 10))

chart <- 
  DERA_decile_pop %>% 
  mutate(colour_var = if_else(as.integer(`DERA score decile`) <= 5, "dark", "light")) %>%
  ggplot(aes(x = area_name, y = value)) +
  geom_col(aes(fill = `DERA score decile`), show.legend = FALSE, position = position_stack(reverse = T), width = 0.2, alpha = 0.9) +
  geom_text(aes(label = paste0(value, "%"), colour = colour_var),
             position = position_stack(vjust = 0.5, reverse = T), size = 2.5) +
  geom_text(aes(x = 0.85, label = `DERA score decile`),
             position = position_stack(vjust = 0.5),
             colour = "black", size = 3, fontface = "bold") +
  scale_color_manual(values = c("#FFFFFF", "#585656"), guide="none") +
    
  scale_fill_brewer(palette = "YlGnBu",
                     label = function(x) paste0(x, ""),
                     direction = -1) +
  scale_y_continuous(expand = c(0, 1)) +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  labs(title = " ",
       subtitle = "Population %\nby decile",
       caption = "Lower deciles correspond to LSOAs most at risk of digital exclusion\nSource: DERA/NHS England\n Contains OS data © Crown copyright and database right 2026",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y =element_blank(),
        legend.position = "none"
  )

ggplot() + 
  theme_void() +
  annotation_custom(grob = ggplotGrob(map),
                    xmin = 0, xmax = 0.8,
                    ymin = 0.1, ymax = 1) +
  annotation_custom(grob = ggplotGrob(chart),
                    xmin = 0.8, xmax = 0.95,
                    ymin = 0, ymax = 1) +
  theme(panel.background = element_blank())


# write data ---------------------------
write_csv(DERA_decile_pop, "data.csv")

ggsave("plot.svg", dpi = 300, scale = 1, bg='#ffffff')
ggsave("plot.png", dpi = 300, scale = 1, bg='#ffffff')


