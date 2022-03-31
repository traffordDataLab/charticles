# Digital Exclusion #

# Source: GMCA
# URL: https://www.gmtableau.nhs.uk/t/GMCA/views/DigitalExclusionRiskIndexv1_5/DERIhomepage?%3Aiid=1&%3AisGuestRedirectFromVizportal=y&%3Aembed=y
# Licence: Open Government Licence 3.0

# load libraries ---------------------------
library(tidyverse) ; library(httr) ; library(sf) ; library(viridis) ; library(ggrepel)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data ---------------------------
raw <- read_csv("https://raw.githubusercontent.com/GreaterManchesterODA/Digital-Exclusion-Risk-Index/main/Version%201.5/LSOA%20calculations%20and%20scores%20(district%20level)_v1.5.csv")

lsoa <- st_read("https://ons-inspire.esriuk.com/arcgis/rest/services/Census_Boundaries/Lower_Super_Output_Areas_December_2011_Boundaries/MapServer/2/query?where=UPPER(lsoa11nm)%20like%20'%25TRAFFORD%25'&outFields=lsoa11cd,lsoa11nm&outSR=4326&f=geojson") 

lookup <- read_csv("https://www.trafforddatalab.io/spatial_data/lookups/administrative_lookup.csv") %>% 
  filter(lad17nm == "Trafford") %>% 
  pull(wd17cd)

wards <- st_read(paste0("https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/Wards_December_2018_Boundaries_V3/MapServer/2/query?where=", 
                        URLencode(paste0("wd18cd IN (", paste(shQuote(lookup), collapse = ", "), ")")), 
                        "&outFields=wd18cd,wd18nm,long,lat&outSR=4326&f=geojson")) %>% 
  select(area_code = wd18cd, area_name = wd18nm, long, lat)

# Mid-2019 population estimates
# Source: Nomis / ONS
# URL: https://www.nomisweb.co.uk/datasets/pestsyoala

lsoa_population <- read_csv(paste0("https://www.nomisweb.co.uk/api/v01/dataset/NM_2010_1.data.csv?geography=",paste(c(lsoa$lsoa11cd), collapse = ','),"&date=latest&gender=0&c_age=200&measures=20100"))


# tidy data ---------------------------
df <- raw %>%
  select(area_code = `LSOA code`, `DERI score` = `DERI score (England IMD)` )

lsoa_digital <- lsoa %>%
  select(area_code = lsoa11cd, area_name = lsoa11nm) %>%
  left_join(df)


DERI_score_pop <- lsoa_population %>% select(area_code = GEOGRAPHY_CODE, population = OBS_VALUE) %>%
  left_join(df, by = "area_code") %>%
  mutate(score_int = as.character(floor(`DERI score`))) %>%
  group_by(score_int) %>%
  summarise(pop = sum(population)) %>%
  mutate(value = round((pop/sum(pop))*100,1),
         indicator = "Population percent by DERI Score (floor function)",
         area_code = "E08000009",
         area_name = "Trafford",
         period = "2019",
         DERI_score = factor(score_int, levels = score_int)) %>%
  select(area_code, area_name, period, indicator, DERI_score, value)

# plot data ---------------------------

map <- ggplot(lsoa_digital) + 
  geom_sf(aes(fill = `DERI score`), color = "#FFFFFF", size = 0, alpha = 0.8) +
  geom_sf(data = wards, fill = "transparent", color = "#FFFFFF", lwd = 0.3) +
  geom_text_repel(data = wards, family="Myriad Pro",
                  aes(x = long, y = lat, label = str_wrap(area_name, width = 15), group = area_name),
                  color = "#212121", size  = 2.5, fontface = "bold",
                  box.padding = 0, point.padding = 0) +
  scale_fill_viridis(option = "D", discrete = F, 
                     label = function(x) paste0(x, ""),
                     direction = -1) +
  labs(x = NULL, y = NULL,
       title = "Digital Exclusion Risk Index",
       subtitle = "Trafford LSOAs, 2019",
       fill = NULL) +
  coord_sf(datum = NA) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.position = "none",
        legend.text = element_text(size = 10))

chart <- DERI_score_pop %>% 
  ggplot(aes(x = area_name, y = value, fill = DERI_score)) +
  geom_col(show.legend = FALSE, position = position_stack(reverse = T), width = 0.2) +
  geom_label(aes(label = paste0(value, "%")),
             position = position_stack(vjust = 0.5),
             colour = "#FFFFFF", fill = NA, label.size = NA, size = 2.5) +
  geom_label(aes(x = 0.85, label = DERI_score),
             position = position_stack(vjust = 0.5),
             colour = "black", fill = NA, label.size = NA, size = 3, fontface = "bold") +
  scale_fill_viridis(option = "D", discrete = T, 
                     label = function(x) paste0(x, ""),
                     direction = -1) +
  scale_y_continuous(expand = c(0, 1)) +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  labs(title = " ",
       subtitle = "Population %\nby Index",
       caption = "Higher index indicates higher risk of digital exclusion\nSource: GMCA/ONS\n Contains OS data © Crown copyright and database right 2022",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y =element_blank(),
  )

ggplot() + 
  theme_void() +
  annotation_custom(grob = ggplotGrob(map),
                    xmin = 0, xmax = 0.8,
                    ymin = 0.1, ymax = 1) +
  annotation_custom(grob = ggplotGrob(chart),
                    xmin = 0.8, xmax = 0.95,
                    ymin = 0, ymax = 1)


# write data ---------------------------
write_csv(DERI_score_pop, "data.csv")

ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300, scale = 1)


