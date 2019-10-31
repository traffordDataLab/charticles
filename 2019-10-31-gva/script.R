# Source: Office for National Statistics
# URL: https://www.ons.gov.uk/economy/grossvalueaddedgva/bulletins/regionalgrossvalueaddedbalanceduk/1998to2017
# Licence: Open Government Licence

library(tidyverse) ; library(httr) ; library(readxl) ; library(packcircles) ; library(scales)
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# read data ---------------------------
tmp <- tempfile(fileext = ".xlsx")
GET(url = "https://www.ons.gov.uk/file?uri=%2feconomy%2fgrossvalueaddedgva%2fdatasets%2fregionalgrossvalueaddedbalancedlocalauthoritiesbynuts1region%2fukdnorthwest/regionalgvabbylaukdnorthwest.xlsx",
    write_disk(tmp))

df <- read_xlsx(tmp, sheet = 4, skip = 1) %>% 
  filter(`LAD code` %in% c("E08000001", "E08000002", "E08000003", "E08000004", "E08000005", "E08000006", "E08000007", "E08000008", "E08000009", "E08000010"),
         SIC07 == "Total") %>% 
  select(area_code = `LAD code`, area_name = `LA name`, value = `20173`) %>% 
  mutate(period = "2017",
         indicator = "Gross Value Added",
         measure = "Sterling",
         unit = "Billions") %>% 
  select(area_code, area_name, indicator, period, measure, unit, value)

# plot data ---------------------------
layout <- circleProgressiveLayout(df, sizecol = "value")
vertices <- circleLayoutVertices(layout, npoints = 100)

ggplot(data = vertices) + 
  geom_polygon(aes(x, y, group = id, fill = factor(id)), color = "transparent", show.legend = FALSE) +
  geom_text(data = layout, aes(x, y), label = paste0(df$area_name, "\n£", round(df$value/1000,1), "b"), 
            check_overlap = TRUE, color = "#FFFFFF") + 
  scale_fill_manual(values = c("#5F4690","#1D6996","#38A6A5","#0F8554","#73AF48","#EDAD08","#E17C05","#CC503E","#94346E","#6F4070")) + 
  labs(title = "Gross Value Added",
       subtitle = "Greater Manchester, 2017",
       caption = "Source: ONS | @traffordDataLab") +
  coord_equal() +
  theme_lab() +
  theme(panel.grid.major.y = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank())

# write data ---------------------------
write_csv(df, "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300, scale = 1)

