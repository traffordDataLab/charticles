# Children in Poverty #

# Source: End Child Poverty
# URL: http://www.endchildpoverty.org.uk/child-poverty-in-your-area-201415-201819/

library(tidyverse) ; library(httr) ; library(readxl) ; library(scales) ; library(ggtext)

# load data ---------------------------
tmp <- tempfile(fileext = ".xlsx")
GET(url = "http://www.endchildpoverty.org.uk/wp-content/uploads/2020/10/local-child-poverty-estimates-ahc-october-2020-1.xlsx",
    write_disk(tmp))
df <- read_excel(tmp, sheet = 3, skip = 1) %>% 
  select(area_code = ...3, area_name = ...2, `2014/15` = `2014/15...9`, `2018/19` = `2018/19...13`) %>% 
  filter(area_name %in% c("Bolton", "Bury", "Manchester", "Oldham", "Rochdale", "Salford", "Stockport", "Tameside", "Trafford", "Wigan")) %>% 
  pivot_longer(!area_code:area_name, names_to = "period", values_to = "value") %>% 
  mutate(indicator = "Children living in poverty, after housing costs",
         measure = "Percentage",
         unit = "Children") %>% 
  select(area_code, area_name, indicator, period, measure, unit, value) 

# plot data ---------------------------
ggplot() +
  geom_segment(
    data = select(df, area_name, value, period) %>% 
      group_by(area_name) %>% 
      summarise(start = range(value)[1], end = range(value)[2]) %>% 
      ungroup(),
    aes(x = start, xend = end, y = area_name, yend = area_name), 
    color = "#e3e2e1", size = 2) +
  geom_point(
    data = select(df, area_name, period, value),
    aes(x = value, y = area_name, fill = factor(period)),
    pch = 21, colour = "#212121", size = 3.5, alpha = 0.8) +
  scale_x_continuous(expand = c(0.005, 0.005), labels = label_percent(accuracy = 1)) +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = c("#e3e2e1", "#d1e231"),
                     labels = c("2014/15", "2018/19")) +
  labs(x = NULL, y = NULL, 
       title = "Children living in poverty, after housing costs", 
       subtitle = paste0("<span style = 'color:#757575;'>Greater Manchester, 2014/15 and 2018/19</span>"),
       caption = "Source: End Child Poverty | @traffordDataLab", 
       fill = NULL) +
  theme_minimal() +
  theme(plot.margin = unit(c(0,1,0,1), "cm"),
        panel.grid.major.x = element_line(size = 0.5, color = "#cbcbcb"),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(face = "bold", hjust = 0),
        plot.title.position = "plot",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_markdown(size = 12, margin = margin(b = 20)),
        plot.caption = element_text(colour = "grey60", margin = margin(t = 20, b = -10)),
        legend.position = c(0.85, 1.1),
        legend.direction = "horizontal",
        legend.text = element_text(size = 10),
        aspect.ratio = 0.3) +
  expand_limits(x = c(0.2, 0.5))

# write data ---------------------------
write_csv(df, "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1, width = 9, height = 5) # adjustment for to improve charticle thumbnail
ggsave("plot.png", dpi = 300, scale = 1)
