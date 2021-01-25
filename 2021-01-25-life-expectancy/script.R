# Life expectancy at MSOA for 2015 to 2019 #

# Source: Public Health England
# URL: https://fingertips.phe.org.uk/profile/local-health
# Licence: OGL 3.0

# load libraries ---------------------------
library(tidyverse) ; library(fingertipsR) ; library(ggtext)

# load and transform data ---------------------------
id <- "Trafford"

df <- fingertips_data(IndicatorID = 93283, AreaTypeID = 3) %>% 
  filter(ParentName == id | AreaName == "England",
         Timeperiod == "2015 - 19") %>% 
  select(AreaName, Sex, Value) %>% 
  pivot_wider(names_from = Sex, values_from = Value) %>% 
  mutate(GenderGap = Female-Male) %>% 
  pivot_longer(-c(AreaName, GenderGap), names_to = "Sex", values_to = "Value")
  
# plot data ---------------------------
ggplot(data = filter(df, AreaName != "England"),
       aes(x = fct_reorder2(AreaName, Value, GenderGap, .desc = TRUE), y = Value)) +
  geom_hline(yintercept = pull(filter(df, AreaName == "England", Sex == "Female"),Value), 
             colour = "#77CAE0", linetype = "dotted") +
  geom_hline(yintercept = pull(filter(df, AreaName == "England", Sex == "Male"),Value), 
             colour = "#03658F", linetype = "dotted") +
  geom_line(color = "#bdbdbd", size = 0.5) +
  geom_point(aes(fill = Sex), size = 4, shape = 21, colour = "#FFFFFF", show.legend = FALSE) +
  geom_richtext(aes(x = 22, y = 79.4, label = "<span style = 'color:#03658F;'>England</span>"), 
                size = 2.5, angle = 90, color = "#212121", lineheight = .9,
                fill = NA, label.color = NA, stat = "unique") +
  geom_richtext(aes(x = 25, y = 83, label = "<span style = 'color:#77CAE0;'>England</span>"), 
                size = 2.5, angle = 90, color = "#212121", lineheight = .9,
                fill = NA, label.color = NA, stat = "unique") +
  geom_richtext(aes(x = 19, y = 76, label = "<span style = 'color:#77CAE0;'>Women</span> in Partington\nhave a lower<br>average life expectancy than<br>the England average for <span style = 'color:#03658F;'>males</span>"), 
                size = 3, color = "#212121", lineheight = .9,
                fill = NA, label.color = NA, stat = "unique") +
  geom_curve(aes(x = 20, y = 75.5, xend = 23, yend = 77.6),
             colour = "#212121", curvature = -0.3, size = 0.3,
             arrow = arrow(length = unit(0.01, "npc"))) +
  scale_colour_manual(values = c("Female" = "#77CAE0", "Male" = "#03658F")) +
  scale_fill_manual(values = c("Female" = "#77CAE0", "Male" = "#03658F")) +
  scale_y_continuous(limits = c(74,88), breaks = c(seq(74,88,2))) +
  labs(title = "Differences in life expectancy at birth between <span style = 'color:#03658F;'>males</span> and <span style = 'color:#77CAE0;'>females</span>",
       subtitle = "<span style = 'color:#757575;'>Average years sorted by gender gap, MSOAs in Trafford, 2015-19</span>",
       caption = "Source: Public Health England | @traffordDataLab",
       x = NULL, y = NULL) +
  coord_flip(clip = "off") +
  theme_minimal() +
  theme(plot.margin = unit(rep(0.5, 4), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        plot.title = element_markdown(size = 14, face = "bold"),
        plot.subtitle = element_markdown(size = 12, margin = margin(b = 20)),
        plot.caption = element_text(colour = "grey60", margin = margin(t = 20, b = -10)),
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.text.y = element_text(size = 11, hjust = 0)) 

# write data ---------------------------
write_csv(select(df, -GenderGap), "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300)

