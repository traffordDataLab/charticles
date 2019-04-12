## Life expectancy at birth vs deprivation ##

# Source: Public Health England and MHCLG
# URL: http://www.localhealth.org.uk/ ; https://fingertips.phe.org.uk/
# Licence: Open Government Licence 3.0

# load libraries ---------------------------
library(tidyverse) ; library(fingertipsR) ; library(ggpmisc)

# load data ---------------------------
lifeExp <- read_csv("LocalHealth_All_indicators_Ward_data.csv") %>%
  filter(`Indicator Name` == "Life expectancy at birth, (upper age band 90+)") %>%
  select(area_code = `Area Code`,
         area_name = `Area Name`,
         sex = Sex,
         age = Value) %>% 
  mutate(age = round(age, 1))

imd <- fingertips_data(IndicatorID = 91872, AreaTypeID = 8) %>% 
  filter(ParentName == "Trafford") %>% 
  select(area_code = AreaCode,
         score = Value)

# tidy data ---------------------------
df <- left_join(lifeExp, imd, by = "area_code")

# query data ---------------------------
df %>% 
  group_by(sex)  %>%
  slice(which.min(age),
        which.max(age))

# plot data ---------------------------
ggplot(data = df, aes(x = score, y = age, colour = sex)) +
  geom_point(shape = 21, size = 2.5, fill = "#ffffff") +
  geom_smooth(method = "lm", se = F, formula = y ~ x, size = 0.8) +
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), formula = y ~ x, parse = TRUE, label.x.npc = "right", label.y = 89.2) +
  geom_label(data = subset(df, area_name == "Hale Barns"),
             aes(score, age, label = area_name), 
             fill = "#f0f0f0", size = 2.3, nudge_y = 0.5) +
  geom_label(data = subset(df, area_name == "Bucklow-St Martins"),
            aes(score, age, label = area_name), 
            fill = "#f0f0f0", size = 2.3, nudge_y = -0.5) +
  scale_colour_manual(values = c("#3C5D9F", "#8B0000")) +
  facet_wrap(~sex, strip.position = "top") +
  scale_x_continuous(limits = c(0, 42)) +
  scale_y_continuous(limits = c(70,90), breaks = seq(70,90, by = 5)) +
  labs(x = "Deprivation score", y = "Life expectancy (years)",
       title = "Life expectancy at birth vs deprivation in Trafford's wards, 2011-2015",
       caption = "Source: Public Health England and MHCLG | @traffordDataLab") +
  theme_minimal(base_family = "Open Sans") +
  theme(plot.margin = unit(c(1.5,1.5,1,1.5), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.grid.major = element_line(linetype = "dotted", colour = "#757575", size = 0.2),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, colour = "#757575", face = "bold", hjust = 0.5, vjust = 8),
        strip.text = element_text(size = 12, colour = "#757575", face = "plain", vjust = 2.5),
        axis.title = element_text(size = 11, colour = "#757575", face = "plain", hjust = 1),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        plot.caption = element_text(size = 10, colour = "#212121", hjust = 1, margin = margin(t = 15)),
        legend.position = "none") 

# write data ---------------------------
write_csv(df, "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png",  dpi = 300, scale = 1)
