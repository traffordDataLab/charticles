# Employee Earnings

# Source: Annual Survey of Hours and Earnings (ASHE)
# URL: https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/placeofresidencebylocalauthorityashetable8
# URL: https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/placeofworkbylocalauthorityashetable7

# Licence: Open Government Licence v3.0

# load libraries ---------------------------
library(tidyverse)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data ---------------------------
raw1 <- read_csv("https://www.nomisweb.co.uk/api/v01/dataset/NM_30_1.data.csv?geography=1778385132&date=latest&sex=1...9&item=2,4&pay=1,5,7&measures=20100,20701") 

raw2 <- read_csv("https://www.nomisweb.co.uk/api/v01/dataset/NM_99_1.data.csv?geography=1778385132&date=latest&sex=1...9&item=2,4&pay=1,5,7&measures=20100,20701")

# tidy data ---------------------------
df <- raw1 %>%
  mutate(category = "Living in Trafford") %>%
  bind_rows(raw2 %>%
              mutate(category = "Working in Trafford")) %>%
  select(date = DATE, area_name = GEOGRAPHY_NAME, category, sex = SEX_NAME, pay = PAY_NAME, item = ITEM_NAME, measures = MEASURES_NAME, value = OBS_VALUE) %>%
  mutate(sex = gsub(" Workers", "", sex)) %>%
  mutate(sex = factor(sex, levels = c("Male Part Time", "Female Part Time", "Male Full Time", "Female Full Time", "Male",  "Female", "Part Time", "Full Time", "Total")),
         category = factor(category, levels = c( "Working in Trafford", "Living in Trafford")))

df_plot <- df %>%
  pivot_wider(names_from = "measures", values_from = "value") %>%
  mutate(upper = Value + ((Confidence/100) * Value),
         lower = Value - ((Confidence/100) * Value))
  

# plot data ---------------------------

ggplot(df_plot %>% filter(item == "Median", pay == "Hourly pay - gross")) + #, measures == "Value"
  #geom_segment(mapping = aes(x = 0, xend = value, y = sex, yend = sex)) +
  geom_line(mapping = aes(Value,sex), colour = "lightgrey", lwd = 4) +
  geom_linerange(aes(y = sex, xmin=lower, xmax=upper)) +
 
  geom_point(mapping = aes(Value, sex, group = category, colour = category), size = 4.5) +
 
  scale_x_continuous(limits = c(0,NA)) +
  scale_color_manual(values = c("#E2642E", "#510022")) +
  labs(title = "Midpoint of hourly pays for employees",
       subtitle = "Trafford, 2024",
       caption = "Black lines represent confidence intervals\nSource: Annual Survey of Hours and Earnings, ONS | @traffordDataLab",
       x = "pounds (£)",
       y = NULL) +
  theme_lab() +
  theme(
    panel.grid.major.y = element_blank(),
    legend.title=element_blank(),
    axis.text.y = element_text(hjust = 1),
    legend.text = element_text(size = 12),
    axis.title.x = element_text(size = 12)
  )

# write data ---------------------------
write_csv(df, "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300, scale = 1)
