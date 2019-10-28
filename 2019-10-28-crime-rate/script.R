# Crime rate #

# Source: Office for National Statistics
# URL: https://www.ons.gov.uk/peoplepopulationandcommunity/crimeandjustice/datasets/recordedcrimedatabycommunitysafetypartnershiparea
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(httr) ; library(readxl) ; library(janitor)
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data ---------------------------
tmp <- tempfile(fileext = ".xls")
GET(url = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fcrimeandjustice%2fdatasets%2frecordedcrimedatabycommunitysafetypartnershiparea%2fcurrent/communitysafetypartnershipareatablesyejune19.xls",
    write_disk(tmp))

df <- read_xls(tmp, sheet = 6, skip = 4) %>% 
  clean_names() %>% 
  filter(police_force_area_name == "Greater Manchester" & 
           community_safety_partnership_name %in% c(NA, "Trafford")) %>% 
  mutate(area_name = case_when(
    is.na(community_safety_partnership_name) ~ "Greater Manchester",
    TRUE ~ community_safety_partnership_name)) %>% 
  select(area_name,
         `Violence with injury` = violence_with_injury, 
         `Violence without injury`= violence_without_injury,
         `Sexual offences` = sexual_offences, 
         `Robbery` = robbery,
         `Domestic burglary` = residential_burglary_per_1_000_household,
         `Bicycle theft` = bicycle_theft, 
         `Vehicle offences` = vehicle_offences,
         `Criminal damage and arson` = criminal_damage_and_arson, 
         `Public order offences` = public_order_offences) %>% 
  gather(group, value, -area_name) %>% 
  mutate(indicator = "Police recorded crime",
         period = "Year ending 2019-06",
         measure = "Rate",
         unit = "Crimes",
         value = as.numeric(value)) %>% 
  select(area_name, indicator, period, measure, unit, value, group)

# plot data ---------------------------
ggplot(df, aes(area_name, value)) +
  geom_col(aes(fill = area_name)) +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  scale_y_continuous(expand = c(0.005, 0.005), limit = c(0,20)) +
  scale_fill_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800"),
                    guide = guide_legend(reverse = TRUE)) +
  scale_x_discrete(limits = rev(levels(factor(df$area_name)))) +
  facet_wrap(~group, nrow = 1, strip.position = "bottom", 
             labeller = label_wrap_gen(width = 15)) +
  labs(x = NULL, y = NULL, fill = NULL,
       title = "Police recorded crime rate",
       subtitle = "Year ending June 2019",
       caption = "Source: Office for National Statistics | @traffordDataLab") +
  theme_lab() +
  theme(axis.text.x = element_blank(),
        strip.text = element_text(face = "italic", hjust = 0.5, vjust = 0.9)) 

# write data ---------------------------
write_csv(df, "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300, scale = 1)

