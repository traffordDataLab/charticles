# Generations #

# Source: Mid-year population estimates, ONS
# URL: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(httr) ; library(readxl) ; library(scales) ; library(ggtext)

lad <- "Trafford"

# load data ---------------------------
tmp <- tempfile(fileext = ".xls")
GET(url = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2019april2020localauthoritydistrictcodes/ukmidyearestimates20192020ladcodes.xls",
    write_disk(tmp))

df <- read_xls(tmp, sheet = 6, skip = 4) %>% 
  filter(Name == lad) %>% 
  select(-c(Geography1, `All ages`)) %>% 
  select(area_code = Code, area_name = Name, everything(), `90` = `90+`) %>%
  pivot_longer(-c(area_code, area_name), names_to = "age", values_to = "n") %>% 
  mutate(period = 2019,
         indicator = "Mid-year population estimates",
         unit = "Persons",
         age = as.integer(age),
         birth_year = 2019-age,
         # Generations based on Pew Research Center definitions
         # https://www.pewresearch.org/fact-tank/2019/01/17/where-millennials-end-and-generation-z-begins
         generation = case_when(
           birth_year >= 1928 & birth_year <= 1945 ~ "Silent Generation",
           birth_year >= 1946 & birth_year <= 1964 ~ "Baby Boomer",
           birth_year >= 1965 & birth_year <= 1980 ~ "Generation X",
           birth_year >= 1981 & birth_year <= 1996 ~ "Millenial",
           birth_year >= 1997 & birth_year <= 2012 ~ "Generation Z"),
         generation = factor(generation, 
                             levels = c("Silent Generation", "Baby Boomer", "Generation X", 
                                        "Millenial", "Generation Z")),
         percent = n/sum(n)
  ) %>% 
  select(area_code, area_name, period, indicator, unit, age, n, percent, birth_year, generation)

# plot data ---------------------------
ggplot(df, aes(x = age, y = percent, fill = generation)) + 
  geom_col(fill = "#ED5E90", colour = NA, alpha = 0.8, width = 0.9) +
  geom_vline(xintercept = 6.5, size = 0.5, colour = "#333333") +
  geom_vline(xintercept = 22.5, size = 0.5, colour = "#333333") +
  geom_vline(xintercept = 38.5, size = 0.5, colour = "#333333") +
  geom_vline(xintercept = 54.5, size = 0.5, colour = "#333333") +
  geom_vline(xintercept = 73.5, size = 0.5, colour = "#333333") +
  geom_hline(yintercept = 0, size = 1.2, colour = "#333333") +
  geom_richtext(aes(x = 7, y = Inf, label = "Generation Z<br><span style = 'color:#757575;font-size:10pt;'>1997-2012</span>"), fill = NA, label.color = NA, size = 5, vjust = 1, hjust = 0) +
  geom_richtext(aes(x = 23, y = Inf, label = "Millenial<br><span style = 'color:#757575;font-size:10pt;'>1981-1996</span>"), fill = NA, label.color = NA, size = 5, vjust = 1, hjust = 0) +
  geom_richtext(aes(x = 39, y = Inf, label = "Generation X<br><span style = 'color:#757575;font-size:10pt;'>1965-1980</span>"), fill = NA, label.color = NA, size = 5, vjust = 1, hjust = 0) +
  geom_richtext(aes(x = 55, y = Inf, label = "Baby Boomer<br><span style = 'color:#757575;font-size:10pt;'>1946-1964</span>"), fill = NA, label.color = NA, size = 5, vjust = 1, hjust = 0) +
  geom_richtext(aes(x = 74, y = Inf, label = "Silent Generation<br><span style = 'color:#757575;font-size:10pt;'>1928-1945</span>"), fill = NA, label.color = NA, size = 5, vjust = 1, hjust = 0) +
  scale_x_continuous(expand = c(0.005, 0.005), breaks = c(0,7,23,39,55,74,90)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(df$percent)*1.1), breaks = pretty_breaks(), labels = percent_format(0.1), position = "right") +
  labs(x = "Age in years", y = NULL, 
       title = paste("The different generations of", lad),
       subtitle = paste0("<span style = 'color:#757575;'>Estimated percentage of residents, mid-2019</span>"),
       caption = "Source: Office for National Statistics | @traffordDataLab",
       tag = "Those aged 90 or more\nare grouped together") +
  theme_minimal() +
  theme(plot.margin = unit(rep(0.5, 4), "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_markdown(size = 14, margin = margin(b = 20)),
        plot.caption = element_text(size = 12, colour = "grey60", margin = margin(t = 20, b = -10)),
        plot.tag.position = c(0.8, 0.04),
        plot.tag = element_text(size = 10, colour = "#757575", hjust = 0),
        axis.title.x = element_text(size = 12, hjust = 0, margin = margin(t = 10)),
        axis.text = element_text(size = 12))

# write data ---------------------------
write_csv(df, "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300, scale = 1)
