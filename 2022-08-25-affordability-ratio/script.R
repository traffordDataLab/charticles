# Median affordability ratio #

# Source: ONS
# URL: https://www.ons.gov.uk/peoplepopulationandcommunity/housing/datasets/ratioofhousepricetoworkplacebasedearningslowerquartileandmedian
# Licence: Open Government Licence v3.0

# load libraries ---------------------------

library(tidyverse) ; library(httr) ; library(readxl)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data ---------------------------
tmp <- tempfile(fileext = ".xlsx")
GET(url = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhousing%2fdatasets%2fratioofhousepricetoworkplacebasedearningslowerquartileandmedian%2fcurrent/ratioofhousepricetoworkplacebasedearnings.xlsx",
    write_disk(tmp))

cipfa <- data.frame (area_code  = c("E06000030", "E06000007", "E06000042", "E08000029", "E06000025", "E08000007", "E06000014", "E06000055", "E06000050", "E06000034", "E08000002", "E06000015", "E06000031", "E06000038", "E06000035"),
                     area_name = c("Swindon", "Warrington", "Milton Keynes", "Solihull", "South Gloucestershire", "Stockport", "York", "Bedford", "Cheshire West and Chester", "Thurrock", "Bury", "Derby", "Peterborough", "Reading", "Medway"))

raw <- read_xlsx(tmp, sheet = 18, skip = 1) 

england <- read_xlsx(tmp, sheet = 6, skip = 1) %>%
  filter(Code == "E92000001") %>% 
  select(area_code = Code, 
         area_name = Name, 
         5:27) %>% 
  gather(period, value, -c(area_code, area_name)) %>% 
  filter(period >= 2010) 

# tidy data ---------------------------
df <- raw %>% 
  filter(Code %in% c(cipfa$area_code, "E08000009")) %>% 
  select(area_code = Code, 
         area_name = Name, 
         5:29) %>% 
  gather(period, value, -c(area_code, area_name)) %>% 
  filter(period >= 2010) %>% 
  mutate(value = as.double(value)) %>%
  bind_rows(england) %>%
  mutate(period = as_factor(period))

mean_cipfa <- df %>%
  filter(area_code %in% c(cipfa$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities mean",
         period = as_factor(period)) %>%
  filter(!is.na(value))

trend <- bind_rows(df %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), mean_cipfa)

data <- df %>%
  mutate(indicator = "Affordability ratio", measure = "ratio", unit = "House price / earnings") %>%
  select(area_code, area_name, indicator, period, measure, unit, value)
  

# plot data ---------------------------

ggplot(trend,
       aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
  geom_line(size = 1) +
  geom_point(shape = 21, size = 2.5, colour = "white") +
  geom_hline(yintercept = 0, size = 1, colour = "#212121") +
  scale_colour_manual(values = c("Trafford" = "#00445e", 
                                 "Similar Authorities mean" = "#00AFBB",                                  "England" = "#ffcb00"),
                      labels=c("Trafford", "Similar Local Authorities*", "England")
  ) +
  scale_fill_manual(values = c("Trafford" = "#00445e", 
                               "Similar Authorities mean" = "#00AFBB", 
                               "England" = "#ffcb00"),
                    labels=c("Trafford", "Similar Local Authorities*", "England")) +
  scale_y_continuous(limits = c(0, NA), breaks = seq(0, 10, by = 2)) +
  scale_x_discrete(expand = c(0.05, 0.05)) +
  labs(
    title = "Ratio of median house prices to median earnings",
    subtitle = NULL,
    caption = "*Mean of the 15 Trafford's CIPFA nearest neighbours\nSource: ONS | @trafforddatalab",
    x = NULL,
    y = "Ratio",
    colour = NULL
  ) +
  theme_lab() +
  theme(
        axis.text.x = element_text(angle = 90, margin = margin(r = 0)),
        legend.position = "top",
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, color = "#cbcbcb"), 
        panel.grid.minor = element_blank()
  ) +
  guides(fill = "none")

# write data ---------------------------
write_csv(data, "data1.csv")
ggsave("plot1.svg", dpi = 300, scale = 1)
ggsave("plot1.png", dpi = 300, scale = 1)

