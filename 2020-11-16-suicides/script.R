# Suicides #

# Source: Office for National Statistics
# URL: https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/suicidesbylocalauthority
# Licence: OGL 3.0

# load libraries ---------------------------
library(tidyverse) ; library(httr) ; library(readxl) ; library(ggtext)

# load data ---------------------------
tmp <- tempfile(fileext = ".xls")
GET(url = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fsuicidesbylocalauthority%2fcurrent/2019localauthorityfinal.xls",
    write_disk(tmp))

# tidy data ---------------------------
cnames <- read_xls(tmp, sheet = 5, skip = 3, n_max = 0) %>%
  select(area_name = ...4, starts_with("20")) %>% 
  names()

rate <- read_xls(tmp, sheet = 5, skip = 4) %>% 
  filter(...4 == "Trafford") %>% 
  select(area_name = ...4, starts_with("Rate")) %>% 
  set_names(cnames) %>% 
  pivot_longer(-area_name, names_to = "period", values_to = "rate")

lcl <- read_xls(tmp, sheet = 5, skip = 4) %>% 
  filter(...4 == "Trafford") %>% 
  select(area_name = ...4, starts_with("LCL")) %>% 
  set_names(cnames) %>% 
  pivot_longer(-area_name, names_to = "period", values_to = "lcl") %>% 
  select(-area_name)

ucl <- read_xls(tmp, sheet = 5, skip = 4) %>% 
  filter(...4 == "Trafford") %>% 
  select(area_name = ...4, starts_with("UCL")) %>% 
  set_names(cnames) %>% 
  pivot_longer(-area_name, names_to = "period", values_to = "ucl") %>% 
  select(-area_name)

lad <- left_join(rate, lcl, by = "period") %>% 
  left_join(ucl)
  
benchmark <- read_xls(tmp, sheet = 5, skip = 4) %>% 
  filter(...2 %in% c("NORTH WEST", "ENGLAND")) %>% 
  select(area_name = ...2, starts_with("Rate")) %>% 
  set_names(cnames) %>% 
  mutate(area_name = str_to_title(area_name)) %>% 
  pivot_longer(-area_name, names_to = "period", values_to = "rate")

df <- bind_rows(lad, benchmark) %>% 
  mutate(area_name = factor(area_name, levels = c("Trafford", "North West", "England"), ordered = TRUE), 
         rate = as.numeric(rate),
         lcl = as.numeric(lcl),
         ucl = as.numeric(ucl))

# summary statistics  ---------------------------
gm <- read_xls(tmp, sheet = 5, skip = 4) %>% 
  filter(...4 %in% c("Bolton","Bury","Manchester","Oldham","Rochdale","Salford","Stockport","Tameside","Trafford","Wigan")) %>% 
  select(area_name = ...4, , rate = Rate...6) %>% 
  mutate(rate = as.numeric(rate)) %>% 
  arrange(desc(rate))

# plot data ---------------------------
ggplot(df, aes(x = period, y = rate, colour = area_name, group = area_name)) +
  geom_hline(aes(yintercept = 0), colour = "#212121", linetype = "solid") +
  geom_line(size = 1) +
  geom_errorbar(data = filter(df, area_name == "Trafford"), 
                 aes(ymin = lcl, ymax = ucl), width = 0.2,  show.legend = FALSE) +
  scale_colour_manual(values = c("Trafford" = "#B5D63D", "North West" = "#a6cee3", "England" = "#1f78b4")) +
  scale_x_discrete(expand = c(0.005, 0.005)) +
  scale_y_continuous(expand = c(0.005, 0.005), limits = c(0, 15), position = "right") +
  labs(
    title = "Suicide registrations",
    subtitle = paste0("<span style = 'color:#757575;'>Trafford, age-standardised suicide rate per 100,000</span>"),
    caption = "Source: ONS",
    x = NULL,
    y = NULL, 
    colour = NULL
  ) +
  theme_minimal() +
  theme(plot.margin = unit(rep(0.5, 4), "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_markdown(size = 12, margin = margin(b = 20)),
        plot.caption = element_text(colour = "grey60", margin = margin(t = 20, b = -10)),
        legend.position = c(0.8, 1.05),
        legend.direction = "horizontal",
        legend.text = element_text(size = 8),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.title.y.right = element_text(size = 12, angle = 0, vjust = 1, margin = margin(l = 5)),
        axis.ticks.x = element_line(colour = "#333333", size = 0.5))

# write data ---------------------------
write_csv(df, "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300, scale = 1)

