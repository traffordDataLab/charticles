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

raw_ratio <- read_xlsx(tmp, sheet = 18, skip = 1) %>% mutate(indicator = "Affordability ratio", measure = "ratio", units = "House price / earnings") %>% mutate(across(everything(), as.character))

raw_earnings <- read_xlsx(tmp, sheet = 17, skip = 1) %>% mutate(indicator = "Median gross annual workplace-based earnings", measure = "median", units = "pounds") %>% mutate(across(everything(), as.character))

raw_price <- read_xlsx(tmp, sheet = 16, skip = 1) %>% mutate(indicator = "Median house price", measure = "median", units = "pounds") %>% mutate(across(everything(), as.character)) %>%
  rename(Code = `Local authority code`, Name = `Local authority name`)

names(raw_price) <- gsub(pattern = "Year ending Sep ", replacement = "", x = names(raw_price))

# tidy data ---------------------------

df <- bind_rows(raw_price, raw_ratio, raw_earnings) %>%
  filter(Name == "Trafford") %>%
  select(area_code = Code, area_name = Name, indicator, measure, units, 5:29) %>% 
  gather(period, value, -c(area_code, area_name, indicator, measure, units)) %>% 
  filter(period >= 2010) %>% 
  mutate(value = round(as.double(value),1)) %>%
  mutate(period = as_factor(period)) %>%
  group_by(indicator, area_name, area_code, measure, units) %>%
  mutate(index = round(100 * value / value[1], 1),
          change_from_2010 = round(index-100,1)) %>%
  select(area_code, area_name, indicator, period, measure, units, value, change_from_2010)

# plot data ---------------------------

ggplot(df,
       aes(x = period, y = change_from_2010, colour = indicator, fill = indicator, group = indicator)) +
  geom_hline(yintercept = 0, size = 1, colour = "#212121") +
  geom_line(size = 1.5) +
  #geom_point(shape = 21, size = 2.5, colour = "white") +
  scale_colour_manual(values = c("Median house price" = "#ca77d0","Affordability ratio" = "#00445e", 
                                 "Median gross annual workplace-based earnings" = "#157642")) +
  scale_fill_manual(values = c("Median house price" = "#ca77d0","Affordability ratio" = "#00445e", 
                               "Median gross annual workplace-based earnings" = "#157642")) +
  scale_y_continuous(limits = c(-20, NA), breaks = seq(-20, 100, by = 20), labels = function(x) paste0(x, "%")) +
  scale_x_discrete(expand = c(0.05, 0.05)) +
  labs(
    title = "Change in house price, earnings and affordability since 2010",
    subtitle = "Trafford 2010 - 2021",
    caption = "ONS | @trafforddatalab",
    x = NULL,
    y = "Percent",
    colour = NULL
  ) +
  theme_lab() +
  theme(
    axis.text.x = element_text(angle = 90, margin = margin(r = 0)),
    legend.position = "top",
    legend.text=element_text(size=12),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size = 0.5, color = "#cbcbcb"), 
    panel.grid.minor = element_blank()
  ) +
  guides(fill = "none",
         colour = guide_legend(nrow=1,byrow=TRUE))


# write data ---------------------------
write_csv(df, "data2.csv")
ggsave("plot2.svg", dpi = 300, scale = 1)
ggsave("plot2.png", dpi = 300, scale = 1)

