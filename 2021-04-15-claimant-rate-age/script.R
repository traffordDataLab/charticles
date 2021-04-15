# Claimant Count by Age #

# Source: Claimant Count, ONS
# URL: https://www.nomisweb.co.uk/sources/cc
# Licence: Open Government Licence v3.0

# Source:Mid-2019 population estimates for local authorities in England
# URL: https://www.nomisweb.co.uk/datasets/pestsyoala
# Licence: Open Government Licence v3.0

# load libraries ---------------------------
library(tidyverse) ; library(svglite)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data ---------------------------
raw <- read_csv("https://www.nomisweb.co.uk/api/v01/dataset/NM_162_1.data.csv?geography=1811939363&date=latestMINUS13-latest&gender=0&age=10...20&measure=1&measures=20100") 

pop_raw <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=1811939363&date=latest&gender=0&c_age=101...191&measures=20100&select=date_name,geography_name,geography_code,gender_name,c_age_name,measures_name,obs_value,obs_status_name")


# tidy data ---------------------------

pop_traf <- pop_raw %>%
  select(age_num = C_AGE_NAME, n = OBS_VALUE) %>%
  mutate(age_num = parse_number(age_num),
         age = cut(age_num,
                       breaks = c(0,15,24,34,44,54,64,120),
                       labels = c("0-15","16-24","25-34","35-44","45-54","55-64","65+"),
                       right = TRUE,
                       include.lowest = TRUE)) %>%
  group_by(age) %>%
  summarise(population = sum(n))

df <- raw %>%
  select(period = DATE_NAME,
         area_code = GEOGRAPHY_CODE,
         area_name = GEOGRAPHY_NAME,
         age = AGE_NAME,
         value = OBS_VALUE) %>%
  mutate(period = as.yearmon(period, "%b %Y"),
         age =  gsub("Aged |All categories: Age ", "", age)) %>%
  mutate(age=fct_collapse(age, `16-24` = c("16-17", "18-24"),
                          `25-34` = c("25-29", "30-34"),
                          `35-44` = c("35-39", "40-44"),
                          `45-54` = c("45-49", "50-54"),
                          `55-64` = c("55-59", "60-64"),
                          `65+` = "65+")) %>%
  filter(!age == "65+") %>%
  group_by(period, area_code, area_name, age) %>%
  summarise_all(sum) %>%
  left_join(pop_traf, by = "age") %>%
  mutate(rate = round((value/population)*100,1),
         date=as.Date(as.yearmon(period), format="%b %Y")) %>%
  mutate(label = if_else(date == "2020-05-01", paste0(age, " years"), "")) %>%
  mutate(label2 = if_else(date == max(df$date), paste0(as.character(rate), "%"), ""))


# plot data ---------------------------

ggplot(df, aes(x = date, y = rate, colour = age)) +
  geom_hline(yintercept = 0, size = 0.3, colour = "#333333") +
  geom_line(size = 1, show.legend = FALSE) +
  geom_text(aes(x = as.Date(as.yearmon("Jul 2020"), format="%b %Y"), label = label, fontface = "bold", hjust = 0, vjust = -0.5), show.legend = FALSE) +
  geom_text(data = df %>% filter(age != "45-54"), aes(x = max(date), y = rate, label = label2), size = 3.5, colour = "grey", fontface = "bold", hjust = 0, vjust = 0, show.legend = FALSE) +
  geom_text(data = df %>% filter(age == "45-54"), aes(x = max(date), y = rate, label = label2), size = 3.5, colour = "grey", fontface = "bold", hjust = 0, vjust = 1, show.legend = FALSE) +
  scale_x_date(date_breaks="1 month", date_labels="%b %y") +
  scale_y_continuous(expand = c(0.005, 0.005)) +
  labs(title = "Claimant rate by age band",
       subtitle = "Trafford, Jan 2020 to Feb 2021",
       caption = "Source: ONS",
       x = NULL,
       y = NULL,
       colour = NULL) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(size=14,face = "bold"),
        plot.subtitle = element_text(size=12),
        plot.margin = unit(rep(0.5,4), "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_line(colour = "#333333", size = 0.5))  +
  coord_cartesian(clip = "off") 

# write data ---------------------------

df <- df %>% 
  mutate(measure = "Claimant Count") %>%
  select(period, area_code, area_name, age, measure, count = value, rate)

write_csv(df, "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300, scale = 1)

