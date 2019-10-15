# Population projection #

# Source: Office for National Statistics
# URL: https://www.nomisweb.co.uk
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(scales) ; library(httr) ; library(readxl) ; library(lubridate) ; library(ggpol)

# read data ---------------------------

# mid-2018 population estimate
# https://www.nomisweb.co.uk/query/construct/summary.asp?mode=construct&version=0&dataset=2002
population_estimate <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=1879048225&date=latest&gender=1,2&c_age=101...191&measures=20100&select=date_name,geography_name,geography_code,gender_name,c_age_name,measures_name,obs_value,obs_status_name") %>% 
  select(PERIOD = DATE_NAME, gender = GENDER_NAME, age = C_AGE_NAME, n = OBS_VALUE) %>% 
  mutate(period = ymd(str_c(PERIOD, "06-30", sep = "-")),
         gender = factor(gender, levels = c("Male", "Female")),
         age = parse_number(age),
         ageband = cut(age,
                       breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,120),
                       labels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
                                  "40-44","45-49","50-54","55-59","60-64","65-69","70-74",
                                  "75-79","80-84","85-89","90+"),
                       right = FALSE)) %>% 
  group_by(period, gender, ageband) %>% 
  summarise(n = sum(n))

# mid-2028 population projection
# https://www.nomisweb.co.uk/query/construct/summary.asp?mode=construct&version=0&dataset=2006
population_projection <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_2006_1.data.csv?geography=1879048225&projected_year=2028&gender=1,2&c_age=101...191&measures=20100&select=geography_name,geography_code,projected_year_name,gender_name,c_age_name,measures_name,obs_value,obs_status_name") %>% 
  select(period = PROJECTED_YEAR_NAME, gender = GENDER_NAME, age = C_AGE_NAME, n = OBS_VALUE) %>% 
  mutate(period = ymd(str_c(period, "06-30", sep = "-")),
         gender = factor(gender, levels = c("Male", "Female")),
         age = parse_number(age),
         ageband = cut(age,
                       breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,120),
                       labels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
                                  "40-44","45-49","50-54","55-59","60-64","65-69","70-74",
                                  "75-79","80-84","85-89","90+"),
                       right = FALSE)) %>% 
  group_by(period, gender, ageband) %>% 
  summarise(n = sum(n))

df <- bind_rows(population_estimate, population_projection)

# plot data ---------------------------
temp <- mutate(df, n = case_when(gender == "Male" ~ n * -1, TRUE ~ n)) 

ggplot() +
  geom_col(data = filter(temp, period == "2018-06-30"), 
           aes(x = ageband, y = n, fill = gender), alpha = 0.6) + 
  geom_line(data = filter(temp, period == "2028-06-30"), 
            aes(x = ageband, y = n, group = gender, colour = gender), stat = "identity", size = 1,) +
  scale_fill_manual(values = c("#7FC5DC", "#7FDCC5")) +
  scale_colour_manual(values = c("#7FC5DC", "#7FDCC5")) +
  facet_share(~gender, dir = "h", scales = "free", reverse_num = TRUE) +
  coord_flip() +
  labs(x = NULL, y = NULL, 
       title = "Age composition of Trafford, mid-2018",
       caption = "Source: Office for National Statistics",
       tag = "mid-2028\nprojection") +
  theme_minimal() +
  theme(plot.margin = unit(c(1,1,1,1), "cm"),
        panel.spacing = unit(0.05, "lines"),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5, vjust = 4),
        plot.subtitle = element_text(hjust = 0.5, vjust = 4),
        strip.text = element_text(size = 11, face = "bold", vjust = 1),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        plot.caption = element_text(size = 9, color = "grey50", hjust = 1, margin = margin(t = 15)),
        legend.position = "none",
        plot.tag.position = c(0.23, 0.85),
        plot.tag = element_text(size = 10, colour = "#757575", hjust = 0))

# write data ---------------------------
write_csv(df, "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300, scale = 1)

# analyse data ---------------------------
estimate <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=1879048225&date=latest&gender=0&c_age=201,203,209&measures=20100&select=date_name,geography_name,geography_code,gender_name,c_age_name,measures_name,obs_value,obs_status_name") %>% 
  select(period = DATE_NAME, ageband = C_AGE_NAME, n = OBS_VALUE) %>% 
  mutate(period = ymd(str_c(period, "06-30", sep = "-")))

projection <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_2006_1.data.csv?geography=1879048225&projected_year=2028&gender=0&c_age=201,203,209&measures=20100&select=geography_name,geography_code,projected_year_name,gender_name,c_age_name,measures_name,obs_value,obs_status_name") %>% 
  select(period = PROJECTED_YEAR_NAME, ageband = C_AGE_NAME, n = OBS_VALUE) %>% 
  mutate(period = ymd(str_c(period, "06-30", sep = "-")))

bind_rows(estimate, projection) %>% 
  spread(period, n) %>% 
  group_by(ageband) %>% 
  mutate(change = `2028-06-30` - `2018-06-30`,
         percent_change = round((change/`2018-06-30`)*100,1))
