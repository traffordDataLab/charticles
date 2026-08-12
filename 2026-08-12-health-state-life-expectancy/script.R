# Healthy Life expectancy at birth vs Life expectancy at birth at MSOA #

# Source: ONS
# URL: https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandlifeexpectancies/datasets/healthstatelifeexpectancyestimatesformiddlelayersuperoutputareasenglandandwales
# Licence: OGL 3.0

# load libraries ---------------------------
library(tidyverse) ; library(ggtext) ; library(readxl)
library(cowplot) ;

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")


# load and transform data ---------------------------
id <- "Trafford"

#House of Commons Library MSOA Names
# URL: https://visual.parliament.uk/msoanames

lookup <- read_csv("https://houseofcommonslibrary.github.io/msoanames/MSOA-Names-Latest.csv") %>%
  filter(Laname=="Trafford") %>%
  select(area_code = msoa11cd, area_name = msoa11hclnm)

url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/healthandlifeexpectancies/datasets/healthstatelifeexpectancyestimatesformiddlelayersuperoutputareasenglandandwales/2019to2023/hslemsoa.xlsx"
download.file(url, dest = "hslemsoa.xlsx")

df1 <- read_excel("hslemsoa.xlsx", sheet = "1", skip = 6) %>%
  filter(grepl("Trafford", `Area name`)) %>%
  mutate(category = "Life expectancy at birth") %>%
  rename(value = LE)

df2 <- read_excel("hslemsoa.xlsx", sheet = "2", skip = 6) %>%
  filter(grepl("Trafford", `Area name`)) %>%
  mutate(category = "Healthy life expectancy at birth") %>%
  rename(value = HLE)

df3 <- read_excel("hslemsoa.xlsx", sheet = "3", skip = 6) %>%
  filter(grepl("Trafford", `Area name`)) %>%
  mutate(category = "Disability-free life expectancy at birth") %>%
  rename(value = DFLE)

df <- bind_rows(df1, df2, df3) %>%
  left_join(lookup, by = c("Area code"="area_code")) %>%
  select(period = Period, area_code = `Area code`, area_name, unit = Sex, value, lower_value =LCI, upper_value = UCI, category) %>%
  mutate(area_name = ifelse(is.na(area_name), "Trafford", area_name)) %>%
  group_by(area_name, unit) %>%
  mutate(diff = round(value[1]- value,1)) %>%
  ungroup()
  

plot <- function (data,sex, x1,x2,x3,l1,r1,r2,xcat) {
  ggplot(data) + 
    geom_line(mapping = aes(value,area_name), colour = "#aca6a0", lwd = 2.5) +
    geom_point(mapping = aes(value, area_name, group = category, fill = category), shape=21, size = 4, color = "transparent") +
    
    geom_text(data %>% filter(period == "2019 - 23", area_name == "Trafford"),mapping = aes(label = sex, x = xcat, y = 30.2),  size = 5, hjust = 1) +
    
    geom_text(data %>% filter(category == "Healthy life expectancy at birth"), mapping = aes(x = x1, area_name, label = value), size = 3.5, hjust = 1) +
    geom_text(data %>% filter(category == "Healthy life expectancy at birth", area_name == "Trafford"),mapping = aes(label = "H L E", x = x1, y = 30),  size = 3.5, hjust = 1) +
    
    geom_text(data %>% filter(category == "Life expectancy at birth"), mapping = aes(x = x2, area_name, label = value), size = 3.5, hjust = 1,  show.legend = FALSE) +
    geom_text(data %>% filter(category == "Life expectancy at birth", area_name == "Trafford"),mapping = aes(label = "L E", x = x2, y = 30),  size = 3.5, hjust = 1, lineheight = 0.9) +
    
    
    geom_text(data %>% filter(category == "Healthy life expectancy at birth"), mapping = aes(x = x3, area_name, label = diff), size = 3.5, hjust = 1) +
    geom_text(data %>% filter(category == "Healthy life expectancy at birth", area_name == "Trafford"),mapping = aes(label = "L E -\nH L E", x = x3, y = 30.5),  size = 3.5, hjust = 0.9, lineheight = 0.9) +
    
    
    expand_limits(y= c(0, 31.3)) +
    scale_x_continuous(limits = c(l1,NA)) +
    scale_fill_manual(values = c("#CBA160", "#4C1130")) +
    theme_lab() +
    theme(
      plot.title.position = "plot",
      panel.grid.major.y = element_line(colour = "#EEEEEE", size = 0.1),
      legend.title=element_blank(),
      axis.text.y = element_text(hjust = 1, face = c(rep('plain', r1), "bold", rep("plain",r2))),
      legend.text = element_text(size = 12)
    )
}

plot1 <- df %>% 
  filter(unit == "Female", category != "Disability-free life expectancy at birth") %>%
  mutate(area_name = fct_reorder(factor(area_name), ifelse(category == "Healthy life expectancy at birth", diff, NA), na.rm = TRUE, .desc=FALSE)) 

pF <- 
  plot(plot1, "Female", 50,53,56,49,14,14,74.5) +
  labs(title = 'Years of life after Healthy life expectancy',
       subtitle = "Trafford MSOAs, 2019 - 23",
       x = NULL, 
       y = NULL) +
  theme(axis.text.x = element_blank())
  
plot2 <- df %>% 
    filter(unit == "Male", category != "Disability-free life expectancy at birth") %>%
    mutate(area_name = fct_reorder(factor(area_name), ifelse(category == "Healthy life expectancy at birth", diff, NA), na.rm = TRUE, .desc=FALSE)) 

pM <- 
  plot(plot2,"Male",50,53,56,49,15,13,71.2) +
  labs(title = NULL,
       subtitle = NULL,
       x = NULL,
       y = NULL,
       caption = "L E: Life expectancy at birth. H L E: Healthy life expectancy at birth. L E - H L E: Difference between L E and H L E. Units: Years\nConfidence intervals ranging from ±2.4 to ±0.2 years\nSource: ONS,PHE | @traffordDataLab") +
  theme(legend.position = "none",
        axis.text.x = element_blank())

plot_grid(pF,pM,  ncol = 1, rel_heights = c(1, 1))

data <- df %>%
  select(area_code,area_name,period,indicator = category, unit, value, upper_value,lower_value)
  
# write data ---------------------------
write_csv(df %>% select(area_code,area_name,period,indicator = category, unit, value, upper_value,lower_value), "data.csv")

ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300)
