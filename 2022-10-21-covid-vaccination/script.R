# Vaccination for Trafford areas #

# Source: PHE
# URL: https://coronavirus.data.gov.uk
# Licence: Open Government Licence v3.0

# load libraries ---------------------------
library("tidyverse")
# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data ---------------------------
raw <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=msoa&areaCode=E08000009&metric=cumVaccinationFirstDoseUptakeByVaccinationDatePercentage&metric=cumVaccinationSecondDoseUptakeByVaccinationDatePercentage&metric=cumVaccinationThirdInjectionUptakeByVaccinationDatePercentage&format=csv") 


# tidy data ---------------------------
df <- raw %>%
  rename(`First Dose` = cumVaccinationFirstDoseUptakeByVaccinationDatePercentage, `Second Dose`= cumVaccinationSecondDoseUptakeByVaccinationDatePercentage,
         `Third Injection` = cumVaccinationThirdInjectionUptakeByVaccinationDatePercentage) %>%
  pivot_longer(`First Dose`:`Third Injection`, "Dose", 
               values_to = "rate") %>%
  select(area_code = areaCode,
         area_name = areaName,
         period = date,
         Dose,
         value = rate) %>%
  mutate(
         area_name = fct_reorder(factor(area_name), ifelse(Dose == "Second Dose", value, NA), na.rm = TRUE),
         measure = "percent",
         unit = "persons",
         indicator =  "Vaccination uptake percentage")


# plot data ---------------------------

ggplot(df, aes(value,area_name)) +
  geom_segment(data = filter(df, Dose == "First Dose"), aes(x = 0, y = area_name, xend = value, yend = area_name), color = "#f0f0f0", size=0.5) +
  geom_point(aes(fill = Dose), size = 4, shape = 21) +
  geom_text(aes(label = round(value,0)), fontface = "bold", color = "black", size = 2, hjust = 0.5, show.legend = FALSE) +
  scale_x_continuous(labels = function(x){ paste0(x, "%") }, limits = c(0, 100)) +
  scale_fill_manual(values = c("Third Injection" = "#aae7d5",
                                 "Second Dose" = "#65D4FC",
                                 "First Dose" = "#25A3D9")) +
  labs(x = "Population percentage (%)", y = NULL,
       title = "COVID 19 Vaccination uptake rate", 
       subtitle = "Trafford MSOAs, 19th of October, 2022", 
       caption = "Source: PHE  |  @traffordDataLab")+
  theme_lab() +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_text(hjust = 1),
        axis.text.x=element_blank(),
        axis.title.x=element_text(size = 12),
        legend.text = element_text(size=10),
        legend.position = "top", 
        legend.title = element_blank())

# write data ---------------------------
write_csv(df, "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300, scale = 1)
