# Businesses #

# Source: Inter Departmental Business Register, ONS
# URL: https://www.ons.gov.uk/aboutus/whatwedo/paidservices/interdepartmentalbusinessregisteridbr
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(formattable) ; library(scales) ; library(htmlwidgets)

# load data ---------------------------
df <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_142_1.data.csv?geography=1946157089,1853882369,2092957699&date=latest&industry=37748736&employment_sizeband=1...9&legal_status=0&measures=20100&select=date_name,geography_name,geography_code,industry_name,employment_sizeband_name,legal_status_name,measures_name,obs_value,obs_status_name") %>% 
  select(area_code = GEOGRAPHY_CODE, area_name = GEOGRAPHY_NAME, period = DATE_NAME,
         value = OBS_VALUE, group = EMPLOYMENT_SIZEBAND_NAME) %>% 
  mutate(indicator = "Number of businesses",
         measure = "Count",
         unit = "Enterprises by employment size band") %>% 
  select(area_code, area_name, indicator, period, measure, unit, value, group) %>% 
  mutate(area_name = fct_relevel(factor(area_name), 
                                 "Trafford", "Greater Manchester", "England"),
         group = fct_relevel(factor(group), 
                             "0 to 4", "5 to 9", "10 to 19", "20 to 49",
                             "50 to 99", "100 to 249", "250 to 499",
                             "500 to 999", "1000+")) %>% 
  arrange(area_name, group)

# plot data ---------------------------
table <- df %>% 
  select(area_name, group, value) %>% 
  group_by(area_name, group) %>% 
  summarise(value = sum(value)) %>% 
  mutate(value = value / sum(value)) %>% 
  ungroup() %>%
  mutate(value = percent(value)) %>% 
  spread(area_name, value) %>%
  select(`Employment size band` = group, everything()) %>% 
  formattable(align = c("l", "r", "r", "r"),
              list(`Trafford` = color_tile("transparent", "lightblue"),
                   `Greater Manchester` = color_tile("transparent", "lightblue"),
                   `England` = color_tile("transparent", "lightblue")))

# write data ---------------------------
write_csv(df, "data.csv")
saveWidget(as.htmlwidget(table), "table.html", selfcontained = TRUE)
