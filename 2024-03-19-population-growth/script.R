# Title # Births, deaths and migration in 2022

# Source: ONS
# URL: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/estimatesofthepopulationforenglandandwales
# Licence: Open Government Licence v3.0

# load libraries ---------------------------

library(tidyverse) ; 
library(httr) ; 
library(readxl) ; 
library(cowplot) ;
library(svglite)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

theme_labP <- function () { 
  theme_lab() %+replace%
    theme(
      plot.subtitle = element_text(hjust=0, size = 12),
      axis.text.y = element_text(hjust=1, size = 10),
      axis.text.x = element_text(hjust=1, size = 10, angle = 90),
      axis.title.y = element_text(size = 10, angle = 90),
      axis.title.x = element_text(size = 10),
      legend.text = element_text(size=8)
    )
}

# load data ---------------------------

tmp1 <- tempfile(fileext = ".xlsx")

GET(url = "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/estimatesofthepopulationforenglandandwales/mid2011tomid2022detailedtimeseries/myebtablesenglandwales20112022v2.xlsx",     
    write_disk(tmp1))

raw1 <- read_xlsx(tmp1, sheet = 10, skip = 1) 

tmp2 <- tempfile(fileext = ".xlsx") 

GET(url = "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/estimatesofthepopulationforenglandandwales/mid20222023localauthorityboundaires/mye22tablesew2023geogs.xlsx",
    write_disk(tmp2)) 

raw2 <- read_xlsx(tmp2, sheet = 10, skip = 7)

tmp3 <- tempfile(fileext = ".xlsx")

GET(url = "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/analysisofpopulationestimatestoolforuk/2022/theanalysisofpopulationestimatestool2022ewpublication.xlsx",     
    write_disk(tmp3))

raw3 <- read_xlsx(tmp3, sheet = 9)


# tidy data ---------------------------
df1 <- raw1 %>%
  filter(laname23 == "Trafford") %>% 
  select(area_code = ladcode23, area_name = laname23, contains(c("202","201"))) %>%   
  pivot_longer(-c(area_code,area_name),names_to = c("indicator","year"), names_sep = -4) %>%
  filter(!indicator %in% c("special_change_", "unattrib_", "other_adjust_", "other_change_")) %>%
  arrange(area_name, year, indicator)

dfP1 <- df1 %>% filter(indicator != "population_", year == "2022", indicator %in% c("internal_net_", "international_net_", "natchange_")) %>%
  mutate(indicator = case_when(indicator == "internal_net_"~ "Internal Migration Net",
                             indicator == "international_net_"~ "International Migration Net",
                             indicator == "natchange_"~ "Births minus Deaths"))

dfP2a <- raw2 %>%
  filter(Name %in% c("ENGLAND", "Trafford")) %>%
  mutate(Name = ifelse(Name == "ENGLAND", "England", "Trafford")) %>%
  mutate(`Population change` = `Estimated Population mid-2022` - `Estimated Population mid-2021`) %>%
  pivot_longer(-c(Code,Name,Geography), names_to = "indicator") %>%
  group_by(Code,Name,Geography) %>%
  mutate(percent = round(value /value[1]*100,2)) %>%
  filter(indicator %in% c("Births", "Deaths", "Births minus Deaths", "Internal Migration Net", "International Migration Net", "Population change"))
 

dfP2 <- dfP2a %>%
  mutate(value = ifelse(indicator == "Deaths", -value, value), 
         percent = ifelse(indicator == "Deaths", -percent, percent)) %>%
  mutate(indicator = factor(indicator, levels = dfP2a %>% ungroup() %>% select(indicator)%>% unique() %>% pull())) 

dfP3 <- df1 %>%
  group_by(year, area_name) %>%
  mutate(per1000 = round((value/value[10])*1000,2)) %>%
  filter(indicator != "population_") %>%
  #mutate(indicator = factor(indicator, levels = df1 %>% select(indicator)%>% unique() %>% pull())) %>%
  mutate(linewidth = ifelse(indicator %in% c("natchange_", "internal_net_", "international_net_"), 2,1)) 


dfP3E <- raw3 %>% 
  filter(code == "E92000001") %>%
  select(-level,-sex, -age) %>%
  group_by(code)%>%
  summarise_all(sum) %>% 
  select(area_code = code, contains(c("202","201"))) %>%   
  pivot_longer(-c(area_code),names_to = c("indicator","year"), names_sep = -4) %>%
  pivot_wider(names_from = "indicator", values_from = "value") %>%
  mutate(natchange_ = births_ - deaths_) %>%
  pivot_longer(-c(area_code, year), names_to = "indicator", values_to = "value") %>%
  mutate(area_name = "England" )%>%
  group_by(year, area_name) %>%
  mutate(per1000 = round((value/value[1])*1000,2))



# plot data ---------------------------

s <- 
  ggplot(dfP1) +
  geom_col(aes(value, indicator), fill = "#194a81", width = 0.7)  + 
  geom_text(aes(value, indicator, label = value, hjust = 0.5 - sign(value)/2), size = 3, vjust = 0.5, fontface = "bold") +
  geom_vline(xintercept = 0, color = "black") +
  scale_x_continuous( expand = expansion(mult = c(0.2, 0.2))) +
  
  labs(x = "estimated count", y = NULL) +
  theme_labP() +
  theme(panel.grid.major.y = element_blank(),
        axis.text.x = element_blank())

m <-
  ggplot(dfP2) +
  geom_col(aes(percent1, indicator, fill = Name), position = position_dodge(width = 0.6))  + 
  geom_vline(xintercept = 0, color = "black") +
  scale_fill_manual(values = c("grey", "#194a81")) +
  scale_y_discrete(limits = rev) +
  labs(fill = NULL, x = "annual population change %", y = NULL) +
  theme_labP() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(size = 0.5, color = "#cbcbcb"),
        legend.key.height = unit(0.2, 'cm'))

top_row <- plot_grid(s, m, nrow = 1)


plotF <- function(dataM,dataComp, linetype_const, values_scale,labels_scale,breaks_scale2,values_scale2,pSubtitle){
  ggplot(dataM) +
  geom_hline(yintercept = 0, color = "black") +
  geom_line(dataComp, mapping = aes(year,per1000, group = indicator, linetype = linetype_const), colour = "#A577A7", linewidth = 1) +
  geom_line(mapping = aes(year,per1000, group = indicator, colour = indicator,  linewidth = linewidth)) +
  scale_linewidth(range = c(1, 1.5), guide = "none") +
  scale_color_manual(values = values_scale,
                     labels= labels_scale) +
  scale_linetype_manual(name = NULL,
                        breaks = breaks_scale2,
                        values = values_scale2 ) +
  labs(color = NULL, x = NULL, y = "per 1K population", subtitle = pSubtitle) +
  theme_labP() +
  theme(legend.justification = 'left',
        legend.box = 'vertical',
        legend.box.just = 'left',
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.y = unit(0.05, 'cm'),
        legend.box.margin = margin(5, 0, 0, 0),
  ) +
  guides(colour = guide_legend(override.aes = list(linewidth=1), order = 1),
         linetype = guide_legend(order = 2)) 
}

p3Data <- dfP3 %>% filter(area_name == "Trafford", indicator %in% c("births_", "deaths_", "natchange_")) 
p3DataComp <- dfP3E %>% filter(indicator %in% c("natchange_"))

r1 <- 
  plotF(p3Data, p3DataComp, "England nat. change", c("births_" = "#1d85a5", "deaths_" = "#194a81", "natchange_" = "#A577A7"),c("births", "deaths", "nat. change"),c("England nat. change"),c("England nat. change" = 3),"Natural change")


p4Data <- dfP3 %>% filter(indicator %in% c("internal_in_", "internal_out_", "internal_net_")) %>%
  mutate(indicator = factor(indicator, levels = c("internal_in_", "internal_out_", "internal_net_")))

p4DataComp <- dfP3E %>% filter(indicator %in% c("internal_net_"))

r2 <- 
  plotF(p4Data, p4DataComp, "England net",c("internal_in_" = "#1d85a5", "internal_out_" = "#194a81", "internal_net_" = "#A577A7"),c("inflow", "outflow", "net"),c("England net"),c("England net" = 3), "Internal Migration")


p5Data <-  dfP3 %>% filter(indicator %in% c("international_in_", "international_out_", "international_net_")) %>%
  mutate(indicator= factor(indicator, levels = c("international_in_", "international_out_", "international_net_")))

p5DataComp <- dfP3E %>% filter(indicator %in% c("international_net_"))

r3 <- plotF(p5Data, p5DataComp, "England net", c("international_in_" = "#1d85a5", "international_out_" = "#194a81", "international_net_" = "#A577A7"), c("inflow", "outflow", "net"), c('England net'), c('England net'=3), "International Migration")


bottom_row <- plot_grid(r1, r2, r3,  nrow = 1)

title_theme <- calc_element("plot.title", theme_labP())

title <- ggdraw() + 
  draw_label(
    "Trafford's drivers of population change in the year to mid-2022",
    fontfamily = title_theme$family,
    fontface = title_theme$face,
    size = title_theme$size,
    colour = title_theme$colour,
    x = 0,
    hjust = 0
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 7)
  )

caption_theme <- calc_element("plot.caption", theme_labP())

caption <- ggdraw() + 
  draw_label(
    "Source: ONS | @traffordDatalab",
    fontfamily = caption_theme$family,
    fontface = caption_theme$face,
    size = caption_theme$size,
    colour = caption_theme$colour,
    x = 0.972,
    hjust = 1
  ) +
  theme(
    #plot.margin = margin(0, 0, 0, 7)
  )



plot_grid(title, top_row, bottom_row, caption,  ncol = 1, rel_heights = c(0.1, 1, 1,0.05))



# write data ---------------------------

dfa <- dfP2a %>%
  ungroup() %>%
  select(area_code = Code, area_name = Name, indicator, `Count year to mid-2022` = value, `% change year to mid-2022` = percent) %>%
  pivot_longer(c("Count year to mid-2022", "% change year to mid-2022"), names_to = "meausre",values_to = "value") %>%
  mutate(period = "2022-06-01")

dfb <- dfP3 %>%
  ungroup() %>%
  select(-linewidth) %>%
  bind_rows(dfP3E %>% ungroup()) %>%
  mutate(indicator = gsub("_"," ",indicator)) %>%
  rename(period = year, `Count year to mid-Year` = value, `Per 1000 year to mid-Year` = per1000) %>%
  pivot_longer(c("Count year to mid-Year", "Per 1000 year to mid-Year"), names_to = "meausre",values_to = "value")



write_csv(bind_rows(dfa,dfb), "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300, scale = 1)

