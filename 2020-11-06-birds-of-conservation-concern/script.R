## Birds of conservation concern ##

# load libraries ---------------------------
library(rebird) ; library(tidyverse) ; library(ggtext)

# load data ---------------------------

# Birds of Conservation Concern 4 list
# Source: https://www.bto.org/sites/default/files/shared_documents/publications/birds-conservation-concern/birds-of-conservation-concern-4-leaflet.pdf
BoCC4 <- read_csv("BoCC4_list.csv") %>% 
  mutate(species = tolower(species))

# Bird sightings at Carrington Moss over last 30 days #
# Source: https://ebird.org/hotspot/L3267897
birds <- ebirdregion(key = "6hanhdiola5p", loc = "L3267897", back = 30) %>% 
  group_by(comName) %>%
  summarise(n = sum(howMany)) %>% 
  mutate(comName = tolower(comName))

# tidy data ---------------------------
df <- birds %>% 
  left_join(BoCC4, by = c("comName"= "species")) %>% 
  mutate(comName = str_to_title(comName),
         category = fct_relevel(category, "Red", "Amber")) %>% 
  filter(!is.na(category))

# plot data ---------------------------
ggplot(df, aes(fct_reorder(comName, n), n)) +
  geom_col(aes(fill = category), colour = NA, width = 0.8, alpha = 0.7) +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  geom_text(aes(label = n), colour = "#212121", size = 3, fontface = "bold", hjust = 0, nudge_y = 0.3) +
  scale_fill_manual(values = c("Red" = "#C22630", "Amber" = "#E98B3A")) + 
  scale_y_continuous(expand = c(0, 0)) +
  facet_grid(category~., scales = "free_y", space = "free") +
  coord_flip(clip = 'off') +
  labs(title = "Birds of conservation concern spotted on Carrington Moss",
       subtitle = paste0("<span style = 'color:#757575;'>last 30 days</span>"),
       caption = "Data: ebird.org | Birds of Conservation Concern",
       x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(plot.margin = unit(rep(1, 4), "cm"),
        panel.spacing = unit(1, "lines"),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_markdown(margin = margin(b = 20)),
        plot.caption = element_text(colour = "grey60", margin = margin(t = 20, b = -10)),
        strip.text = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none") 

# write data ---------------------------
write_csv(df, "data.csv")
ggsave("plot.svg", dpi = 300, scale = 1)
ggsave("plot.png", dpi = 300, scale = 1)
