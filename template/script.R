## Title ##

# load libraries ---------------------------

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data ---------------------------
raw <- 

# tidy data ---------------------------
df <- raw 

# plot data ---------------------------

# write data ---------------------------
write_csv(df, ".csv")
ggsave(".svg", dpi = 300, scale = 1)
ggsave(".png", dpi = 300, scale = 1)

