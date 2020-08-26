library(tidyverse)
library(cowplot)
library(magick)

avatar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/avatar.csv')
scene_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/scene_description.csv')

avatar$character <- as.factor(avatar$character)
avatar$book <- as.factor(avatar$book)

summary(avatar)

### Momo and Appa

key_chars <- c("Aang", "Sokka", "Katara", "Toph", "Zuko")
key_cols <- c("#D3C37A", "#5897FA", "#6482A8", "#5A7F44", "#722B0D")

avatar$momo <- str_detect(avatar$character_words, "Momo")

avatar$appa <- str_detect(avatar$character_words, "Appa")

avatar_appa <- avatar %>% 
  filter(avatar$appa == TRUE)

avatar_momo <- avatar %>% 
  filter(avatar$momo == TRUE)

appa <- avatar_appa %>% 
  group_by(character) %>%
  mutate(n = row_number()) %>% 
  summarise(n = max(n)) %>%
  filter(character %in% key_chars) %>% 
  ggplot(aes(character, n, fill = character)) +
  geom_bar(stat = "identity") +
  labs(x = "Who speaks", y = "number of mentions") +
  coord_flip(ylim = c(0, 100)) +
  scale_fill_manual(values = key_cols) +
  theme_minimal() +
  theme(legend.position = "none")
  


momo <- avatar_momo %>% 
  group_by(character) %>%
  mutate(n = row_number()) %>% 
  summarise(n = max(n)) %>%
  add_row(character = "Zuko", n = 0) %>% 
  filter(character %in% key_chars) %>% 
  ggplot(aes(character, n, fill = character)) +
  geom_bar(stat = "identity") +
  labs(x = "Who speaks", y = "number of mentions") +
  scale_y_reverse() +
  coord_flip(ylim = c(0, 100)) +
  scale_fill_manual(values = key_cols) +
  theme_minimal() +
  theme(legend.position = "none")



a <- ggdraw() +
  draw_image("appa.png") +
  draw_plot(appa)


m <- ggdraw() +
  draw_image("momo.png") +
  draw_plot(momo)


plot_row <- plot_grid(a, m)

# now add the title
title <- ggdraw() + 
  draw_label("Whose name is said more often, and who speaks it?",
    fontface = 'bold',
    x = 0,
    hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 7))


plot_grid(title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1))


ggsave("momo-appa.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 10, height = 9, units = "in") 
