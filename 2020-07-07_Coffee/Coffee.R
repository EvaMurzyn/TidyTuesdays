library(tidyverse)
library(paletteer)
library(ggthemes)
library(reshape2)

coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

### Profiles of popular coffee varieties---------------

# Means
varieties <- coffee_ratings %>% 
  group_by(species, variety) %>%
  mutate(n = row_number()) %>% 
  summarise(aroma = mean(aroma), 
            flavor = mean(flavor), 
            aftertaste = mean(aftertaste),
            acidity = mean(acidity),
            body = mean(body),
            balance = mean(balance),
            clean_cup = mean(clean_cup),
            sweetness = mean(sweetness),
            cupper_points = mean(cupper_points),
            n = max(n)) %>% 
  arrange(desc(n)) %>% 
  filter(variety != "Other" & variety != "NA")

popular_varieties <- varieties %>% 
  filter(n > 25) %>% 
  pivot_longer(cols = -c(species, variety, n), names_to = "quality", values_to = "rating")

popular_varieties %>% 
  ggplot(aes(rating, fct_rev(quality), fill = quality)) +
  geom_bar(stat = "identity", color = NA) +
  facet_wrap(~variety) +
  theme_solarized_2() +
  scale_fill_paletteer_d("dutchmasters::little_street") +
  labs(title = "Rating profiles for most common 
seven varieties of Arabica coffee", y = "Quality") +
  guides(fill=FALSE)

ggsave("varieties.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 10, height = 9, units = "in") 

### Same plot but grouped by rating type-----------
popular_varieties %>% 
  ggplot(aes(rating, variety, fill = variety)) +
  geom_bar(stat = "identity", color = NA) +
  facet_wrap(~quality) +
  theme_solarized_2() +
  scale_fill_paletteer_d("dutchmasters::anatomy") +
  labs(title = "Rating profiles for most common seven varieties of Arabica coffee", y = "Quality") +
  guides(fill=FALSE) +
  coord_cartesian(xlim = c(5, 10))

ggsave("varieties1.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 10, height = 9, units = "in") 

### More detailed look--------------

top_7_list <- coffee_ratings %>% 
  filter(variety != "Other" & variety != "NA") %>% 
  group_by(variety) %>% 
  tally() %>% 
  top_n(7) %>% 
  select(-n)

top_7_list <- as_vector(top_7_list)

top_7 <- coffee_ratings %>% 
  filter(variety %in% top_7_list) %>% 
  select(variety,aroma, flavor, aftertaste, acidity, body, balance, clean_cup, sweetness, cupper_points) %>% 
  pivot_longer(cols = -variety, names_to = "quality", values_to = "rating") %>% 
  filter(rating != 0)


top_7 %>% 
  ggplot(aes(rating, variety, color = variety)) +
  geom_jitter(height = 0.15) +
  facet_wrap(~quality) +
  theme_solarized_2() +
  scale_color_paletteer_d("dutchmasters::anatomy") +
  labs(title = "Rating profiles for most common seven varieties of Arabica coffee", y = "Quality") +
  guides(color=FALSE)

ggsave("varieties_dot.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 10, height = 9, units = "in")

### Compare Arabica and Robusta ----------------------
species <- coffee_ratings %>% 
  group_by(species) %>%
  mutate(n = row_number()) %>% 
  summarise(aroma = mean(aroma), 
            flavor = mean(flavor), 
            aftertaste = mean(aftertaste),
            acidity = mean(acidity),
            body = mean(body),
            balance = mean(balance),
            clean_cup = mean(clean_cup),
            sweetness = mean(sweetness),
            cupper_points = mean(cupper_points),
            n = max(n))

species_melt <- species %>% 
  select(-n) %>% 
  melt()

species_melt %>% 
  ggplot(aes(value, variable, fill = species)) +
  geom_bar(stat = "identity", color = NA, position = "dodge") +
  #facet_wrap(~species) +
  theme_solarized_2() +
  scale_fill_paletteer_d("dutchmasters::little_street") +
  labs(title = "Comparison of Arabica and Robusta ratings", y = "Quality", x = "Mean score")


ggsave("species.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 6, height = 5, units = "in") 
