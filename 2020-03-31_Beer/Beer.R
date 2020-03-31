# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-03-31/readme.md

library(tidyverse)
library(tsibble)
library(forcats)
library(RColorBrewer)
library(hrbrthemes)
library(janitor)
library(ggthemes)

brewer_size <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewer_size.csv')
beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')


### Are breweries consolidating? -------------------------------------

brewer_size$brewer_size <- as.factor(brewer_size$brewer_size)


brewer_size_clean <- brewer_size %>%  filter(brewer_size != "1,000,000 to 6,000,000 Barrels (5)" &
         brewer_size != "1,000,001 to 6,000,000 Barrels (5)" & 
         brewer_size != "1,000,001 to 6,000,000 Barrels" &
         brewer_size != "1,000,000 to 6,000,000 Barrels" & 
         brewer_size != "Zero Barrels" &
         brewer_size != "Barrels (31 gallons) (2)" &
         brewer_size != "0 Barrels" &
         brewer_size != "Under 1 Barrel" &  
         brewer_size != "Total") %>% 
  droplevels() %>% 
  mutate(barrels_per_brewer = total_barrels/n_of_brewers)

#collapse factor levels for better facet wrapping
brewer_size_clean$size_bucket <- fct_collapse(brewer_size_clean$brewer_size, tiny = c("1 to 1,000 Barrels","1,001 to 7,500 Barrels", "7,501 to 15,000 Barrels"), 
                                 small = c("15,001 to 30,000 Barrels","30,001 to 60,000 Barrels", "60,001 to 100,000 Barrels"), 
                                 medium = c("100,001 to 500,000 Barrels", "500,001 to 1,000,000 Barrels"),
                                 large = c("1,000,001 to 1,999,999 Barrels", "2,000,000 to 6,000,000 Barrels"),
                                 huge = c("6,000,001 Barrels and Over"))

#reorder fatcors to be more legible
brewer_size_clean$size_bucket <- factor(brewer_size_clean$size_bucket, levels = c("tiny","small", "medium", "large", "huge"))

brewer_size_clean$brewer_size <- factor(brewer_size_clean$brewer_size, levels = c("1 to 1,000 Barrels", 
                                                                                  "1,001 to 7,500 Barrels" , "7,501 to 15,000 Barrels", 
                                                                                  "15,001 to 30,000 Barrels" , "30,001 to 60,000 Barrels",
                                                                                  "60,001 to 100,000 Barrels", "100,001 to 500,000 Barrels",
                                                                                  "500,001 to 1,000,000 Barrels", "1,000,001 to 1,999,999 Barrels",
                                                                                  "2,000,000 to 6,000,000 Barrels", "6,000,001 Barrels and Over"))

# Make a custom palette
colourCount = length(unique(brewer_size_clean$brewer_size))
getPalette = colorRampPalette(brewer.pal(8, "Set2"))

# How does beer production look like across different company sizes?

brewer_size_clean %>%
  as_tsibble(key = brewer_size, index = year) %>%
  mutate(total_barrels = total_barrels/1000000) %>% 
  ggplot() +
  geom_line(aes(x = year, y = total_barrels, color = brewer_size), size = 1) +
  facet_wrap(~size_bucket, scales = "free_y", nrow = 5) +
  scale_color_manual(values = getPalette(colourCount)) +
  theme_wsj() +
  labs(title = "Beer production in millions of barrels", subtitle = "across different brewery sizes") +
  ylab("Total barrels produced in millions") +
  scale_x_continuous(breaks = 2009:2019) +
  theme(legend.title = element_blank())

ggsave("beer_production.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 10, height = 9, units = "in")            


# What about the numbers of breweries on the market?
brewer_size_clean %>%
  as_tsibble(key = brewer_size, index = year) %>% 
  ggplot() +
  geom_line(aes(x = year, y = n_of_brewers, color = brewer_size), size = 1) +
  facet_wrap(~size_bucket, scales = "free_y", nrow = 5) +
  scale_color_manual(values = getPalette(colourCount)) +
  theme_wsj() +
  labs(title = "Number of brewing companies", subtitle = "across different brewery sizes") +
  ylab("Number of breweries") +
  scale_x_continuous(breaks = 2009:2019) +
  theme(legend.title = element_blank())

ggsave("beer_companies.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 10, height = 9, units = "in")   

# Finally, average production for company
brewer_size_clean %>%
  as_tsibble(key = brewer_size, index = year) %>% 
  ggplot() +
  geom_line(aes(x = year, y = barrels_per_brewer, color = brewer_size), size = 1) +
  facet_wrap(~size_bucket, scales = "free_y", nrow = 5) +
  scale_color_manual(values = getPalette(colourCount)) +
  labs(title = "Number of brewing companies", subtitle = "across different brewery sizes") +
  ylab("Number of breweries") +
  scale_x_continuous(breaks = 2009:2019) +
  theme(legend.title = element_blank())

# Not much happening here


### Which states have most beer production?---------------------
library(sf)
library(rnaturalearth)
library(rnaturalearthhires)
library(viridis)

theme_set(theme_bw())

map <- ne_states('united states of america', returnclass = "sf") %>% 
  filter(name != "Alaska") %>% 
  filter(name != "Hawaii")


beer_stats <- beer_states %>% 
  group_by(year, state) %>% 
  summarise(total_barrels = sum(barrels)) %>%
  group_by(state) %>% 
  summarise(average_barrels = mean(total_barrels, na.rm = TRUE)) %>%
  mutate(average_barrels = average_barrels/1000000)

median_beer_production <- median(beer_stats$average_barrels)
mean_beer_production <- mean(beer_stats$average_barrels, na.rm = TRUE)

plot1 <- beer_states %>% 
  group_by(year, state) %>% 
  summarise(total_barrels = sum(barrels)) %>%
  group_by(state) %>% 
  summarise(average_barrels = mean(total_barrels, na.rm = TRUE)) %>%
  mutate(average_barrels = average_barrels/1000000)

plot1_join <- inner_join(map, plot1, by = c("postal" = "state"))


ggplot(data = plot1_join) +
  geom_sf(aes(geometry = geometry, fill= average_barrels)) +
  coord_sf(xlim = c(-130, -65), ylim = c(22, 52), expand = FALSE) + 
  scale_fill_gradient2(midpoint = 6.78, mid="antiquewhite", high="orangered", low="black") + # use mean production as midpoint of scale
  labs(title = "Average beer production (between 2009-2019) by state") +
  labs(fill = "Millions of barrels") + 
  ggthemes::theme_map()    


 ggsave("beer_states_average.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 7, height = 6, units = "in")  


# calculate change across time

plot2 <- beer_states %>% 
  group_by(year, state) %>% 
  summarise(all_barrels = sum(barrels)) %>% 
  pivot_wider(id_cols = state, names_from = year, values_from = all_barrels) %>% 
  clean_names() %>% 
  mutate(change = (x2019-x2009)/1000000)

plot2_join <- inner_join(map, plot2, by = c("postal" = "state"))


ggplot(plot2_join) +
  geom_sf(aes(geometry = geometry, fill= change)) +
  coord_sf(xlim = c(-130, -65), ylim = c(22, 52), expand = FALSE) + 
  scale_fill_gradient2(midpoint = 0, mid="antiquewhite", high="orangered", low="black") +
  labs(title = "Change in beer production by state between 2009 and 2019") +
  labs(fill = "Millions of barrels") + 
  ggthemes::theme_map()

ggsave("beer_states_change.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 7, height = 6, units = "in")
