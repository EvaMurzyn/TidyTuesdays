library(tidyverse)
library(tidytuesdayR)
library(janitor)
library(sf)
library(rnaturalearth)
library(rgeos)
library(extrafont)
library(hrbrthemes)


tuesdata <- tidytuesdayR::tt_load('2020-08-04')
energy_types <- tuesdata$energy_types
country_totals <- tuesdata$country_totals

options(scipen = 999)

energy_l1 <- energy_types %>% 
  filter(level == "Level 1") %>% 
  clean_names()

# I want to be looking at percentages, not raw numbers

energy_l1 <- energy_l1 %>% 
  group_by(country_name) %>% 
  mutate(total_2016 = sum(x2016), total_2017 = sum(x2017), total_2018 = sum(x2018))

energy_perc <- energy_l1 %>% 
  mutate(perc_2016 = x2016/total_2016, perc_2017 = x2017/total_2017, perc_2018 = x2018/total_2018) %>% 
  select(-total_2016, -total_2017, -total_2018)

### Look at changes between 2016-2018. Which modes of energy production are going up or down across Europe?-------

energy_perc <- energy_perc %>% 
  mutate(change = perc_2018 - perc_2016)


# create the map component
eur_map <- ne_countries(returnclass = "sf")

# fix issues with matching
eur_map$admin[41] <- "Czechia"
eur_map$admin[19] <- "Bosnia & Herzegovina"
eur_map$admin[104] <- "North Macedonia"
eur_map$admin[148] <- "Serbia"
energy_perc$country_name[which(energy_perc$country == "UK")] <- "United Kingdom"


# join them

energy_perc_map <- inner_join(eur_map, energy_perc, by = c("admin" = "country_name"))

# Plot!
myFont <- "Roboto Condensed"


energy_perc_map %>% 
  filter(type.y != "Other") %>% 
  ggplot() +
   geom_sf(aes(fill= change)) +
   coord_sf(xlim = c(-20, 30), ylim = c(25, 70), expand = FALSE) + 
   facet_wrap(~type.y) +
   scale_fill_gradient2(midpoint = 0, mid="lightgoldenrod", high="forestgreen", low="firebrick1") +
   labs(title = "Changes in proportion of energy produced by different means", subtitle = "Between 2016 and 2018") +
   labs(fill = "Change") + 
   ggthemes::theme_map() +
   theme(text = element_text(size = 12, family = myFont)) +
  theme(legend.position="right")

ggsave("changes.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 10, height = 9, units = "in") 


### Overall statistics for production------

energy_production <- energy_l1 %>% 
  group_by(type) %>% 
  summarise(total_2016 = sum(x2016), total_2017 = sum(x2017), total_2018 = sum(x2018)) %>% 
  pivot_longer(-type, names_to = "year", values_to = "production")

energy_production$year[grep("total_2016", energy_production$year)] <- "2016"
energy_production$year[grep("total_2017", energy_production$year)] <- "2017"
energy_production$year[grep("total_2018", energy_production$year)] <- "2018"

energy_production %>%
  filter(type != "Other") %>% 
  ggplot(aes(year,production, fill = type)) +
  geom_bar(stat = "identity") +
  facet_wrap(~type, scales = "free") +
  theme_ipsum() +
  labs(title = "Total energy production in GWh (Gigawatt hours)", subtitle = "In Europe between 2016 and 2018") +
  theme(legend.position = "none")

ggsave("production_gwh.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 10, height = 9, units = "in") 


#looking at % of needs met by each type

energy_production_perc <- energy_perc %>% 
  group_by(type) %>% 
  summarise(perc_2016 = mean(perc_2016), perc_2017 = mean(perc_2017), perc_2018 = mean(perc_2018)) %>% 
  pivot_longer(-type, names_to = "year", values_to = "production")

energy_production_perc$year[grep("perc_2016", energy_production_perc$year)] <- "2016"
energy_production_perc$year[grep("perc_2017", energy_production_perc$year)] <- "2017"
energy_production_perc$year[grep("perc_2018", energy_production_perc$year)] <- "2018"


energy_production_perc %>%
  filter(type != "Other") %>% 
  ggplot(aes(year,production, fill = type)) +
  geom_bar(stat = "identity") +
  facet_wrap(~type, scales = "free") +
  theme_ipsum() +
  labs(title = "Mean proportion of energy produced by various sources", 
       subtitle = "In Europe between 2016 and 2018", 
       y = "Proportion of all energy produced") +
  theme(legend.position = "none")

ggsave("production_perc.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 10, height = 9, units = "in") 
