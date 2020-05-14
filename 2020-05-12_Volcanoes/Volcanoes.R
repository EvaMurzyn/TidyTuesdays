library(tidyverse)
library(forcats)
library(hrbrthemes)
library(paletteer)

volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')



volcano$last_eruption_year <- as.numeric(volcano$last_eruption_year)

### Plot all volcanoes using mapview--------------------------

library(mapview)

volcano_limited <- volcano %>% 
  select(volcano_name, country, primary_volcano_type, latitude, longitude, elevation, population_within_10_km, last_eruption_year)

map <- mapview(volcano_limited, xcol = "longitude", ycol = "latitude", zcol = "last_eruption_year", cex = "population_within_10_km", crs = 4269, grid = FALSE)

mapshot(map, url = "map.html")

### Which volcanoes have ejected most mass overall, throughout history? Looking only at eruptions for which we know the VEI.-----------

biggest_spewers <- eruptions %>%
  filter(vei != "NA") %>% 
  mutate(count = 1) %>% 
  group_by(volcano_name) %>% 
  summarise(total_vei = sum(vei, na.rm = TRUE), mean_vei = mean(vei), max_vei = max(vei),
            total_eruptions = sum(count), latitude = mean(latitude), longitude = mean(longitude), last_event = max(start_year))

# add country data

volcano_locations <- volcano %>% select(volcano_name, primary_volcano_type, country, region)

biggest_spewers <- left_join(biggest_spewers, volcano_locations, by = "volcano_name")

# collapse some type factor levels to help with visualisation
biggest_spewers$primary_volcano_type <- biggest_spewers$primary_volcano_type %>% 
  fct_collapse(Caldera = c("Caldera", "Caldera(s)"), 
                                 Complex = c("Complex", "Complex(es)"), 
                                 Lava_dome = c("Lava dome", "Lava dome(s)"),
                                 Pyroclastic_cone = c("Pyroclastic cone", "Pyroclastic cone(s)"),
                                 Shield = c("Shield", "Shield(s)"),
                                 Stratovolcano = c("Stratovolcano", "Stratovolcano(es)", "Stratovolcano?")) 

biggest_spewers$primary_volcano_type %>% as.character() %>% replace_na("Unknown") -> biggest_spewers$primary_volcano_type

# Scatterplot of number of eruptions and mean VEI, coloured by type

palette_volc <- paletteer_c("pals::kovesi.diverging_rainbow_bgymr_45_85_c67", 17)


biggest_spewers %>% ggplot(aes(x=mean_vei, y = total_eruptions, color = primary_volcano_type)) +
  geom_jitter() +
  theme_ft_rc() +
  scale_color_manual(values = palette_volc) +
  labs(title = "Mean Volcanic Explosivity Index and total number of eruptions", 
       subtitle = "For events with known VEI", 
       x = "Mean VEI score for a volcano", y = "Total eruptions with known VEI",
       color = "Volcano type") +
  annotate("text", x = 1, y = 187, label = "Piton de la Fournaise, France", color = "#FEA069FF") +
  annotate("text", x = 2.5, y = 180, label = "Asosan, Japan", color = "#085CF8FF") +
  annotate("text", x = 2.35, y = 172, label = "Etna, Italy", color = "#FE8784FF") +
  annotate("text", x = 2.4, y = 152, label = "Villarrica, Chile", color = "#FE8784FF") +
  annotate("text", x = 2.9, y = 130, label = "Asamayama, Japan", color = "#167BBAFF") +
  annotate("text", x = 4, y = 105, label = "Sheveluch, Russia", color = "#FE8784FF") +
  annotate("text", x = 6.3, y = 15, label = "Cerro Blanco, Argentina
                   and
              Kurile Lake, Russia", color = "#085CF8FF")

ggsave("vei.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 10, height = 9, units = "in") 


### Look at just the biggest booms! and create an animated map------------------

big_eruptions <- eruptions %>% 
  filter(vei >= 5)

big_eruptions <- left_join(big_eruptions, volcano_locations, by = "volcano_name")

# split by centuries
round_to_decade = function(value){ return(round(value / 100) * 100) } #this will get me century data

big_eruptions$start_century <- round_to_decade(big_eruptions$start_year)
big_eruptions$start_century <- as.integer(big_eruptions$start_century)


library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(gganimate)

map_data <- ne_coastline(returnclass = "sf")

big_eruptions_ad <- big_eruptions %>% 
  filter(start_year > 0)

anim <- ggplot() +
  geom_sf(data = map_data) +
  theme_ft_rc() +
  geom_point(data = big_eruptions_ad, aes(x = longitude, y = latitude, size = as.factor(vei), color = as.factor(vei))) +
  scale_color_manual(values = c("#FCB74AFF", "#FEA069FF", "#FE8784FF")) +
  labs(title = "Eruptions with Volcanic Explosivity Index larger than 4", subtitle = "Century beginning: {closest_state} AD", color = "VEI", size = "VEI") +
  transition_states(rev(start_century), transition_length = 2, state_length = 20) +
  enter_grow() +
  exit_shrink()

animate(anim, end_pause = 15, width = 800, height = 600)

anim_save("ad_big_events.gif")


### Fancy circular bar graph of event numbers by volcano type-----------------------

volcano_locations <- volcano %>% select(volcano_name, primary_volcano_type, country, region)

full_events <- left_join(events, volcano_locations, by = "volcano_name")

# Merge some values together for cleaner data
full_events$primary_volcano_type <- full_events$primary_volcano_type %>% 
  fct_collapse(Caldera = c("Caldera", "Caldera(s)"), 
               Complex = c("Complex", "Complex(es)"), 
               Lava_dome = c("Lava dome", "Lava dome(s)"),
               Pyroclastic_cone = c("Pyroclastic cone", "Pyroclastic cone(s)"),
               Shield = c("Shield", "Shield(s)"),
               Stratovolcano = c("Stratovolcano", "Stratovolcano(es)", "Stratovolcano?")) 

#replace NA's with 'Unknown'
full_events$primary_volcano_type %>% as.character() %>% replace_na("Unknown") -> full_events$primary_volcano_type

# Summarise data to get our values, and discard two volcano types with insufficient data points
events_summary <- full_events %>% 
  mutate(count = 1) %>% 
  group_by(primary_volcano_type, event_type) %>% 
  summarize(event_count = sum(count)) %>%
  filter(primary_volcano_type != "Maar(s)") %>% 
  filter(primary_volcano_type != "Tuff cone(s)") %>% 
  filter(event_type != "VEI (Explosivity Index)") %>% 
  mutate(percent = (event_count/sum(event_count)*100))

#select only events more frequent than 5%
common_events <- events_summary %>% 
  filter(percent > 5)

# Set the variables as factors
common_events$primary_volcano_type <- as_factor(common_events$primary_volcano_type)
common_events$event_type <- as_factor(common_events$event_type)

# https://www.r-graph-gallery.com/297-circular-barplot-with-groups.html

common_events <- common_events %>% arrange(primary_volcano_type, percent)
common_events$id <- seq(1, nrow(common_events))

# Get the name and the y position of each label
label_data <- common_events
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# Make the plot
ggplot(common_events, aes(x=as.factor(id), y=percent, fill=primary_volcano_type)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity") +
  ylim(-50,120) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  scale_fill_manual(values = palette_volc) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=percent+10, label=event_type, hjust=hjust), color="black", 
            fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
  labs(title = "Most freqent events by volcano type", fill = "Volcano type")


ggsave("events.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 15, height = 10, units = "in") 

#and now make a more useful way of showing it

ggplot(common_events, aes(event_type, percent, fill = event_type)) +
  geom_bar(stat = "identity") +
  facet_wrap(~primary_volcano_type) +
  theme_ft_rc() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  labs(title = "Most common events by volcano type", subtitle = "all events more frequent than 5%", fill = "Event type") +
  scale_fill_manual(values = palette_volc)


ggsave("events_sane.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 10, height = 10, units = "in") 
