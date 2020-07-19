library(tidyverse)
library(hrbrthemes)
library(extrafont)
library(paletteer)


astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

### Shuttle types going into orbit

astronauts$ascend_shuttle_type <- NA

# sort the shuttles into categories based on text

astronauts$ascend_shuttle_type[grep("Vostok", astronauts$ascend_shuttle)] <- "Vostok"
astronauts$ascend_shuttle_type[grep("STS", astronauts$ascend_shuttle)] <- "Space Shuttle"
astronauts$ascend_shuttle_type[grep("Mercury-Atlas|MA", astronauts$ascend_shuttle)] <- "Mercury-Atlas"
astronauts$ascend_shuttle_type[grep("Gemini|gemini", astronauts$ascend_shuttle)] <- "Gemini"
astronauts$ascend_shuttle_type[grep("Soyuz|soyuz", astronauts$ascend_shuttle)] <- "Soyuz"
astronauts$ascend_shuttle_type[grep("Apollo|apollo", astronauts$ascend_shuttle)] <- "Apollo"
astronauts$ascend_shuttle_type[grep("Voskhod", astronauts$ascend_shuttle)] <- "Voskhod"
astronauts$ascend_shuttle_type[grep("Shenzhou", astronauts$ascend_shuttle)] <- "Shenzhou"

astronauts$ascend_shuttle_type <- as.factor(astronauts$ascend_shuttle_type)

#check that the right numbers are there
summary(astronauts$ascend_shuttle_type)

# create a general summary for each mission
mission_numbers <- astronauts %>% 
  group_by(ascend_shuttle_type, ascend_shuttle) %>% 
  mutate(n = row_number()) %>%
  summarise(max_mission_length = max(hours_mission), 
            astronauts_on_board = max(n), 
            eva_instances = max(field21), 
            eva_time = sum(eva_hrs_mission), 
            year = max(year_of_mission))

# create a summary for each vehicle type
mission_summaries <- mission_numbers %>% 
  group_by(ascend_shuttle_type) %>%
  mutate(n = row_number()) %>% 
  summarise(missions = max(n), astronauts_ferried = sum(astronauts_on_board), first_year = min(year), last_year = max(year)) %>% 
  filter(ascend_shuttle_type != "NA") %>% 
  pivot_longer(cols = c(first_year, last_year), names_to = "operational", values_to = "years")

# set up fonts to work, for some reason hrbrmstr themes stopped formatting the titles right
myFont <- "Roboto Condensed"


#plot!
mission_summaries %>% 
  ggplot(aes(years, ascend_shuttle_type)) +
  geom_line(aes(size = missions, color = astronauts_ferried)) +
  theme_ft_rc() +
  #scale_color_paletteer_c("viridis::plasma") +
  scale_color_gradient(low = "#5390D9", high = "#80FFDB") +
  labs(title = "Ascent vehicles used in the space programmes", subtitle = "Information about number of missions and astronauts ferried", x = "Years used", y = "Vehicle class") + 
  theme(plot.title = element_text(family = myFont, face = "bold", hjust = 0))

ggsave("ascent_vehicles.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 10, height = 9, units = "in") 

### Astronaut occupations - who went in more than 1 capacity?
### Unfinished code

#jobs <- astronauts %>% 
#  group_by(name, sex, occupation) %>% 
#  summarise(total_hours = max(total_hrs_sum), total_eva_hrs = max(total_eva_hrs)) %>% 
#  mutate(roles = row_number())
  
