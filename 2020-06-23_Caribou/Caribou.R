library(tidyverse)
library(reactable)
library(lubridate)

individuals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/individuals.csv')
locations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/locations.csv')


### Good overview of the different site characteristics ### ------------------

locations$animal_id <- as.factor(locations$animal_id)
locations$study_site <- as.factor(locations$study_site)
locations$season <- as.factor(locations$season)
locations$year <- year(locations$timestamp)

summary(locations)

#look at measures per site

measures_per_site <- locations %>% 
  group_by(study_site, animal_id) %>%
  tally() %>% 
  mutate(nrow = row_number()) %>% 
  summarise(individuals = max(nrow), 
            mean_measurements = round(mean(n), digits = 0), 
            max_measurements = max(n))


top_caribou_per_site <- locations %>% 
  group_by(study_site, animal_id) %>%
  tally() %>% 
  top_n(1) %>% 
  select(-n)

years_active_per_site <- locations %>% 
  group_by(study_site) %>% 
  summarise(earliest_observations = min(year), latest_observations = max(year))

sex_per_site <- individuals %>% 
  group_by(study_site, sex) %>%
  tally() %>% 
  pivot_wider(names_from = "sex", values_from = "n")
  

sex_per_site[is.na(sex_per_site)] <- 0

sex_per_site <- sex_per_site %>% mutate(proportion_female = round(f/sum(f,m), digits = 2)) %>% 
  select(study_site, proportion_female)

calfs_per_site <- individuals %>% 
  group_by(study_site, with_calf) %>%
  tally() %>% 
  filter(with_calf == TRUE)


site_info <- left_join(measures_per_site, top_caribou_per_site, by = "study_site") %>% 
  left_join(years_active_per_site, by = "study_site") %>% 
  left_join(sex_per_site, by = "study_site") %>% 
  left_join(calfs_per_site, by = "study_site") %>% 
  mutate(calves_counted = n) %>% 
  select(study_site, earliest_observations, latest_observations, individuals, proportion_female, 
         calves_counted, mean_measurements, max_measurements, animal_id)



### Making a pretty table with reactable.###------------
# https://themockup.blog/posts/2020-05-13-qb-salaries-vs-playoff-appearances/

site_info %>% 
  reactable()

make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}

good_color <- make_color_pal(c("#faffe2", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"), bias = 2)

site_info %>% reactable(
  pagination = FALSE,
  compact = TRUE,
  borderless = FALSE,
  striped = FALSE,
  fullWidth = FALSE,
  # apply defaults
  # 100 px and align to center of column
  defaultColDef = colDef(
    align = "center",
    minWidth = 100), 
  columns = list(
    study_site = colDef(name = "Study site"),
    earliest_observations = colDef(name = "Earliest observation"),
    latest_observations = colDef(name = "Latest observation"),
    individuals = colDef(name = "Individuals monitored",
                         style = function(value) {
                           value
                           normalized <- (value - min(site_info$individuals)) / (max(site_info$individuals) - min(site_info$individuals))
                           color <- good_color(normalized)
                           list(background = color) } ),
    proportion_female = colDef(name = "Proportion female"),
    calves_counted = colDef(name = "Calfs seen"),
    mean_measurements = colDef(name = "Mean GPS measures per individual",
                               style = function(value) {
                                 value
                                 normalized <- (value - min(site_info$mean_measurements)) / (max(site_info$mean_measurements) - min(site_info$mean_measurements))
                                 color <- good_color(normalized)
                                 list(background = color) }),
    max_measurements = colDef(name = "Highest number of GPS measures",
                              style = function(value) {
                                value
                                normalized <- (value - min(site_info$max_measurements)) / (max(site_info$max_measurements) - min(site_info$mean_measurements))
                                color <- good_color(normalized)
                                list(background = color) }),
    animal_id = colDef(name = "Record holder for most GPS measures")
                               )
)

