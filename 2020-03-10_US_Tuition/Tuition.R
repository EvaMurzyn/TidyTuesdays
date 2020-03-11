library(tidyverse)
library(janitor)
library(hrbrthemes)
library(patchwork)
library(RColorBrewer)


tuition_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv')

tuition_income <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_income.csv') 

salary_potential <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/salary_potential.csv')

historical_tuition <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/historical_tuition.csv')

diversity_school <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/diversity_school.csv')


### General data tidying ------------------------------- 

#Pivot the diversity data wider
diversity <- diversity_school %>% 
  na.omit() %>% 
  pivot_wider(id_cols = c(name, total_enrollment), names_from = category, values_from = enrollment) %>% 
  clean_names()

diversity$women_percent = ((diversity$women*100)/diversity$total_enrollment)
diversity$minority_percent = ((diversity$total_minority*100)/diversity$total_enrollment)

#Identify which colleges are giving me issues for pivot wider
tidy_income <- tuition_income %>% 
  na.omit() %>% 
  filter(year==2018)  %>% 
  pivot_wider(id_cols = c(name, campus, total_price), names_from = income_lvl, values_from = net_cost, values_fn = list(net_cost = length)) %>% 
  clean_names() %>% 
  filter(over_110_000 > 1)

tidy_income


# pivot the data wider, removing the 3 institutions with multiple entries. There is certainly a better way of doing this.
income <- tuition_income %>% 
  na.omit() %>% 
  filter(year==2018) %>% 
  filter(name != "California College San Diego") %>% 
  filter(name != "Stevens-Henager College") %>% 
  filter(name != "Laurel Technical Institute") %>% 
  pivot_wider(id_cols = c(name, campus, total_price), names_from = income_lvl, values_from = net_cost) %>% 
  clean_names() 


### Graph the rise in tuition costs--------------------

historical_tuition %>% 
  filter(tuition_type == "All Constant") %>%
  ggplot(aes(year, tuition_cost)) +
  geom_bar(stat = "identity", fill = "seagreen4") +
  facet_wrap(vars(type), nrow = 3) +
  labs(title = "Rise in adjusted tution cost 
across different types of universities") +
  xlab("Academic year") + 
  ylab("Tuition cost in USD (inflation adjusted)") +
  theme_ipsum_rc() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### Compare fees across college types and degree lengths, and then against student diversity, and student income potential-------------
# combine data frames

total <- inner_join(tuition_cost, diversity)
total <- inner_join(total, salary_potential)
total$average_total_cost =(total$out_of_state_total + total$in_state_total)/2
total$type <- as.factor(total$type)

total %>% 
  ggplot(aes(degree_length, average_total_cost)) +
  geom_boxplot() +
  facet_wrap(vars(type))

#Nothing interesting happening here, as private places don't have 2 year degrees.
# Looks like we want to be looking just 4 year stuff

p1 <- total %>% 
  filter(degree_length == "4 Year") %>% 
  ggplot(aes(type, minority_percent)) +
  geom_boxplot(fill = "seagreen3") +
  labs(title = "Proportion of minority students") +
  xlab("School type") + 
  ylab("% of students") +
  theme_ipsum_rc() 


p2 <-total %>% 
  filter(degree_length == "4 Year") %>% 
  ggplot(aes(type, women_percent)) +
  geom_boxplot(fill = "seagreen3") +
  labs(title = "Proportion of female students") +
  xlab("School type") + 
  ylab("% of students") +
  theme_ipsum_rc() 

p3 <- total %>% 
  filter(degree_length == "4 Year") %>% 
  ggplot(aes(type, stem_percent)) +
  geom_boxplot(fill = "seagreen2") +
  labs(title = "Proportion of STEM students") +
  xlab("School type") + 
  ylab("% of students") +
  theme_ipsum_rc() 

p4 <- total %>% 
  filter(degree_length == "4 Year") %>% 
  ggplot(aes(type, make_world_better_percent)) +
  geom_boxplot(fill = "seagreen1") +
  coord_cartesian(ylim = c(0, 100)) +
  labs(title = "Proportion of students who think 
they are making the world a better place") +
  xlab("School type") + 
  ylab("% of students") +
  theme_ipsum_rc() 

((p1 | p2)/
    (p3 | p4)) 

# Scatterplots: STEM, Pay and making the world a better place

p5 <-total %>% 
  filter(degree_length == "4 Year") %>% 
  ggplot(aes(stem_percent, early_career_pay)) +
  geom_point(aes(color = type), alpha = 0.5) +
  geom_smooth(method='lm', formula= y~x, color = "seagreen4") +
  labs(title = "Early career pay and 
% of students in STEM", color = "School type") +
  xlab("Percent of students in STEM") + 
  ylab("Early career pay") +
  theme_ipsum_rc() + 
  scale_color_brewer(palette = "Dark2")

p6 <- total %>% 
  filter(degree_length == "4 Year") %>% 
  ggplot(aes(make_world_better_percent, early_career_pay)) +
  geom_point(aes(color = type), alpha = 0.5) +
  geom_smooth(method='lm', formula= y~x, color = "seagreen4") +
  labs(title = "Early career pay and % of students 
who think they are making 
the world a better place", color = "School type") +
  xlab("Percent of students who think they are making the world a better place") + 
  ylab("Early career pay") +
  theme_ipsum_rc() + 
  scale_color_brewer(palette = "Dark2")

p7 <- total %>% 
  filter(degree_length == "4 Year") %>% 
  ggplot(aes(stem_percent, make_world_better_percent)) +
  geom_point(aes(color = type), alpha = 0.5) +
  geom_smooth(method='lm', formula= y~x, color = "seagreen4") +
  labs(title = "% of STEM students and 
% of students who think they 
are making the world a better place", color = "School type") +
  xlab("Percent of students in STEM") + 
  ylab("Percent of students who think they are making the world a better place") +
  theme_ipsum_rc() + 
  scale_color_brewer(palette = "Dark2")

((p5 | p6 | p7))

# Female and minority students vs early career pay

p8 <-total %>% 
  filter(degree_length == "4 Year") %>% 
  ggplot(aes(minority_percent, early_career_pay)) +
  geom_point(aes(color = type), alpha = 0.5) +
  geom_smooth(method='lm', formula= y~x, color = "seagreen4") +
  labs(title = "Early career pay and 
% of minority students", color = "School type") +
  xlab("Percent of minority students") + 
  ylab("Early career pay") +
  theme_ipsum_rc() + 
  scale_color_brewer(palette = "Dark2")

p9 <-total %>% 
  filter(degree_length == "4 Year") %>% 
  ggplot(aes(women_percent, early_career_pay)) +
  geom_point(aes(color = type), alpha = 0.5) +
  geom_smooth(method='lm', formula= y~x, color = "seagreen4") +
  labs(title = "Early career pay and 
% of female students", color = "School type") +
  xlab("Percent of female students") + 
  ylab("Early career pay") +
  theme_ipsum_rc() + 
  scale_color_brewer(palette = "Dark2")

((p8 | p9))


### Geographical plot of the enrollment and % of women by state-------------------

library(sf)
library(rnaturalearth)
library(rnaturalearthhires)
library(viridis)

theme_set(theme_bw())

map <- ne_states('united states of america', returnclass = "sf") %>% 
  filter(name != "Alaska") %>% 
  filter(name != "Hawaii")

p10 <- tuition_cost %>% 
  inner_join(diversity) %>% 
  group_by(state) %>% 
  summarise(average_enrolment = mean(total_enrollment)) %>%
  inner_join(map, by = c("state" = "name")) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill= average_enrolment)) +
  coord_sf(xlim = c(-130, -65), ylim = c(22, 52), expand = FALSE) + 
  scale_fill_viridis() +
  labs(title = "Average school enrollment by state") +
  theme(legend.title = element_blank())

  

p11 <- tuition_cost %>% 
  inner_join(diversity) %>% 
  group_by(state) %>% 
  summarise(average_women = mean(women_percent)) %>%
  inner_join(map, by = c("state" = "name")) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill= average_women)) +
  coord_sf(xlim = c(-130, -65), ylim = c(22, 52), expand = FALSE) + 
  scale_fill_viridis() +
  labs(title = "Average percentage of women by state")+
  theme(legend.title = element_blank())


p12 <- tuition_cost %>% 
  inner_join(diversity) %>% 
  group_by(state) %>% 
  summarise(average_minorities = mean(minority_percent)) %>%
  inner_join(map, by = c("state" = "name")) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill= average_minorities)) +
  coord_sf(xlim = c(-130, -65), ylim = c(22, 52), expand = FALSE) + 
  scale_fill_viridis() +
  labs(title = "Average percentage of minorities by state") +
  theme(legend.title = element_blank())

((p10)/
    (p11)/
    (p12))

