# https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-03-24

library(tidyverse)
library(tsibble)
library(RColorBrewer)
library(hrbrthemes)
library(kableExtra) # create a nicely formated HTML table



tbi_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv')
tbi_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_year.csv')
tbi_military <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_military.csv')

#set a colour poalette for injury causes

injury_colours <- c("Assault" = "firebrick1", "Intentional self-harm" = "darkorange2", 
                    "Motor vehicle crashes" = "darkgoldenrod2", "Other or no mechanism specified" = "maroon", 
                    "Other unintentional injury, mechanism unspecified" = "seagreen2",
                    "Unintentional falls" = "dodgerblue2", "Unintentionally struck by or against an object" = "turquoise2")



### How do the different causes look across the years?-------------------------


# Make it into a tsibble
tbi_year_tsibble <- tbi_year %>% 
  as_tsibble(key = c("type", "injury_mechanism") , index = "year")

# reorder factor levels
tbi_year_tsibble$type <- factor(tbi_year_tsibble$type, levels = c("Emergency Department Visit","Hospitalizations","Deaths"))

tbi_year_tsibble %>% 
  filter(injury_mechanism !="Total") %>% 
  ggplot() +
  geom_line(aes(x = year, y = rate_est, color = injury_mechanism), size = 2) +
  labs(title = "Prevalence of different injury mechanisms across years") +
  xlab("Year") + 
  ylab("Rate/100,000") +
  theme_ipsum_rc() + 
  scale_colour_manual(values = injury_colours) +
  facet_wrap(~type, scales = "free_y") + # make the y scales independent
  theme(legend.position="bottom") +
  theme(legend.title = element_blank())


ggsave("causes_time.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 11, height = 6, units = "in")


### Did any causes get more prevalent compared to other causes?
# Left for later, need to learn plyr 


### What are the most common causes across different age groups?
top_age <- tbi_age %>% 
  group_by(age_group, type) %>% 
  arrange(rate_est) %>% 
  top_n(1)

top_age$type <- factor(top_age$type, levels = c("Emergency Department Visit","Hospitalizations","Deaths"))

top_age$age_group <- factor(top_age$age_group, levels = c("0-4", "5-14", "0-17", "15-24", 
                                                          "25-34", "35-44", "45-54", "55-64", "66-74", "75+", "Total"))

# new colour scale to match the factor level spelling
injury_colours1 <- c("Assault" = "firebrick1", 
                     "Intentional self-harm" = "darkorange2", 
                    "Motor Vehicle Crashes" = "darkgoldenrod2", 
                    "Unintentional Falls" = "dodgerblue2", 
                    "Unintentionally struck by or against an object" = "turquoise2")


#graph
top_age %>% 
  filter(age_group != "0-17") %>%
  filter(age_group != "Total") %>% 
  ggplot(aes(fct_rev(age_group), rate_est)) + # reversed order of factors to get younger at top
  geom_bar(stat = "identity", aes(fill = injury_mechanism)) + 
  coord_flip() +
  facet_wrap(~type, scales = "free_x") +
  scale_fill_manual(values = injury_colours1, drop = FALSE) +
  labs(title = "Most common injury mechanism by age group and source type") +
  xlab("Age group") + 
  ylab("Rate/100,000") +
  theme_ipsum_rc() +
  theme(legend.title = element_blank())

ggsave("causes_age.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 11, height = 6, units = "in")

# table
top_age %>%
  select(age_group, type, injury_mechanism, rate_est) %>%
  filter(age_group != "0-17") %>% 
  arrange(age_group, type) %>%
  kable("html", escape = FALSE, align = "c", caption = "Causes by age") %>%
  kable_styling(bootstrap_options = 
                  c("striped", "condensed", "bordered"), 
                full_width = FALSE)


