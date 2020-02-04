# Get the Data

sf_trees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')

library(tidyverse)
library(lubridate)
library(hrbrthemes)
library(stringr)
library(treemapify)

summary(sf_trees)

#change relevant things to factors
sf_trees$legal_status <- as.factor(sf_trees$legal_status)
sf_trees$species <- as.factor(sf_trees$species)
sf_trees$site_info <- as.factor(sf_trees$site_info)
sf_trees$caretaker <- as.factor(sf_trees$caretaker)
sf_trees$legal_status <- as.factor(sf_trees$legal_status)

#add a decade variable to make things look more legible
sf_trees$decade <- floor(sf_trees$year / 10) * 10

#make a genus variable to collapse individual species
sf_trees$genus <- word(sf_trees$species, 1)
sf_trees$genus <- as.factor(sf_trees$genus)

#make a common name variable
sf_trees$common <- word(sf_trees$species, 2, sep = fixed('::'))
sf_trees$common <- as.factor(sf_trees$common)


# how many trees are planted each year? ------------------------------------------------------------
sf_trees$year <- year(sf_trees$date)
sf_trees$year <- as.numeric(sf_trees$year)

planting <- sf_trees %>% subset(year != 2020) %>%  count(year)
summary(planting)

p1 <- ggplot(planting, aes(year, n))

p1 + geom_line(size = 1, col= "chartreuse4", na.rm = TRUE) +  coord_cartesian(ylim = c(0, 5000)) +
  labs(y= "Number of Trees Planted", x = "Year") + ggtitle("Tree planting across the last 60 years") +
  theme_ft_rc()

# Which species are the most popular across decades?-------------------------------------------------


by_decade <- sf_trees%>%  
  subset(year != 2020) %>%
  filter(!grepl("Tree(s)", species, fixed = TRUE)) %>% 
  group_by(decade, common) %>%
  summarise(Freq = n())


summary(by_decade)

#select top 10 species by decade
top_10 <- by_decade %>%
  group_by(decade) %>%
  top_n(10, Freq)

summary(top_10)

# make a treeplot

ggplot(top_10, aes(area = Freq, label = common, fill = as.factor(decade))) +
  geom_treemap(show.legend = FALSE) +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T) +
  facet_wrap(~ decade)  + 
  ggtitle("Top species across the last 7 decades") +
  theme_ft_rc()


# Same for genus-------------------------
by_decade_g <- sf_trees%>%  
  subset(year != 2020) %>%
  filter(!grepl("Tree(s)", species, fixed = TRUE)) %>% 
  group_by(decade, genus) %>%
  summarise(Freq = n())


summary(by_decade_g)

#select top 10 genus by decade
top_10_g <- by_decade_g %>%
  group_by(decade) %>%
  top_n(10, Freq)

summary(top_10_g)

# make a treeplot

ggplot(top_10_g, aes(area = Freq, label = genus, fill = as.factor(decade))) +
  geom_treemap(show.legend = FALSE) +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T) +
  facet_wrap(~ decade)  + 
  ggtitle("Top genus across the last 7 decades") +
  theme_ft_rc()

# try geographical mapping--------------------------

