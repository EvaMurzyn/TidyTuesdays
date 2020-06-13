library(tidyverse)
library(hrbrthemes)
library(paletteer)

firsts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/firsts.csv')

### fix errors ###--------------- 

##Automatic tidying by keywords

ind <- grep("Parks|secretary|union|Fire|whaleship", firsts$accomplishment)
firsts$category[ind] <- "Social & Jobs"


ind <- grep("college|Psychiatric|Greek|graduate", firsts$accomplishment)
firsts$category[ind] <- "Education & Science"

ind <- grep("solicitor|Solicitor|attorney|NYPD|court|police|Court", firsts$accomplishment)
firsts$category[ind] <- "Law"


ind <- grep("NBA|coach|Basketball|Baseball|Super Bowl|basketball|Football|football|NFL|Olympic|sports|Presidents Cup|
            Championship|North Pole|Mount Everest|baseball|player|American League MVP|Tennis", firsts$accomplishment)
firsts$category[ind] <- "Sports"


ind <- grep("ambassador|Ambassador|embassy|policy|Policy|ballot|Surgeon|legislature|Foreign|public office|Presidential debate", firsts$accomplishment)
firsts$category[ind] <- "Politics"

ind <- grep("Poet|Literature|Actor|best-selling|film|museum|game show|Illustrated", firsts$accomplishment)
firsts$category[ind] <- "Arts & Entertainment"

ind <- grep("priest", firsts$accomplishment)
firsts$category[ind] <- "Religion"

ind <- grep("combat|West Point|Air Force", firsts$accomplishment)
firsts$category[ind] <- "Military"

### Timelines of black female and male firsts ### -----------------

#Get the earliest achievements

firsts_category <- firsts %>% 
  group_by(category) %>% 
  top_n(-1, year)


firsts %>% 
  ggplot(aes(year, category, shape = gender, color = category)) +
  geom_jitter(height = 0.25) +
  theme_ft_rc() +
  labs(title = "African American achievements across history", subtitle = "by achievement category and gender",
       x = "Year", y = "Category", shape = "Gender", color = "Achievement category") +
  annotate("text",
           x = 1735,
           y = 7.4, 
           label = "1738 - First free African-American community", 
           hjust = 0,
           size = 3,
           color = "gray80",
           family = "Roboto Condensed") +
  annotate("text",
           x = 1760,
           y = 1.4, 
           label = "1760 - First known African-American published author", 
           hjust = 0,
           size = 3,
           color = "gray80",
           family = "Roboto Condensed") +
  annotate("text",
           x = 1773,
           y = 6.4, 
           label = "1773 - First separate African-American church", 
           hjust = 0,
           size = 3,
           color = "gray80",
           family = "Roboto Condensed") +
  annotate("text",
           x = 1778,
           y = 4.4, 
           label = "1778 - First African-American U.S. military regiment", 
           hjust = 0,
           size = 3,
           color = "gray80",
           family = "Roboto Condensed") +
  annotate("text",
           x = 1783,
           y = 2.4, 
           label = "1783 - First African-American to formally practice medicine", 
           hjust = 0,
           size = 3,
           color = "gray80",
           family = "Roboto Condensed") +
  annotate("text",
           x = 1845,
           y = 3.4, 
           label = "1845 - First African-American licensed to practice law", 
           hjust = 0,
           size = 3,
           color = "gray80",
           family = "Roboto Condensed") +
  annotate("text",
           x = 1847,
           y = 5.4, 
           label = "1847 - First African-American president of any nation", 
           hjust = 0,
           size = 3,
           color = "gray80",
           family = "Roboto Condensed") +
  annotate("text",
           x = 1879,
           y = 8.4, 
           label = "1899 - First African-American 
to achieve world championship in any sport", 
           hjust = 0,
           size = 3,
           color = "gray80",
           family = "Roboto Condensed") +
  scale_color_paletteer_d("dichromat::BluetoOrange.8")

ggsave("achievements.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 11, height = 9, units = "in")  


### Look at just education and science, and highlight female achievements ###------------------

science <- firsts %>% 
  filter(category == "Education & Science") %>% 
  mutate(decade = year - year %% 10) %>% 
  group_by(decade) %>% 
  mutate(index = row_number()) %>% 
  ungroup()


## Fix incorrect genders

ind <- which(science$accomplishment == "First institute of higher learning created to educate African-Americans")
science$gender[ind] <- "African-American Firsts"

ind <- which(science$accomplishment == "First fully state-supported four-year institution of higher learning for African-Americans")
science$gender[ind] <- "African-American Firsts"

ind <- which(science$person == "Sigma Pi Phi")
science$gender[ind] <- "African-American Firsts"

ind <- which(science$person == "Alpha Phi Alpha (ΑΦΑ), at Cornell Universitys")
science$gender[ind] <- "African-American Firsts"

ind <- grep("fraternity", science$accomplishment)
science$gender[ind] <- "African-American Firsts"

ind <- which(science$person == "Mary Eliza Mahoney, Boston, Massachusetts.[53]")
science$gender[ind] <- "Female African-American Firsts"

ind <- which(science$person == "Mabel Byrd[88]]")
science$gender[ind] <- "Female African-American Firsts"

ind <- which(science$person == "Edith Irby Jones[159]")
science$gender[ind] <- "Female African-American Firsts"

ind <- which(science$person == "Ruby Bridges")
science$gender[ind] <- "Female African-American Firsts"

ind <- which(science$person == "Lillian Lincoln")
science$gender[ind] <- "Female African-American Firsts"

ind <- which(science$person == "Ruth J. Simmons at Brown University")
science$gender[ind] <- "Female African-American Firsts"

ind <- which(science$person == "Dr. Carla Hayden[248]")
science$gender[ind] <- "Female African-American Firsts"

ind <- which(science$person == "Altha Stewart[250]")
science$gender[ind] <- "Female African-American Firsts"

ind <- which(science$gender == "Female African American Firsts")
science$gender[ind] <- "Female African-American Firsts"

## Pick out just the women for easier reference

f_science <- science %>% 
  filter(gender == "Female African-American Firsts") %>% 
  select(accomplishment, person, decade)

#Plot and annotate!

ggplot(science, aes(decade, 1, group = index, fill = gender)) +
  geom_bar(stat = 'identity') +
  theme_ft_rc() +
  coord_flip() +
  expand_limits(y = c(0, 25)) +
  scale_fill_manual(values = c("#FF8000FF", "#FFEE99FF")) +
  labs(title = "Firsts in Science and Education", 
       fill = "Gender", 
       y = "count", 
       subtitle = "Highlighting some of the women's accomplishments") +
  annotate("text",
           x = 1850,
           y = 7, 
           label = "Sarah Jane Woodson Early - first woman college instructor", 
           hjust = 0,
           size = 4,
           color = "gray80",
           family = "Roboto Condensed") +
  annotate("text",
           x = 1860,
           y = 7, 
           label = "Dr. Rebecca Davis Lee Crumpler - first woman to earn an M.D.", 
           hjust = 0,
           size = 4,
           color = "gray80",
           family = "Roboto Condensed") +
  annotate("text",
           x = 1870,
           y = 7, 
           label = "Mary Eliza Mahoney - first formally graduated nurse", 
           hjust = 0,
           size = 4,
           color = "gray80",
           family = "Roboto Condensed") +
    annotate("text",
           x = 1880,
           y = 7, 
           label = "Judy W. Reed - first woman to hold a patent", 
           hjust = 0,
           size = 4,
           color = "gray80",
           family = "Roboto Condensed") +
  annotate("text",
           x = 1890,
           y = 7, 
           label = "Ida Rollins - first woman with a dental degree", 
           hjust = 0,
           size = 4,
           color = "gray80",
           family = "Roboto Condensed") +
  annotate("text",
           x = 1900,
           y = 7, 
           label = "Alpha Kappa Alpha - first intercollegiate African-American sorority", 
           hjust = 0,
           size = 4,
           color = "gray80",
           family = "Roboto Condensed") +
    annotate("text",
           x = 1920,
           y = 7, 
           label = "Sadie Tanner Mossell - first PhD degree (in Economics)", 
           hjust = 0,
           size = 4,
           color = "gray80",
           family = "Roboto Condensed") +
  annotate("text",
           x = 1930,
           y = 7, 
           label = "Jane Matilda Bolin - first woman graduate from Yale Law", 
           hjust = 0,
           size = 4,
           color = "gray80",
           family = "Roboto Condensed")  +
  annotate("text",
           x = 1950,
           y = 7, 
           label = "Edith Irby Jones - first University of Arkansas graduate", 
           hjust = 0,
           size = 4,
           color = "gray80",
           family = "Roboto Condensed") +
  annotate("text",
           x = 1960,
           y = 7, 
           label = "Ruby Bridges - first to attend an all-white elementary school", 
           hjust = 0,
           size = 4,
           color = "gray80",
           family = "Roboto Condensed") +
  annotate("text",
           x = 1990,
           y = 7, 
           label = "Dr. Mae Jemison - first woman astronaut", 
           hjust = 0,
           size = 4,
           color = "gray80",
           family = "Roboto Condensed") +
  annotate("text",
           x = 2000,
           y = 7, 
           label = "Ruth J. Simmons - first president of an Ivy League university", 
           hjust = 0,
           size = 4,
           color = "gray80",
           family = "Roboto Condensed") +
  annotate("text",
           x = 2010,
           y = 7, 
           label = "Altha Stewart - first president of the American Psychiatric Association", 
           hjust = 0,
           size = 4,
           color = "gray80",
           family = "Roboto Condensed")


ggsave("science.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 11, height = 9, units = "in")  

  


