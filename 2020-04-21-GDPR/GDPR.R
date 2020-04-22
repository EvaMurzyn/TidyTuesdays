library(tidyverse)
library(forcats)
library(hrbrthemes)
library(RColorBrewer)
library(gt)
library(webshot)


gdpr_violations <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')
gdpr_text <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_text.tsv')

str(gdpr_violations)

options(scipen = 999) #remove scientific notation


###Most expensive violation type by country!------------------

exp_country <- gdpr_violations %>% 
  group_by(name) %>% 
  arrange(price) %>% 
  top_n(1, price) %>% 
  ungroup() %>% 
  distinct()

# create a grouping factor to create our 2 graphs
exp_country$name <- as.factor(exp_country$name)
exp_country <- exp_country %>% mutate(price_size = 
                         ifelse(price > 2000000, "High", "Low"))

#graph of all
exp_country %>% 
  filter(id !=8) %>% # removing double Greece entry
  mutate(name = fct_reorder(name, price)) %>%
  mutate(price = price/1000) %>% 
  ggplot(aes(x = name, y = price, fill = type)) +
  geom_bar(stat = "identity") +
  labs(title = "Largest GDPR violations for each country") +
  xlab("Country") + ylab("Fine in thousands of Euro") +
  coord_flip() +
  theme_ipsum() +
  scale_fill_brewer(palette = "Dark2")

  

# just the smaller fines [zoom!]
exp_country %>% 
  filter(id !=8) %>% # removing double Greece entry
  filter(price_size == "Low") %>% 
  mutate(name = fct_reorder(name, price)) %>%
  mutate(price = price/1000) %>% 
  ggplot(aes(x = name, y = price, fill = type)) +
  geom_bar(stat = "identity") +
  labs(title = "Largest GDPR violations for each country") +
  xlab("Country") + ylab("Fine in thousands of Euro") +
  coord_flip() +
  theme_ipsum() +
  scale_fill_brewer(palette = "Dark2")


# Two graphs were pasted together using paint....

### average GDPR fines per type------------------------

#fix error in data entry in id == 99, 89 and 37

gdpr_violations[gdpr_violations$id == 99, 9] = "Failure to implement sufficient measures to ensure information security"
gdpr_violations[gdpr_violations$id == 99, 8] = "Art. 32 GDPR"
gdpr_violations[gdpr_violations$id == 89, 9] = "Unknown"
gdpr_violations[gdpr_violations$id == 37, 9] = "Unknown"


exp_type <- gdpr_violations %>% 
  mutate(count = 1) %>% 
  group_by(type) %>%
  summarise(count = sum(count), mean_fine = mean(price), min = min(price), max = max(price)) %>% 
  arrange(desc(mean_fine))


#gt table of mean, min and max fines by type
gt <- exp_type %>% 
  gt(rowname_col = "type")  %>% 
  tab_header(title = "Mean GDPR fines per violation type") %>% 
  tab_stubhead(label = "Violation type") %>% 
  cols_label(mean_fine = html("Mean fine"), count = html("Number of incidents"), 
             min = html("Minimum fine"), max = html("Maximum fine")) %>% 
  fmt_currency(columns = vars(mean_fine, min, max), currency = "euro")

gt %>% gtsave("table.png")
