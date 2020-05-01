# https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-04-28

library(plyr)
library(tidyverse)
library(tsibble)
library(hrbrthemes)
library(paletteer) # https://evamaerey.github.io/ggplot2_grammar_guide/paletteer#1


grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv', guess_max = 40000)
#synopses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/synopses.csv')
cpi <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/cpi.csv')
#pre_1985_starts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/pre-1985-starts.csv')


options(scipen = 999)

# Adjust all prices for inflation: 
# Multiply by CPI and divide by 100
# https://towardsdatascience.com/the-what-and-why-of-inflation-adjustment-5eedb496e080

#Step 1 - extract year and month date from weekly and cpi

grosses$year_month <- format(as.Date(grosses$week_ending), "%Y-%m")
grosses$year <- format(as.Date(grosses$week_ending), "%Y")
cpi$year_month <- format(as.Date(cpi$year_month), "%Y-%m")

summary(cpi)

# Step 2 join frames

grosses_adj <- left_join(grosses, cpi)


#Step 3 - calculate the cpi adjusted values for all prices

grosses_adj <- grosses_adj %>% 
  mutate(weekly_gross = (weekly_gross/cpi*100)) %>% 
  mutate(weekly_gross_overall = (weekly_gross_overall/cpi*100)) %>% 
  mutate(avg_ticket_price = (avg_ticket_price/cpi*100)) %>% 
  mutate(top_ticket_price = (top_ticket_price/cpi*100))


# Change theatre and show to factors

grosses_adj$show <- as.factor(grosses_adj$show)
grosses_adj$theatre <- as.factor(grosses_adj$theatre)

summary(grosses_adj)


#### Question 1 : Number of shows per month and tickets sold-----------------------

grosses_adj$year_month <- yearmonth(grosses_adj$year_month)

monthly <- grosses_adj %>%
  filter(week_ending != "2020-03-01") %>% # removing this since this month is incomplete and artficially low
  mutate(count = 1) %>% 
  group_by(year_month) %>%
  summarise(total_shows = sum(count), monthly_seats = sum(seats_sold)) %>% 
  as_tsibble(index = year_month) %>% 
  mutate(total_seat_bin = case_when(
    monthly_seats <500000 ~ "Under 500,000",
    monthly_seats <750000 ~ "Under 750,000",
    monthly_seats <1000000 ~ "Under 1,000,000",
    monthly_seats <1250000 ~ "Under 1,250,000",
    monthly_seats <1500000 ~ "Under 1,500,000",
    monthly_seats >1500000 ~ "Over 1,500,000")) # binning the seats because I want to use a pretty palette

 
monthly$total_seat_bin <- fct_relevel(monthly$total_seat_bin, "Under 500,000","Under 750,000", 
                                      "Under 1,000,000", "Under 1,250,000", 
                                      "Under 1,500,000", "Over 1,500,000")

monthly %>% 
  ggplot(aes(year_month, total_shows, color = total_seat_bin)) +
  geom_point() +
  geom_smooth(method = "lm", color = "azure4") +
  labs(title = "Numbers of titles running  and total seats sold per month", 
  y = "Total shows", x = "Year", color = "Total seats sold") +
  theme_ft_rc() +
  scale_color_paletteer_d("vapeplot::mallsoft") +
  guides(color = guide_legend(reverse=T))

ggsave("show_numbers.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 10, height = 7, units = "in")


# Q2: Who has been selling out? ---------------------------
#Shows which have usually sold over capacity

full_house <- grosses_adj %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(show) %>% 
  summarise(mean_capacity = mean(pct_capacity), seats_sold = sum(seats_sold), ticket_price = mean(avg_ticket_price)) %>%
  filter(mean_capacity > 1) %>% 
  arrange(desc(mean_capacity)) %>% 
  mutate(price_bin = case_when(
    ticket_price < 20 ~ "Under $20",
    ticket_price < 40 ~ "Under $40",
    ticket_price < 60 ~ "Under $60",
    ticket_price < 80 ~ "Under $80",
    ticket_price < 99 ~ "Under $99")) # again, binning for visual effect
  
full_house %>%
  mutate(show = fct_reorder(show, seats_sold)) %>%
  ggplot(aes(show, seats_sold, fill = price_bin)) +
  geom_bar(stat = "identity") +
  coord_flip() + 
  labs(title = "Shows that have been usually selling over capacity", subtitle = "(on average between 1985-2020)", 
       y = "Seats sold", x = "Show", fill = "Average ticket price") +
  theme_ft_rc() +
  scale_fill_paletteer_d("vapeplot::macplus") +
  guides(fill = guide_legend(reverse=T))

ggsave("full_house.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 10, height = 7, units = "in")


# Q4: What shows have been running non stop? What are their qualities?-------------------

evergreen <- grosses_adj %>% 
  mutate(year = as.numeric(year)) %>%
  group_by(year, show) %>% 
  summarise(mean_capacity = mean(pct_capacity), seats_sold = sum(seats_sold), ticket_price = mean(avg_ticket_price)) %>% 
  mutate(present = 1) %>% 
  group_by(show) %>% 
  summarise(total_years = sum(present), mean_capacity = mean(mean_capacity), seats_sold = sum(seats_sold), ticket_price = mean(ticket_price)) %>% 
  filter(total_years >= 10) %>%  pivot_longer(-c(show, total_years), names_to = "measure", values_to = "value") %>%
  mutate(measure = as.factor(measure))

#tidy up the factor levels- change order and rename
evergreen$measure <- fct_relevel(evergreen$measure,"seats_sold", "ticket_price", "mean_capacity")
evergreen$measure <- revalue(evergreen$measure, c("seats_sold" = "Seats sold", 
                                                  "ticket_price" = "Average ticket price", 
                                                  "mean_capacity" = "Mean theatre capacity filled"))

evergreen %>% 
  mutate(show = fct_reorder(show, total_years)) %>%
  mutate(total_years = as.factor(total_years)) %>% 
  ggplot(aes(x = show, y = value, fill = total_years)) +
  geom_bar(stat = "identity") +
  facet_wrap(~measure, scales = "free_x") +
  coord_flip() +
  theme_ft_rc() +
  scale_fill_paletteer_d("vapeplot::vaporwave") +
  guides(fill = guide_legend(reverse=T)) +
  labs(title = "Shows that have been running for more than 10 years total", 
       x = "Show", fill = "Years running")

# ggsave("evergreens.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 12, height = 7, units = "in")
