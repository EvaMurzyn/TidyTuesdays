# TidyTuesday 
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-02-11/readme.md

# https://tidyverts.org/ - tools for time series

library(tidyverse)
library(lubridate)
library(tsibble)
library(feasts)
library(fable)
library(countrycode)
library(hrbrthemes)
library(RColorBrewer)
library(gridExtra)

#General tidying -------------------------------------------
hotels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv')

summary(hotels)
str(hotels)
head(hotels)

#change relevant variables into factors and make a variable for total length of stay
hotels1 <- hotels %>% 
  mutate_at(vars(hotel, meal, market_segment, distribution_channel, 
                                  reserved_room_type, assigned_room_type, deposit_type,
                                  customer_type, reservation_status, customer_type, deposit_type, 
                                  agent, company, market_segment, distribution_channel), as.factor) %>% 
  mutate(total_stay = stays_in_weekend_nights + stays_in_week_nights)

#create a proper date of planned arrival
hotels1$arrival_date <- ymd(paste(hotels1$arrival_date_year,
                                  hotels1$arrival_date_month,
                                  hotels1$arrival_date_day_of_month, sep=" "))


#change the country codes into proper names. Categories are represented in the ISO 3155-3:2013 format
?codelist
hotels1$country <- countrycode(hotels$country, "iso3c", "un.name.en", warn = TRUE) %>% 
                  as.factor()


str(hotels1)
summary(hotels1)


# Who are the no-shows? Lead Time--------------------------------------------------

# lead time - analyse
options(scipen=999)

group_by(hotels1, reservation_status) %>%
  summarise(
    count = n(),
    mean = mean(lead_time, na.rm = TRUE),
    sd = sd(lead_time, na.rm = TRUE)
  )

lead.aov <- aov(lead_time ~ reservation_status, data = hotels1)
summary(lead.aov)
TukeyHSD(lead.aov)

# graph this

pl <- ggplot(hotels1, aes(reservation_status, lead_time))

pl + geom_boxplot(aes(fill = reservation_status), show.legend = FALSE) + 
  xlab("Reservation status") + ylab("Lead time") + 
  labs(title = "Hotel no-shows have significantly lower lead times than other bookings... ",
       subtitle = "F=6155, p<0.001") + 
  theme_ipsum_rc() + scale_fill_brewer(palette = "Set2")

ggsave("lead_time.png", plot = last_plot(), device = "png", path = NULL, scale = 0.5, width = 6, height = 6, units = "in")


# Who are the no-shows? People in the party--------------------------------------------------

adults <- group_by(hotels1, reservation_status) %>%
  summarise(
    count = n(),
    mean = mean(adults, na.rm = TRUE),
    sd = sd(adults, na.rm = TRUE)
  )

adults.aov <- aov(adults ~ reservation_status, data = hotels1)
summary(adults.aov)
TukeyHSD(adults.aov)

pa <- ggplot(adults, aes(x=reservation_status, y=mean, fill=reservation_status)) + 
  geom_bar(stat="identity", position=position_dodge(),show.legend = FALSE)  +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2, position=position_dodge(.9)) + 
  theme_ipsum_rc() + scale_fill_brewer(palette = "Set2") +
  xlab("Reservation status") + ylab("Mean number of adults on the booking") + 
  labs(title = "Hotel no-shows have significantly fewer adults... ",
       subtitle = "F=322.3, p<0.001")
  
pa
  
ggsave("adults.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 6, height = 6, units = "in")
  
  
babies <- group_by(hotels1, reservation_status) %>%
  summarise(
    count = n(),
    mean = mean(babies, na.rm = TRUE),
    sd = sd(babies, na.rm = TRUE)
  )

babies.aov <- aov(babies ~ reservation_status, data = hotels1)
summary(babies.aov)
TukeyHSD(babies.aov)

pb <- ggplot(babies, aes(x=reservation_status, y=mean, fill=reservation_status)) + 
  geom_bar(stat="identity", position=position_dodge(),show.legend = FALSE)  +
  geom_errorbar(aes(ymin=mean, ymax=mean+sd), width=.2, position=position_dodge(.9)) + 
  theme_ipsum_rc() + scale_fill_brewer(palette = "Set2") +
  xlab("Reservation status") + ylab("Mean number of babies on the booking") + 
  labs(title = "but have the same number of babies... ",
       subtitle = "p>0.05")

pb

ggsave("babies.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 6, height = 6, units = "in")

children <- group_by(hotels1, reservation_status) %>%
  summarise(
    count = n(),
    mean = mean(children, na.rm = TRUE),
    sd = sd(children, na.rm = TRUE)
  )


children.aov <- aov(children ~ reservation_status, data = hotels1)
summary(children.aov)
TukeyHSD(children.aov)

pc <- ggplot(children, aes(x=reservation_status, y=mean, fill=reservation_status)) + 
  geom_bar(stat="identity", position=position_dodge(),show.legend = FALSE)  +
  geom_errorbar(aes(ymin=mean, ymax=mean+sd), width=.2, position=position_dodge(.9)) + 
  theme_ipsum_rc() + scale_fill_brewer(palette = "Set2") +
  xlab("Reservation status") + ylab("Mean number of children on the booking") + 
  labs(title = "and children as the other two categories.",
       subtitle = "p>0.05")

pc

ggsave("children.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 6, height = 6, units = "in")


grid.arrange(pa, pb, pc, nrow=1)

# Who are the no-shows? Deposit--------------------
# Work in progress - need to make new data tables with % of each category having no deposit, non-refund, or refund deposit

dep <- ggplot(hotels1, aes(deposit_type, reservation_status))

dep + geom_jitter(fill=reservation_status)

# What's the no-show seasonal pattern? ----------------------------------
# Code largely taken from https://github.com/conkline/TidyTuesdayScripts/blob/master/tidytuesday2102020_hotels.R

#construct tstibble of no-shows per month. We're excluding the cancellations.
hotels_ts <- hotels1 %>%
  filter(reservation_status != "Canceled") %>% 
  select(reservation_status, arrival_date) %>%
  mutate(rows = rownames(.))

# Make a new variable in which all no-shows are coded as 1, and everything else is 0
hotels_ts <- mutate(hotels_ts, no_show = ifelse(grepl("No-Show", reservation_status), "1","0"))

hotels_ts$no_show <- as.numeric(hotels_ts$no_show)

hotels_ts1 <- as_tsibble(hotels_ts,
                        index = "arrival_date",
                        interval = "Daily",
                        key = "rows") %>%
  index_by(year_month = ~ yearmonth(.)) %>% # monthly aggregates
  summarize(
    perc_no_shows = sum(no_show, na.rm = TRUE) / length(no_show)
  )



#set theme
theme_set(theme_bw())

#extract time series features and plot by season
hotel_features <- hotels_ts1 %>% features(perc_no_shows, feat_stl)

# create the ggplot
seasonal_plot <- hotels_ts1 %>% 
  mutate(year = year(year_month)) %>%
  mutate(month = month(year_month, label = T, abbr = T)) %>%
  ggplot() + geom_line(aes(x=month, y=perc_no_shows,
                           group=as.character(year),
                           color = as.character(year)), size = 2) + 
  scale_color_discrete(name = "Year") + xlab("") + 
  ylab("% No shows out of non-canceled bookings") + 
  labs(title = "Hotel no-shows may have a slight seasonal trend...",
       subtitle = "STL seasonal strength = 0.790") + theme_ipsum_rc() + scale_color_brewer(palette = "Set2")

seasonal_plot

ggsave("seasonal_plot.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 10, height = 6, units = "in")
