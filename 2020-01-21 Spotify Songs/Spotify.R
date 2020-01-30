#### http://www.sthda.com/english/wiki/be-awesome-in-ggplot2-a-practical-guide-to-be-highly-effective-r-software-and-data-visualization#geom_smooth-add-regression-line-or-smoothed-conditional-mean


# Get the Data

library(tidyverse)

spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')


summary(spotify_songs)
spotify_songs


### RQ 1: Have songs gotten lounder over time?

#change date to an actual date
library(lubridate)
spotify_songs$track_album_release_date_fix <- ymd(spotify_songs$track_album_release_date)

#change to just years
spotify_songs$track_album_release_year <- year(spotify_songs$track_album_release_date_fix)

spotify_songs$track_album_release_year <- as.character(spotify_songs$track_album_release_year)

#replace the NA's with year from the original column

spotify_songs$track_album_release_year <- coalesce(spotify_songs$track_album_release_year, spotify_songs$track_album_release_date)

spotify_songs$track_album_release_year <- as.numeric(spotify_songs$track_album_release_year)


# change genres to factors

spotify_songs$playlist_genre <- as.factor(spotify_songs$playlist_genre)
spotify_songs$playlist_subgenre <- as.factor(spotify_songs$playlist_subgenre)

# 31 records are NA
which(is.na(spotify_songs$track_album_release_year)) 


# create a basic plot instruction for which variables I'm plotting
b <- ggplot(spotify_songs, aes(x = track_album_release_year, y = loudness))

# specify a scatter plot with a regression line
b + geom_point(alpha = 0.1,  position = position_jitter()) + geom_smooth(method = lm) +
  labs(y= "Loudness", x = "Release year")

###Quantify the effect

#check for outliers
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(spotify_songs$track_album_release_year, main="Year", sub=paste("Out: ", boxplot.stats(spotify_songs$track_album_release_year)$out))  # box plot for 'speed'
boxplot(spotify_songs$loudness, main="Loudness", sub=paste("Out: ", boxplot.stats(spotify_songs$loudness)$out))  # box plot for 'distance'

# check normality
library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(spotify_songs$track_album_release_year, na.rm=TRUE), main="Density Plot: Year", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(spotify_songs$track_album_release_year), 2)))  # density plot for 'speed'
polygon(density(spotify_songs$track_album_release_year), col="cornsilk4")
plot(density(spotify_songs$loudness), main="Density Plot: Loudness", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(spotify_songs$loudness), 2)))  # density plot for 'dist'
polygon(density(spotify_songs$loudness), col="cornsilk4")

#run correlation
cor.test(spotify_songs$track_album_release_year, spotify_songs$loudness)

###RQ2: Are all genres gettin louder?

#atach permanent colours to the genres
library(RColorBrewer)
myColours <- brewer.pal(6, "Set2")
names(myColours) <- levels(spotify_songs$playlist_genre)
colScale <- scale_colour_manual(name = "genre",values = myColours)

# make the graphs

g1 <- spotify_songs %>%
  filter(playlist_genre == "edm") %>%
  ggplot(aes(x = track_album_release_year, y = loudness, colour = playlist_genre)) +
  geom_point(alpha = 0.1,  position = position_jitter()) + geom_smooth(method = 'lm') + 
  colScale + 
  coord_cartesian( ylim = c(-50, 5)) +
  labs(y= "Loudness", x = "Release year")


g2 <- spotify_songs %>%
  filter(playlist_genre == "latin") %>%
  ggplot(aes(x = track_album_release_year, y = loudness, colour = playlist_genre)) +
  geom_point(alpha = 0.1,  position = position_jitter()) + geom_smooth(method = lm) + 
  colScale + 
  coord_cartesian( ylim = c(-50, 5)) +
  labs(y= "Loudness", x = "Release year")

g3 <- spotify_songs %>%
  filter(playlist_genre == "pop") %>%
  ggplot(aes(x = track_album_release_year, y = loudness, colour = playlist_genre)) +
  geom_point(alpha = 0.1,  position = position_jitter()) + geom_smooth(method = lm) + 
  colScale + 
  coord_cartesian( ylim = c(-50, 5)) +
  labs(y= "Loudness", x = "Release year")


g4 <- spotify_songs %>%
  filter(playlist_genre == "r&b") %>%
  ggplot(aes(x = track_album_release_year, y = loudness, colour = playlist_genre)) +
  geom_point(alpha = 0.1,  position = position_jitter()) + geom_smooth(method = lm) + 
  colScale + 
  coord_cartesian( ylim = c(-50, 5)) +
  labs(y= "Loudness", x = "Release year")

g5 <- spotify_songs %>%
  filter(playlist_genre == "rap") %>%
  ggplot(aes(x = track_album_release_year, y = loudness, colour = playlist_genre)) +
  geom_point(alpha = 0.1,  position = position_jitter()) + geom_smooth(method = lm) + 
  colScale + 
  coord_cartesian( ylim = c(-50, 5)) +
  labs(y= "Loudness", x = "Release year")

g6 <- spotify_songs %>%
  filter(playlist_genre == "rock") %>%
  ggplot(aes(x = track_album_release_year, y = loudness, colour = playlist_genre)) +
  geom_point(alpha = 0.1,  position = position_jitter()) + geom_smooth(method = lm) + 
  colScale + 
  coord_cartesian( ylim = c(-50, 5)) +
  labs(y= "Loudness", x = "Release year")

library(gridExtra)
grid.arrange(g1, g2, g3, g4, g5, g6, top="Song loudness across time by genres", ncol=2)


#look at overall genre diferences

library(Hmisc)

p <- ggplot(spotify_songs, aes(x=playlist_genre, y=loudness, fill=playlist_genre)) +
  geom_violin(alpha = 0.5) + stat_summary(fun.data=mean_sdl, mult=1, geom="pointrange")

#not sure why I'm getting errors :(


# Make a model with genre and year as predictors of loudness

model <- lm(loudness ~ track_album_release_year + playlist_genre, data = spotify_songs)
anova(model)
summary(model)

#Looks like year is a big predictor, and then not being edm is significantly releated to being quieter. 

#####RQ3: Are there seasonal trends in songs?

# extract a month factor in as well!
spotify_songs$track_album_release_month <- month(spotify_songs$track_album_release_date_fix)

