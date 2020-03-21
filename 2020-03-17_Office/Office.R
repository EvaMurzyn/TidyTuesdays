# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-03-17/readme.md

#install.packages("schrute")
library(tidyverse)
library(schrute)
library(RColorBrewer)
library(hrbrthemes)
library(forcats)
library(tidytext)


office_ratings <- readr::read_csv('office_ratings.csv')
scripts <- schrute::theoffice

summary(scripts)
summary(office_ratings)

### Look at episode ratings across time-------------------

office_ratings %>% 
  ggplot(aes(episode, imdb_rating)) +
  geom_bar(aes(fill = as.factor(season)), stat = "identity", colour = "black") +
  facet_wrap(vars(season)) +
  theme_modern_rc() +
    labs(title = "The Office ratings", subtitle = "for each spisode in every season") +
  ylab("Average IMBD rating") + 
  theme(legend.position="bottom") + 
  scale_fill_discrete(name="",
                      breaks=c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
                      labels=c("Season 1", "Season 2","Season 3","Season 4",
                               "Season 5","Season 6","Season 7","Season 8","Season 9")) +
  scale_fill_brewer(palette = "Spectral")

ggsave("ratings.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 6, height = 6, units = "in")

### Combine the dataframes to look at ratings -------------------
# vs director, writer and speaking character


# Who has most speaking lines?
scripts$character <- as.factor(scripts$character)

scripts %>%
  group_by(character) %>% 
  tally() %>% 
  top_n(20) %>%
  arrange(desc(n)) %>% 
  ggplot(aes(fct_reorder(character, n), n)) +
  geom_bar(stat = "identity") +
  coord_flip() + 
  theme_modern_rc() +
  labs(title = "Speaking lines for all the episodes") +
  xlab("Character") + 
  ylab("Lines spoken") +
  scale_fill_brewer(palette = "Spectral")

ggsave("lines.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 6, height = 6, units = "in")

### Selecting just lines from top 20 speakers

top_speakers <- scripts %>%
  group_by(character) %>% 
  tally() %>% 
  top_n(20) %>% 
  select(-n)

top_s <- as.character(top_speakers$character)

# make a new dataframe that only contains the top 20 speaking characters
series <- scripts %>% 
  select(season, episode, director, writer, character, text) %>% 
  filter(character %in% top_s) %>% 
  mutate(speaking = 1)


# which writer gave most lines to which character?

# See how many episodes each writer has made
writers <- series %>% 
  group_by(writer, episode, season) %>% 
  tally() %>% 
  mutate(episode_count = 1) %>% 
  group_by(writer) %>% 
  summarise(episodes = sum(episode_count))

# Remove joint authorship cases
writers <- writers[ !grepl(";", writers$writer),]

# Get total sum of times each character spoke for each writer
writer_lines <- series %>%
  group_by(writer, character) %>% 
  summarise(lines = sum(speaking))

# Calculate average speaking rate per episode
writer_lines_a <- writer_lines %>% 
  inner_join(writers) %>% 
  mutate(average_lines = lines/episodes)
  
# Graph every writer's top speaking character  
writer_lines_a %>%
  select(writer, character, average_lines) %>% 
  group_by(writer) %>% 
  top_n(n=1, wt = average_lines) %>% 
  arrange(desc(average_lines)) %>% 
  ggplot(aes(fct_reorder(writer, average_lines), average_lines, fill = character)) +
  geom_bar(stat = "identity", aes(fill = character), colour = "black") +
  coord_flip() +
  scale_fill_brewer(palette = "Spectral") + 
  theme_modern_rc() +
  labs(title = "Who talks most in each writer's work?", subtitle = "for episodes with only one writer") +
  ylab("Average lines per episode written") + 
  xlab("Writer")

ggsave("writer_lines.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 6, height = 6, units = "in")

### Sentiment analysis-------------------------
# Are there differences in the mood by writer?

#select just some columns
tidy_scripts <- scripts %>% 
  select(season, episode, writer, character, text)

#remove episodes with joint authorship
tidy_scripts <- tidy_scripts[ !grepl(";", tidy_scripts$writer),]

#fix contractions
fix.contractions <- function(doc) {
  # "won't" is a special case as it does not expand to "wo not"
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
  return(doc)
}

tidy_scripts$text <- sapply(tidy_scripts$text, fix.contractions)


# remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)

tidy_scripts$text <- sapply(tidy_scripts$text, removeSpecialChars)

# change to lowercase
tidy_scripts$text <- sapply(tidy_scripts$text, tolower)


tidy_scripts_filtered <- tidy_scripts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  distinct()

# most frequently used words

tidy_scripts_filtered %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n)) +
  xlab("") + 
  ylab("Song Count") +
  ggtitle("Most Frequently Used Words in The Office") +
  scale_fill_brewer(palette = "Spectral") + 
  theme_modern_rc() +
  coord_flip()

ggsave("common_words.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 6, height = 6, units = "in")


# Whose writing is most lexically dense?

lex_density_per_writer <- tidy_scripts %>%
  unnest_tokens(word, text) %>%
  group_by(writer, episode) %>%
  summarise(lex_density = n_distinct(word)/n()) %>%
  arrange(desc(lex_density))

nb.cols <- 32
mycolors <- colorRampPalette(brewer.pal(8, "Spectral"), (nb.cols))

lex_density_per_writer %>% 
  ggplot(aes(lex_density, writer)) +
  geom_point(aes(color = writer)) +
  geom_label(label = "Highest lexical density",
             x=0.36,
             y= "Greg Daniels",
             color = "gray90",
             fill = "gray5",
             label.padding = unit(0.35, "lines")) +
  scale_fill_manual(values = mycolors) + 
  theme_modern_rc() +
  xlab("Lexical density") + 
  ylab("Writer") +
  ggtitle("Lexical density of episodes by writer") +
  theme(legend.position = "none")
  
ggsave("lexical_density.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 8, height = 8, units = "in")  


# Overall mood plot

office_nrc <- tidy_scripts %>%
  unnest_tokens(word, text) %>% 
  inner_join(get_sentiments("nrc"))

office_nrc %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>% 
  ggplot(aes(fct_reorder(sentiment, word_count), word_count)) +
  geom_bar(stat = "identity", aes(fill = sentiment), color = "black") +
  guides(fill = FALSE) + #Turn off the legend
  labs(x = NULL, y = "Word Count") +
  ggtitle("Office NRC Sentiment") +
  theme_modern_rc() +
  scale_fill_brewer(palette = "Spectral") + 
  coord_flip()

# Look at positive/negative rations by writer

writer_nrc <- office_nrc %>%
  group_by(sentiment, writer) %>%
  summarise(word_count = n())%>% 
  filter(sentiment %in% c("positive","negative")) %>% 
  pivot_wider(names_from = sentiment, values_from = word_count) %>% 
  mutate(ratio = (positive/negative))
 
writer_nrc %>% 
  ggplot(aes(fct_reorder(writer, ratio), ratio)) +
  geom_bar(stat = "identity", color = "black") +
  coord_flip() +
  labs(y = "Ratio of positive words for each negative", x = NULL) +
  ggtitle("Who is the most positive writer?") +
  theme_modern_rc() +
  scale_fill_brewer(palette = "Spectral")

ggsave("nrc_writer.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 6, height = 6, units = "in")  
