library(tidyverse)
library(janitor)

cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')
boston_cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')



### Data tidying-------------------

#are there cocktail duplicates across lists?

names_a <- boston_cocktails %>% 
  select(drink = name) %>% 
  unique()

names_b <- cocktails %>% 
  select(drink) %>% 
  unique()

names <- rbind(names_a, names_b)

duplicates <- names$drink[duplicated(names$drink)]

# we have 84 duplicates. Do they have the same recipes?

ingredients_a <- boston_cocktails %>%
  select(drink = name, ingredient) %>% 
  mutate(source = "boston")

ingredients_bt <- cocktails %>% 
  select(drink, ingredient) %>%
  mutate(source = "general")

ingredients <- rbind(ingredients_a, ingredients_b)

duplicate_drinks <- ingredients %>% 
  filter(drink %in% duplicates)

duplicate_drinks_1<- duplicate_drinks %>% 
  mutate(count = 1) %>% 
  select(-ingredient) %>% 
  pivot_wider(names_from = "ingredient_simple", values_from = "count")

# Sort of - lots of alternate names for ingredients!

# Remove the duplicates from general pool - Boston list seems more reliable.
`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))


ingredients_b <- cocktails %>% 
  select(drink, ingredient) %>%
  filter(drink %not in% duplicates) %>% 
  mutate(source = "general")

ingredients <- rbind(ingredients_a, ingredients_b)
# We have a lot of fancy names for various vodkas, rums etc. We need to tidy it.

ingredients$ingredient <- str_to_title(ingredients$ingredient)
ingredients$ingredient_simple <- ingredients$ingredient
ingredients$ingredient_simple <- str_replace(ingredients$ingredient, "Fresh |Canned ", "")

ingredients$ingredient_simple[grep("Rum|Rhum|Bacardi", ingredients$ingredient)] <- "Rum"
ingredients$ingredient_simple[grep("Vodka|Absolut", ingredients$ingredient)] <- "Vodka"
ingredients$ingredient_simple[grep("Gin", ingredients$ingredient)] <- "Gin"
ingredients$ingredient_simple[grep("Juice Of A Lime", ingredients$ingredient)] <- "Lime Juice"
ingredients$ingredient_simple[grep("Juice Of A Lemon|Juice Of Lemon|Lemon Or Lime Juice", ingredients$ingredient)] <- "Lemon Juice"
ingredients$ingredient_simple[grep("Juice Of An Orange|Juice Of Orange", ingredients$ingredient)] <- "Orange Juice"
ingredients$ingredient_simple[grep("Absinthe", ingredients$ingredient)] <- "Absinthe"
ingredients$ingredient_simple[grep("Amaretto Di Saronno", ingredients$ingredient)] <- "Amaretto"
ingredients$ingredient_simple[grep("Agave", ingredients$ingredient)] <- "Agave syrup"
ingredients$ingredient_simple[grep("Tequila", ingredients$ingredient)] <- "Tequila"
ingredients$ingredient_simple[grep("Whiskey|Whisky|Scotch|Rye|Johnnie Walker", ingredients$ingredient)] <- "Whiskey"
ingredients$ingredient_simple[grep("Bourbon|Wild Turkey|Jim Beam", ingredients$ingredient)] <- "Bourbon"
ingredients$ingredient_simple[grep("Espresso|Brewed|Black Coffee", ingredients$ingredient)] <- "Coffee"
ingredients$ingredient_simple[grep("Coffee Brandy|Coffee-Flavored Liqueur|Coffee-Flavored Brandy|Kahlua|Tia Maria", ingredients$ingredient)] <- "Coffee Liqueur"
ingredients$ingredient_simple[grep("Ginger Ale", ingredients$ingredient)] <- "Ginger Beer"
ingredients$ingredient_simple[grep("Light Cream|And-Half|Heavy Cream|Heavy Cream|Sweet Cream", ingredients$ingredient)] <- "Cream"
ingredients$ingredient_simple[grep("Orgeat", ingredients$ingredient)] <- "Almond Syrup"
ingredients$ingredient_simple[grep("Almond Extract", ingredients$ingredient)] <- "Almond Flavoring"
ingredients$ingredient_simple[grep("Egg", ingredients$ingredient)] <- "Egg"
ingredients$ingredient_simple[grep("Cognac", ingredients$ingredient)] <- "Cognac"
ingredients$ingredient_simple[grep("Sherry", ingredients$ingredient)] <- "Sherry"
ingredients$ingredient_simple[grep("Cider", ingredients$ingredient)] <- "Cider"
ingredients$ingredient_simple[grep("Chartreuse", ingredients$ingredient)] <- "Chartreuse"
ingredients$ingredient_simple[grep("Wine", ingredients$ingredient)] <- "Wine"
ingredients$ingredient_simple[grep("Port", ingredients$ingredient)] <- "Port"
ingredients$ingredient_simple[grep("Amaro", ingredients$ingredient)] <- "Amaro"
ingredients$ingredient_simple[grep("Bitters", ingredients$ingredient)] <- "Bitters"
ingredients$ingredient_simple[grep("Anis*", ingredients$ingredient)] <- "Anisette"
ingredients$ingredient_simple[grep("Apricot Flavored|Apricot-Flavored", ingredients$ingredient)] <- "Apricot Brandy"
ingredients$ingredient_simple[grep("Irish Cream", ingredients$ingredient)] <- "Baileys Irish Cream"
ingredients$ingredient_simple[grep("Blackberry", ingredients$ingredient)] <- "Blackberry Liqueur"
ingredients$ingredient_simple[grep("Brandy", ingredients$ingredient)] <- "Brandy"
ingredients$ingredient_simple[grep("Cola", ingredients$ingredient)] <- "Coca Cola"
ingredients$ingredient_simple[grep("Cointreau|Triple Sec", ingredients$ingredient)] <- "Cointreau or Triple Sec"
ingredients$ingredient_simple[grep("Creme De Cacao|Chocolate Liqueur", ingredients$ingredient)] <- "Creme De Cacao"
ingredients$ingredient_simple[grep("Creme De Menthe", ingredients$ingredient)] <- "Creme De Menthe"
ingredients$ingredient_simple[grep("Carbonated Water|Club Soda", ingredients$ingredient)] <- "Soda Water"
ingredients$ingredient_simple[grep("Champagne", ingredients$ingredient)] <- "Champagne"
ingredients$ingredient_simple[grep("Curacao", ingredients$ingredient)] <- "Curacao"
ingredients$ingredient_simple[grep("Lillet", ingredients$ingredient)] <- "Lillet"
ingredients$ingredient_simple[grep("Mint", ingredients$ingredient)] <- "Mint"
ingredients$ingredient_simple[grep("Sugar", ingredients$ingredient)] <- "Sugar"

# Using this to look for similar ingredients with different names
#list <- ingredients %>% 
#  group_by(ingredient_simple) %>% 
#  tally()

# Which ingredients have more than 10 occurrences?
frequent_list <- ingredients %>% 
  group_by(ingredient_simple) %>% 
  tally() %>% 
  filter(n>10) %>% 
  arrange(desc(n))

top_list <- ingredients %>% 
  group_by(ingredient_simple) %>% 
  tally() %>% 
  filter(n>50) %>% 
  arrange(desc(n))


# Make a list of ingredient names we want to look at

frequent_name_list <- as.vector(frequent_list$ingredient_simple)
top_name_list <- as.vector(top_list$ingredient_simple)

#### Now following a tutorial for making co-occurence plots. Step 1 - getting data in same format. -----------

# select only rows with ingredients of interest

f_target_ingredients <- ingredients %>% 
  filter(ingredient_simple %in% frequent_name_list) %>% 
  select(drink, ingredient_simple)

t_target_ingredients <- ingredients %>% 
  filter(ingredient_simple %in% top_name_list) %>% 
  select(drink, ingredient_simple)


# ditch drinks with less than 2 ingredients of interest?

f_key_drinks <- f_target_ingredients %>% 
  group_by(drink) %>% 
  tally() %>% 
  filter(n > 1)

t_key_drinks <- t_target_ingredients %>% 
  group_by(drink) %>% 
  tally() %>% 
  filter(n > 1)

f_drink_list <- as.vector(f_key_drinks$drink)

t_drink_list <- as.vector(t_key_drinks$drink)

f_target_drinks <- f_target_ingredients %>% 
  filter(drink %in% f_drink_list) %>% 
  group_by(drink) %>% 
  mutate(ingredient_number = row_number())

t_target_drinks <- t_target_ingredients %>% 
  filter(drink %in% t_drink_list) %>% 
  group_by(drink) %>% 
  mutate(ingredient_number = row_number())

# Get it into a wide shape

f_target_drinks_wide <- f_target_drinks %>% 
  pivot_wider(names_from = "ingredient_number", values_from = "ingredient_simple") %>% 
  ungroup() %>% 
  select(-drink) %>% 
  clean_names()

t_target_drinks_wide <- t_target_drinks %>% 
  pivot_wider(names_from = "ingredient_number", values_from = "ingredient_simple") %>% 
  ungroup() %>% 
  select(-drink) %>% 
  clean_names()


f_target_drinks_list <- f_target_drinks_wide %>% 
  mutate(list = paste(x1, x2, x3, x4, x5, x6, x7, x8, sep = ", ")) %>% 
  select(list)

t_target_drinks_list <- t_target_drinks_wide %>% 
  mutate(list = paste(x1, x2, x3, x4, x5, x6, sep = ", ")) %>% 
  select(list)

f_target_drinks_list$list <- str_remove_all(f_target_drinks_list$list, "NA, ")
f_target_drinks_list$list <- str_remove_all(f_target_drinks_list$list, ", NA")

t_target_drinks_list$list <- str_remove_all(t_target_drinks_list$list, "NA, ")
t_target_drinks_list$list <- str_remove_all(t_target_drinks_list$list, ", NA")

write_tsv(f_target_drinks_list, "f_list.txt")

write_tsv(t_target_drinks_list, "t_list.txt")

#Step 2 - actual analysis. What are the links between top 22 ingredients?
#https://www.r-bloggers.com/turning-keywords-into-a-co-occurrence-network/
library(ggnetwork)
library(tnet)
library(network) # keep after tnet

t_list <- read_tsv("t_list.txt")$list %>%
  str_split(", ")


weights <- read_tsv("t_list.txt")$list %>%
  str_split(", ") %>%  
  lapply(function(x) {
    expand.grid(x, x, w = 1 / length(x), stringsAsFactors = FALSE)
  }) %>%
  bind_rows


weights <- apply(weights[, -3], 1, str_sort) %>%
  t %>%
  data.frame(stringsAsFactors = FALSE) %>%
  mutate(w = weights$w)


weights2 <- group_by(weights, X1, X2) %>%
  summarise(w = sum(w)) %>%
  filter(X1 != X2)

# undirected network
n <- network(weights2[, -3], directed = FALSE)

stopifnot(nrow(weights2) == network.edgecount(n))
set.edge.attribute(n, "weight", weights2$w)


# weighted degree at alpha = 1
t <- as.edgelist(n, attrname = "weight") %>%
  symmetrise_w %>%
  as.tnet %>%
  degree_w

stopifnot(nrow(t) == network.size(n))
set.vertex.attribute(n, "degree_w", t[, "output" ])

# show only keywords at or above median weighted degree
l <- n %v% "degree_w"
l <- ifelse(l >= 0, network.vertex.names(n), NA)

stopifnot(length(l) == network.size(n))
set.vertex.attribute(n, "label", l)

library(paletteer)

ggplot(n, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(aes(color = weight)) +
  geom_nodes() +
  geom_nodelabel(aes(size = degree_w, label = label),
                 color = "grey20", label.size = 0.25) +
  geom_nodelabel(aes(size = degree_w, label = label, fill = degree_w, alpha = 0.5),
                 color = "grey20", label.size = 0.25) +
  scale_size_continuous(range = c(3, 8)) +
  scale_color_gradient2(low = "grey25", midpoint = 0.75, high = "black") +
  guides(size = FALSE, color = FALSE) + 
  labs(title = "Network graph of top 22 cocktail ingredients", subtitle = "Gin and lemon juice are most frequently mixed", fill = "Degree of connectedness") +
  theme_blank() +
  scale_fill_paletteer_c("grDevices::Viridis")

ggsave("22_ingredients.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 10, height = 9, units = "in") 


### Option 2: Look at the main connections of one ingredient - Who puts eggs in a cocktail?-------------

f_list <- read_tsv("f_list.txt")$list %>%
  str_split(", ")


f_weights <- read_tsv("f_list.txt")$list %>%
  str_split(", ") %>%  
  lapply(function(x) {
    expand.grid(x, x, w = 1 / length(x), stringsAsFactors = FALSE)
  }) %>%
  bind_rows


f_weights <- apply(f_weights[, -3], 1, str_sort) %>%
  t %>%
  data.frame(stringsAsFactors = FALSE) %>%
  mutate(w = f_weights$w)


f_weights2 <- group_by(weights, X1, X2) %>%
  summarise(w = sum(w)) %>%
  filter(X1 != X2) %>% 
  filter(X1 == "Egg")

# undirected network
n1 <- network(f_weights2[, -3], directed = FALSE)

stopifnot(nrow(f_weights2) == network.edgecount(n1))
set.edge.attribute(n1, "weight", f_weights2$w)


# weighted degree at alpha = 1
t1 <- as.edgelist(n1, attrname = "weight") %>%
  symmetrise_w %>%
  as.tnet %>%
  degree_w

stopifnot(nrow(t1) == network.size(n1))
set.vertex.attribute(n1, "degree_w", t1[, "output" ])

# show only keywords at or above median weighted degree
l1 <- n1 %v% "degree_w"
l1 <- ifelse(l1 >= 0, network.vertex.names(n1), NA)

stopifnot(length(l1) == network.size(n1))
set.vertex.attribute(n1, "label", l1)


ggplot(n1, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(aes(color = weight, size = weight)) +
  geom_nodes() +
  geom_nodelabel(aes(size = 15, label = label),
                 color = "grey20", label.size = 1, label.padding = unit(0.5, "lines")) +
  scale_color_paletteer_c("grDevices::Viridis") +
  guides(size = FALSE, color = FALSE) + 
  labs(title = "What does egg go with?", subtitle = "Graph of most common accompaniaments to egg (white or yolk) in a cocktail") +
  theme_blank()

ggsave("egg.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 11, height = 8, units = "in") 
