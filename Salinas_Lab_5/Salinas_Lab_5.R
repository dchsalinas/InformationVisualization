# Diana Salinas
# September 28, 2022
# Lab 5A

rm(list = ls())
# install.packages("tidyverse")
# install.packages("RColorBrewer")
# install.packages("productplots")
# install.packages("pacman")
# install.packages("reshape2")
# install.packages("rvest")
# install.packages ("robotstxt")
# install.packages ("xml2")

pacman::p_load(tidyverse, RColorBrewer, productplots,reshape2, rvest, robotstxt, xml2, dplyr)
library(tidyverse)
library(rvest)
library(robotstxt)
library(xml2)
library(dplyr)

paths_allowed("https://www.metacritic.com/")
# Output:
# www.metacritic.com                      
# [1] TRUE


####################### PART A ###################################

# 1. We will need to be more specific in our URL than the one used to verify that this website allows
# bots. Begin by adding R code to read the html structure from
page <- read_html("https://www.metacritic.com/browse/games/score/metascore/year/all")

# 2. inspect the xml
xml_structure(page)

# 3. Is your data clean (no extra newlines or whitespace)?
# If not, what stringr function(s) will remove whitespace?

# The title is clean.

title <- page %>% 
  html_nodes(".title h3") %>%
  html_text()

View(title)

# 4. Is your data clean (including listing an absolute url)
# If not, what base-R function allows you to concatenate

# I used based to concatenate the rest of the url. 

url <- page %>%
  html_nodes("a.title") %>%
  html_attr("href") %>%
  str_replace(" ", "")

url <- gsub(" ", "", paste("https://www.metacritic.com", url))

print(url)

platform <- page %>%
  html_nodes(".platform .data") %>%
  html_text() %>%
  trimws() %>% 
  str_replace("[\r\n]", "")

print(platform)

# 5.Did you get 100 observations?
# I did.
metacritic <- page %>%
  html_nodes(".clamp-metascore .positive") %>%
  html_text() %>%
  as.numeric()

print(metacritic)

# 6. Did you get 100 observations?
# I did.
user <- page %>%
  html_nodes(".user") %>%
  html_text() %>%
  as.numeric()

print(user)

# 7. set of genres associated with each video game
oneURL <- url[1]
page2 <- read_html(oneURL)
g <- page2 %>% 
  html_nodes(".product_genre .data") %>% 
  html_text() %>% 
  str_c(collapse = ", ")

print(g)
# Did you successfully grab just one value for genre? 
# You should have. What is the value for g
# (include the output as a comment in your script)?

# "Role-Playing,Action RPG"

# 8. 
count <- 1
genre <- c()
for(x in url) {
  genre1 <- read_html(x) %>% 
    html_nodes(".product_genre .data") %>% 
    html_text() %>% 
    str_c(collapse = ", ")
  genre <- c(genre, genre1)
}

genre %>%
  str_squish

# creating the tibble
videoGames2022 <- tibble(
  title = title,
  url = url,
  platform = platform,
  metacritic = metacritic,
  user = user,
  genre = genre
)

videoGames2022b <- videoGames2022

# 9. Which vectors, if any, have NA values. If any, 
# why (you may have to go to the metacritics website
# to figure out why).Because it is TBD. No scores have been added yet.

na_user <- videoGames2022 %>%
  filter(is.na(user))
            
# 10. 
videoGames2022 <- na.omit(videoGames2022)
write_csv(videoGames2022, "data/videoGames2022.csv")

# 11. How may title are on more than one platform? There are 59.
videoGames2022a <- videoGames2022 %>%
  group_by(title) %>%
  filter(n() > 1)

print(head(videoGames2022a, n=6))
# A tibble: 6 × 5
# # Groups:   title [6]
# title                       url             platf…¹ metac…²  user
# <chr>                       <chr>           <chr>     <dbl> <dbl>
# 1 Portal Companion Collection https://www.me… Switch       96   8.4
# 2 God of War                  https://www.me… PC           93   8.5
# 3 Xenoblade Chronicles 3      https://www.me… Switch       89   8.6
# 4 The Last of Us Part I       https://www.me… PlaySt…      89   5.9
# 5 I Was a Teenage Exocolonist https://www.me… PC           89   8.1
# 6 NORCO                       https://www.me… PC           88   7.1

# 12. Does this correlation surprise you?
videoGames2022 %>%
  ggplot(mapping = aes(x = metacritic, y = user)) +
  geom_point(color = "blue") 

videoGames2022 %>%
  summarize(r = cor(metacritic, user))
# # A tibble: 1 × 1
# r
# <dbl>
#   1 0.104

####################### PART B ###################################

glimpse(videoGames2022)

videoGames <- read.csv("data/videoGames2022.csv")

# What is the maximum number of genres (the maximum number of
# commas + 1) for the 100 video games? 
# Copy and paste your output as a comment in your
# script.
str_count(videoGames$genre, ',') %>%
  max()

# # A tibble: 1 × 2
# `","`     n
# <chr> <int>
#   1 ,        91
# > str_count(videoGames2022$genre, ',') 
# [1] 1 1 1 1 2 1 2 4 2 3 1 2 1 2 2 1 3 1 3 1 1 1 1 4 3 1 2 2 1 2 1 3 2
# [34] 2 2 2 1 2 1 1 1 2 2 1 2 3 2 2 3 1 1 3 5 1 2 2 1 3 3 3 2 2 2 2 2 1
# [67] 4 2 4 1 6 2 2 1 1 3 2 4 2 1 4 1 1 2 4 4 2 2 2 2 1
# > str_count(videoGames2022$genre, ',') %>%
#   +   max()
# [1] 6
# > So 6 + 1 = 7


# How many unique genres? Copy and paste your output 
# as a comment in your script.
n_distinct(videoGames$genre)
# [1] 34
# There are 34 unique genres in the code.

videoGames2022b <- videoGames %>%
  separate(genre, into=c ("genre1", "genre2", "genre3", "genre4", "genre5", "genre6", "genre7"), sep = ",") %>%
  pivot_longer(genre1:genre7, names_to = "genreNum", values_to = "genre") %>%
  na.omit()

# 1. What are the top three genres?
# The top 3 genres are Role-Playing, General, and Action.
videoGames2022ba <- videoGames2022b %>%
  group_by(genre) %>%
  count(genre) %>%
  arrange(desc(n))
  
videoGames2022b %>%
  group_by(genre) %>%
  count(genre) %>%
  ggplot() +
  geom_col(mapping = aes(x = reorder(genre, n), y = n, fill = genre)) +
  coord_flip() +
  guides(fill = "none")
  
# 2. Which platform has the LEAST games on the website? 
# Xbox One has the least.
videoGames2022b %>%
  group_by(platform) %>%
  count(platform) %>%
  ggplot() +
  geom_col(mapping = aes(x = reorder(platform, n), y = n, fill = platform)) +
  coord_flip() +
  guides(fill = "none")

# 3. Which genres are found on all platforms?
# Action, 2D, and Beat-'Em-Up are the only ones that are found in all platforms.
videoGames2022b %>%
  group_by(genre, platform) %>%
  count(genre) %>%
  ggplot() +
  geom_col(mapping = aes(x = reorder(genre, n), y = n, fill = genre)) +
  coord_flip() +
  guides(fill = "none") +
  facet_wrap(~ platform, ncol = 3)

# 4. Which genre is the most popular for users?
# Puzzle is the most popular for users. 

videoGames2022b %>%
  group_by(genre) %>%
  summarise(mean = mean(user)) %>%
  arrange(desc(mean)) %>%
  ggplot() +
  geom_col(mapping = aes(x = reorder(genre, mean), y = mean, fill = genre)) +
  coord_flip() +
  guides(fill = "none")

# 5. Which genre is the most popular among critics?
# Linear is the most popular among critics. 
videoGames2022b %>%
  group_by(genre) %>%
  summarise(mean = mean(metacritic)) %>%
  arrange(desc(mean)) %>%
  ggplot() +
  geom_col(mapping = aes(x = reorder(genre, mean), y = mean, fill = genre)) +
  coord_flip() +
  guides(fill = "none")


