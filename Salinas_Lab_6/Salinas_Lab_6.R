# Diana Salinas
# October 10, 2022
# Lab 6

rm(list = ls())
# install.packages("tidyverse")
# install.packages("RColorBrewer")
# install.packages("productplots")
# install.packages("pacman")
# install.packages("reshape2")
# install.packages("rvest")
# install.packages("leaflet")

pacman::p_load(tidyverse, RColorBrewer, productplots,reshape2, rvest)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(leaflet)

# Data
nyc <- read.csv("http://data.insideairbnb.com/united-states/ny/new-york-city/2022-09-07/visualisations/listings.csv")
write_csv(nyc, path ="data/NYC_2022_listings.csv") 

nyc <- read_csv("data/NYC_2022_listings.csv")
View(nyc)

nyc <- nyc %>%
  rename("borough" = `neighbourhood_group`) %>%
  rename("min_nights" = `minimum_nights`) %>%
  rename("num_reviews" = `number_of_reviews`) %>%
  rename("count" = `calculated_host_listings_count`) %>%
  rename("availability" = `availability_365`)
  
# x <- nyc$last_review
# nyc <- nyc
#   mutate(last_review = as.Date(mdy(x), format = '%m-%d-%Y'))
# last_review is alreayd a date data type


# filter OUT all availability that are 0
nyc <- nyc %>%
  filter(availability>0)

# 1. For each type of room, which borough has the most 
# Airbnbs? Which boroughs do not have hotel rooms?
# Bronx and Staten Island do not have hotel rooms. 
# For Entire home/apt, hotel room, and shared room, Manhattan has the most.
# For Private room, Brooklyn has the most.
nyc1 <- nyc %>%
  group_by(room_type, borough) %>%
  count()

nyc1 %>%
  ggplot(mapping = aes(x= room_type,
                       y=n, 
                       fill=borough)) +
  geom_bar(position = "dodge", stat="identity") +
  labs(title = "Number of Airbnbs by types of room in NYC",
       subtitle = "Diana Salinas",
       x = " Types of Rooms",
       y = "Number") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# 2. Which borough has the most Airbnbs? 
# Which borough has the least Airbnbs?
# Manhattan has the most Airbnbs, and Staten Island
# has the least.

nyc2 <- nyc %>%
  group_by(borough) %>%
  count() %>%
  arrange(n)

nyc2 %>%
  ggplot(mapping = aes(x= reorder(borough, +n),
                       y=n, 
                       fill=borough)) +
  geom_bar(position = "dodge", stat="identity") +
  labs(title = "Number of Airbnbs in NYC",
       subtitle = "Diana Salinas",
       x = "NYC Borough",
       y = "Number") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position="none")

# 3. Which borough has the highest mean price? 
# Which has the lowest?
# Manhattan has the highest mean price, 
# and the Bronx has the lowest.
nyc3 <- nyc %>%
  group_by(borough)%>%
  summarise_at(vars(price), list(n = mean)) %>%
  arrange(n)

nyc3 %>%
  ggplot() +
  geom_col(mapping = aes(x= reorder(borough, +n),
                         y=n, 
                         fill=borough)) +
  labs(title = "Average Price by NYC Borough",
       subtitle = "Diana Salinas",
       x = "NYC Borough",
       y = "Average Price") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position="none")

# 4.Within each of the five boroughs, which 
# neighborhood has the second highest cost 
# (list cost, borough and neighborhood)? 
# Within each of the five boroughs, which 
# neighborhood has the second lowest cost 
# (list cost, borough and neighborhood)?

nyc4a <- nyc %>%
  group_by(borough, neighbourhood, price) %>%
  select(borough, neighbourhood, price) %>%
  arrange(desc(price))

nyc4a %>%
  top_n(1)

# Brookly, Fordham has the second lowest cost at $10.
# Manhattan, Hell's Kitchen has the secone highest at $10,000.

nyc4 <- nyc %>%
  group_by(borough, neighbourhood)%>%
  summarise_at(vars(price), list(avg = mean)) %>%
  arrange(avg)

# What is the second highest average cost? 
nyc4 %>%
  slice_max(avg, n = 1)
# The second highest average cost is Staten Island, 
# Fort Wadsworth with $650. 

# borough       neighbourhood    avg
# <chr>         <chr>          <dbl>
#   1 Bronx         Riverdale       626.
# 2 Brooklyn      Prospect Park   709.
# 3 Manhattan     SoHo            562.
# 4 Queens        Hollis Hills    497 
# 5 Staten Island Fort Wadsworth  650 

# What is the second lowest average cost?
nyc4 %>%
  slice_min(avg, n = 1)
# The second lowest average cost is Queens, 
# Hollis with $83.60.

# borough       neighbourhood   avg
# <chr>         <chr>         <dbl>
#   1 Bronx         Hunts Point    64  
# 2 Brooklyn      Midwood        99.5
# 3 Manhattan     Inwood        112. 
# 4 Queens        Hollis         83.6
# 5 Staten Island Grant City     60.1

nyc4 %>%
  ggplot(mapping = aes(x= reorder(neighbourhood, +avg),
                       y=avg, 
                       fill=neighbourhood)) +
  geom_bar(position = "dodge", stat="identity") +
  labs(title = "Average Price by Neighborhood",
       subtitle = "Diana Salinas",
       x = "Neighborhood",
       y = "Average Price by Neighborhood") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position="none") +
  coord_flip() +
  facet_wrap(~ borough, scales = "free")

# 5. 

nyc5 <- nyc %>%
  group_by(borough, neighbourhood)%>%
  summarise_at(vars(price), list(avg = mean)) %>%
  arrange(avg) %>%
  slice_max(avg, n = 5)

nyc5 %>%
  ggplot(mapping = aes(x= reorder(neighbourhood, +avg),
                       y=avg, 
                       fill=neighbourhood)) +
  geom_bar(position = "dodge", stat="identity") +
  labs(title = "Average Price by Neighborhood",
       subtitle = "Diana Salinas",
       x = "Neighborhood",
       y = "Average Price by Neighborhood") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position="none") +
  facet_wrap(~ borough, scales = "free")

# 6. Which borough has the highest number of reviews? 
# How many? Brooklyn has the highest number of reviews with 
# 322108 reviews. 

nyc6 <- nyc %>%
  select(borough, num_reviews, last_review, price) %>%
  filter(last_review > '2022-01-01')

nyc6 %>%
  ggplot() +
  geom_point(mapping = aes(x=last_review,
                           y=num_reviews, 
                           color = borough,
                           size= price,
                           alpha = 0.3)) +
  labs(title = "Airbnb Reviews",
       subtitle = "2022",
       x = "Date of Last Review",
       y = "Number of Reviews") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_colour_brewer(palette = "Set2")

# NOTE: you will need to write an R code chunk 
# that answers this question.
nyc6a <- nyc6 %>%
  group_by(borough) %>%
  summarise(num_reviews = sum(num_reviews),
            .groups = 'drop') %>%
  arrange(num_reviews)
print(nyc6a)
# borough       num_reviews
# <chr>               <dbl>
#   1 Staten Island       13701
# 2 Bronx               35937
# 3 Queens             156521
# 4 Manhattan          282640
# 5 Brooklyn           322108

# 7. Approximately (i.e., look at your plots rather 
# than write the R code to give the exact number),
# what are the maximum number of reviews for each borough?

# The maximum number of reviews for the Bronx is 490,
# for Brooklyn is 700, for Manhattan it is around 1440, for 
# Queens it is 700, and for State Island it is around 340.

nyc7 <- nyc6 

nyc7 %>%
  ggplot() +
  geom_point(mapping = aes(x=last_review,
                           y=num_reviews, 
                           color = borough,
                           size= price,
                           alpha = 0.3)) +
  labs(title = "Airbnb Reviews",
       subtitle = "2022",
       x = "Date of Last Review",
       y = "Number of Reviews") +
  scale_colour_brewer(palette = "Set2") +
  theme(legend.position="bottom") +
  facet_wrap(~ borough, ncol = 5) 

# 8. What are the top 4 Airbnbs for each borough’s neighborhood? 
# Copy and paste your output as a comment in your R script. 

nyc_neighbordhood_top4 <- nyc %>%
  group_by(borough, neighbourhood) %>%
  summarise(avg_price=mean(price),
            avg_long=mean(longitude), 
            avg_lat=mean(latitude)) %>%
    slice_max(neighbourhood, n = 4)

nyc_neighbordhood_top4 %>%
  leaflet(options = leafletOptions(zoomSnap=0.1)) %>%
  addTiles() %>%
  setView(-74.00, 40.71, zoom = 12) %>%
  addMarkers(~avg_long, ~ avg_lat)

print(nyc_neighbordhood_top4)

# A tibble: 20 × 5
# # Groups:   borough [5]
# borough       neighbourhood      avg_price avg_long avg_lat
# <chr>         <chr>                  <dbl>    <dbl>   <dbl>
#   1 Bronx         Woodlawn               179.     -73.9    40.9
# 2 Bronx         Williamsbridge         126.     -73.9    40.9
# 3 Bronx         Westchester Square      88.5    -73.8    40.8
# 4 Bronx         West Farms              87.5    -73.9    40.8
# 5 Brooklyn      Windsor Terrace        176.     -74.0    40.7
# 6 Brooklyn      Williamsburg           235.     -74.0    40.7
# 7 Brooklyn      Vinegar Hill           300.     -74.0    40.7
# 8 Brooklyn      Sunset Park            139.     -74.0    40.7
# 9 Manhattan     West Village           383.     -74.0    40.7
# 10 Manhattan     Washington Heights     130.     -73.9    40.8
# 11 Manhattan     Upper West Side        252.     -74.0    40.8
# 12 Manhattan     Upper East Side        280.     -74.0    40.8
# 13 Queens        Woodside               111.     -73.9    40.7
# 14 Queens        Woodhaven               89.2    -73.9    40.7
# 15 Queens        Whitestone             191.     -73.8    40.8
# 16 Queens        Sunnyside              114.     -73.9    40.7
# 17 Staten Island Woodrow                115      -74.2    40.5
# 18 Staten Island Willowbrook            329      -74.1    40.6
# 19 Staten Island Westerleigh            104      -74.1    40.6
# 20 Staten Island West Brighton           71.9    -74.1    40.6
# > 
