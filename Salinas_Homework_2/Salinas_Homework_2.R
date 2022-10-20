# Diana Salinas
# CSCI 444
# Homework 2
# Washington DC Bike Sharing Rentals

rm(list = ls())
# install.packages("tidyverse")
# install.packages("lubridate")
library(tidyverse)
library(lubridate)
library(rvest)

path_csv = paste0(getwd(), "/bike_data")
file_names <- list.files(pattern = "\\.csv$", full.names = TRUE, path = path_csv)
bikes = do.call(rbind, lapply(file_names, function(x) read.csv(x, header = FALSE)))
write_csv(bikes, path = "data/bikes.csv", append = TRUE)

# Data Wrangling 
# Question Number 1
bikes1 <- read_csv("data/bikes.csv")

# a.
bikes1a <- bikes1 %>%
  mutate(Season = case_when(Season == 1 ~ "Winter", 
            Season == 2 ~ "Spring", 
            Season == 3 ~ "Summer", 
            Season == 4 ~ "Fall"))

# b
bikes1b <- bikes1a %>%
  mutate(`Member type` = case_when(`Member type` == "Casual" ~ "Non-Member", 
                                   `Member type` == "Member" ~ "Member"))
# c
bikes1c <- bikes1b %>%
  rename("Member" = `Member type`)

# d
v <- bikes1c$`Start date`

bikes1d <- bikes1c %>%
  mutate(date = as.Date(mdy_hm(v), format = '%m-%d-%Y'))

glimpse(bikes1d)

# What are the new data types of these variables? 
# In a table list the variable name followed by its
# data type. How many observations in bikes?

# Question Number 2
# How many observations in weather? 
# There are 365 observations in weather.
weather <- read_csv("data/weather-2019.csv")

# a
weather$date <- as.Date(with(weather, paste(Year, Month, Day, sep="-")), "%Y-%m-%d")

# b
weather <- weather %>%
  rename("Temperature" = `Daily maximum temperature`)

c2F <- function(x){
  x <- (9/5) * x + 32
  return(x)
}

weather$Temperature <- c2F(weather$Temperature)

# Question Number 3
bike_count <- bikes1d %>%
  group_by(date) %>% 
  count() %>% 
  rename("Bike Rentals" = n)

bike_count_merge <- merge(bike_count, weather, by="date")

bike_count_merge %>%
  ggplot() +
  geom_point(mapping = aes(x=date,
                           y=`Bike Rentals`, 
                           color = Temperature,
                           alpha = 0.25)) +
  labs(title = "Question Number 3",
       subtitle = "Diana Salinas")

# What trends do you see with bike rentals relative 
# to temperature? Do you see any outliers in the
# data points (state what months, approximately – 
# don’t need to list exact dates)? Are these the
# dates you would expect, why or why not?
  
# Question Number 4
bike_membership <- bikes1d %>%
  group_by(date, Member) %>%
  count()  %>% 
  rename("Bike Rentals" = n)

bike_membership_merge <- merge(bike_membership, weather, by="date")

bike_membership_merge %>%
  ggplot() +
  geom_point(mapping = aes(x=date,
                           y=`Bike Rentals`, 
                           color = Temperature,
                           alpha = 0.25)) +
  labs(title = "Question Number 4",
       subtitle = "Diana Salinas") +
  facet_wrap(~ Member)

# Question Number 5
memberC <- bikes1d %>%
  group_by(date, Member, Season) %>%
  count()  %>% 
  rename("Bike Rentals" = n)

memberC5 <- merge(memberC, weather, by="date")

memberC5 %>%
  ggplot() +
  geom_col(mapping = aes(x=Member,
                           y=`Bike Rentals`,
                            fill = Member)) +
  labs(title = "Question Number 5",
       subtitle = "Diana Salinas")

memberC6 <- memberC5

memberC6 %>%
  ggplot(mapping = aes(x= Member,
                       y=`Bike Rentals`, 
                       fill = Season)) +
  geom_col(position = "dodge") +
  labs(title = "Question Number 5",
       subtitle = "Diana Salinas")

# Question Number 6

memberOther <- bikes1d %>%
  group_by(date, Member) %>%
  count()  %>% 
  rename("Bike Rentals" = n)

memberC6 <- merge(memberOther, weather, by="date")

bike_count_merge1 <- memberC6

bike_count_merge1 %>%
  ggplot(mapping = aes(x= date,
                       y=`Bike Rentals`, 
                       color = Member)) +
  geom_bar(position = "dodge", stat="identity") +
  labs(title = "Question Number 6",
       subtitle = "Diana Salinas") +
  facet_wrap(~ Month)

members <- bike_count_merge1 %>%
  arrange(desc(`Bike Rentals`)) %>%
  select(date, Member, `Bike Rentals`)
  
