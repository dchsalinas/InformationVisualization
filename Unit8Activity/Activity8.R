# Diana Salinas
# October 3, 2022
# Activity 8

rm(list = ls())
# install.packages("tidyverse")
# install.packages("RColorBrewer")
# install.packages("productplots")
# install.packages("pacman")
# install.packages("reshape2")
# install.packages("rvest")
# install.packages("lubridate")

pacman::p_load(tidyverse, RColorBrewer, productplots,reshape2, rvest, dplyr)
library(tidyverse)
library(rvest)
library(lubridate)
library(dplyr)

weather <- read.csv("data/weather.csv")
view(weather)
weatherA <- weather

weatherA <- weatherA %>% 
  pivot_longer(d1:d30, names_to = "day", values_to = "value") %>%
  mutate(day = substr(day, 2, 10))

weatherA <- weatherA %>%
  mutate(date = ymd(paste0(year, sep ="-", month, sep= "-", day))) %>%
  filter(value != -9999)

weatherB <- weatherA %>%
  filter(month == month(today())) %>%
  filter(day == day(today())) %>%
  group_by(year) %>%
  mutate(avg = mean(value)) %>%
  distinct(avg)

weatherB %>%
  ggplot() +
  geom_line(mapping = aes(x = year, y = avg), color = "slateblue")

 
