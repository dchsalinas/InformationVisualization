# Diana Salinas
# October 17, 2022
# Lab 7

rm(list = ls())
# install.packages("tidyverse")
# install.packages("socviz")
# install.packages("maps")
# install.packages("mapproj")
# install.packages("usmap")
# install.packages("gridExtra")
# install.packages("cowplot")
# install.packages("pacman")
# install.packages("RColorBrewer")

pacman::p_load(tidyverse, maps, mapproj, usmap, cowplot, socviz, RColorBrewer, gridExtra, dplyr )

# Data
organRaw <- read.csv("data/Donor___State_of_Donation_Service_Area_by_Donation_Year,_Donor_Age,_Organ_2021.csv", check.names = FALSE)
View(organRaw)

# cols.num <- c(6:53)
# organRaw[cols.num] <- sapply(organRaw[cols.num],as.numeric)
# sapply(organRaw, class)

organ <- organRaw %>%
  pivot_longer(cols=c(6:53),
               names_to="full",
               values_to="n")

us_pop <- read.csv("data/state_pop.csv")
View(us_pop)

organA <- merge(organ, us_pop,by="full")

us_states <- us_map("state") %>%
  rename("long" = x) %>%
  rename("lat" = y) %>%
  rename("state" = abbr) 

organB <- left_join(us_states, organA, by="state")

# Exercises
# 1. Which state appears to have the highest 
# number of transplants? Why
# are some states colored gray?

# Texas and California appear to be the highest. The reason some states are
# grey is because they have NA values or are below 1000.
totalTransplants <- organ %>% 
  filter(Date == "To Date") %>% 
  rename(transplants = n) %>%
  na.omit()

organC <- left_join(us_states, totalTransplants, by = "full")

organC %>%
  ggplot(mapping = aes(x = long, y = lat, group = group, fill = transplants)) +
  geom_polygon(color = "gray", size=0.2) +
  coord_equal() +
  scale_fill_distiller(palette = "Set1") +
  labs(title = "US Organ Transplants")+
  theme_map() 

# Copy and paste your output as a 
# comment in your script.
totalTransplants %>%
  slice_max(transplants, n = 5) %>%
  arrange(desc(transplants))

# Date    Age      Organ      `All DSA States` State un…¹ full  trans…²
# <chr>   <chr>    <chr>                 <int>      <int> <chr>   <int>
#   1 To Date All Ages All Donors           424027          1 Cali…   45629
# 2 To Date All Ages All Donors           424027          1 Texas   32627
# 3 To Date All Ages All Donors           424027          1 Penn…   28175
# 4 To Date All Ages All Donors           424027          1 New …   27209
# 5 To Date All Ages All Donors           424027          1 Flor…   23330

totalTransplants %>%
  slice_min(transplants, n = 5) %>%
  arrange(desc(transplants))
# Date    Age      Organ      `All DSA States` State un…¹ full  trans…²
# <chr>   <chr>    <chr>                 <int>      <int> <chr>   <int>
#   1 To Date All Ages All Donors           424027          1 New …     449
# 2 To Date All Ages All Donors           424027          1 West…     409
# 3 To Date All Ages All Donors           424027          1 Sout…     370
# 4 To Date All Ages All Donors           424027          1 Verm…     217
# 5 To Date All Ages All Donors           424027          1 Dela…     197

# 2. Now, which state appears to have the 
# highest number of living donors per capita? 
# Minnesota appears to have the highest number of living donors per capita.

popTransplants <- organA %>% 
  filter(Age == "All Ages") %>% 
  filter(Date == "To Date") %>% 
  rename(transplants = n) %>%
  na.omit()

popTransplants <-popTransplants %>% 
  mutate(capita = transplants /pop)

organD <- left_join(us_states, popTransplants, by = "full")

organD %>%
  ggplot(mapping = aes(x = long, y = lat, group = group, fill = capita)) +
  geom_polygon(color = "gray", size=0.2) +
  scale_fill_distiller(palette = "Spectral", name="per capita") +
  coord_equal() +
  labs(title = "US Organ Transplants by Population")+
  theme_map() 

popTransplants %>%
  slice_max(capita, n = 5) %>%
  arrange(desc(capita))
# full    Date      Age      Organ All DSA States State unassigned transplants state      pop      capita
# 1 District of Columbia To Date All Ages All Donors         424027                1        6536    DC   705749 0.009261083
# 2            Minnesota To Date All Ages All Donors         424027                1       14413    MN  5639632 0.002555663
# 3               Kansas To Date All Ages All Donors         424027                1        6454    KS  2913314 0.002215347
# 4         Pennsylvania To Date All Ages All Donors         424027                1       28175    PA 12801989 0.002200830
# 5        Massachusetts To Date All Ages All Donors         424027                1       13782    MA  6949503 0.001983163

popTransplants %>%
  slice_min(capita, n = 5) %>%
  arrange(desc(capita))

# full    Date      Age      Organ All DSA States State unassigned transplants state     pop       capita
# 1  South Dakota To Date All Ages All Donors         424027                1         370    SD  884659 0.0004182402
# 2       Vermont To Date All Ages All Donors         424027                1         217    VT  623989 0.0003477625
# 3 New Hampshire To Date All Ages All Donors         424027                1         449    NH 1359711 0.0003302172
# 4 West Virginia To Date All Ages All Donors         424027                1         409    WV 1792147 0.0002282179
# 5      Delaware To Date All Ages All Donors         424027                1         197    DE  973764 0.0002023077

# What state has the most donors per capita? What
# major hospital is located in this state? HINT: It is considered one 
# of the top hospitals in which
# to get a kidney transplant.

# The District of Columbia has the most donors per capita.
# MedStar Georgetown University Hospital is located here.

# 3. Which type of organ is the most 
# transplanted? Which is the least?
# The organ that is most transplanted is Kidney, and
# the organ that is the least is the Intestines.
Question3 <- organ %>%
  rename(transplants = n) %>%
  filter(Organ != "All Donors") %>% 
  na.omit()

Question3 <- left_join(us_states, Question3, by = "full")

Question3 %>%
  ggplot(mapping = aes(x = long, y = lat, group = group, fill = transplants)) +
  geom_polygon(color = "gray", size=0.2) +
  scale_fill_distiller(palette = "Spectral") +
  coord_equal() +
  labs(title = "US Organ Transplants by Organ")+
  scale_color_discrete(name="no. of transplants")+
  theme_map() +
  facet_wrap( ~ Organ)

# 4.Copy and paste your output as a comment in your script. Is there a change in
# these plots relative to the plots in question 3 (i.e., do the top states still appear to be the top)?

Question4a1 <- organ %>%
  rename(transplants = n) %>%
  filter(Organ == "Kidney") %>% 
  na.omit()

Question4a <- left_join(us_states, Question4a1, by = "full")

kidney_map <- Question4a %>%
  ggplot(mapping = aes(x = long, y = lat, group = group, fill = transplants)) +
  geom_polygon(color = "gray", size=0.2) +
  scale_fill_distiller(palette = "Spectral", name="no. of transplants") +
  coord_equal() +
  labs(title = "US Kidney Transplants")+
  labs(col="no. of transplants")+
  theme_map()

Question4a1 %>%
  slice_max(transplants, n = 5) %>%
  arrange(desc(transplants))

# Date  Age      Organ  `All DSA States` `State unassigned` full         transplants
# <chr> <chr>    <chr>             <int>              <int> <chr>              <int>
#   1 0     All Ages Kidney           395292                  0 California         42563
# 2 0     All Ages Kidney           395292                  0 Texas              30332
# 3 0     All Ages Kidney           395292                  0 Pennsylvania       25589
# 4 0     All Ages Kidney           395292                  0 New York           24213
# 5 0     All Ages Kidney           395292                  0 Florida            21564

#####$#######
Question4b1 <- organ %>%
  rename(transplants = n) %>%
  filter(Organ == "Liver") %>% 
  na.omit()

Question4b <- left_join(us_states, Question4b1, by = "full")

liver_map <- Question4b %>%
  ggplot(mapping = aes(x = long, y = lat, group = group, fill = transplants)) +
  geom_polygon(color = "gray", size=0.2) +
  scale_fill_distiller(palette = "Spectral", name="no. of transplants") +
  coord_equal() +
  labs(title = "US Liver Transplants")+
  theme_map()

grid.arrange(kidney_map, liver_map)


Question4b1 %>%
  slice_max(transplants, n = 5) %>%
  arrange(desc(transplants))

# Date  Age      Organ `All DSA States` `State unassigned` full         transplants
# <chr> <chr>    <chr>            <int>              <int> <chr>              <int>
#   1 0     All Ages Liver           206137                  1 California         21712
# 2 0     All Ages Liver           206137                  1 Texas              16439
# 3 0     All Ages Liver           206137                  1 Pennsylvania       16209
# 4 0     All Ages Liver           206137                  1 Florida            13782
# 5 0     All Ages Liver           206137                  1 New York           11303

# 5. At a glance, name 2 states that seem to have improved most in the
# last 20 years? In what year did the number of kidney transplants jump, especially for these
# two states? What changed in this year?

# Two states which seem to have improved the most in the last 20 years 
# are North and South Dakota since there colors barely change from blue (unlike most of the other states).
# In 2019, the number of kidney transplants jumped, and especially in 
# Texas and California. It may be becuase the economy was recovering and doing well
# before the pandemic hit. 

kidneyByAge <- organA %>% 
  filter(Age == "All Ages") %>% 
  filter(Organ == "Kidney") %>% 
  filter(Date != "0") %>% 
  rename(transplants = n) %>%
  na.omit()

Question5 <- left_join(us_states, kidneyByAge, by = "full")

Question5 %>%
  ggplot(mapping = aes(x = long, y = lat, group = group, fill = transplants)) +
  geom_polygon(color = "gray", size=0.2) +
  scale_fill_distiller(palette = "Spectral", name="no. of transplants") +
  coord_equal() +
  labs(title = "US Organ Transplants by Organ")+
  theme_map() +
  facet_wrap( ~ Date)
