# Diana Salinas
# Start Date: October 12, 2022
# End Date:
# Birth and Pregancy Data

rm(list = ls())
# install.packages("tidyverse")
# install.packages("pacman")
# install.packages("rvest")
# install.packages("readxl")
# install.packages("readr")

pacman::p_load(tidyverse, RColorBrewer, productplots,reshape2, readr, rvest, robotstxt, xml2, dplyr, readxl)

####################### Birth - Low Birth Rate ###################################

LB2016 <- read_excel("data/Matt_1.xls", sheet="2016_Low_Birth_Weight")
LB2017 <- read_excel("data/Matt_1.xls", sheet="2017_Low_Birth_Weight")
LB2018 <- read_excel("data/Matt_1.xls", sheet="2018_Low_Birth_Weight")
LB2019 <- read_excel("data/Matt_1.xls", sheet="2019_Low_Birth_Weight")
LB2020 <- read_excel("data/Matt_1.xls", sheet="2020_Low_Birth_Weight")

vLB2016 <- read_excel("data/Matt_1.xls", sheet="2016_Very_Low_Birth_Weight")
vLB2017 <- read_excel("data/Matt_1.xls", sheet="2017_Very_Low_Birth_Weight")
vLB2018 <- read_excel("data/Matt_1.xls", sheet="2018_Very_Low_Birth_Weight")
vLB2019 <- read_excel("data/Matt_1.xls", sheet="2019_Very_Low_Birth_Weight")
vLB2020 <- read_excel("data/Matt_1.xls", sheet="2020_Very_Low_Birth_Weight")

AllPreg2016 <- read_excel("data/Matt_1.xls", sheet="All_Preg_2016")
AllPreg2017 <- read_excel("data/Matt_1.xls", sheet="All_Preg_2017")
AllPreg2018 <- read_excel("data/Matt_1.xls", sheet="All_Preg_2018")
AllPreg2019 <- read_excel("data/Matt_1.xls", sheet="All_Preg_2019")
AllPreg2020 <- read_excel("data/Matt_1.xls", sheet="All_Preg_2020")

LiveBirths2016 <- read_excel("data/Matt_1.xls", sheet="LiveBirths2016")
LiveBirths2017 <- read_excel("data/Matt_1.xls", sheet="LiveBirths2017")
LiveBirths2018 <- read_excel("data/Matt_1.xls", sheet="LiveBirths2018")
LiveBirths2019 <- read_excel("data/Matt_1.xls", sheet="LiveBirths2019")
LiveBirths2020 <- read_excel("data/Matt_1.xls", sheet="LiveBirths2020")

FetalDeaths2016 <- read_excel("data/Matt_1.xls", sheet="FetalDeaths_2016")
FetalDeaths2017 <- read_excel("data/Matt_1.xls", sheet="FetalDeaths_2017")
FetalDeaths2018 <- read_excel("data/Matt_1.xls", sheet="FetalDeaths_2018")
FetalDeaths2019 <- read_excel("data/Matt_1.xls", sheet="FetalDeaths_2019")
FetalDeaths2020 <- read_excel("data/Matt_1.xls", sheet="FetalDeaths_2020")

IT2016 <- read_excel("data/Matt_1.xls", sheet="IT_2016")
IT2017 <- read_excel("data/Matt_1.xls", sheet="IT_2017")
IT2018 <- read_excel("data/Matt_1.xls", sheet="IT_2018")
IT2019 <- read_excel("data/Matt_1.xls", sheet="IT_2019")
IT2020 <- read_excel("data/Matt_1.xls", sheet="IT_2020")

print(LB2016)
print(sapply(LB2016, class)) 

# change columns White to Total to Numeric to numeric
cols.num <- c(2:6)
LB2016[cols.num] <- sapply(LB2016[cols.num],as.numeric)
LB2017[cols.num] <- sapply(LB2017[cols.num],as.numeric)
LB2018[cols.num] <- sapply(LB2018[cols.num],as.numeric)
LB2019[cols.num] <- sapply(LB2019[cols.num],as.numeric)
LB2020[cols.num] <- sapply(LB2020[cols.num],as.numeric)

vLB2016[cols.num] <- sapply(vLB2016[cols.num],as.numeric)
vLB2017[cols.num] <- sapply(vLB2017[cols.num],as.numeric)
vLB2018[cols.num] <- sapply(vLB2018[cols.num],as.numeric)
vLB2019[cols.num] <- sapply(vLB2019[cols.num],as.numeric)
vLB2020[cols.num] <- sapply(vLB2020[cols.num],as.numeric)

AllPreg2016[cols.num] <- sapply(AllPreg2016[cols.num],as.numeric)
AllPreg2017[cols.num] <- sapply(AllPreg2017[cols.num],as.numeric)
AllPreg2018[cols.num] <- sapply(AllPreg2018[cols.num],as.numeric)
AllPreg2019[cols.num] <- sapply(AllPreg2019[cols.num],as.numeric)
AllPreg2020[cols.num] <- sapply(AllPreg2020[cols.num],as.numeric)

LiveBirths2016[cols.num] <- sapply(LiveBirths2016[cols.num],as.numeric)
LiveBirths2017[cols.num] <- sapply(LiveBirths2017[cols.num],as.numeric)
LiveBirths2018[cols.num] <- sapply(LiveBirths2018[cols.num],as.numeric)
LiveBirths2019[cols.num] <- sapply(LiveBirths2019[cols.num],as.numeric)
LiveBirths2020[cols.num] <- sapply(LiveBirths2020[cols.num],as.numeric)

FetalDeaths2016[cols.num] <- sapply(FetalDeaths2016[cols.num],as.numeric)
FetalDeaths2017[cols.num] <- sapply(FetalDeaths2017[cols.num],as.numeric)
FetalDeaths2018[cols.num] <- sapply(FetalDeaths2018[cols.num],as.numeric)
FetalDeaths2019[cols.num] <- sapply(FetalDeaths2019[cols.num],as.numeric)
FetalDeaths2020[cols.num] <- sapply(FetalDeaths2020[cols.num],as.numeric)

IT2016[cols.num] <- sapply(IT2016[cols.num],as.numeric)
IT2017[cols.num] <- sapply(IT2017[cols.num],as.numeric)
IT2018[cols.num] <- sapply(IT2018[cols.num],as.numeric)
IT2019[cols.num] <- sapply(IT2019[cols.num],as.numeric)
IT2020[cols.num] <- sapply(IT2020[cols.num],as.numeric)

sapply(IT2016, class)

# All Pregnancies 
AllPreg2016 <- AllPreg2016 %>%
  pivot_longer(cols =c(2:5),
               names_to="Race",
               values_to="Amount 2016") %>%
  rename('Total 2016' = Total)

AllPreg2017 <- AllPreg2017 %>%
  pivot_longer(cols =c(2:5),
               names_to="Race",
               values_to="Amount 2017") %>%
  rename('Total 2017' = Total)


AllPreg2018 <- AllPreg2018 %>%
  pivot_longer(cols =c(2:5),
               names_to="Race",
               values_to="Amount 2018") %>%
  rename('Total 2018' = Total)

AllPreg2019 <- AllPreg2019 %>%
  pivot_longer(cols =c(2:5),
               names_to="Race",
               values_to="Amount 2019") %>%
  rename('Total 2019' = Total)

AllPreg2020 <- AllPreg2020 %>%
  pivot_longer(cols =c(2:5),
               names_to="Race",
               values_to="Amount 2020") %>%
  rename('Total 2020' = Total)

AllPreg <- left_join(AllPreg2016, AllPreg2017, by=c("Geography  ", "Race")) 
AllPreg <- left_join(AllPreg, AllPreg2018, by=c("Geography  ", "Race"))
AllPreg <- left_join(AllPreg, AllPreg2019, by=c("Geography  ", "Race"))
AllPreg <- left_join(AllPreg, AllPreg2020, by=c("Geography  ", "Race"))
AllPreg <- AllPreg %>% relocate(Race, .before = `Total 2016`)

# IT Deaths
IT2016 <- IT2016 %>%
  pivot_longer(cols =c(2:5),
               names_to="Race",
               values_to="Amount 2016") %>%
  rename('Total 2016' = Total)

IT2017 <- IT2017 %>%
  pivot_longer(cols =c(2:5),
               names_to="Race",
               values_to="Amount 2017") %>%
  rename('Total 2017' = Total)

IT2018 <- IT2018 %>%
  pivot_longer(cols =c(2:5),
               names_to="Race",
               values_to="Amount 2018") %>%
  rename('Total 2018' = Total)

IT2019 <- IT2019 %>%
  pivot_longer(cols =c(2:5),
               names_to="Race",
               values_to="Amount 2019") %>%
  rename('Total 2019' = Total)

IT2020 <- IT2020 %>%
  pivot_longer(cols =c(2:5),
               names_to="Race",
               values_to="Amount 2020") %>%
  rename('Total 2020' = Total)

IT <- left_join(IT2016, IT2017, by=c("Geography  ", "Race")) 
IT <- left_join(IT, IT2018, by=c("Geography  ", "Race"))
IT <- left_join(IT, IT2019, by=c("Geography  ", "Race"))
IT <- left_join(IT, IT2020, by=c("Geography  ", "Race"))
IT <- IT %>% relocate(Race, .before = `Total 2016`)

# Low Birth Weight
LB2016 <- LB2016 %>%
  pivot_longer(cols =c(2:5),
               names_to="Race",
               values_to="Amount 2016") %>%
  rename('Total 2016' = Total)

LB2017 <- LB2017 %>%
  pivot_longer(cols =c(2:5),
               names_to="Race",
               values_to="Amount 2017") %>%
  rename('Total 2017' = Total)

LB2018 <- LB2018 %>%
  pivot_longer(cols =c(2:5),
               names_to="Race",
               values_to="Amount 2018") %>%
  rename('Total 2018' = Total)

LB2019 <- LB2019 %>%
  pivot_longer(cols =c(2:5),
               names_to="Race",
               values_to="Amount 2019") %>%
  rename('Total 2019' = Total)

LB2020 <- LB2020 %>%
  pivot_longer(cols =c(2:5),
               names_to="Race",
               values_to="Amount 2020") %>%
  rename('Total 2020' = Total)

LB <- left_join(LB2016, LB2017, by=c("Geography  ", "Race")) 
LB <- left_join(LB, LB2018, by=c("Geography  ", "Race"))
LB <- left_join(LB, LB2019, by=c("Geography  ", "Race"))
LB <- left_join(LB, LB2020, by=c("Geography  ", "Race"))
LB <- LB %>% relocate(Race, .before = `Total 2016`)

# Live Birth Weight
LiveBirths2016 <- LiveBirths2016 %>%
  pivot_longer(cols =c(2:5),
               names_to="Race",
               values_to="Amount 2016") %>%
  rename('Total 2016' = Total)

LiveBirths2017 <- LiveBirths2017 %>%
  pivot_longer(cols =c(2:5),
               names_to="Race",
               values_to="Amount 2017") %>%
  rename('Total 2017' = Total)

LiveBirths2018 <- LiveBirths2018 %>%
  pivot_longer(cols =c(2:5),
               names_to="Race",
               values_to="Amount 2018") %>%
  rename('Total 2018' = Total)

LiveBirths2019 <- LiveBirths2019 %>%
  pivot_longer(cols =c(2:5),
               names_to="Race",
               values_to="Amount 2019") %>%
  rename('Total 2019' = Total)

LiveBirths2020 <- LiveBirths2020 %>%
  pivot_longer(cols =c(2:5),
               names_to="Race",
               values_to="Amount 2020") %>%
  rename('Total 2020' = Total)

LiveBirths <- left_join(LiveBirths2016, LiveBirths2017, by=c("Geography  ", "Race")) 
LiveBirths <- left_join(LiveBirths, LiveBirths2018, by=c("Geography  ", "Race"))
LiveBirths <- left_join(LiveBirths, LiveBirths2019, by=c("Geography  ", "Race"))
LiveBirths <- left_join(LiveBirths, LiveBirths2020, by=c("Geography  ", "Race"))
LiveBirths <- LiveBirths %>% relocate(Race, .before = `Total 2016`)


# Very Low Birth Weight
vLB2016 <- vLB2016 %>%
  pivot_longer(cols =c(2:5),
               names_to="Race",
               values_to="Amount 2016") %>%
  rename('Total 2016' = Total)

vLB2017 <- vLB2017 %>%
  pivot_longer(cols =c(2:5),
               names_to="Race",
               values_to="Amount 2017") %>%
  rename('Total 2017' = Total)

vLB2018 <- vLB2018 %>%
  pivot_longer(cols =c(2:5),
               names_to="Race",
               values_to="Amount 2018") %>%
  rename('Total 2018' = Total)

vLB2019 <- vLB2019 %>%
  pivot_longer(cols =c(2:5),
               names_to="Race",
               values_to="Amount 2019") %>%
  rename('Total 2019' = Total)

vLB2020 <- vLB2020 %>%
  pivot_longer(cols =c(2:5),
               names_to="Race",
               values_to="Amount 2020") %>%
  rename('Total 2020' = Total)

VeryLowWeight <- left_join(vLB2016, vLB2017, by=c("Geography  ", "Race")) 
VeryLowWeight <- left_join(VeryLowWeight, vLB2018, by=c("Geography  ", "Race"))
VeryLowWeight <- left_join(VeryLowWeight, vLB2019, by=c("Geography  ", "Race"))
VeryLowWeight <- left_join(VeryLowWeight, vLB2020, by=c("Geography  ", "Race"))
VeryLowWeight <- VeryLowWeight %>% relocate(Race, .before = `Total 2016`)

# Fetal Deaths
FetalDeaths2016 <- FetalDeaths2016 %>%
  pivot_longer(cols =c(2:5),
               names_to="Race",
               values_to="Amount 2016") %>%
  rename('Total 2016' = Total)

FetalDeaths2017 <- FetalDeaths2017 %>%
  pivot_longer(cols =c(2:5),
               names_to="Race",
               values_to="Amount 2017") %>%
  rename('Total 2017' = Total)

FetalDeaths2018 <- FetalDeaths2018 %>%
  pivot_longer(cols =c(2:5),
               names_to="Race",
               values_to="Amount 2018") %>%
  rename('Total 2018' = Total)

FetalDeaths2019 <- FetalDeaths2019 %>%
  pivot_longer(cols =c(2:5),
               names_to="Race",
               values_to="Amount 2019") %>%
  rename('Total 2019' = Total)

FetalDeaths2020 <- FetalDeaths2020 %>%
  pivot_longer(cols =c(2:5),
               names_to="Race",
               values_to="Amount 2020") %>%
  rename('Total 2020' = Total)

FetalDeaths <- left_join(FetalDeaths2016, FetalDeaths2017, by=c("Geography  ", "Race")) 
FetalDeaths <- left_join(FetalDeaths, FetalDeaths2018, by=c("Geography  ", "Race"))
FetalDeaths <- left_join(FetalDeaths, FetalDeaths2019, by=c("Geography  ", "Race"))
FetalDeaths <- left_join(FetalDeaths, FetalDeaths2020, by=c("Geography  ", "Race"))
FetalDeaths <- FetalDeaths %>% relocate(Race, .before = `Total 2016`)

write_csv(AllPreg, path = "data/AllPregnancies.csv", append = FALSE ) 
write_csv(FetalDeaths, path = "data/FetalDeaths.csv", append = FALSE ) 
write_csv(IT, path = "data/IT.csv", append = FALSE ) 
write_csv(LiveBirths, path = "data/LiveBirths.csv", append = FALSE ) 
write_csv(VeryLowWeight, path = "data/VeryLowBirthWeight.csv", append = FALSE ) 
write_csv(LB, path = "data/LowBirthWeight.csv", append = FALSE ) 

