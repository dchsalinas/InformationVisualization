# Diana Salinas
# Puma Graphing

rm(list = ls())
pacman::p_load(tidyverse, scales, stringr, stringi, maps, mapproj, usmap, cowplot, socviz, RColorBrewer, gridExtra, dplyr )

HPL <- read.csv("data/HighestPayingLocations.csv", check.names = FALSE)

x <- HPL$PUMA
x1 <- stri_replace_last(x, fixed = ",", "!")  
Year <- HPL$Year
Wage <- HPL$`Average Wage` %>%
  as.numeric()

HighestPayingLocations <- tibble(x1, Year, Wage)

HPLc <- HighestPayingLocations %>%
  separate(x1, into=c ("Area", "State"), sep = "!")
 
us_states <- us_map("state") %>%
  rename("long" = x) %>%
  rename("lat" = y) %>%
  rename("State" = abbr) 

HPLd <- HPLc %>%
  select(State, Wage) 
  
  
HPLd %>%
  group_by(State) %>%
  summarise(
    average = mean(Wage),
    n=n()
  )

agg_df <- aggregate(HPLd$Wage, by=list(HPLd$State), FUN=mean)
agg_df <- agg_df %>%
  rename("State" = Group.1) %>%
  rename("Average" = x) 

write.csv(agg_df,"Average.csv", row.names = FALSE)

agg_df1 <- read_csv("data/Average.csv")

HPL_map <- left_join(us_states, agg_df1, by="State", all=TRUE)

HPL_map %>%
  ggplot(mapping = aes(x = long, y = lat, group = group, fill = Average)) +
  geom_polygon(color = "gray", linewidth=0.2) +
  coord_equal() +
  labs(title = "Average Across the States")+
  scale_x_continuous(labels = comma) +
  theme_map() 

