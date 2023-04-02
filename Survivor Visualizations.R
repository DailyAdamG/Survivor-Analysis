#Set working directory

setwd("C:/Users/daily/OneDrive/Desktop/Repositories/Survivor-Analysis")

#Load libraries

library(tidyverse)
library(survivoR)
library(sqldf)
library(ggplot2)

#Create dataframe for castaway info

player_info <- castaways

#Only keep US info and start from Season 11 (first season with hidden idols)

player_info <- player_info %>%
  filter(version == 'US') %>%
  filter (season >= 11)

#Combine castaway dataframes into one dataframe

player_info <- sqldf("SELECT p.*, c.gender, c.race, c.ethnicity, c.poc, c.occupation, c.personality_type
      FROM player_info p
      JOIN castaway_details c
      ON p.castaway_id = c.castaway_id")

#Create age grouping column

player_info <- player_info %>%
  mutate(age_group = ifelse(age > 45, '46+',
                            ifelse(age >= 36 & age <= 45, '36-45',
                                   ifelse(age >= 26 & age <= 35, '26-35', 'Under 26'))))

#Create column combining gender & poc

player_info <- player_info %>%
  mutate(gender_poc = paste(gender, poc, sep = ' '))

#Create column combining gender & age_group

player_info <- player_info %>%
  mutate(gender_age_group = paste(gender, age_group, sep = ' '))

#Create column combining gender, poc, personality_type & age_group

player_info <- player_info %>%
  mutate(gender_poc_personality_age_group = paste(gender, poc, personality_type, age_group, sep = ' '))

#Create dataframe for only survivor winners

winners <- player_info %>%
  filter(result == 'Sole Survivor')

#Create visual for male vs. female place

#Create color vector

colors <- c('Male' = 'lightblue',
            'Female' = 'pink')

#Create density chart

player_info %>%
  ggplot(aes(x = order, fill = gender)) +
  geom_density(alpha = 0.8) +
  scale_fill_manual(values = colors) +
  labs(title = 'Place by Gender Density Chart')

#Create visual for white vs. poc

#Create color vector

colors <- c('White' = 'snow',
            'POC' = 'tan')

#Create density chart

player_info %>%
  ggplot(aes(x = order, fill = poc)) +
  geom_density(alpha = 0.8) +
  scale_fill_manual(values = colors) +
  labs(title = 'Place by Race Density Chart')

#Create visual for age group

#Create color vector

colors <- c('Under 26' = 'green',
            '26-35' = 'blue',
            '36-45' = 'red',
            '46+' = 'purple')

#Create density chart

player_info %>%
  ggplot(aes(x = order, fill = age_group)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = colors) +
  labs(title = 'Place by Age Group Density Chart')

#Create visual for gender and poc

#Create color vector

colors <- c('Male White' = 'pink',
            'Female White' = 'snow',
            'Male POC' = 'brown',
            'Female POC' = 'tan')

#Create density chart

player_info %>%
  ggplot(aes(x = order, fill = gender_poc)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = colors) +
  labs(title = 'Place by Gender & POC Group Density Chart')

#Create visual for gender and age_group

#Create color vector

colors <- c('Male Under 26' = 'forestgreen',
            'Male 26-35' = 'blue',
            'Male 36-45' = 'red',
            'Male 46+' = 'purple',
            'Female Under 26' = 'green',
            'Female 26-35' = 'lightblue',
            'Female 36-45' = 'pink',
            'Female 46+' = 'yellow')

#Create density chart

player_info %>%
  ggplot(aes(x = order, color = gender_age_group)) +
  geom_density(alpha = 0.5) +
  scale_color_manual(values = colors) +
  labs(title = 'Place by Gender & Age Group Density Chart')

#Survivor winners by state

winners %>%
  group_by(state) %>%
  tally()

#Survivor winners by gender

winners %>%
  group_by(gender) %>%
  tally()

#Survivor winners by poc

winners %>%
  group_by(poc) %>%
  tally()

#Survivor winners by personality_type

winners %>%
  group_by(personality_type) %>%
  tally()

#Survivor winners by age_group

winners %>%
  group_by(age_group) %>%
  tally()

#Survivor winners by gender_poc

winners %>%
  group_by(gender_poc) %>%
  tally()

#Survivor winners by gender_age_group

winners %>%
  group_by(gender_age_group) %>%
  tally()

#Survivor winners by gender_poc_personality_age_group

winners %>%
  group_by(gender_poc_personality_age_group) %>%
  tally() %>%
  print(n = 100)