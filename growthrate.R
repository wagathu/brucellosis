#Getting population growth rate from KNBS projections
#last updated: 20th November 2023

library(readxl)
library(tidyverse)

#Set working directory
setwd("C:/Users/angel/OneDrive/Documents/GitHub/health_finance")

# The data was scraped from the Analytical Report on Population Projections (Vol XIV) available on KNBS website
# 2009 growth rates ----

#Load the projection data for 2009
p09 <- read_excel("Projections.xlsx",
                  sheet = "2009")

colnames(p09) <- p09[1,] #Make the 1st row the header
p09 <- p09[-1, ]  #Remove the 1st row


#Make long for analysis
p09 <- p09 %>%
  pivot_longer(
    cols = -County,
    names_to = "Year",
    values_to = "pop"
  )
#Add labels
attr(p09$pop, "label") <- "Projected Population"

#remove duplicates
p09<- p09 %>%
  filter(duplicated(p09) != T) %>%
  filter(Year <= "2015") #filtered so only consecutive years

# Sort the dataset by County and date
p09 <- p09%>%
  arrange(County, Year)

# Group the dataset by County
p09 <- p09%>%
  group_by(County) %>%
  mutate(
    # Calculate year-on-year population growth
    year_on_year_growth = (pop - lag(pop)) / lag(pop) 
  ) %>%
  summarise(growth_rate = mean(year_on_year_growth, na.rm = T))

write_csv(p09,
          "./03_tempfiles/growthrate2009.csv")


# 2019 growth rates ----

#Load the projection data for 2019
p19 <- read_excel("Projections.xlsx",
                  sheet = "2019") %>%
  select(County, "2020", "2021", "2022", "2023", "2024", "2025")

#Make long for analysis
p19 <- p19 %>%
  pivot_longer(
    cols = -County,
    names_to = "Year",
    values_to = "pop"
  )

#Add labels
attr(p19$pop, "label") <- "Projected Population"

#remove duplicates
p19<- p19 %>%
  filter(duplicated(p19) != T) %>%
  filter(!is.na(County))

# Sort the dataset by County and date
p19 <- p19%>%
  arrange(County, Year)

# Group the dataset by County
p19 <- p19 %>%
  group_by(County) %>%
  mutate(
    pop = as.numeric(str_remove_all(pop, ",")),
    # Calculate year-on-year population growth
    year_on_year_growth = (pop - lag(pop)) / lag(pop) 
  ) %>%
  summarise(growth_rate = mean(year_on_year_growth, na.rm = T))

write_csv(p19,
          "./03_tempfiles/growthrate2019.csv")


# Decide which growth rate you want to use or perhaps get an average value