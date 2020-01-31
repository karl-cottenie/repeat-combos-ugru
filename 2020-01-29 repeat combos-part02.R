##############################
## Repeat combos - purrr-ing along
## Part 02
##
## Karl Cottenie
##
## 2020-01-29
##############################

# install.packages("tidyverse")
# uncomment the above line if tidyverse is not installed yet
library(tidyverse)

# install.packages("lubridate")
# uncomment the above line if lubridate is not installed yet
library(lubridate)

# Startup ends here

# Import data set and data wrangling -----------------

serengeti = read_csv("consensus_data.csv")
# For meta data on this data set, visit this link:
# https://datadryad.org/stash/dataset/doi:10.5061/dryad.5pt92

serengeti
summary(serengeti)

# create a community composition data set, we will come back to this later ------
serengeti_wide = serengeti %>% mutate(Year = year(DateTime)) %>% 
  # create a Year variable
  dplyr::select(Year, SiteID, Species, Count) %>% 
  # select only the variables of interest
  mutate(Count = as.numeric(Count)) %>% 
  # convert count into correct type
  group_by(Year, SiteID, Species) %>% 
  # create groups
  summarise(Total = sum(Count)) %>% 
  # create total counts per species for each Year-SiteID combination
  ungroup() %>% 
  # some housekeeping
  spread(Species, Total) 
# create a sample by species community composition

serengeti_wide[is.na(serengeti_wide)] = 0 
# NA are basically absent species

summary(serengeti_wide) # check if everything is ok

# Simple statistical analysis ---------

lionregr = lm(I(lionMale^0.25) ~ I(lionFemale^0.25),
              data = serengeti_wide, 
              subset = serengeti_wide$Year == 2012)

# basic statistics
anova(lionregr)

# plot the results
plot(I(lionMale^0.25) ~ I(lionFemale^0.25),
   data = serengeti_wide, 
   subset = serengeti_wide$Year == 2012)
# now we can easily add the regression line to the plot
abline(lionregr)

# FOR LOOP: repeat the regression analysis for every year  -----------
lion_year_for = vector("list", length = length(unique(serengeti_wide$Year)))
names(lion_year_for) = as.character(unique(serengeti_wide$Year))

for (i in unique(serengeti_wide$Year)){
  lion_year_for[[as.character(i)]] = lm(I(lionMale^0.25) ~ I(lionFemale^0.25),
                data = serengeti_wide, 
                subset = serengeti_wide$Year == i)
}
lion_year_for

# LAPPLY: repeat the regression analysis for every year ------
serengeti_year = split(serengeti_wide, serengeti_wide$Year)

lion_year_lapply = lapply(serengeti_year, function(x){
  lm(I(lionMale^0.25) ~ I(lionFemale^0.25),
     data = x)
})
lion_year_lapply
# easier compared to creating a for loop

# MAP: repeat the regression analysis for every year ------
lion_year_map = map(serengeti_year, function(x){
  lm(I(lionMale^0.25) ~ I(lionFemale^0.25),
     data = x)
})
lion_year_map
# similar to lapply

# further playing with the difference between lapply and map -----------

lion_year_lapply %>% lapply(function(x){
  summary(x)
})

lion_year_lapply %>% lapply(summary)

lion_year_lapply %>% map(summary)

lion_year_lapply %>% lapply(function(x){
  x$coefficients
})

lion_year_lapply %>% map_df(function(x){
  x$coefficients
})
# map derivatives convert the output into a usable format afterwards
# which is harder to do with lapply

lion_year_lapply %>% map_df( ~ .x$coefficients)
# in addition, map derivatives can use shorthand to avoid creating anonymous functions

lion_year_map %>% map( ~ summary(.x))

lion_year_map %>% map( ~ summary(.x)$adj)

serengeti_year %>% map(dim)

serengeti_year %>% map_df(dim)

serengeti_year %>% map( ~  lm(I(lionMale^0.25) ~ I(lionFemale^0.25), data = .x))

