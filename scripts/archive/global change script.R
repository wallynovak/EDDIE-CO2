# Eddie Module on global Change
  
## Install libraries     
#Install  libraries for use to examine data using tidyverse this only has to be done one time and a # can be put in front of the code to comment it out so it does not run in the future as now.   
# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("janitor")
# install.packages("lubridate")
# install.packages("broom")
# install.packages("patchwork")
# install.packages("plotly")


## Load libraries    
# Load the libraries each time you run a script or program   
library(tidyverse) # loads a lot of libraries in this one to add more functionality
library(readxl) # allows you to read in excel files
library(janitor) # cleans up column names and removes empty columns if wanted
library(lubridate) # allows easy conversion of varaibles to date format
library(broom) # cleans up output for easy presentation
library(scales) # works with the x and y scales on the plots
library(patchwork) # allows you to plot several graphs on one page
library(plotly)

# turn off sci notation
options(scipen=999)

## Read files
# Recent global temperatures 
global_temp.df <- read_excel("../data/climate_change_module_dataset.xls", 
                             sheet = "Global Temperature",
                             skip=4)

# Global CO<sub>2</sub>
loa_co2.df <- read_excel("../data/climate_change_module_dataset.xls", 
                         sheet = "Mauna Loa CO2",
                         skip=4)


# Vostock temperatures
vostok_temp.df <- read_excel("../data/climate_change_module_dataset.xls", 
                             sheet = "Vostok Temp",
                             skip=4)


# Vostock CO<sub>2</sub>
vostok_co2.df <- read_excel("../data/climate_change_module_dataset.xls", 
                            sheet = "Vostok CO2",
                            skip=4)


# Plot of global temperatures
# Note this uses a different approach to explore the data
# The graph will be interactive
global_temp.plot <- global_temp.df %>%
  ggplot(aes(year, temp_c)) +
  geom_line()+
  geom_point()
# global_temp.plot
ggplotly(global_temp.plot)


# Plot of global tempreatures - subset of data
# Enter the years here
min_year_global = 1976
max_year_global =  1998


global_temp.df %>%
  filter(year >= min_year_global & year <= max_year_global) %>%
  summarize(time_difference_years = max_year_global - min_year_global,
            min_temp_c = min(temp_c, na.rm=TRUE), 
            max_temp_c = max(temp_c, na.rm=TRUE))



global_temp.plot <- global_temp.df %>%
  filter(year >= min_year_global & year <= max_year_global) %>%
  ggplot(aes(year, temp_c)) +
  geom_line()+
  geom_point()
# global_temp.plot
ggplotly(global_temp.plot)


# Plot of Vostok tempreatures ------
vostok_temp.plot <- vostok_temp.df %>%
  ggplot(aes(ice_age_year_bp, temp_c)) +
  geom_line()+
  geom_point() +
  scale_x_continuous(label=comma)
# vostok_temp.plot
ggplotly(vostok_temp.plot)


# Plot of Vostok tempreatures - subset
# Enter the years here
min_year_vostok = 231979
max_year_vostok =  237643


vostok_temp.df %>%
  filter(ice_age_year_bp >= min_year_vostok & ice_age_year_bp <= max_year_vostok) %>%
  summarize(time_difference_years = max_year_vostok - min_year_vostok,
            min_temp_c = min(temp_c, na.rm=TRUE), 
            max_temp_c = max(temp_c, na.rm=TRUE))


# plot of temp over time for sub area
vostok_temp.plot <- vostok_temp.df %>%
  filter(ice_age_year_bp >= min_year_vostok & ice_age_year_bp <= max_year_vostok) %>%
  ggplot(aes(ice_age_year_bp, temp_c)) +
  geom_line()+
  geom_point()
# vostok_temp.plot
ggplotly(vostok_temp.plot)




