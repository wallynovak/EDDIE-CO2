# Eddie Module on global Change
# write whatever you want.
  
## Install libraries     
# Install  libraries for use to examine data using tidyverse 
# If you have not used any of these before, please remove the # and run the line
# this only has to be done one time and a # can be put in front of the code to comment it out so it does not run in the future.   
# install.packages("tidyverse")
# install.packages("readr")
# install.packages("janitor")
# install.packages("lubridate")
# install.packages("broom")
# install.packages("patchwork")
# install.packages("plotly")

# *************************
## Load libraries    
# Load the libraries each time you run a script or program   
library(tidyverse) # loads a lot of libraries in this one to add more functionality
library(readr) # allows you to read in data files
library(janitor) # cleans up column names and removes empty columns if wanted
library(lubridate) # allows easy conversion of varaibles to date format
library(broom) # cleans up output for easy presentation
library(scales) # works with the x and y scales on the plots
library(patchwork) # allows you to plot several graphs on one page
library(plotly)

# turn off sci notation
options(scipen=999)

# *************************
## Read data files
# This code will allow you to upload csv and txt files.
# The code was written so that it will automatically go find this file in the data folder.
# These datasets were downloaded from the websites, then opened in Excel.
# In Excel the header material was deleted so that the first row was column headings.
# It is possible to do all the cleaning in R. 

# Recent global temperatures 
global_temp.df <-read_csv("data/global_temp_cleaned.csv")

# Global CO<sub>2</sub>
loa_co2.df <- read_csv("data/loa_co2_cleaned.csv")

# Vostok temperatures
vostok_temp.df <- read_tsv("data/vostok.1999.temp.data_cleaned.txt")

# Vostok CO<sub>2</sub>
vostok_co2.df <- read_tsv("data/vostok.icecore.co2_cleaned.txt")


# *************************
# ACTIVITY A - TEMPERATURE
# Plot of global temperatures
# first we set up the how we want to use the data to make the graph
global_temp.plot <- global_temp.df %>%
  ggplot(aes(year, annual_temp)) + # here we are choosing the x axis as year and the y as the column titled J-D
  geom_line()+
  geom_point()+
  geom_smooth(method='lm') 

# then we plot it, this is also interactive so you can see the values when your cursor is on the plot
ggplotly(global_temp.plot)

# slope of the line
# do the statistical analyses
score_model<- lm(annual_temp ~ year, data=global_temp.df)
summary(score_model)

# how do you read this output?
# you are interested in the equation of the line
#   - the Estimate column shows values for the intercept (b) and the slope (for the variable)

# If you would like to use only a subset of the data, you can do here
# Enter the years here, writing over the blue text.
min_year_global <- 1960
max_year_global <- 2020

# set up the graph
global_temp_subset.df <- global_temp.df %>%
  filter(year >= min_year_global & year <= max_year_global)

#set up the graph
global_temp_subset.plot <- global_temp_subset.df %>%
  filter(year >= min_year_global & year <= max_year_global) %>%
  ggplot(aes(year, annual_temp)) +
  geom_line()+
  geom_point()+
  geom_smooth(method='lm')
  
# plot the graph interactively
ggplotly(global_temp_subset.plot)

# slope of the line
# do the statistical analyses
score_model<- lm(annual_temp ~ year, data=global_temp_subset.df)
summary(score_model)

# *************************
# ACTIVITY B - CO2 CONCENTRATIONS  
# here you are repeating all the steps that were done above. 
# you can copy and paste the code from above!
# You have to change: 
# 1) wherever it is referring to the dataset.
#   - the phrase 'global_temp.df' should be replaced with 'loa_co2.df' (the new file name)
# 2) where you are making a graph
#   - the term 'annual_temp' should be replaced with 'annual_co2' (the new column heading)
# 3) if you make a subset dataset and plot, 
#   - you also might want to rename 'global_temp_subset.df' to 'global_co2_subset.df' 



# *************************
# ACTIVITY C - VOSTOK TEMPERATURES
# Set up the plot of Vostok temperatures
vostok_temp.plot <- vostok_temp.df %>%
  ggplot(aes(ice_age_year_bp, temp_c)) +
  geom_line()+
  geom_point() +
  scale_x_continuous(label=comma)

# vostok_temp.plot
ggplotly(vostok_temp.plot)

# linear regression
# do the statistical analyses
score_model<- lm(temp_c ~ ice_age_year_bp, data=vostok_temp.df)
summary(score_model)


# Plot a Subset of Vostok temperatures
# Enter the years here, writing over the blue text
min_year_vostok <- 128405
max_year_vostok <- 136819

# Now make a new dataframe with just these data
vostok_temp_subset.df <- vostok_temp.df %>%
  filter(ice_age_year_bp >= min_year_vostok & ice_age_year_bp <= max_year_vostok)
  
# get things set up for the plot of temperature for subset of the data
vostok_temp_subset.plot <- vostok_temp_subset.df %>%
  filter(ice_age_year_bp >= min_year_vostok & ice_age_year_bp <= max_year_vostok) %>%
  ggplot(aes(ice_age_year_bp, temp_c)) +
  geom_line()+
  geom_point()+
  geom_smooth(method='lm')

# vostok_temp.plot
ggplotly(vostok_temp_subset.plot)

# slope of the line on the subset of data
# do the statistical analyses
score_model<- lm(temp_c ~ ice_age_year_bp, data=vostok_temp_subset.df)
summary(score_model)

# *************************
# VOSTOK CO2 CONCENTRATIONS
# here you are repeating all the steps that were done above for the Vostok temperature. 
# you can copy and paste the code from above!
# You have to change: 
# 1) wherever it is referring to the dataset.
#   - the phrase 'vostok_temp.df' should be replaced with 'vostok_co2.df' (the new file name)
# 2) where you are making a graph
#   - the term 'ice_age_year_bp' should be replaced with 'gas_age_year_bp' (the new x column heading)
#   - the term 'temp_c' should be replaced with 'co2_ppmv' (the new y column heading)
# 3) for the subset dataset and plot, 
#   - rename 'vostok_temp_subset.df' to 'vostok_co2_subset.df' 



