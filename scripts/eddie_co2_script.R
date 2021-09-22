# Eddie Climate Change Module
# How do current changes in CO2 compare to past, pre-historic, natural changes in CO2? 

  
## Install libraries     
# Install libraries to examine data. 
# this only has to be done one time and a # can be put in front of the code to comment it out so it does not run in the future.   
install.packages("broom") # cleans up output for easy presentation
install.packages("janitor") # cleans up column names and removes empty columns if needed
install.packages("lubridate") # allows easy conversion of variables to date format 
install.packages("patchwork") # allows you to plot several graphs on one page
install.packages("plotly") # makes your graph interactive
install.packages("readr") # allows you to read in data files
install.packages("scales") # works with the x and y scales on the plots
install.packages("tidyverse") # loads of tools in this one to add more functionality

# *************************
## LOAD LIBRARIES  
# Load the libraries each time you run a script or program   
# don't worry too much if you get some red text on the console screen below. 
library(broom) 
library(janitor) 
library(lubridate)
library(patchwork) 
library(plotly) 
library(readr) 
library(scales) 
library(tidyverse) 

# turn off sci notation
options(scipen=999)

# *************************
## READ THE DATA FILES
# This code will allow you to upload csv and txt files.
# The code was written so that it will automatically go find this file in the data folder.
# These datasets were downloaded from the websites
# The datasets were edited to remove the header information
# This type of minor cleaning can be done in R, txt software, or any spreadsheet software


# Mauna Loa CO2 data
loa_co2.df <- read_csv("data/loa_co2_cleaned.csv")

# Vostok ice core CO2 data
vostok_co2.df <- read_tsv("data/vostok.icecore.co2_cleaned.txt")


# *************************
# ACTIVITY A - CO2 CONCENTRATIONS
# How much is current CO2 changing?

# plot global CO2 concentrations
# first we set up the how we want to use the data to make the graph
loa_co2.plot <- loa_co2.df %>% # set up a plot using the dataframe called loa_co2.df
  ggplot(aes(year, annual_co2_ppm)) + # x is year and y is annual_co2_ppm
  geom_point()+ # plot points 
  geom_smooth(method='lm') # and add a line of best fit through the whole dataset

# then we plot it, this is also interactive so you can see the values 
# when your cursor is on the plot
ggplotly(loa_co2.plot)

# the slope of this line will be the rate of change
# determine the slope of the line
# do the statistical analyses
# after you run both of these lines of code, the output will show up in the console below
score_model <- lm(annual_co2_ppm ~ year, data=loa_co2.df)
summary(score_model)

# reading the output
# you are interested in the equation of the line
#   - the Estimate column shows values for the intercept (b) and the slope (for the variable)
#   - so you are interested in the value for slope
# (what are the units for the slope?)


# Question: What is the current rate of CO2 change?
# write down your answer, with units!
# you can type it right in this document!
#
#

# ACTIVITY B
# If you would like to use only a subset of the data, you can do that here
# Enter the years here, writing over the blue text.
min_year_global <- 1980
max_year_global <- 2020

# create a new dataframe that contains just this subset of data
loa_co2_subset.df <- loa_co2.df %>%
  filter(year >= min_year_global & year <= max_year_global)

#set up the graph
loa_co2_subset.plot <- loa_co2_subset.df %>%
  filter(year >= min_year_global & year <= max_year_global) %>%
  ggplot(aes(year, annual_co2_ppm)) +
  geom_point()+
  geom_smooth(method='lm') 
  
# plot the interactive graph
ggplotly(loa_co2_subset.plot)

# determine the slope of the line
# do the statistical analyses
score_model <- lm(annual_co2_ppm ~ year, data=loa_co2_subset.df)
summary(score_model)


# Question: What is the current rate of CO2 change within a subset of the period?
# write down your answer, with units!
# 
#

# *************************
# ACTIVITY C - VOSTOK CO2 CONCENTRATIONS
# What is the fastest rate of CO2 change in pre-historic times?

# Set up the plot of Vostok CO2 concentrations
vostok_co2.plot <- vostok_co2.df %>%
  ggplot(aes(gas_age_years_before_present, co2_ppm)) +
  geom_point()+
  geom_line()+ # connect the points with lines
  scale_x_continuous(label=comma) # format the x axis tick marks this way

# Plot the interactive graph
ggplotly(vostok_co2.plot)

# Ok, so you can see that this graph shows really variable CO2 conentrations over long periods of time
# You should identify a period of rapid change, and use that to determine the rate of change.

# Plot a subset of Vostok CO2 concentrations
# Enter the years here, writing over the blue text (up to 6 digits)
min_year_vostok <- 2342
max_year_vostok <- 17695

# Now make a new dataframe with this subset of the data
vostok_co2_subset.df <- vostok_co2.df %>%
  filter(gas_age_years_before_present >= min_year_vostok & gas_age_years_before_present <= max_year_vostok)
  
# Get things set up for the plot of co2 for subset of the data
vostok_co2_subset.plot <- vostok_co2_subset.df %>%
  filter(gas_age_years_before_present >= min_year_vostok & gas_age_years_before_present <= max_year_vostok) %>%
  ggplot(aes(gas_age_years_before_present, co2_ppm)) +
  geom_point()+
  geom_line()+
  geom_smooth(method='lm') 

# Plot the interactive graph
ggplotly(vostok_co2_subset.plot)

# Determine the slope of the line on the subset of data
# Do the statistical analyses
score_model <- lm(co2_ppm ~ gas_age_years_before_present, data=vostok_co2_subset.df)
summary(score_model)


# Question: What is the fastest pre-historic rate of CO2 change?
# write down your answer, with units!
# 
#


# Now you can address the overaching question! (the Big Picture Question)
# BPQ: How do current changes in CO2 compare to past, pre-historic changes in CO2? 
#
#



# *************************
# OPTIONAL: EXPLORE TEMPERATURE CHANGES
# *************************

# upload the data files
# recent global temperatures 
global_temp.df <-read_csv("data/global_temp_cleaned.csv")

# Vostok temperatures
vostok_temp.df <- read_tsv("data/vostok.1999.temp.data_cleaned.txt")

# ACTIVITY D - TEMPERATURE
# How much is current temperature changing?

# here you are repeating all the steps that were done above. 
# you can copy and paste the code from above!
# You have to change: 
# 1) wherever you are referring to the dataframe:
#   - the phrase 'loa_co2.df' should be replaced with 'global_temp.df' (the new file name)
# 2) where you are making a graph:
#   - the term 'annual_co2_ppm' should be replaced with 'annual_temp_c' (the new column heading)
# 3) if you make a subset dataframe and plot, 
#   - you also might want to rename 'global_co2_subset.df' to 'global_temp_subset.df' 


# Question: How much is current temperature changing? 
# Answer:
#
#

# plot global temps
# first we set up the how we want to use the data to make the graph
loa_co2.plot <- global_temp.df %>% # set up a plot using the dataframe called loa_co2.df
  ggplot(aes(year, annual_temp_c) + # x is year and y is annual_co2_ppm
  geom_point()+ # plot points 
  geom_smooth(method='lm') # and add a line of best fit through the whole dataset

# then we plot it, this is also interactive so you can see the values 
# when your cursor is on the plot
ggplotly(loa_co2.plot)

# the slope of this line will be the rate of change
# determine the slope of the line
# do the statistical analyses
# after you run both of these lines of code, the output will show up in the console below
score_model <- lm(annual_temp_c ~ year, data=global_temp.df)
summary(score_model)

# reading the output
# you are interested in the equation of the line
#   - the Estimate column shows values for the intercept (b) and the slope (for the variable)
#   - so you are interested in the value for slope
# (what are the units for the slope?)


# Question: What is the current rate of temperature change?
# write down your answer, with units!
# you can type it right in this document!
#
#

# If you would like to use only a subset of the data, you can do that here
# Enter the years here, writing over the blue text. default min = 1980, max 2020
min_year_global <- 1980
max_year_global <- 2020

# create a new dataframe that contains just this subset of data
loa_co2_subset.df <- global_temp.df %>%
  filter(year >= min_year_global & year <= max_year_global)

#set up the graph
loa_co2_subset.plot <- loa_co2_subset.df %>%
  filter(year >= min_year_global & year <= max_year_global) %>%
  ggplot(aes(year, annual_temp_c)) +
  geom_point()+
  geom_smooth(method='lm') 
  
# plot the interactive graph
ggplotly(loa_co2_subset.plot)

# determine the slope of the line
# do the statistical analyses
score_model <- lm(annual_co2_ppm ~ year, data=loa_co2_subset.df)
summary(score_model)


# Question: What is the current rate of temperature change within a subset of the period?
# write down your answer, with units!
# 
#

# *************************
# ACTIVITY E - VOSTOK TEMPERATURES
# What is the fastest rate of temperature change in pre-historic times?

# here you are repeating all the steps that were done above for the Vostok temperature. 
# you can copy and paste the code from above!
# You have to change: 
# 1) wherever it is referring to the dataset.
#   - the phrase 'vostok_co2.df' should be replaced with 'vostok_temp.df' (the new file name)
# 2) where you are making a graph
#   - the term 'gas_age_year_before_present' should be replaced with 'ice_age_year_before_present' (the new x column heading)
#   - the term 'co2-ppm' should be replaced with 'temp_c' (the new y column heading)
# 3) for the subset dataset and plot, 
#   - rename 'vostok_co2_subset.df' to 'vostok_temp_subset.df' 


# Question: What is the fastest pre-historic rate of temperature change? 
# Answer:
#
#

