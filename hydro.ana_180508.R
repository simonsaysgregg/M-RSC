## Analysis of Morrisville RSC data from Providance Farm, Mason Farm Road
## monitoring period July 2017-June 2018
## data located ./Working
## Hydrology Analysis

## Visualizing data
require("ggplot2")      # Powerful and aesthetic plotting system for R
require("gridExtra")    # Arrange multiple ggplots in same figure (multi-panels)
require("scales")       #
require("RColorBrewer") # creates nice color schemes
require("corrplot")     # A graphical display of a correlation matrix between all combinations of variables
## Statistical analysis
require("stats")        # Lots of stats stuff
## Data management
require("plyr")         # Allows you t split data structure into groups (pollutant type, location, etc.) and apply function on each group
require("dplyr")
require("zoo")          # Helps streamline data if you have irregular time series
require("reshape2")     # Convert data with "wide" columns to "long" columns
require("lubridate")    # Date and time data made easy! See reference PDF in Google Drive
require("data.table")
#require("xlsx")        # creates errors # Reads and writes to xlsx file
require("purrr")
require("tidyr")
require("fBasics")
require("pls")
## Mapping tools
require("stringi")
require("ggmap")        # Plotting of maps same as you would with ggplot2
require("maptools")     # Read, write, and handle Shapefiles in R
require("mapdata")      # Supplement to maps package

## Read file from ./Working folder
DS <- read.csv("./Working/dataset.csv")
# View(DS)
# Format date time
DS$timestamp <- ymd_hms(DS$timestamp)

## Select columns for hyrology analysis
DS.hydro <- DS %>%
            select("timestamp",
                   "rain.in",
                   "in1.ft",
                   "in2.ft",
                   "in2.hobo.ft",
                   "out.ft",
                   "out.velo",
                   "well.ft")

## Convert to metric units 
DS.hydro.metric <- DS.hydro %>%
                   transmute(timestamp = timestamp,
                             rainfall = rain.in * 25.4,
                             in1.m = in1.ft * 0.3048,
                             in2.m = in2.ft * 0.3048,
                             in2.hobo.m = in2.hobo.ft * 0.3048,
                             out.m = out.ft * 0.3048,
                             out.velo = out.velo / 3.28084,
                             well.m = well.ft * 0.3048)
# View(DS.hydro.metric)

## Prepare for Event Delineation
# Replace rainfall NAs with 0
DS.hydro.metric$rainfall[is.na(DS.hydro.metric$rainfall)] <- 0
# View(DS.hydro.metric)

## Delineate Events
# Code found online
# column 1 is time stamp in POSIXct format, 
# column 2 is rainfall depth; current data interval: two minute
# Change accordingly, units minutes
Rain_Over_0<- DS.hydro.metric[DS.hydro.metric[,2]!=0,]  
# Create vector increasing by 1 as Diff=>60 (Time specific) 
# change value of Diff here to change the Minimum Interval Time (MIT)
# input your value of MIT (in minutes); current MIT: 720 (12-hours).
Rainindex<-c(0,cumsum(diff(Rain_Over_0[,1])>720)) 
# Split into list of events
RainEvents<-split(Rain_Over_0, Rainindex) 
# Returns a list of events 
# Use sapply functions to determine the rain statistics 
#View(RainEvents)

## Calculates mean of Duration & Rainfall Accumulaiton 
# Returns a data frame of values same length as list
Rainsum <- RainEvents %>%
  map_df(function(df) {summarise(df, Duration = ((max(timestamp)-min(timestamp))/3600),
                                 Accumulation = sum(rainfall))})

