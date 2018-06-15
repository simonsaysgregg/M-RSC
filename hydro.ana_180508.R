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
require("TTR")
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
## units feet
DS <- read.csv("./Working/dataset.csv")
# View(DS)

## Read file from ./Working folder
# units metric
DS.flow <- read.csv("./Working/flow.dataset.csv")
# View(DS.flow)

# Format date time
DS$timestamp <- ymd_hms(DS$timestamp)
DS.flow$timestamp <- ymd_hms(DS.flow$timestamp)

## Select columns for hyrology analysis
DS.rain.metric <- DS %>%
            select("timestamp",
                   "rain.in") %>%
            transmute(timestamp = timestamp,
                      rainfall = rain.in * 25.4)
# View(DS.rain.metric)

## Combine rainfall with flow data set
RSC.hydro.m <- left_join(DS.rain.metric, DS.flow, by = "timestamp")
# View(RSC.hydro.m)
## Remove extra position column
RSC.hydro.m <- (RSC.hydro.m) %>%
                select(timestamp,
                       rainfall, 
                       in1.m_flow,
                       in2.m_flow,
                       in2.hobo.m_flow,
                       out.flow.roll.ASABE)
# View(RSC.hydro.m)



## Prepare for Event Delineation
# Replace rainfall NAs with 0
RSC.hydro.m$rainfall[is.na(RSC.hydro.m$rainfall)] <- 0
# View(RSC.hydro.m)

## Delineate Events
# Code found online
# column 1 is time stamp in POSIXct format, 
# column 2 is rainfall depth; current data interval: two minute
# Change accordingly, units minutes
Rain_Over_0<- RSC.hydro.m[RSC.hydro.m[,2]!=0,]  
# Create vector increasing by 1 as Diff=>60 (Time specific) 
# change value of Diff here to change the Minimum Interval Time (MIT)
# input your value of MIT (in minutes); current MIT: 720 (12-hours).
Rainindex<-c(0,cumsum(diff(Rain_Over_0[,1])>720)) 
# Split into list of events
RainEvents<-split(Rain_Over_0, Rainindex) 
# Returns a list of events 
# Use sapply functions to determine the rain statistics 
# View(RainEvents)

## Calculates mean of Duration & Rainfall Accumulaiton 
# Returns a data frame of values same length as list
Rainsum <- RainEvents %>%
  map_df(function(df) {summarise(df, Duration = ((max(timestamp)-min(timestamp))/3600),
                                 Accumulation = sum(rainfall))})
# View(Rainsum)
