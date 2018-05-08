## Analysis of Morrisville RSC data from Providance Farm, Mason Farm Road
## monitoring period July 2017-June 2018
## data location in TBD

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
## Mapping tools
require("stringi")
require("ggmap")        # Plotting of maps same as you would with ggplot2
require("maptools")     # Read, write, and handle Shapefiles in R
require("mapdata")      # Supplement to maps package

## Create Time Series
## Use in matching observations
## 'by' 2-min interval
ts <- seq(ymd_hm("2017-07-12 0:00"), ymd_hm("2018-06-20 0:00"), by = 120) 
## Make data frame
ts.df <- data.frame(timestamp=ts)
##View(ts.df)

## Begin joining datasets
## Read file MRSC_rain
rain <- read.csv("MRSC_rain.csv")
## Reformat dates
rain$timestamp <- mdy_hm(rain$timestamp, tz = "GMT")
##View(rain)
## Join rainfall data
DS <- left_join(ts.df, rain, by = "timestamp") 
##View(DS)

## Read file MRSC_In1_level
in1.ft <- read.csv("MRSC_In1_level.csv")
## Reformat dates
in1.ft$timestamp <- mdy_hm(in1.ft$timestamp, tz = "GMT")
##View(in1.ft)
## Join in1.ft data
DS <- left_join(DS, in1.ft, by = "timestamp") 
##View(DS)

## Read file MRSC_In2_level
in2.ft <- read.csv("MRSC_In2_level.csv")
## Reformat dates
in2.ft$timestamp <- mdy_hm(in2.ft$timestamp, tz = "GMT")
##View(in2.ft)
## Join in2.ft data
DS <- left_join(DS, in2.ft, by = "timestamp") 
##View(DS)

## Read file MRSC_out_level
out.ft <- read.csv("MRSC_out_level.csv")
## Reformat dates
out.ft$timestamp <- mdy_hm(out.ft$timestamp, tz = "GMT")
##View(out.ft)
## Join out.ft data
DS <- left_join(DS, out.ft, by = "timestamp") 
##View(DS)

## Read file MRSC_out_velocity
out.velo <- read.csv("MRSC_out_velocity.csv")
## Reformat dates
out.velo$timestamp <- mdy_hm(out.velo$timestamp, tz = "GMT")
##View(out.velo)
## Join out.velo data
DS <- left_join(DS, out.velo, by = "timestamp") 
##View(DS)

