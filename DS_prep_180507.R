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
ts <- seq.POSIXt(as.POSIXlt("2017-07-12 0:00"), as.POSIXlt("2018-06-20 0:00"), by = 120)
## format UTC TZ
ts <- format.POSIXct(ts, '%y-%m-%d %H:%M')
## Make data frame
ts.df <- data.frame(timestamp=ts)
##View(ts.df)

## Begin joining datasets
## Read file
LSVFS.1 <- read.csv("")

## Reformat dates
LSVFS.1$date.time <- as.POSIXct(LSVFS.1$date.time, 
                                format = "%m/%d/%y %H:%M", tz = "est")