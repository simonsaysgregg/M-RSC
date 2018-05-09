## for purpose of flow function development

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

## Begin flow calculation
# user defined functions
# Inlet1 flow calculation
flow.in1 <- function(in.m) { 
  ifelse(in1.m < 0.1524){                                                             # determination of which weir calculation to use; stage in meters
    in1.flow <- (1344 * (in1.m^2.5))                                                # V-notch flow
  } else {
    in1.flow <- (1344 * (0.5^2.5))+((3970.8 * (in1.m^1.5))-(1323.6 * (in1.m^2.5)))  # V-notch + retangular w/ contraction flow
  }
  
}

## Practice using mutate and function
in1.m <- DS.hydro.metric$in1.m
flow.in1(in1.m)
View(in1.m)
