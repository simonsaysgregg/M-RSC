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

##Begin user defined functions###########################################
## For Runoff estimation using SCS CN method
# Inlet1 runoff estimation function
runoff.in1 <- function(acc, CN, WA = 17.72) { 
  # convert accumulation to inches
  acc.in <- acc * 3.94
  # surface storage
  S <- (1000/CN) - 10
  # runoff units: inches
  Q <- ((acc.in - (0.2 * S))^2)/(acc.in + (0.8 * S))
  # conversion to volume: cubic feet
  Q.vol <- Q * WA * (43560 / 12)
  # conversion to cubic meters
  Q.vol <- Q.vol * 0.0283
  return(Q.vol)
}

# Inlet12 runoff estimation function
runoff.in2 <- function(acc, CN, WA = 30.59) { 
  # convert accumulation to inches
  acc.in <- acc * 3.94
  # surface storage
  S <- (1000/CN) - 10
  # runoff units: inches
  Q <- ((acc.in - (0.2 * S))^2)/(acc.in + (0.8 * S))
  # conversion to volume: cubic feet
  Q.vol <- Q * WA * (43560 / 12)
  # conversion to cubic meters
  Q.vol <- Q.vol * 0.0283
  return(Q.vol)
}

# Runon runoff estimation function
runoff.runon <- function(acc, CN, WA = 1.95) { 
  # convert accumulation to inches
  acc.in <- acc * 3.94
  # surface storage
  S <- (1000/CN) - 10
  # runoff units: inches
  Q <- ((acc.in - (0.2 * S))^2)/(acc.in + (0.8 * S))
  # conversion to volume: cubic feet
  Q.vol <- Q * WA * (43560 / 12)
  # conversion to cubic meters
  Q.vol <- Q.vol * 0.0283
  return(Q.vol)
}
######################### End
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
                      rainfall.mm = rain.in * 25.4,
                      int.5min = (rainfall.mm + lag(rainfall.mm) + 
                                    (0.5*(apply(cbind((lag(rainfall.mm, 2)), lead(rainfall.mm)), 1, max))))*12)
# View(DS.rain.metric)

## Combine rainfall with flow data set
RSC.hydro.m <- left_join(DS.rain.metric, DS.flow, by = "timestamp")
# View(RSC.hydro.m)
## Remove extra position column
RSC.hydro.m <- (RSC.hydro.m) %>%
                select(timestamp,
                       rainfall.mm,
                       int.5min,
                       in1.m_flow,
                       dryout.m_flow,
                       in2.m_flow,
                       in2.hobo.m_flow,
                       out.flow,
                       out.flow.roll.ASABE) 
# View(RSC.hydro.m)

## Remove extra data; only rain and time stamp for delineation
RSC.rain <- (RSC.hydro.m) %>%
  select(timestamp,
         rainfall.mm)
# View(RSC.rain)

## Rainfall event delineation
# Exstract from Drizzle0.9.5
depth <- RSC.rain$rainfall.mm
depth[depth == 0] <- NA
nalocf <- function(x) na.locf(x, maxgap = 359, na.rm = FALSE)
rain.index <- cumsum(diff(!is.na(c(NA, nalocf(depth)))) > 0) + nalocf(0*depth)

## Addend rain index to file to be delineated
RSC.hydro.m[,"rain.index"] <- rain.index

## Runoff event delineation
# Extend rain.idex for drawdown period of 12 hours in present case
# to change drawdown change DD; depends of data interval (DD.in.hours*60)/data.interval.in.minutes
runoff.del <- function(x, DD=362) {
  l <- cumsum(! is.na(x))
  c(NA, x[! is.na(x)])[replace(l, ave(l, l, FUN=seq_along) > DD, 0) + 1]
}
# vetor to process
k <- RSC.hydro.m$rain.index
# function operation
RSC.hydro.m$storm.index <- runoff.del(k)

## Replace NAs with zero
# rain index
RSC.hydro.m$rain.index <- (RSC.hydro.m$rain.index) %>%
                           replace_na(0)
# View(RSC.hydro.m)
# storm index
RSC.hydro.m$storm.index <- (RSC.hydro.m$storm.index) %>%
                            replace_na(0)
# View(RSC.hydro.m)

## Split into list of events
RainEvents <- split(RSC.hydro.m, RSC.hydro.m$storm.index) 
# Returns a list of events 
# View(RainEvents)

## Calculates mean of Duration & Rainfall Accumulaiton 
# Returns a data frame of values same length as list
Rainsum <- RainEvents %>%
  map_df(function(df) {summarise(df, start = min(timestamp),
                                 end = max(timestamp),
                                 Duration = ((max(timestamp)-min(timestamp))/3600),
                                 Accumulation = sum(rainfall.mm, na.rm = TRUE),
                                 max.intensity5 = max(int.5min, na.rm = TRUE),
                                 runoff.est.in1 = runoff.in1(Accumulation, CN = 86),
                                 in1.vol = sum(in1.m_flow, na.rm = TRUE) * (Duration * 3600),
                                 dryout.vol = sum(dryout.m_flow, na.rm = TRUE) * (Duration * 3600),
                                 runoff.est.in2 = runoff.in2(Accumulation, CN = 84),
                                 in2.vol = sum(in2.m_flow, na.rm = TRUE) * (Duration * 3600),
                                 in2.hobo.vol = sum(in2.hobo.m_flow, na.rm = TRUE) * (Duration * 3600),
                                 runoff.est.runon = runoff.runon(Accumulation, CN = 87),
                                 out.vol = sum(out.flow, na.rm = TRUE) * (Duration * 3600),
                                 out.vol.roll = sum(out.flow.roll.ASABE, na.rm = TRUE) * (Duration * 3600))})
# View(Rainsum)


## Mutate to provide additional hydrology analsis
Rainsum_event_analysis <- (Rainsum) %>%
  subset(Accumulation >= 5.0) %>%
  mutate(in.sum = in1.vol + in2.hobo.vol + runoff.est.runon,
         flow.vol.perc_diff.roll = ((as.numeric(in.sum) - as.numeric(out.vol.roll)) / as.numeric(in.sum)) * 100,
         flow.vol.perc_diff = ((as.numeric(in.sum) - as.numeric(out.vol)) / as.numeric(in.sum)) * 100)
#View(Rainsum_event_analysis)
## Summarise rainfall info
Rainfall_event.summary <- (Rainsum[-1, ]) %>%
  select(Duration,
         Accumulation,
         max.intensity5) %>%
  summarise_all(funs(median, min, max), na.rm = TRUE) 
#View(Rainfall_event.summary)

## Write .csv file for exporting data frames
write.csv(Rainsum_event_analysis, "./Working/Rainsum_event_analysis.csv")