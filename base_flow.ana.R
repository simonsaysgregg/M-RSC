## Analysis of Morrisville RSC base flow data from Providance Farm, Mason Farm Road
## monitoring period July 2017-June 2018
## data located ./Working
## Base flow Hydrology Analysis
## infiltration Estimation + Baseflow correction

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
require("magrittr")
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

## Purpose of estimating infiltration rate
## create dataset of dry periods
inflows <- DS.flow %>%
  select(timestamp,
         in1.m_flow,
         in2.hobo.m_flow,
         out.flow)
DryEvents <- DS %>%
  select(timestamp,
         rain.in,
         well.ft) %>%
  transmute(timestamp = timestamp,
            rainfall.mm = rain.in * 25.4,
            well.cm = well.ft * 30.48)
DryEvents <- left_join(DryEvents, inflows, by = "timestamp")
# View(DryEvents)

## Rainfall event delineation
## copied and modified from previous usage (Hydro.ana) line 155
depth1 <- DryEvents$rainfall.mm
depth1[depth1 == 0] <- NA
nalocf <- function(x) na.locf(x, maxgap = 359, na.rm = FALSE)
rain.index1 <- cumsum(diff(!is.na(c(NA, nalocf(depth1)))) > 0) + nalocf(0*depth1)
## Addend rain index to file to be delineated
DryEvents[,"rain.index"] <- rain.index1
## Runoff event delineation
# Extend rain.idex for drawdown period of 12 hours in present case
# to change drawdown change DD; depends of data interval (DD.in.hours*60)/data.interval.in.minutes
runoff.del1 <- function(x, DD=362) {
  l <- cumsum(! is.na(x))
  c(NA, x[! is.na(x)])[replace(l, ave(l, l, FUN=seq_along) > DD, 0) + 1]
}
# vetor to process
k1 <- DryEvents$rain.index
# function operation
DryEvents$storm.index <- runoff.del(k1)
## Replace NAs with zero
# rain index
DryEvents$rain.index <- (DryEvents$rain.index) %>%
  replace_na(0)
# View(DryEvents)
# storm index
DryEvents$storm.index <- (DryEvents$storm.index) %>%
  replace_na(0)
# View(DryEvents)
## Antecedant dry period analysis
## Similar to rainfall event delineation
# Exstract from Drizzle0.9.5 + modified
event1 <- DryEvents$storm.index
event1[event1 != 0] <- NA
ADP.index1 <- cumsum(diff(!is.na(c(NA, (event1)))) > 0) + (0*event1)
# Add ADP index as new variable
DryEvents[, "ADP.index"] <- ADP.index1
#Replace index NAs with zero
DryEvents$ADP.index[is.na(DryEvents$ADP.index)] <- 0 
# Confirm
# View(DryEvents)

## Subet observations for data set for purposes of infiltration analysis
infil.ds <- DryEvents %>%
  select(timestamp,
        well.cm,
        ADP.index) %>%
  subset(ADP.index != 0) 
# View(infil.ds)

## Average well deeps over interval
# Copy working df
infil.pros <- infil.ds %>%
  subset(!is.na(well.cm))
# #View(infil.pros)
# create counting index for interval generation
infil.pros1 <- infil.pros %>%
                    mutate(timestamp = ymd_hms(timestamp),
                           hour = floor_date(timestamp, 'hour')) %>%
                    group_by(ADP.index, hour) %>%
                  summarise(well.cm = mean(well.cm))
# View(infil.pros1)
# Add count hour
infil.pros2 <- infil.pros1 %>%
  group_by(ADP.index) %>%
  mutate(start = min(hour),
         durat = (hour - start) / 86400)
# View(infil.pros2)

## Infiltraiton estimation
infil.est <- infil.pros2 %>%
  select(well.cm,
         ADP.index,
         durat)
# View(infil.est)

## Plot
ggplot(infil.est)+
  geom_point(aes(x = as.numeric(durat), y = well.cm, color = ADP.index))+
  scale_color_continuous(aes(value = "red"))+
  labs(y = "Well Stage (cm)", x = "ADP (hrs)")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))


