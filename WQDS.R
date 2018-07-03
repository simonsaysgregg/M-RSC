## Analysis of Morrisville RSC WQ data from Providance Farm, Mason Farm Road
## monitoring period July 2017-June 2018
## data location in ./Working
## dataset preparation + analysis

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
## units TSS mg/L, all others ug/L
DS.wq <- read.csv("./Working/DS.wq_results_180425.csv")
# View(DS.wq)

## Rename columns
colnames(DS.wq) <- c("samp.date", 
                     "site",
                     "event",
                     "TKN",
                     "TKN.qc",
                     "NOx",
                     "NOx.qc",
                     "NH3N",
                     "NH3N.qc",
                     "TP",
                     "TP.qc",
                     "OP",
                     "OP.qc",
                     "TSS",
                     "TSS.qc")
# View(DS.wq)

##Format date time
DS.wq$samp.date <- mdy(DS.wq$samp.date)

## Summarize storm flow concentrations per site
storm.wq <- (DS.wq) %>%
  select(samp.date, 
         site, 
         event, 
         TKN, 
         NOx, 
         NH3N, 
         TP, 
         OP, 
         TSS) %>%
  subset(event == "storm")
#View(storm.wq)
storm.sum.wq <- (storm.wq) %>%
  group_by(as.character(site)) %>%
  summarise_at(vars(-samp.date, -site, -event), funs(mean, median, max, min, var, sd))
#View(storm.sum.wq)

## Summarize base flow concentrations per site
base.wq <- (DS.wq) %>%
  select(samp.date, 
         site, 
         event, 
         TKN, 
         NOx, 
         NH3N, 
         TP, 
         OP, 
         TSS) %>%
  subset(event == "base")
#View(storm.wq)
base.sum.wq <- (base.wq) %>%
  group_by(as.character(site)) %>%
  summarise_at(vars(-samp.date, -site, -event), funs(mean, median, max, min, var, sd))
#View(base.sum.wq)