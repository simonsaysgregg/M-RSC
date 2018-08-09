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
require("magrittr")
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

# direct precipitation estimation function
runoff.dp <- function(acc, CN, WA = 0.0025) { # Practice area 0.0025 base on 10sqm 
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

# Event flow correction
# valid for flows >= 0.066
evt.flow.corr <- function(in1flow, lag1, na.rm = TRUE){
  ifelse(in1flow >= 0.066, (5.59*(in1flow) - 0.289 + 0.909 * lag1), in1flow)
}

# base flow correction
# valid for flows <= 0.0014
base.flow.corr <- function(in1flow, lag1){
  ifelse(in1flow <= 0.0014, (-1.13*(in1flow) - 0.006 + 0.935 * lag1), in1flow)
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

## Antecedant dry period analysis
## Similar to rainfall event delineation
# Exstract from Drizzle0.9.5 + modified
event <- RSC.hydro.m$storm.index
event[event != 0] <- NA
ADP.index <- cumsum(diff(!is.na(c(NA, (event)))) > 0) + (0*event)
# Add ADP index as new variable
RSC.hydro.m[, "ADP.index"] <- ADP.index
#Replace index NAs with zero
RSC.hydro.m$ADP.index[is.na(RSC.hydro.m$ADP.index)] <- 0 
# Confirm
# View(RSC.hydro.m)

## Summary of ADP
ADP.sum <- (RSC.hydro.m) %>%
  group_by(ADP.index) %>%
  summarise(duation = difftime(max(timestamp), min(timestamp), units = "days")) 
#View(ADP.sum)
# Range in days
# 0.01250 13.99028
# Median in days
# 2.26
ADP.26 <- RSC.hydro.m %>%
  subset(ADP.index == 15)
#View(ADP.26)

## Split into list of events
RainEvents <- split(RSC.hydro.m, RSC.hydro.m$storm.index) 
# Returns a list of events 
# View(RainEvents)

## Calculates mean of Duration & Rainfall Accumulaiton 
# Returns a data frame of values same length as list
Rainsum <- RainEvents %>%
  map_df(function(df) {summarise(df, event = mean(storm.index),
                                 start = min(timestamp),
                                 end = max(timestamp),
                                 Duration = ((max(timestamp)-min(timestamp))/3600),
                                 Accumulation = sum(rainfall.mm, na.rm = TRUE),
                                 max.intensity5 = max(int.5min, na.rm = TRUE),
                                 runoff.est.in1 = runoff.in1(Accumulation, CN = 86),
                                 in1.vol = sum(in1.m_flow * 120, na.rm = TRUE) * (Duration * 3600),
                                 dryout.vol = sum(dryout.m_flow * 120, na.rm = TRUE) * (Duration * 3600),
                                 runoff.est.in2 = runoff.in2(Accumulation, CN = 84),
                                 in2.vol = sum(in2.m_flow * 120, na.rm = TRUE) * (Duration * 3600),
                                 in2.hobo.vol = sum(in2.hobo.m_flow * 120, na.rm = TRUE) * (Duration * 3600),
                                 runoff.est.runon = runoff.runon(Accumulation, CN = 87),
                                 out.vol = sum(out.flow * 120, na.rm = TRUE) * (Duration * 3600),
                                 out.vol.roll = sum(out.flow.roll.ASABE * 120, na.rm = TRUE) * (Duration * 3600))})
# View(Rainsum)


## Mutate to provide additional hydrology analsis
Rainsum_event_analysis <- (Rainsum) %>%
  subset(Accumulation >= 5.0) %>%
  mutate(in.sum = in1.vol + in2.hobo.vol + runoff.est.runon,
         flow.vol.perc_diff.roll = ((as.numeric(in.sum) - as.numeric(out.vol.roll)) / as.numeric(in.sum)) * 100,
         flow.vol.perc_diff = ((as.numeric(in.sum) - as.numeric(out.vol)) / as.numeric(in.sum)) * 100)
#View(Rainsum_event_analysis)
## Write .csv file for exporting data frames
write.csv(Rainsum_event_analysis, "./Working/Rainsum_event_analysis.csv")

## Summarise rainfall info
Rainfall_event.summary <- (Rainsum[-1, ]) %>%
  select(Duration,
         Accumulation,
         max.intensity5) %>%
  summarise_all(funs(median, min, max), na.rm = TRUE) 
#View(Rainfall_event.summary)

## Determine events where correction model applies
Rainsum.corr <- (Rainsum) %>%
  subset(start > as.POSIXct("2017-09-14") & 
         Accumulation >= 6.350 & Accumulation <= 38.608 &
         max.intensity5 >= 13.71 & max.intensity5 <= 109.73)  
#View(Rainsum.corr)

## Create vector of event numbers that apply for correction
event.ana.vec <- Rainsum.corr$event
#View(event.ana.vec)

## Subset events from list matching event vector
# Matches event observation #; need to remove event 0 == observation 1
eventsub_1 <- RainEvents[-1] 
evt.ana.corr <- eventsub_1[event.ana.vec]
#View(evt.ana.corr)

## Create lag variable in data frame
evt.corr <- evt.ana.corr %>%
  map_df(~ mutate(.x, in1lag1 = lag(in1.m_flow)))
#View(evt.corr)

## Apply flow corrections
evt.corr.1 <- evt.corr %>%
  mutate(in1.corr = evt.flow.corr(in1.m_flow, in1lag1))
#View(evt.corr.1)

## Temp
corted.flow.plot <- evt.corr.1 %>%
  select(timestamp,
         in1.m_flow,
         in1.corr,
         dryout.m_flow) %>%
  melt(id = "timestamp")
ggplot(corted.flow.plot)+
  geom_point(aes(x = timestamp, y = value, color = variable, shape = variable))+
  scale_color_manual(values = c("red", "blue", "black"), labels = c("Weir", "Corrected Weir", "Dry Pond Outlet"))+
  scale_shape_manual(values = c(0,1,2), labels = c("Weir", "Corrected Weir", "Dry Pond Outlet"))+
  labs(y = "Flow Rate (cms)", x = "Date")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

## Percent diff on influent methods
mod.eff <- (evt.corr.1) %>%
  subset(timestamp > as.POSIXct("2018-05-25")) %>%
  group_by(storm.index) %>%
  summarise(perc_diffin.dry = (( sum(dryout.m_flow, na.rm = TRUE) - sum(in1.m_flow, na.rm = TRUE) / sum(dryout.m_flow, na.rm = TRUE))) * 100,
            perc_diffmod.dry = ((sum(dryout.m_flow, na.rm = TRUE) - sum(in1.corr, na.rm = TRUE) / sum(dryout.m_flow, na.rm = TRUE) * 100)))
#View(mod.eff)

## Read ET data sets
## Read file from ./Working folder
## units feet
ETCRONOS <- read.csv("./Working/CRONOSresults_KRDU.csv", skip = 14)
colnames(ETCRONOS) <- c("timestamp",
                        "OMIT",
                        "Daily.et.in")
ETCRONOS <- ETCRONOS %>%
                select(timestamp, 
                       Daily.et.in)
# View(ETCRONOS)
## Reformat dates
ETCRONOS$timestamp <- mdy(ETCRONOS$timestamp, tz = "GMT")
# View(ETCRONOS)
ET_KRDU <- read.csv("./Working/ET_KRDU.csv", skip = 14)
colnames(ET_KRDU) <- c("timestamp",
                        "OMIT",
                       "reference crop ET",
                        "Daily.et.in")
ET_KRDU <- ET_KRDU %>% 
                select(timestamp, 
                       Daily.et.in)
## Reformat dates
ET_KRDU$timestamp <- mdy(ET_KRDU$timestamp, tz = "GMT")
# View(ET_KRDU)
## Bind datasets
ET <- rbind(ETCRONOS,ET_KRDU)
#View(ET)
## Join ET data to hydr.ana
evt.corr.2 <- left_join(evt.corr.1, ET, by = "timestamp") 
## View(evt.corr.2)
## Distribut dily values across the day
evt.corr.2 <- evt.corr.2 %>%
  mutate(ET.mm = (Daily.et.in * 25.4) / 10368000,
         ET = na.locf(ET.mm, na.rm = FALSE))
# View(evt.corr.2)


## Water Ballance Calcultaion 
# using some equations from line 206
hydr.ana <- (evt.corr.2) %>%
  select(timestamp,
         rainfall.mm,
         in1.corr,
         in2.hobo.m_flow,
         out.flow,
         ET,
         storm.index) %>%
  group_by(storm.index) %>%
  summarise(Duration = ((max(timestamp)-min(timestamp))/3600),
            Accumulation = sum(rainfall.mm, na.rm = TRUE),
            in1.vol = sum(in1.corr * 120, na.rm = TRUE) * (Duration * 3600),
            in2.hobo.vol = sum(in2.hobo.m_flow * 120, na.rm = TRUE) * (Duration * 3600),
            runoff.est.runon = runoff.runon(Accumulation, CN = 87),
            ET = sum(ET, na.rm = TRUE),
            direct.precip = runoff.dp(Accumulation, CN = 80),  #            ## CN base on Good quality open space on HSG D
            out.vol = sum(out.flow * 120, na.rm = TRUE) * (Duration * 3600)) %>%
  mutate(insum = in1.vol + in2.hobo.vol + runoff.est.runon + direct.precip - ET, 
         perc.diff =((as.numeric(insum) - as.numeric(out.vol)) / as.numeric(insum)) * 100,
         frac.inflow = as.numeric(insum) / as.numeric(out.vol))
View(hydr.ana)

