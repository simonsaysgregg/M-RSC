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
runoff.runon <- function(acc, CN, WA = 0.74) { 
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
runoff.dp <- function(acc, WA = 0.0025) { # Practice area 0.0025 base on 10sqm 
  # Accumulation conversion to meters
  acc <- acc / 1000
  # Conversion: acres to square meters
  WA <- WA * (43560 / (3.2808^2))
  # Multiply
  Q.vol <- acc * WA
    return(Q.vol)
}

# Event flow correction
# valid for flows >= 0.066
evt.flow.corr <- function(in1flow, na.rm = TRUE){
  ifelse(in1flow >= 0.066, (5.31*(in1flow) - 0.27), in1flow)
}

# base flow correction
# valid for flows <= 0.0014
base.flow.corr <- function(in1flow){
  ifelse(in1flow <= 0.0012, (-4.05*(in1flow) - 0.006), in1flow)
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

## Read ET values
ETCRONOS <- read.csv("./Working/Gregg_Simon_KRDU_DailyEvap.csv", skip = 14)
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

RSC.hydro.m <- left_join(RSC.hydro.m, ETCRONOS, by = "timestamp") 
## View(evt.corr.2)
## Distribut daily values across the day
RSC.hydro.m <- RSC.hydro.m %>%
  mutate(ET.mm = (Daily.et.in * 25.4) / 720,
         ET = na.locf(ET.mm, na.rm = FALSE))
# View(RSC.hydro.m)

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
                                 in1.vol = sum(in1.m_flow * 120, na.rm = TRUE),
                                 dryout.vol = sum(dryout.m_flow * 120, na.rm = TRUE) ,
                                 runoff.est.in2 = runoff.in2(Accumulation, CN = 84),
                                 in2.vol = sum(in2.m_flow * 120, na.rm = TRUE) ,
                                 in2.hobo.vol = sum(in2.hobo.m_flow * 120, na.rm = TRUE) ,
                                 runoff.est.runon = runoff.runon(Accumulation, CN = 87),
                                 ET.sum = sum(ET, na.rm = TRUE),
                                 direct.precip = runoff.dp(Accumulation),
                                 out.vol = sum(out.flow * 120, na.rm = TRUE) ,
                                 out.vol.roll = sum(out.flow.roll.ASABE * 120, na.rm = TRUE) )})
# View(Rainsum)

# ## Mutate to provide additional hydrology analsis
# Rainsum_event_analysis <- (Rainsum) %>%
#   subset(Accumulation >= 5.0) %>%
#   mutate(in.sum = in1.vol + in2.hobo.vol + runoff.est.runon,
#          flow.vol.perc_diff.roll = ((as.numeric(in.sum) - as.numeric(out.vol.roll)) / as.numeric(in.sum)) * 100,
#          flow.vol.perc_diff = ((as.numeric(in.sum) - as.numeric(out.vol)) / as.numeric(in.sum)) * 100)
# #View(Rainsum_event_analysis)
# ## Write .csv file for exporting data frames
# # write.csv(Rainsum_event_analysis, "./Working/Rainsum_event_analysis.csv")

## Summarise rainfall info
Rainfall_event.summary <- (Rainsum[-1, ]) %>%
  select(start,
         Duration,
         Accumulation,
         max.intensity5)
# View(Rainfall_event.summary)
autum <- Rainfall_event.summary %>%
  subset(start >= "2017-09-22" & start <= "2018-03-20") %>% 
  summarise(rainfall_sum = sum(Accumulation, na.rm = TRUE)) 
# View(autum)
sum1 <- Rainfall_event.summary %>%
  subset(start <= "2017-09-22") %>% 
  summarise(rainfall_sum = sum(Accumulation, na.rm = TRUE)) 
# View(sum1)
sum2 <- Rainfall_event.summary %>%
  subset(start >= "2018-03-20") %>% 
  summarise(rainfall_sum = sum(Accumulation, na.rm = TRUE)) 
# View(sum2)
# max events
event.max <- Rainfall_event.summary %>%
  group_by(month=floor_date(start, "month")) %>%
  summarise(max.acc = max(Accumulation, na.rm = TRUE),
            max.int = max(max.intensity5, na.rm = TRUE))
# View(event.max)

## Determine events where correction model applies
Rainsum.corr <- (Rainsum) %>%
  subset(start > as.POSIXct("2017-09-14") &
         Accumulation >= 6.350 & Accumulation <= 38.608 &
         max.intensity5 >= 13.71 & max.intensity5 <= 109.73)
# View(Rainsum.corr)

## Create vector of event numbers that apply for correction
event.ana.vec <- Rainsum.corr$event
#View(event.ana.vec)

## Subset events from list matching event vector
# Matches event observation #; need to remove event 0 == observation 1
eventsub_1 <- RainEvents[-1]
evt.ana.corr <- eventsub_1[event.ana.vec]
# View(evt.ana.corr)

## make evt.ana.corr a data.frame
evt.ana.corr1 <- data.frame(Reduce(rbind, evt.ana.corr))
# View(evt.ana.corr1)

## Apply flow corrections
evt.corr <- evt.ana.corr1 %>%
  mutate(in1.corr = evt.flow.corr(in1.m_flow))
# View(evt.corr)

## Summarise hydrologic rainfall events of analysis
corr.evt.sum <- (evt.corr) %>%
  group_by(storm.index) %>%
  summarise(Accumulation = sum(rainfall.mm, na.rm = TRUE),
            Duration = ((max(timestamp)-min(timestamp))),
            max.intensity5 = max(int.5min, na.rm = TRUE))
# View(corr.evt.sum)
# range(corr.evt.sum$Accumulation)
# returns: 6.35 - 38.608
# median(corr.evt.sum$Accumulation)
# returns: 21.336
# sum(corr.evt.sum$Accumulation)
# returns: 287.782
# range(corr.evt.sum$Duration)
# returns: 1.04 - 20.37
# median(corr.evt.sum$Duration)
# returns: 14.1
# range(corr.evt.sum$max.intensity5)
# returns: 13.72 - 109.73
# median(corr.evt.sum$max.intensity5)
# returns: 27.43

## Summary analysis hydrologic events ADP 
ana.ADP.evt <- ADP.sum %>%
  subset(ADP.index == 23 |
         ADP.index == 26 |
         ADP.index == 32 |
         ADP.index == 42 |
         ADP.index == 46 |
         ADP.index == 69 |
         ADP.index == 70 |
         ADP.index == 76 |
         ADP.index == 78 |
         ADP.index == 80 |
         ADP.index == 82 |
         ADP.index == 83 |
         ADP.index == 86 |
         ADP.index == 89 |
         ADP.index == 91) 
# View(ana.ADP.evt)
# range(ana.ADP.evt$duation)
# returns: 0.04 - 4.87
# median(ana.ADP.evt$duation)
# returns: 1.92

## Summary analysis for WQ events ADP for WQ 
ana.ADP.WQ <- ADP.sum %>%
  subset(ADP.index == 64 |
           ADP.index == 65 |
           ADP.index == 66 |
           ADP.index == 67 |
           ADP.index == 73 |
           ADP.index == 75 |
           ADP.index == 78 |
           ADP.index == 79 |
           ADP.index == 89 |
           ADP.index == 91) 
# View(ana.ADP.WQ)
# range(ana.ADP.evt$duation)
# returns: 0.04 - 4.87
# median(ana.ADP.evt$duation)
# returns: 1.92

## Summary analysis for WQ events ADP for WQ 
ana.evt.WQ <- Rainsum[-1,] %>%
  subset(event == 64 |
           event == 65 |
           event == 66 |
           event == 67 |
           event == 73 |
           event == 75 |
           event == 78 |
           event == 79 |
           event == 89 |
           event == 91) 
# View(ana.evt.WQ)
# range(ana.evt.WQ$Duration)
# returns: 13.50000 - 51.36667
# median(ana.evt.WQ$Duration)
# returns: 35.66667
# range(ana.evt.WQ$max.intensity5)
# returns:3.048 - 109.728
# median(ana.evt.WQ$max.intensity5)
# returns: 11.43

## Temp
corted.flow.plot <- evt.corr %>%
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
mod.eff <- (evt.corr) %>%
  subset(timestamp > as.POSIXct("2018-05-25")) %>%
  group_by(storm.index) %>%
  summarise(perc_diffin.dry = (( sum(dryout.m_flow, na.rm = TRUE) - sum(in1.m_flow, na.rm = TRUE) / sum(dryout.m_flow, na.rm = TRUE))) * 100,
            perc_diffmod.dry = ((sum(dryout.m_flow, na.rm = TRUE) - sum(in1.corr, na.rm = TRUE) / sum(dryout.m_flow, na.rm = TRUE) * 100)))
# View(mod.eff)

## Water Ballance Calcultaion
hydr.ana <- (evt.corr) %>%
  select(timestamp,
         rainfall.mm,
         in1.corr,
         in2.hobo.m_flow,
         ET,
         storm.index) %>%
  group_by(storm.index) %>%
  summarise(Start = min(timestamp),
            Duration = ((max(timestamp)-min(timestamp))),
            Accumulation = sum(rainfall.mm, na.rm = TRUE),
            in1.vol = sum(in1.corr * 120, na.rm = TRUE) ,
            in2.hobo.vol = sum(in2.hobo.m_flow * 120, na.rm = TRUE),
            runoff.est.runon = runoff.runon(Accumulation, CN = 87),
            ET = sum(ET, na.rm = TRUE),
            direct.precip = runoff.dp(Accumulation),
            exfil = (0.42/24) * Duration) %>% # conversion days to hours
  mutate(insum = in1.vol + in2.hobo.vol + runoff.est.runon + direct.precip,
         frac.in1 = in1.vol / as.numeric(insum),
         frac.in2 = in2.hobo.vol / as.numeric(insum),
         frac.runon = runoff.est.runon / as.numeric(insum),
         frac.directp = direct.precip / as.numeric(insum),
         frac.ET = - (ET / as.numeric(insum)),
         frac.exfil = - (exfil / as.numeric(insum)),
         frac.delta = - (frac.ET + frac.exfil) - 1)
# View(hydr.ana)

## Median inflow element fractions
# median(hydr.ana$frac.in1)
# returns: 0.3167
# median(hydr.ana$frac.in2)
# returns: 0.2774
# median(hydr.ana$frac.runon)
# returns: 0.3999

## Bar chart of event data
# showing elements of hydrology as fraction of inflow
hydr.ana.bar <- hydr.ana %>%
  select(starts_with("frac."),
         storm.index) %>%
  melt(id = "storm.index")
# View(hydr.ana.bar)
# bar chart
ggplot(data = hydr.ana.bar, aes(x = as.character(storm.index), y = value, fill = variable)) +
  geom_col()+
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"), labels = c("IN1 Flow", "IN2 Flow", "Runon", "Direct Precipitation", "ET", "Exfiltration", "Outflow"))+
  labs(y = "Components as Fraction of Total Inflow", x = "Event")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

## pEAK INFLOW RATES  
peak.in <- evt.corr %>%
  select(timestamp,
         rainfall.mm,
         in1.corr,
         in2.hobo.m_flow,
         storm.index) %>%
  group_by(storm.index) %>%
  summarise(maxin1 = max(in1.corr, na.rm = TRUE),
            maxin2 = max(in2.hobo.m_flow, na.rm = TRUE),
            varin1 = var(in1.corr, na.rm = TRUE),
            varin2 = var(in2.hobo.m_flow, na.rm = TRUE)) 
# View(peak.in)

  # range(peak.in$maxin1)
  # returns: 0.001144186 0.454802176
  # median(peak.in$maxin1)
  # returns: 0.1719473
  # range(peak.in$maxin2)
  # returns: 0.1221213 0.6140065
  # median(peak.in$maxin2)
  # returns: 0.2430239
  # median(peak.in$varin1)
  # returns: 0.003366857
  # median(peak.in$varin2)
  # returns: 0.002277754
  
## Box plot of peak inflows


## Normality and similarity btw influent
in.sig <- hydr.ana %>%
  subset(storm.index == 23 |
           storm.index == 32 |
           storm.index == 42 |
           storm.index == 46 |
           storm.index == 76 |
           storm.index == 78 |
           storm.index == 80 |
           storm.index == 86 |
           storm.index == 89 |
           storm.index == 91) %>%
  select(in1.vol,
         in2.hobo.vol)

## Normality tes of inflow volumes
#shapiro.test(as.numeric(in.sig$in1.vol))
# Shapiro-Wilk normality test
#
# data:  as.numeric(in.sig$in1.vol)
# W = 0.86832, p-value = 0.09556
#shapiro.test(as.numeric(in.sig$in2.hobo.vol))
# Shapiro-Wilk normality test
#
# data:  as.numeric(in.sig$in2.hobo.vol)
# W = 0.80414, p-value = 0.01628

## significant difference between inflow volumes
# wilcox.test(as.numeric(in.sig$in1.vol), as.numeric(in.sig$in2.hobo.vol), paired = TRUE)
# Wilcoxon signed rank test
#
# data:  as.numeric(in.sig$in1.vol) and as.numeric(in.sig$in2.hobo.vol)
# V = 50, p-value = 0.01953
# alternative hypothesis: true location shift is not equal to 0

# ## Peak Flow Reduction
# # same subset as previous
# evt.PR <- evt.corr.2 %>%
#   subset(storm.index == 23 |
#            storm.index == 32 |
#            storm.index == 42 |
#            storm.index == 46 |
#            storm.index == 76 |
#            storm.index == 78 |
#            storm.index == 80 |
#            storm.index == 86 |
#            storm.index == 89 |
#            storm.index == 91) %>%
#   select(storm.index,
#          in1.m_flow,
#          in2.hobo.m_flow,
#          out.flow) %>%
#   group_by(storm.index) %>%
#   summarise(in1p = max(in1.m_flow, na.rm = TRUE),
#          in2p = max(in2.hobo.m_flow, na.rm = TRUE),
#          outp = max(out.flow, na.rm = TRUE))
# # View(evt.PR)
# 
# ## Grab frac of inflow volumes
# in.frac <- hydr.ana %>%
#   subset(storm.index == 23 |
#            storm.index == 32 |
#            storm.index == 42 |
#            storm.index == 46 |
#            storm.index == 76 |
#            storm.index == 78 |
#            storm.index == 80 |
#            storm.index == 86 |
#            storm.index == 89 |
#            storm.index == 91) %>%
#   select(frac.in1,
#          frac.in2,
#          insum,
#          storm.index)
# 
# ## Join datasets
# evt.PR <- left_join(evt.PR, in.frac, by = "storm.index")
# 
# ## Calculate inlet Peak Flow
# # composited (weighted average method)
# evt.PR <- evt.PR %>%
#   group_by(storm.index) %>%
#   mutate(in.peak = (in1p * frac.in1) + (in2p * frac.in2),
#          PR = ((in.peak - outp) / as.numeric(in.peak)) * 100,
#          in.med = median(c(in1p, in2p)),
#          PR.med = ((in.med - outp) / as.numeric(in.med) * 100))
# # View(evt.PR)
# # median(as.numeric(evt.PR$PR))
# # -192.4113

## Baseflow corections and analysis
# Determine events where correction model applies
base.corr <- (RSC.hydro.m) %>%
  subset(timestamp >= as.POSIXct("2017-09-14") & 
           ADP.index > 0 ) %>%
  select(timestamp,
         ADP.index,
         in1.m_flow,
         in2.hobo.m_flow,
         out.flow)
#View(base.corr)

## ADP event sum
base.1 <- base.corr %>%
  group_by(ADP.index) %>%
  summarise(duration = difftime(max(timestamp), min(timestamp), units = "days"),
         in.vol = sum((in1.m_flow + in2.hobo.m_flow) * 120, na.rm = TRUE),
         out.vol = sum(out.flow * 120, na.rm = TRUE)) %>%
  mutate(perc_diff= ((in.vol - out.vol) / as.numeric(in.vol)) * 100)
# View(base.1)

## inflow ouflow
base.bal <- base.corr %>%
  select(timestamp,
         in1.m_flow,
         in2.hobo.m_flow,
         out.flow) %>%
  melt(id = "timestamp")

## Apply baseflow corrections
base.corr.1 <- base.corr %>%
  mutate(in1.base.corr = base.flow.corr(in1.m_flow))
#View(base.corr.1)

## select original values for understanding
scrap2 <- (RSC.hydro.m) %>%
  subset(timestamp >= as.POSIXct("2017-09-14") & 
           ADP.index > 0) %>%
  select(timestamp,
         in1.m_flow)
# Rename columns
colnames(scrap2) <- c("timestamp", "in1.m_flow.OG")

## Temp
corted.baseflow.plot <- base.corr.1 %>%
  select(timestamp,
         in1.base.corr)
# join
corted.baseflow.plot.1 <- left_join(corted.baseflow.plot, scrap2, by = "timestamp")
# melt
corted.baseflow.plot.m <- corted.baseflow.plot.1 %>%
  melt(id = "timestamp")

# plot
ggplot(corted.baseflow.plot.m)+
  geom_point(aes(x = timestamp, y = value, color = variable, shape = variable))+
  scale_color_manual(values = c("red", "black"), labels = c("Corrected In1 Weir", "IN1 Weir"))+
  scale_shape_manual(values = c(0,1,2), labels = c("Corrected In1 Weir", "IN1 Weir"))+
  labs(y = "Flow Rate (cms)", x = "Date")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))


ggplot(base.bal)+
  geom_point(aes(x = timestamp, y = value, color = variable, shape = variable))+
 #scale_color_manual(values = c("red", "black"), labels = c("Corrected In1 Weir", "IN1 Weir"))+
  #scale_shape_manual(values = c(0,1,2), labels = c("Corrected In1 Weir", "IN1 Weir"))+
  labs(y = "Flow Rate (cms)", x = "Date")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))


## Load reductions
load <- hydr.ana %>%
  select(insum,  # insum in m^3
         Accumulation) %>%
  mutate(IN.TKN = (as.numeric(insum) * 1.6) / 1000,
         IN.TAN = (as.numeric(insum) * 0.15) / 1000,
         IN.NO3 = (as.numeric(insum) * 0.3) / 1000,
         IN.ON = (as.numeric(insum) * 1.5) / 1000,
         IN.TN = (as.numeric(insum) * 2.0) / 1000,# concentration units mg/L from WQDS Inlet concentration observed
         IN.OP = (as.numeric(insum) * 0.05) / 1000,
         IN.PBP = (as.numeric(insum) * 0.3) / 1000,
         IN.TP = (as.numeric(insum) * 0.3) / 1000, 
         IN.TSS = (as.numeric(insum) * 116.3) / 1000,
         OUT.TKN = (as.numeric(insum) * 1.4) / 1000,
         OUT.TAN = (as.numeric(insum) * 0.1) / 1000,
         OUT.NO3 = (as.numeric(insum) * 0.6) / 1000,
         OUT.ON = (as.numeric(insum) * 1.2) / 1000,
         OUT.TN = (as.numeric(insum) * 2.0) / 1000,# concentration units mg/L from WQDS Inlet concentration observed
         OUT.OP = (as.numeric(insum) * 0.07) / 1000,
         OUT.PBP = (as.numeric(insum) * 0.1) / 1000,
         OUT.TP = (as.numeric(insum) * 0.2) / 1000, 
         OUT.TSS = (as.numeric(insum) * 37.9) / 1000) 
# View(load)

## Annual Loads
load1 <- load[,3:20]
# View(load1)
# fraction of normal rainfall observed during monitoring
n.rain.frac <- 1169.1632 / sum(load$Accumulation) 
# drainage area (ha)
DA <- 20.3
# Annual percent load reduction + export
load2 <- load1 %>%
  summarise(TKN.MASS.IN = sum(IN.TKN), # sum mass in kgrams
            TAN.MASS.IN = sum(IN.TAN),
            NO3.MASS.IN = sum(IN.NO3),
            ON.MASS.IN = sum(IN.ON),
            TN.MASS.IN = sum(IN.TN),
            OP.MASS.IN = sum(IN.OP),
            PBP.MASS.IN = sum(IN.PBP),
            TP.MASS.IN = sum(IN.TP),
            TSS.MASS.IN = sum(IN.TSS),
            TKN.MASS.OUT = sum(OUT.TKN),
            TAN.MASS.OUT = sum(OUT.TAN),
            NO3.MASS.OUT = sum(OUT.NO3),
            ON.MASS.OUT = sum(OUT.ON),
            TN.MASS.OUT = sum(OUT.TN),
            OP.MASS.OUT = sum(OUT.OP),
            PBP.MASS.OUT = sum(OUT.PBP),
            TP.MASS.OUT = sum(OUT.TP),
            TSS.MASS.OUT = sum(OUT.TSS)) %>%
  transmute(TKN.m.red = ((TKN.MASS.IN - TKN.MASS.OUT)  / TKN.MASS.IN) * 100,
            TAN.m.red = ((TAN.MASS.IN - TAN.MASS.OUT)  / TAN.MASS.IN) * 100,
            NO3.m.red = ((NO3.MASS.IN - NO3.MASS.OUT)  / NO3.MASS.IN) * 100,
            ON.m.red = ((ON.MASS.IN - ON.MASS.OUT)  / ON.MASS.IN) * 100,
            TN.m.red = ((TN.MASS.IN - TN.MASS.OUT)  / TN.MASS.IN) * 100,
            OP.m.red = ((OP.MASS.IN - OP.MASS.OUT)  / OP.MASS.IN) * 100,
            PBP.m.red = ((PBP.MASS.IN - PBP.MASS.OUT)  / PBP.MASS.IN) * 100,
            TP.m.red = ((TP.MASS.IN - TP.MASS.OUT)  / TP.MASS.IN) * 100,
            TSS.m.red = ((TSS.MASS.IN - TSS.MASS.OUT)  / TSS.MASS.IN) * 100,
            TKN.m.OUT = ((TKN.MASS.OUT) / (DA )) * n.rain.frac,
            TAN.m.OUT = ((TAN.MASS.OUT) / (DA )) * n.rain.frac,
            NO3.m.OUT = ((NO3.MASS.OUT) / (DA )) * n.rain.frac,
            ON.m.OUT = ((ON.MASS.OUT) / (DA )) * n.rain.frac,
            TN.m.OUT = ((TN.MASS.OUT) / (DA )) * n.rain.frac,
            OP.m.OUT = ((OP.MASS.OUT) / (DA )) * n.rain.frac,
            PBP.m.OUT = ((PBP.MASS.OUT) / (DA )) * n.rain.frac,
            TP.m.OUT = ((TP.MASS.OUT) / (DA )) * n.rain.frac,
            TSS.m.OUT = ((TSS.MASS.OUT) / (DA)) * n.rain.frac)
# View(load2)

# Annual Mass load reduction
load3 <- load1 %>%
  summarise(TKN.MASS.IN = sum(IN.TKN), # sum mass in kgrams
            TAN.MASS.IN = sum(IN.TAN),
            NO3.MASS.IN = sum(IN.NO3),
            ON.MASS.IN = sum(IN.ON),
            TN.MASS.IN = sum(IN.TN),
            OP.MASS.IN = sum(IN.OP),
            PBP.MASS.IN = sum(IN.PBP),
            TP.MASS.IN = sum(IN.TP),
            TSS.MASS.IN = sum(IN.TSS),
            TKN.MASS.OUT = sum(OUT.TKN),
            TAN.MASS.OUT = sum(OUT.TAN),
            NO3.MASS.OUT = sum(OUT.NO3),
            ON.MASS.OUT = sum(OUT.ON),
            TN.MASS.OUT = sum(OUT.TN),
            OP.MASS.OUT = sum(OUT.OP),
            PBP.MASS.OUT = sum(OUT.PBP),
            TP.MASS.OUT = sum(OUT.TP),
            TSS.MASS.OUT = sum(OUT.TSS)) %>%
  transmute(TKN.m.red.in = ((TKN.MASS.IN)) * n.rain.frac,
            TKN.m.red.out = ((TKN.MASS.OUT)) * n.rain.frac,
            TAN.m.red.in = ((TAN.MASS.IN)  ) * n.rain.frac,
            TAN.m.red.out = ((TAN.MASS.OUT) ) * n.rain.frac,
            NO3.m.red.in = ((NO3.MASS.IN)  ) * n.rain.frac,
            NO3.m.red.out = ((NO3.MASS.OUT) ) * n.rain.frac,
            ON.m.red.in = ((ON.MASS.IN)  ) * n.rain.frac,
            ON.m.red.out = ((ON.MASS.OUT)) * n.rain.frac,
            TN.m.red.in = ((TN.MASS.IN) ) * n.rain.frac,
            TN.m.red.out = ((TN.MASS.OUT) ) * n.rain.frac,
            OP.m.red.in = ((OP.MASS.IN)  ) * n.rain.frac,
            OP.m.red.out = ((OP.MASS.OUT)  ) * n.rain.frac,
            PBP.m.red.in = ((PBP.MASS.IN)) * n.rain.frac,
            PBP.m.red.out = ((PBP.MASS.OUT)) * n.rain.frac,
            TP.m.red.in = ((TP.MASS.IN) ) * n.rain.frac,
            TP.m.red.out = ((TP.MASS.OUT) ) * n.rain.frac,
            TSS.m.red.in = ((TSS.MASS.IN) ) * n.rain.frac,
            TSS.m.red.out = ((TSS.MASS.OUT)) * n.rain.frac)
# View(load3)

## base flow loads
base.load <- base.corr %>% 
  select(in1.m_flow) %>% 
  summarise(tot.vol = sum(in1.m_flow * 120, na.rm = TRUE)) 
# View(base.load)

# Mass sum
base.load1 <- base.load %>%
  transmute(IN.TKN = tot.vol * 0.7 / 1000,
            IN.TAN = tot.vol * 0.09 / 1000,
            IN.NO3 = tot.vol * 0.1 / 1000,
            IN.ON = tot.vol * 0.6 / 1000,
            IN.TN = tot.vol * 0.9 / 1000,# concentration units mg/L from WQDS Inlet concentration observed
            IN.OP = tot.vol * 0.09 / 1000,
            IN.PBP = tot.vol * 0.04 / 1000,
            IN.TP = tot.vol * 0.1 / 1000, 
            IN.TSS = tot.vol * 3.3 / 1000,
            OUT.TKN = tot.vol * 0.5 / 1000,
            OUT.TAN = tot.vol * 0.06 / 1000,
            OUT.NO3 = tot.vol * 0.1 / 1000,
            OUT.ON = tot.vol * 0.5 / 1000,
            OUT.TN = tot.vol * 0.7 / 1000,# concentration units mg/L from WQDS Inlet concentration observed
            OUT.OP = tot.vol * 0.1 / 1000,
            OUT.PBP = tot.vol * 0.04 / 1000,
            OUT.TP = tot.vol * 0.1 / 1000, 
            OUT.TSS = tot.vol * 2.8 / 1000)
# View(base.load1)

# Annual percent load reduction + mass export
base.load2 <- base.load1 %>%
    transmute(TKN.m.red = ((IN.TKN - OUT.TKN) / IN.TKN) * 100 ,
            TAN.m.red = ((IN.TAN - OUT.TAN) / IN.TAN) * 100,
            NO3.m.red = ((IN.NO3 - OUT.NO3) / IN.NO3) * 100 ,
            ON.m.red = ((IN.ON - OUT.ON) / IN.ON) * 100 ,
            TN.m.red = ((IN.TN - OUT.TN) / IN.TN) * 100,
            OP.m.red = ((IN.OP - OUT.OP) / IN.OP) * 100,
            PBP.m.red = ((IN.PBP - OUT.PBP) / IN.PBP) * 100,
            TP.m.red = ((IN.TP - OUT.TP) / IN.TP) * 100,
            TSS.m.red = ((IN.TSS - OUT.TSS) / IN.TSS) * 100,
            TKN.m.OUT = ((OUT.TKN) / (DA )),
            TAN.m.OUT = ((OUT.TAN) / (DA )),
            NO3.m.OUT = ((OUT.NO3) / (DA )),
            ON.m.OUT = ((OUT.ON) / (DA )),
            TN.m.OUT = ((OUT.TN) / (DA )),
            OP.m.OUT = ((OUT.OP) / (DA )),
            PBP.m.OUT = ((OUT.PBP) / (DA )),
            TP.m.OUT = ((OUT.TP) / (DA )),
            TSS.m.OUT = ((OUT.TSS) / (DA)))
# View(base.load2)

## annual baseflow mass load reduction
base.load3 <- base.load1 %>%
  transmute(TKN.m.red.in = ((IN.TKN) ) ,
            TKN.m.red.out = ((OUT.TKN) ) ,
            TAN.m.red.in = ((IN.TAN) ),
            TAN.m.red.out = ((OUT.TAN) ),
            NO3.m.red.in = ((IN.NO3) ) ,
            NO3.m.red.out = ((OUT.NO3) ) ,
            ON.m.red.in = ((IN.ON) ) ,
            ON.m.red.out = ((OUT.ON) ) ,
            TN.m.red.in = ((IN.TN) ),
            TN.m.red.out = ((OUT.TN) ),
            OP.m.red.in = ((IN.OP) ),
            OP.m.red.out = ((OUT.OP) ),
            PBP.m.red.in = ((IN.PBP) ),
            PBP.m.red.out = ((OUT.PBP) ),
            TP.m.red.in = ((IN.TP)),
            TP.m.red.out = ((OUT.TP) ),
            TSS.m.red.in = ((IN.TSS) ),
            TSS.m.red.out = ((OUT.TSS) ))
# View(base.load3)

## Total mass load reduction
tot.LR <- load3 %>%
  transmute(TKN = (((TKN.m.red.in + base.load3$TKN.m.red.in) - (TKN.m.red.out + base.load3$TKN.m.red.out)) / (TKN.m.red.in + base.load3$TKN.m.red.in)) * 100,
            TAN = (((TAN.m.red.in + base.load3$TAN.m.red.in) - (TAN.m.red.out + base.load3$TAN.m.red.out)) / (TAN.m.red.in + base.load3$TAN.m.red.in)) * 100,
            NO3 = (((NO3.m.red.in + base.load3$NO3.m.red.in) - (NO3.m.red.out + base.load3$NO3.m.red.out)) / (NO3.m.red.in + base.load3$NO3.m.red.in)) * 100,
            ON = (((ON.m.red.in + base.load3$ON.m.red.in) - (ON.m.red.out + base.load3$ON.m.red.out)) / (ON.m.red.in + base.load3$ON.m.red.in)) * 100,
            TN = (((TN.m.red.in + base.load3$TN.m.red.in) - (TN.m.red.out + base.load3$TN.m.red.out)) / (TN.m.red.in + base.load3$TN.m.red.in)) * 100,
            OP = (((OP.m.red.in + base.load3$OP.m.red.in) - (OP.m.red.out + base.load3$OP.m.red.out)) / (OP.m.red.in + base.load3$OP.m.red.in)) * 100,
            PBP = (((PBP.m.red.in + base.load3$PBP.m.red.in) - (PBP.m.red.out + base.load3$PBP.m.red.out)) / (PBP.m.red.in + base.load3$PBP.m.red.in)) * 100,
            TP = (((TP.m.red.in + base.load3$TP.m.red.in) - (TP.m.red.out + base.load3$TP.m.red.out)) / (TP.m.red.in + base.load3$TP.m.red.in)) * 100,
            TSS = (((TSS.m.red.in + base.load3$TSS.m.red.in) - (TSS.m.red.out + base.load3$TSS.m.red.out)) / (TSS.m.red.in + base.load3$TSS.m.red.in)) * 100)
# View(tot.LR)

## Total mass export
tot.EE <- load2 %>%
  select(ends_with(".OUT"))
tot.BE <- base.load2 %>%
  select(ends_with(".OUT"))
tot.E <- tot.EE %>%
  transmute(TKN = TKN.m.OUT + tot.BE$TKN.m.OUT,
            TAN = TAN.m.OUT + tot.BE$TAN.m.OUT,
            NO3 = NO3.m.OUT + tot.BE$NO3.m.OUT,
            ON = ON.m.OUT + tot.BE$ON.m.OUT,
            TN = TN.m.OUT + tot.BE$TN.m.OUT,
            OP = OP.m.OUT + tot.BE$OP.m.OUT,
            PBP = PBP.m.OUT + tot.BE$PBP.m.OUT,
            TP = TP.m.OUT + tot.BE$TP.m.OUT,
            TSS = TSS.m.OUT + tot.BE$TSS.m.OUT)
# View(tot.E)

## significance in load analysis
# event load reductions
# TKN
wilcox.test(load$IN.TKN, load$OUT.TKN, alternative = "t", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns
# Wilcoxon signed rank test
# 
# data:  load$IN.TKN and load$OUT.TKN
# V = 210, p-value = 1.907e-06
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   2.357077 3.708090
# sample estimates:
#   (pseudo)median 
# 3.005374 

# TAN
wilcox.test(load$IN.TAN, load$OUT.TAN, alternative = "t", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns
# Wilcoxon signed rank test
# 
# data:  load$IN.TAN and load$OUT.TAN
# V = 210, p-value = 1.907e-06
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   0.5892693 0.9270226
# sample estimates:
#   (pseudo)median 
# 0.7513434

# NO3
wilcox.test(load$IN.NO3, load$OUT.NO3, alternative = "t", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns
# Wilcoxon signed rank test
# 
# data:  load$IN.NO3 and load$OUT.NO3
# V = 0, p-value = 1.907e-06
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   -5.562136 -3.535616
# sample estimates:
#   (pseudo)median 
# -4.50806 

# ON
wilcox.test(load$IN.ON, load$OUT.ON, alternative = "t", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns
# Wilcoxon signed rank test
# 
# data:  load$IN.ON and load$OUT.ON
# V = 210, p-value = 1.907e-06
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   3.535616 5.562136
# sample estimates:
#   (pseudo)median 
# 4.50806  

# TN
wilcox.test(load$IN.TN, load$OUT.TN, alternative = "t", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns
# NaN

# OP
wilcox.test(load$IN.OP, load$OUT.OP, alternative = "t", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns:
# Wilcoxon signed rank test
# 
# data:  load$IN.OP and load$OUT.OP
# V = 0, p-value = 1.907e-06
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   -0.3708090 -0.2357077
# sample estimates:
#   (pseudo)median 
# -0.3005374

# PBP
wilcox.test(load$IN.PBP, load$OUT.PBP, alternative = "t", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns:
# Wilcoxon signed rank test
# 
# data:  load$IN.PBP and load$OUT.PBP
# V = 210, p-value = 1.907e-06
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   2.357077 3.708090
# sample estimates:
#   (pseudo)median 
# 3.005374 

# TP
wilcox.test(load$IN.TP, load$OUT.TP, alternative = "t", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns:
# Wilcoxon signed rank test
# 
# data:  load$IN.TP and load$OUT.TP
# V = 210, p-value = 1.907e-06
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   1.178539 1.854045
# sample estimates:
#   (pseudo)median 
# 1.502687


# TSS
wilcox.test(load$IN.TSS, load$OUT.TSS, alternative = "t", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns
# Wilcoxon signed rank test
# 
# data:  load$IN.TSS and load$OUT.TSS
# V = 210, p-value = 1.907e-06
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   923.9743 1453.5715
# sample estimates:
#   (pseudo)median 
# 1178.106 


