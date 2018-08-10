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
                           day = floor_date(timestamp, 'day')) %>%
                    group_by(ADP.index, day) %>%
                  summarise(well.cm = mean(well.cm))
# View(infil.pros1)
# Add count day
infil.pros2 <- infil.pros1 %>%
  group_by(ADP.index) %>%
  mutate(start = min(day),
         end = max(day),
         total = (end - start)  / 86400, # conversion to days
         durat = (day - start) / 86400) %>%
  subset(as.numeric(total) >= 1)
# View(infil.pros2)

## Infiltraiton estimation
infil.est <- infil.pros2 %>%
  select(well.cm,
         ADP.index,
         durat) %>%
  subset(ADP.index != 25) %>%
  subset(ADP.index != 41) %>%
  subset(ADP.index != 91) %>%
  mutate(delta.stage = lag(well.cm) - well.cm) %>%
  subset(!is.na(delta.stage)) 
# ## count
# countess <- seq_len(181)
# infil.est[, "Position"] <- countess
# View(infil.est)


## Plot infil.est
ggplot(infil.est, aes(x = well.cm, y = delta.stage))+
  geom_point(aes(x = well.cm, y = delta.stage))+
  #geom_text(aes(label = ifelse(delta.stage > 2,as.character(Position),'')),hjust=0,vjust=0)+
  labs(y = "Change in Well Stage (cm/day)", x = "Well Stage (cm)")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))+
  geom_smooth(method = lm, se = FALSE, aes(x = well.cm, y = delta.stage))


# ## Seasonal
# ## subset seasons
# # 26-68-- Autum-Winter
# # 68-93-- Spring-Summer
# autwin <- infil.est %>%
#   subset(ADP.index <= 74)
# sprsum <- infil.est %>%
#   subset(ADP.index >= 75)
# ## Plot Autum-Winter
# ggplot(autwin)+
#   geom_point(aes(x = well.cm, y = delta.stage, color = "black"))+
#   #geom_point(data = infil.est[c(151,93,89,88,95,101), ], aes(color = "red"))+
#   #geom_text(aes(label = ifelse(delta.stage > 2,as.character(Position),'')),hjust=0,vjust=0)+
#   labs(y = "Change in Well Stage (cm/day)", x = "Well Stage (cm)")+
#   theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))+
#   geom_smooth(method = lm, se = FALSE, aes(x = well.cm, y = delta.stage))
# ## Plot Sprin- Summer
# ggplot(sprsum)+
#   geom_point(aes(x = well.cm, y = delta.stage, color = "black"))+
#   #geom_point(data = infil.est[c(151,93,89,88,95,101), ], aes(color = "red"))+
#   #geom_text(aes(label = ifelse(delta.stage > 2,as.character(Position),'')),hjust=0,vjust=0)+
#   labs(y = "Change in Well Stage (cm/day)", x = "Well Stage (cm)")+
#   theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))+
#   geom_smooth(method = lm, se = FALSE, aes(x = well.cm, y = delta.stage))

## Linear model development
##lm
lmbf <- lm(delta.stage ~ well.cm , data = infil.est[,])
summary(lmbf)
par(mfrow=c(2,2))
plot(lmbf)
# normality
#shapiro.test(infil.est1$log.stage)

# ## mutate for appropriate normality corrections
# infil.est1 <- infil.est %>%
#   mutate(log.stage = log(well.cm + 0.01))
# 
# ##lm
# lmbf1 <- lm(delta.stage ~ well.cm, data = autwin[,])
# summary(lmbf1)
# par(mfrow=c(2,2))
# plot(lmbf1)

# Autocorrelation
#acf(lmbf$residuals)
runs.test(lmbf$residuals)
dwtest(lmbf)


#### Not finished
# # Rectify autocorrelation
# infilltrate <- na.omit(infil.est[,])
# resid_linear <- lmbf$residuals
# infilltrate[, "resid_linear"] <- resid_linear
# infilltrate <- na.omit(infilltrate[,])
# infilltrate.1 <- slide(infilltrate, Var="resid_linear", NewVar = "lag1", slideBy = -1)
# infilltrate.2 <- na.omit(infilltrate.1)
# 
# lmbf.auto <- lm(delta.stage ~ well.cm + lag1, data = infilltrate.2[,])
# summary(lmbf.auto)
# par(mfrow=c(2,2))
# plot(lmbf.auto)