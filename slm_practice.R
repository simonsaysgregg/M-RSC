#Linear Regression practice
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
require("DataCombine")
require("gvlma")
## Mapping tools
require("stringi")
require("ggmap")        # Plotting of maps same as you would with ggplot2
require("maptools")     # Read, write, and handle Shapefiles in R
require("mapdata")      # Supplement to maps package

## Read file from ./Working folder
DS.flow <- read.csv("./Working/flow.dataset.csv")
# View(DS.flow)
## Read file from ./Working folder
DS.events <- read.csv("./Working/Rainsum_event_analysis.csv")
# View(DS.events)
# Format date time
DS.flow$timestamp <- ymd_hms(DS.flow$timestamp)
#View(DS.flow)
# Format date time
DS.events$start <- ymd_hms(DS.events$start)
#View(DS.events)


## Subsets events for use in creation of correction
# Event summary from hydro.ana_
corr.events <- (DS.events) %>%
  subset(start >= as.POSIXct("2018-05-25") & start <= as.POSIXct("2018-07-06"))
#View(corr.events)

## Create a INflow dataset
DS.inflow <- (DS.flow) %>%
  select(timestamp,
         in1.m_flow,
         dryout.m_flow)%>%
  subset(#timestamp >= as.POSIXct("2018-05-28 02:28:00") & timestamp <= as.POSIXct("2018-05-30 06:28:00") |
          # timestamp >= as.POSIXct("2018-05-30 14:58:00") & timestamp <= as.POSIXct("2018-05-31 11:04:00") |
           #timestamp >= as.POSIXct("2018-06-10 20:20:00") & timestamp <= as.POSIXct("2018-06-11 14:26:00") |
           timestamp >= as.POSIXct("2018-06-26 07:50:00") & timestamp <= as.POSIXct("2018-06-26 20:14:00") |
           timestamp >= as.POSIXct("2018-07-05 13:26:00") & timestamp <= as.POSIXct("2018-07-06 07:56:00")) 
#View(DS.inflow)

## Melt inflow Dataset 
DS.inflow.m <- (DS.inflow) %>%
  select(timestamp,
         in1.m_flow,
         dryout.m_flow)%>%
  melt(id = "timestamp")
#View(DS.inflow.m)
## Plot events for calibration
ggplot(DS.inflow.m, aes(x = timestamp))+
  geom_point(aes(y = value, color = variable))+
  scale_shape_manual(values = c("2", "16"), labels = c("Weir", "Outlet"))+
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "6 day")+
  labs(y = "Flow Rate (cms)", x = "Date")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

## scatter plot DS.inflow
ggplot(DS.inflow, aes(x = in1.m_flow, y = dryout.m_flow))+
  geom_point()+
  geom_smooth(method = lm)+
  geom_abline(aes(intercept = 0, slope = 1))

## box plots DS.inflow
ggplot(DS.inflow.m)+
  geom_boxplot(aes(x = variable, y = value))

## Density DS.inflow
ggplot(DS.inflow, aes(x = in1.m_flow))+
  geom_density()

ggplot(DS.inflow, aes(x = dryout.m_flow))+
  geom_density()

## subset flow values to remove low event flows
flow.short <- DS.inflow %>%
  subset(in1.m_flow >= 0.05)
#View(flow.short)
# melt
flow.short.m <- flow.short %>%
  select(timestamp,
         in1.m_flow,
         dryout.m_flow)%>%
  melt(id = "timestamp")

## Plot events for calibration
ggplot(DS.inflow.m, aes(x = timestamp))+
  geom_point(aes(y = value, color = variable))+
  scale_shape_manual(values = c("2", "16"), labels = c("Weir", "Outlet"))+
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "6 day")+
  labs(y = "Flow Rate (cms)", x = "Date")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

## scatter plot
ggplot(flow.short, aes(x = in1.m_flow, y = dryout.m_flow))+
  geom_point()+
  geom_smooth(method = lm)+
  geom_abline(aes(intercept = 0, slope = 1))

## box plots
ggplot(flow.short.m)+
  geom_boxplot(aes(x = variable, y = value))

## Density
ggplot(flow.short, aes(x = in1.m_flow))+
  geom_density()

ggplot(DS.inflow, aes(x = dryout.m_flow))+
  geom_density()

## lm
## diagnostic plots
lm1 <- lm(dryout.m_flow ~ in1.m_flow, data = flow.short)
summary(lm1)
par(mfrow=c(2,2))
plot(lm1)

## normality and autocorrelation corrections
# rolling average 30-min window plus lag1
# Acrsin sqrt transform response
flow.short.corr <- flow.short %>%
transmute(weir = rollapply(in1.m_flow, 15, mean, fill = NA),
           dryout = rollapply(dryout.m_flow, 15, mean, fill = NA),
           log.weir = log(weir + 0.01),
           log.dryout = log(dryout + 0.01),
           arc.dryout = asin(sqrt(dryout.m_flow)),
           arc.dryout.roll = asin(sqrt(dryout)))
View(flow.short.corr)

lm1 <- lm(log.dryout ~ log.weir, data = flow.short.corr)
summary(lm1)
par(mfrow=c(2,2))
plot(lm1)

# Rectify autocorrelation
DS.flow1 <- na.omit(flow.short.corr)
resid_linear <- lm1$residuals
DS.flow1[, "resid_linear"] <- resid_linear
DS.flow2 <- slide(DS.flow1, Var="resid_linear", NewVar = "lag1", slideBy = -1)
DS.flow3 <- na.omit(DS.flow2)

lm2 <- lm(log.dryout ~ log.weir + lag1, data = DS.flow3)
summary(lm2)
par(mfrow=c(2,2))
plot(lm2)

acf(lm2$residuals)
















