## For flow calibration and diagnostic analysis

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
# Format date time
DS.flow$timestamp <- ymd_hms(DS.flow$timestamp)
#View(DS.flow)

## Creation of dataset for analysis & correction 
# from after dryout installation
DS.flow.both <- (DS.flow)%>%
  select(timestamp,
         in1.m_flow,
         dryout.m_flow)%>%
  subset(timestamp >= "2018-05-25 15:12:00" & timestamp <= "2018-06-27 07:14:00")
#View(DS.flow.both)

## Primary inlet diagnosis and correction
## linear regression of inflow methods
linear <- lm(log(dryout.m_flow) ~ in1.m_flow, data = DS.flow.both)
summary(linear)
# Check model mean residuals -- should be near zero
mean(linear$residuals)
# resutns: 1.354635e-16

# Equal variance check
par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
plot(linear)

# Autocorrelation check
acf(linear$residuals)

# Rectify autocorrelation
DS.flow1 <- na.omit(DS.flow.both)
resid_linear <- linear$residuals
DS.flow1[, "resid_linear"] <- resid_linear
DS.flow2 <- slide(DS.flow1, Var="resid_linear", NewVar = "lag1", slideBy = -1)
DS.flow3 <- na.omit(DS.flow2)
linear2 <- lm(log(dryout.m_flow) ~ in1.m_flow + lag1, data = DS.flow3)

# Review model
summary(linear2)
mean(linear2$residuals)
# returns: -3.182592e-17
acf(linear2$residuals)
gvlma(quad)

# Equal variance check
par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
plot(linear2)

## Polynomial regression of inflow methods
quad <- lm(log(dryout.m_flow) ~ in1.m_flow + I(in1.m_flow^2), data = DS.flow)
summary(quad)
# Check model mean residuals -- should be near zero
mean(quad$residuals)
# resutns:  2.710028e-16
# Equal variance check
par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
plot(quad)

# Autocorrelation check
acf(quad$residuals)

# Rectify autocorrelation
DS.flowq1 <- na.omit(DS.flow.both)
resid_quad <- quad$residuals
DS.flowq1[, "resid_quad"] <- resid_quad
DS.flowq2 <- slide(DS.flowq1, Var="resid_quad", NewVar = "lag1", slideBy = -1)
DS.flowq3 <- na.omit(DS.flowq2)
quad2 <- lm(log(dryout.m_flow) ~ in1.m_flow + lag1, data = DS.flow3)

# Review model
summary(quad2)
mean(quad2$residuals)
# returns: -3.182592e-17
acf(quad2$residuals)
gvlma(quad2)

# Equal variance check
par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
plot(quad2)

## In1 flow correction with polynomial
##### need to update
DS.flow.quad <- (DS.flow)%>%
  select(timestamp,
         in1.m_flow,
         dryout.m_flow)%>%
  mutate(in1.m_flow.quad = 0.006089 + (2.507 * in1.m_flow) - (8.725 * in1.m_flow^2) + 0.007274)
#View(DS.flow.quad)
## Melt in1 flow Dataset 
DS.flow.quad.melt <- (DS.flow.quad) %>%
  melt(id = "timestamp")
#View(DS.flow.quad.melt)
#plot
ggplot(DS.flow.quad.melt, aes(x = timestamp))+
  geom_line(aes(y = value, colour = variable))

## In1 flow correction with linear model
DS.flow.lin <- (DS.flow)%>%
  select(timestamp,
         in1.m_flow,
         dryout.m_flow)%>%
  mutate(in1.m_flow.lin = 0.006251 + (1.838 * in1.m_flow) + 0.007274)
#View(DS.flow.lin)
## Melt in1 flow Dataset 
DS.flow.lin.melt <- (DS.flow.lin) %>%
  melt(id = "timestamp")
#View(DS.flow.lin.melt)
#plot
ggplot(DS.flow.lin.melt, aes(x = timestamp))+
  geom_line(aes(y = value, colour = variable))



## Secondary inlet diagnosis and correction
DS.IN2.flow <- (DS.flow)%>%
  select(timestamp,
         in2.hobo.m_flow,
         in2.m_flow)
## Melt in2 flow Dataset 
DS.IN2.flow.melt <- (DS.IN2.flow) %>%
  melt(id = "timestamp")
#View(DS.IN2.flow.melt)
#plot
ggplot(DS.IN2.flow.melt, aes(x = timestamp))+
  geom_line(aes(y = value, colour = variable))

