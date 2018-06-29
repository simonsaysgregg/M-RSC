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

#plot
ggplot(DS.flow, aes(x = in1.m_flow, y = dryout.m_flow))+
  geom_smooth(method = nlm)


## Polynomial regression of inflow methods
quad <- lm(dryout.m_flow ~ in1.m_flow + I(in1.m_flow^2), data = DS.flow)
summary(quad)

## In1 flow correction with polynomial
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

## linear regression of inflow methods
linear <- lm(dryout.m_flow ~ in1.m_flow, data = DS.flow)
summary(linear)

## In1 flow correction with polynomial
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
