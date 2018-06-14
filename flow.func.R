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
DS <- read.csv("./Working/dataset.csv")
# View(DS)
# Format date time
DS$timestamp <- ymd_hms(DS$timestamp)

## Select columns for hyrology analysis
DS.hydro <- DS %>%
  select("timestamp",
         "rain.in",
         "in1.ft",
         "dryout.ft",
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
            dryout.m = dryout.ft * 0.3048,
            in2.m = in2.ft * 0.3048,
            in2.hobo.m = in2.hobo.ft * 0.3048,
            out.m = out.ft * 0.3048,
            out.velo = out.velo * 0.3048,
            out.velo.roll = rollapply(out.velo, 10, mean, fill = NA),
            well.m = well.ft * 0.3048)
# View(DS.hydro.metric)

## Begin flow calculation
# user defined functions
# Inlet1 flow calculation function
flow.in1 <- function(in.m) { 
  ifelse(in.m < 0.1524, (1344 * (in.m^2.5)), ((1344 * (0.1524^2.5))+((3970.8 * (in.m^1.5))-(1323.6 * (in.m^2.5)))))                      
  # IFELSE determination of which weir calculation to use; stage in meters                                             
  # TRUE V-notch flow
  # FALSE V-notch + retangular w/ contraction flow
}

# Inlet2 flow calculation function
flow.in2 <- function(in.m) { 
  ifelse(in.m < 0.0889, (2868 * (in.m^2.5)), ((2868 * (0.0889^2.5))+((7279.8 * (in.m^1.5))-(2647.2 * (in.m^2.5)))))                      
  # IFELSE determination of which weir calculation to use; stage in meters                                             
  # TRUE V-notch flow
  # FALSE V-notch + retangular w/ contraction flow
}

# Outlet flow calculation
# ASABE area determination
# from ASABE Soil and Water Conservation ENGR
area.out.ASABE <- function(stage){
  ((0.9144^2) * acos((0.9144 - stage)/0.9144)) - ((0.9144 - stage) * sqrt(2*0.9144*stage - (stage^2))) 
}

# Outlet flow function
flow.out <- function(V,A){
  (V * A)
}
  
# Outlet flow velocity sign change
#out.sign <- function(velo){
 # ifelse(velo<0, velo * (-1), velo)
#}
  
## Mutate to add flow calculation using function
DS.hydro.metric <- DS.hydro.metric %>%
  mutate(in1.m_flow = flow.in1(in1.m),
         in2.m_flow = flow.in2(in2.m),
         in2.hobo.m_flow = flow.in2(in2.hobo.m),
         area.ASABE = area.out.ASABE(out.m),
         out.flow.roll.ASABE = flow.out(out.velo.roll, area.ASABE))
#View(DS.hydro.metric)

## Create flow dataset
DS.flow <- (DS.hydro.metric) %>%
  select(timestamp, 
         in1.m_flow, 
         in2.m_flow, 
         in2.hobo.m_flow, 
         out.flow.roll.ASABE) 
#View(DS.flow)  

## Create outlet flow dataset
DS.outflow <- (DS.hydro.metric) %>%
  select(timestamp,
         out.flow.roll.ASABE) 
#View(DS.flow)  

## Create a flow dataset: exp
DS.flow.exp <- (DS.hydro.metric) %>%
  select(timestamp, 
         in1.m_flow, 
         in2.m_flow, 
         out.flow.roll.ASABE) 
#View(DS.flow.exp)  

## Create a INflow dataset
DS.inflow <- (DS.hydro.metric) %>%
  select(timestamp, 
         in1.m_flow, 
         in2.m_flow, 
         in2.hobo.m_flow) 
#View(DS.inflow)  

## Melt Dataset
DS.flow.melt <- (DS.flow) %>%
  melt(id = "timestamp")
#View(DS.flow.melt)

## Melt outflow Dataset
DS.outflow.melt <- (DS.outflow) %>%
  melt(id = "timestamp")
#View(DS.outflow.melt)

## Melt flow Dataset exp
DS.flow.melt.exp <- (DS.flow.exp) %>%
  melt(id = "timestamp")
#View(DS.flow.melt.exp)

## Melt inflow Dataset 
DS.inflow <- (DS.inflow) %>%
  melt(id = "timestamp")
#View(DS.inflow)

## Plot Flow
ggplot(DS.flow.melt, aes(x = timestamp))+
  geom_line(aes(y = value, colour = variable))

## Plot Flow
ggplot(DS.outflow.melt, aes(x = timestamp))+
  geom_line(aes(y = value, colour = variable))

## Plot Flow
ggplot(DS.flow.melt.exp, aes(x = timestamp))+
  geom_line(aes(y = value, colour = variable))

## Plot inFlow
ggplot(DS.inflow, aes(x = timestamp))+
  geom_line(aes(y = value, colour = variable))

## Decide to use DS.flow for remainder of calculations
## Write .csv file for use in analysis
write.csv(DS.flow, "./Working/flow.dataset.csv")