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

##Begin user defined functions###########################################
## For flow calculation
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
  ((1.2192^2) * acos((1.2192 - stage)/1.2192)) - ((1.2192 - stage) * sqrt(2*1.2192*stage - (stage^2))) 
}

# Outlet flow function
flow.out <- function(V,A){
  (V * A)
}

# Dry pond outlet discharge estimation
# General function for compound outlet structure
# inv.diff == difference btw multi flow control structures 
# current case: orfice + wier
flow.dryout <- function(dryout.m, in1.m, Cd = 0.41, inv.diff = 0.635, area.orf.m = 0.0081, grav = 9.8, C = 1.84, L = 4.237) { 
  ## modify following equaiton for drypond outlet
  # Hobo 2.0833' (0.635m) below west broad cret weir
  # Hobo 1.45' (0.442m) below orfice center
  # weir crest 0.63' (0.1920m) above orfice center
  # weir crest length 13.9' (4.237m)
  # two orifi
  # in1.m 0.13' (0.0396m) above orfice center
  # rolling average to scrub noise of movement
  dryout.m <- rollapply(dryout.m, 5, mean, fill = NA)
  
  ifelse(dryout.m < inv.diff, (((Cd * area.orf.m) * sqrt(2 * grav * ((dryout.m - 0.442) - (in1.m + 0.0396)))) * 2), ((((Cd * area.orf.m) * sqrt(2 * grav * ((dryout.m - 0.442) - (in1.m + 0.0396)))) * 2) + (C * (L - (0.2 * (dryout.m - inv.diff))) * ((dryout.m - inv.diff)^1.5))))     
}
  
  ## End user defined functions##############################################

## Read file from ./Working folder
DS <- read.csv("./Working/dataset.csv")
# View(DS)
# Format date time
DS$timestamp <- ymd_hms(DS$timestamp)
#View(DS)

## Rainfall normalization 
# select columns
rain.norm <- DS %>%
  select("timestamp",
         "rain.in")
#View(rain.norm)
# Filter rows matching date periods for manual rain gauge measurements
#2017/07/19->2017/09/15
rain1 <- rain.norm %>%
  subset(timestamp >= "2017-07-19 08:00" & timestamp <= "2017-09-15 07:58")
#View(rain1)
# sum rain fall to determine normalization factor
#sum(rain1$rain.in)
# returns: 6.51
# normalize by 3.71/6.51
rain1.norm <- rain1 %>%
  mutate(rain.in.norm = rain.in * (3.71/6.51))
#View(rain1.norm)

# Filter rows matching date periods for manual rain gauge measurements
#2017/09/15->2017/11/10
rain2 <- rain.norm %>%
  subset(timestamp >= "2017-09-15 08:00" & timestamp <= "2017-11-10 07:58")
#View(rain2)
# sum rain fall to determine normalization factor
# sum(rain2$rain.in, na.rm = TRUE)
# returns: 2.52
# normalize by 4.59/2.52
rain2.norm <- rain2 %>%
  mutate(rain.in.norm = rain.in * (4.59/2.52))
#View(rain2.norm)

# Filter rows matching date periods for manual rain gauge measurements
#2017/11/10->2017/12/06
rain3 <- rain.norm %>%
  subset(timestamp >= "2017-11-10 08:00" & timestamp <= "2017-12-06 07:58")
#View(rain3)
# sum rain fall to determine normalization factor
# sum(rain3$rain.in, na.rm = TRUE)
# returns: 0.97
# normalize by 1.07/0.97
rain3.norm <- rain3 %>%
  mutate(rain.in.norm = rain.in * (1.07/0.97))
#View(rain3.norm)

# Filter rows matching date periods for manual rain gauge measurements
#2018/01/31->2018/02/27
rain4 <- rain.norm %>%
  subset(timestamp >= "2018-01-31 08:00" & timestamp <= "2018-02-27 07:58")
#View(rain4)
# sum rain fall to determine normalization factor
# sum(rain4$rain.in, na.rm = TRUE)
# returns: 1.69
# normalize by 1.59/1.69
rain4.norm <- rain4 %>%
  mutate(rain.in.norm = rain.in * (1.59/1.69))
#View(rain4.norm)

# Filter rows matching date periods for manual rain gauge measurements
#2018/02/27->2018/04/23
rain5 <- rain.norm %>%
  subset(timestamp >= "2018-02-27 08:00" & timestamp <= "2018-04-23 07:58")
#View(rain5)
# sum rain fall to determine normalization factor
# sum(rain5$rain.in, na.rm = TRUE)
# returns: 8.89
# normalize by 9.4/8.89
rain5.norm <- rain5 %>%
  mutate(rain.in.norm = rain.in * (9.4/8.89))
#View(rain5.norm)

# Filter rows matching date periods for manual rain gauge measurements
#2018/04/23->2018/06/25
rain6 <- rain.norm %>%
  subset(timestamp >= "2018-04-23 08:00" & timestamp <= "2018-06-25 07:58")
#View(rain6)
# sum rain fall to determine normalization factor
# sum(rain6$rain.in, na.rm = TRUE)
# returns: 5.44
# normalize by 7.32/5.44
rain6.norm <- rain6 %>%
  mutate(rain.in.norm = rain.in * (7.32/5.44))
#View(rain6.norm)

## Insert additional rainfall normalized

## Join normalized rainfall to original dataset
# insert additional normalized rainfall datasets below
rain.correct <- rbind(rain1.norm, rain2.norm, rain3.norm, rain4.norm, rain5.norm, rain6.norm) %>%
  select("timestamp",
         "rain.in.norm")
DS <- left_join(DS, rain.correct, by = "timestamp")
#View(DS)

## Select columns for hyrology analysis
DS.hydro <- DS %>%
  select("timestamp",
         "rain.in.norm",
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
            rainfall = rain.in.norm * 25.4,
            in1.m = in1.ft * 0.3048,
            dryout.m = dryout.ft * 0.3048,
            in2.m = in2.ft * 0.3048,
            in2.hobo.m = in2.hobo.ft * 0.3048,
            out.m = out.ft * 0.3048,
            out.velo = out.velo * 0.3048,
            out.velo.roll = rollapply(out.velo, 10, mean, fill = NA),
            well.m = well.ft * 0.3048)
# View(DS.hydro.metric)

## Mutate to add flow calculation using function
DS.hydro.metric <- DS.hydro.metric %>%
  mutate(in1.m_flow = flow.in1(in1.m),
         dryout.m_flow = flow.dryout(dryout.m, in1.m),
         in2.m_flow = flow.in2(in2.m),
         in2.hobo.m_flow = flow.in2(in2.hobo.m),
         area.ASABE = area.out.ASABE(out.m),
         out.flow.roll.ASABE = flow.out(out.velo.roll, area.ASABE))
#View(DS.hydro.metric)

## Create flow dataset
DS.flow <- (DS.hydro.metric) %>%
  select(timestamp, 
         in1.m_flow,
         dryout.m_flow,
         in2.m_flow, 
         in2.hobo.m_flow, 
         out.flow.roll.ASABE) 
#View(DS.flow)  

## Create outlet flow dataset
# experimental: to determine method of velocity processing: see beyond
############ 
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
  subset(timestamp > "2018/05/25" & timestamp < "2018/06/29") %>%
  select(timestamp, 
         #in1.m_flow,
         dryout.m_flow) 
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
############

## Decide to use DS.flow for remainder of calculations
## Write .csv file for use in analysis
write.csv(DS.flow, "./Working/flow.dataset.csv")