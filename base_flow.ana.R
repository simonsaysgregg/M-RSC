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

## Plot well stage over monitoring period
ggplot(infil.ds, aes(x = timestamp, y = well.cm))+
  geom_point(aes(x = timestamp, y = well.cm))+
  labs(y =  "Well Stage (cm)", x = "Date")+
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 18))


## Average well depths over interval
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
  subset(as.numeric(total) >= 2)
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
# View(infil.est)


## Plot infil.est stage vs. change
ggplot(infil.est, aes(x = well.cm, y = delta.stage))+
  geom_point(aes(x = well.cm, y = delta.stage))+
  labs(y = "Change in Well Stage (cm/day)", x = "Well Stage (cm)")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))+
  geom_smooth(method = lm, se = FALSE, aes(x = well.cm, y = delta.stage))

## recreatoin of well stage for individaual ADPs
# Average well depths over interval
# Copy working df
infil.recreat <- infil.ds %>%
  subset(!is.na(well.cm))
# #View(infil.recreat)
# create counting index for interval generation
infil.recreat1 <- infil.recreat %>%
  mutate(timestamp = ymd_hms(timestamp),
         hour = floor_date(timestamp, 'hour')) %>%
  group_by(ADP.index, hour) %>%
  summarise(well.cm = mean(well.cm))
# View(infil.recreat1)

# Add count day
infil.recreat2 <- infil.recreat1 %>%
  group_by(ADP.index) %>%
  mutate(start = min(hour),
         end = max(hour),
         total = (end - start) / 86400, # conversion to days
         durat = (hour - start) / 86400) %>%
  subset(ADP.index != 25) %>%
  subset(ADP.index != 41) %>%
  subset(ADP.index != 91)
# View(infil.recreat2)

## Plot infil.est durat vs. stage ### Double check!!
ggplot(infil.recreat2, aes(x = durat, y = well.cm))+
  geom_point(aes(x = durat, y = well.cm))+
  labs(y = "Well Stage (cm)", x = "Days" )+
  geom_hline(yintercept = 99)+
  annotate("text", x = 10, y = 99.5, label = "Autumn-Winter", size = 5)+
  annotate("text", x = 10, y = 98.5, label = "Spring-Summer", size = 5)+
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 18))


## Seasonal
## subset seasons
# 26-68-- Autum-Winter
# 68-93-- Spring-Summer
autwin <- infil.est %>%
  subset(ADP.index <= 74)
sprsum <- infil.est %>%
  subset(ADP.index >= 75)
## Plot Autum-Winter
ggplot(autwin)+
  geom_point(aes(x = well.cm, y = delta.stage))+
  labs(y = "Change in Well Stage (cm/day)", x = "Well Stage (cm)")+
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 18))+
  geom_smooth(method = lm, se = FALSE, aes(x = well.cm, y = delta.stage))

## Plot Sprin- Summer
ggplot(sprsum)+
  geom_point(aes(x = well.cm, y = delta.stage))+
  labs(y = "Change in Well Stage (cm/day)", x = "Well Stage (cm)")+
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 18))+
  annotate("text", x = 95.5, y = 2.5, label = "italic(y) == 3026.3 - 63.7(x) + 0.33(x) ^ 2", parse = TRUE, size = 5)+
  annotate("text", x = 95.5, y = 2.25, label = "italic(R) ^ 2 == 0.35", parse = TRUE, size = 5)+
  annotate("text", x = 95.5, y = 2, label = "italic(RMSE) == 0.55", parse = TRUE, size = 5)+
  geom_smooth(aes(x = well.cm, y = delta.stage), method = "gam", formula = y ~ poly(x, 2) , se = FALSE )


## Linear model development
##lm
lmbfspr <- lm(delta.stage ~ well.cm + I(well.cm^2) , data = sprsum[,])
summary(lmbfspr)
par(mfrow=c(2,2))
plot(lmbfspr)

# Call:
#   lm(formula = delta.stage ~ well.cm + I(well.cm^2), data = sprsum[, 
#                                                                    ])
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.43235 -0.25757 -0.06632  0.20755  2.67594 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)  3026.2756   929.1180   3.257  0.00197 **
#   well.cm       -63.6757    19.4205  -3.279  0.00184 **
#   I(well.cm^2)    0.3349     0.1015   3.301  0.00173 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.5615 on 53 degrees of freedom
# Multiple R-squared:  0.3546,	Adjusted R-squared:  0.3302 
# F-statistic: 14.56 on 2 and 53 DF,  p-value: 9.131e-06

#Root mean square error:
RSS5 <- c(crossprod(lmbfspr$residuals))

MSE5 <- RSS5 / length(lmbfspr$residuals)

RMSE5 <- sqrt(MSE5)
# View(RMSE5)
# Returns: 0.5462371


# Autocorrelation
# acf(lmbfspr$residuals)
# Not autocorrelated

##lm
lmbfaut <- lm(delta.stage ~ well.cm + I(well.cm^2) , data = autwin[,])
summary(lmbfaut)
par(mfrow=c(2,2))
plot(lmbfaut)

# Call:
#   lm(formula = delta.stage ~ well.cm + I(well.cm^2), data = autwin[, 
#                                                                    ])
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.6702 -0.4567 -0.2416  0.1760  4.1421 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)   75.561384 225.790052   0.335    0.739
# well.cm       -1.523276   4.376860  -0.348    0.728
# I(well.cm^2)   0.007705   0.021207   0.363    0.717
# 
# Residual standard error: 0.8914 on 111 degrees of freedom
# Multiple R-squared:  0.01677,	Adjusted R-squared:  -0.0009483 
# F-statistic: 0.9465 on 2 and 111 DF,  p-value: 0.3912

# Autocorrelation
# acf(lmbfaut$residuals)
# runs.test(lmbfaut$residuals)
# Returns:
# Runs Test - Two sided
# 
# data:  lmbfaut$residuals
# Standardized Runs Statistic = -2.2578, p-value = 0.02396
# dwtest(lmbfaut)
# returns
# Durbin-Watson test
# 
# data:  lmbfaut
# DW = 1.4151, p-value = 0.0006461
# alternative hypothesis: true autocorrelation is greater than 0
