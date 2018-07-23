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
require("timeSeries")
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
          # timestamp >= as.POSIXct("2018-06-10 20:20:00") & timestamp <= as.POSIXct("2018-06-11 14:26:00") |
           timestamp >= as.POSIXct("2018-06-26 07:50:00") & timestamp <= as.POSIXct("2018-06-26 20:14:00") |
           timestamp >= as.POSIXct("2018-07-05 13:26:00") & timestamp <= as.POSIXct("2018-07-06 07:56:00")
           ) 
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
# average values over 20-min non-rolling
weir <- aggregate(list(flow.short$in1.m_flow), list(cut(as.POSIXlt(flow.short$timestamp), "20 mins")), FUN = mean)
dryout <- aggregate(list(flow.short$dryout.m_flow), list(cut(as.POSIXlt(flow.short$timestamp), "20 mins")), FUN = mean)
# add to new dataframe
flow.short.corr <- data.frame(weir)
colnames(flow.short.corr) <- c("timestamp", "weir")
# Repeat for dryout to new dataframe
scrap <- data.frame(dryout)
colnames(scrap) <- c("timestamp", "dryout")
## Join dryout data
flow.short.corr <- left_join(flow.short.corr, scrap, by = "timestamp") 
#View(flow.short.corr)
## Remove na
flow.short.corr <- na.omit(flow.short.corr)
# mutate for appropriate normality corrections
flow.short.corr <- flow.short.corr %>%
mutate(log.weir = log(weir + 0.01),
       log.dryout = log(dryout + 0.01))
#View(flow.short.corr)

##lm
lm1 <- lm(dryout ~ weir, data = flow.short.corr[,])
summary(lm1)
par(mfrow=c(2,2))
plot(lm1)

## autocorrelation factor
acf(lm1$residuals)

# Rectify autocorrelation
DS.flow1 <- na.omit(flow.short.corr[,])
resid_linear <- lm1$residuals
DS.flow1[, "resid_linear"] <- resid_linear
DS.flow2 <- slide(DS.flow1, Var="resid_linear", NewVar = "lag1", slideBy = -1)
DS.flow3 <- na.omit(DS.flow2)

lm2 <- lm(dryout ~ weir + lag1, data = DS.flow3[,])
summary(lm2)
par(mfrow=c(2,2))
plot(lm2)

## autocorrelation factor
acf(lm2$residuals)

#Root mean square error:
RSS <- c(crossprod(lm2$residuals))
  
MSE <- RSS / length(lm2$residuals)
  
RMSE <- sqrt(MSE)
# View(RMSE)

## Repeat with all events 
## Create a INflow dataset
DS.inflow1 <- (DS.flow) %>%
  select(timestamp,
         in1.m_flow,
         dryout.m_flow)%>%
  subset(timestamp >= as.POSIXct("2018-05-28 02:28:00") & timestamp <= as.POSIXct("2018-05-30 06:28:00") |
         timestamp >= as.POSIXct("2018-05-30 14:58:00") & timestamp <= as.POSIXct("2018-05-31 11:04:00") |
         timestamp >= as.POSIXct("2018-06-10 20:20:00") & timestamp <= as.POSIXct("2018-06-11 14:26:00") |
         timestamp >= as.POSIXct("2018-06-26 07:50:00") & timestamp <= as.POSIXct("2018-06-26 20:14:00") |
         timestamp >= as.POSIXct("2018-07-05 13:26:00") & timestamp <= as.POSIXct("2018-07-06 07:56:00")) 
#View(DS.inflow1)

## Melt inflow Dataset 
DS.inflow1.m <- (DS.inflow1) %>%
  select(timestamp,
         in1.m_flow,
         dryout.m_flow)%>%
  melt(id = "timestamp")
#View(DS.inflow1.m)
## Plot events for calibration
ggplot(DS.inflow1.m, aes(x = timestamp))+
  geom_point(aes(y = value, color = variable))+
  scale_shape_manual(values = c("2", "16"), labels = c("Weir", "Outlet"))+
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "6 day")+
  labs(y = "Flow Rate (cms)", x = "Date")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

## scatter plot DS.inflow
ggplot(DS.inflow1, aes(x = in1.m_flow, y = dryout.m_flow))+
  geom_point()+
  geom_smooth(method = lm)+
  geom_abline(aes(intercept = 0, slope = 1))

## box plots DS.inflow
ggplot(DS.inflow1.m)+
  geom_boxplot(aes(x = variable, y = value))

## Density DS.inflow
ggplot(DS.inflow1, aes(x = in1.m_flow))+
  geom_density()

ggplot(DS.inflow1, aes(x = dryout.m_flow))+
  geom_density()

## subset flow values to remove low event flows
flow.short1 <- DS.inflow1 %>%
  subset(in1.m_flow >= 0.05)
#View(flow.short1)
# melt
flow.short1.m <- flow.short1 %>%
  select(timestamp,
         in1.m_flow,
         dryout.m_flow)%>%
  melt(id = "timestamp")

## Plot events for calibration
ggplot(DS.inflow1.m, aes(x = timestamp))+
  geom_point(aes(y = value, color = variable))+
  scale_shape_manual(values = c("2", "16"), labels = c("Weir", "Outlet"))+
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "6 day")+
  labs(y = "Flow Rate (cms)", x = "Date")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

## scatter plot
ggplot(flow.short1, aes(x = in1.m_flow, y = dryout.m_flow))+
  geom_point()+
  geom_smooth(method = lm)+
  geom_abline(aes(intercept = 0, slope = 1))

## box plots
ggplot(flow.short1.m)+
  geom_boxplot(aes(x = variable, y = value))

## Density
ggplot(flow.short1, aes(x = in1.m_flow))+
  geom_density()

ggplot(DS.inflow1, aes(x = dryout.m_flow))+
  geom_density()

## lm
## diagnostic plots
lm1.1 <- lm(dryout.m_flow ~ in1.m_flow, data = flow.short1)
summary(lm1.1)
par(mfrow=c(2,2))
plot(lm1.1)

## normality and autocorrelation corrections
# average values over 20-min non-rolling
weir <- aggregate(list(flow.short1$in1.m_flow), list(cut(as.POSIXlt(flow.short1$timestamp), "20 mins")), FUN = mean)
dryout <- aggregate(list(flow.short1$dryout.m_flow), list(cut(as.POSIXlt(flow.short1$timestamp), "20 mins")), FUN = mean)
# add to new dataframe
flow.short1.corr <- data.frame(weir)
colnames(flow.short1.corr) <- c("timestamp", "weir")
# Repeat for dryout to new dataframe
scrap1 <- data.frame(dryout)
colnames(scrap1) <- c("timestamp", "dryout")
## Join dryout data
flow.short1.corr <- left_join(flow.short1.corr, scrap1, by = "timestamp") 
#View(flow.short1.corr)
## Remove na
flow.short1.corr <- na.omit(flow.short1.corr)
# mutate for appropriate normality corrections
flow.short1.corr <- flow.short1.corr %>%
  mutate(log.weir = log(weir + 0.01),
         log.dryout = log(dryout + 0.01))
#View(flow.short1.corr)

##lm
lm1.2 <- lm(dryout ~ weir, data = flow.short1.corr[,])
summary(lm1.2)
par(mfrow=c(2,2))
plot(lm1.2)

## autocorrelation factor
acf(lm1.2$residuals)

# Rectify autocorrelation
DS.flow1.1 <- na.omit(flow.short1.corr[,])
resid_linear <- lm1.2$residuals
DS.flow1.1[, "resid_linear"] <- resid_linear
DS.flow2.1 <- slide(DS.flow1.1, Var="resid_linear", NewVar = "lag1", slideBy = -1)
DS.flow3.1 <- na.omit(DS.flow2.1)

lm2.1 <- lm(dryout ~ weir + lag1, data = DS.flow3.1[-c(32,33,9,10),])
summary(lm2.1)
par(mfrow=c(2,2))
plot(lm2.1)

# Returns:
#   Call:
#   lm(formula = dryout ~ weir + lag1, data = DS.flow3.1[-c(32, 33, 
#                                                           9, 10), ])
# 
# Residuals:
#   Min         1Q     Median         3Q        Max 
# -0.0304952 -0.0017258  0.0005178  0.0026977  0.0203560 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.289160   0.007508  -38.51   <2e-16 ***
#   weir         5.588461   0.094294   59.27   <2e-16 ***
#   lag1         0.908847   0.056378   16.12   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.006385 on 66 degrees of freedom
# Multiple R-squared:  0.9816,	Adjusted R-squared:  0.981 
# F-statistic:  1757 on 2 and 66 DF,  p-value: < 2.2e-16

## autocorrelation factor
acf(lm2.1$residuals)

#Root mean square error:
RSS1 <- c(crossprod(lm2.1$residuals))

MSE1 <- RSS1 / length(lm2.1$residuals)

RMSE1 <- sqrt(MSE1)
# View(RMSE)