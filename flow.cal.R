## For flow calibration and diagnostic analysis
## Creation of calibration model for inflow corrections
## 2nd in processing order

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
require("lawstat")
require("lmtest")
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

## Plot of whole dryout period#########
## Create a INflow dataset
DS.inflow <- (DS.flow) %>%
  select(timestamp,
         in1.m_flow,
         dryout.m_flow)%>%
  subset(timestamp >= as.POSIXct("2018-05-25 00:00:00") & timestamp <= as.POSIXct("2018-07-10 00:00:00")) 
#View(DS.inflow)

## Duration Plot: total 
## Melt inflow Dataset 
DS.inflow.m <- (DS.inflow) %>%
  select(timestamp,
         in1.m_flow,
         dryout.m_flow)%>%
  melt(id = "timestamp")
#View(DS.inflow.m)
## Plot events for calibration
ggplot(DS.inflow.m, aes(x = timestamp))+
  geom_line(aes(y = value, color = variable, linetype = variable))+
  scale_linetype_manual(values = c("longdash", "dotdash"), labels = c("Weir", "Dry Pond Outlet"))+
  scale_color_manual(values = c("red", "black"), labels = c("Weir", "Dry Pond Outlet"))+
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "6 day")+
  labs(y = "Flow Rate (cms)", x = "Date")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

###############

## Model development with all events 
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

## Diagnostic Plots
## Melt inflow Dataset 
DS.inflow1.m <- (DS.inflow1) %>%
  select(timestamp,
         in1.m_flow,
         dryout.m_flow)%>%
  melt(id = "timestamp")
#View(DS.inflow1.m)
## Plot events for calibration
ggplot(DS.inflow1.m, aes(x = timestamp))+
  geom_point(aes(y = value, color = variable, shape = variable))+
  scale_shape_manual(values = c(16, 1), labels = c("Weir", "Dry Pond Outlet"))+
  scale_color_manual(values = c("red", "black"), labels = c("Weir", "Dry Pond Outlet"))+
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "6 day")+
  labs(y = "Flow Rate (cms)", x = "Date")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 18))
## scatter plot DS.inflow
ggplot(DS.inflow1, aes(x = in1.m_flow, y = dryout.m_flow))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(y = "Dry Pond Outlet (cms)", x = "Weir (cms)")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

## box plots DS.inflow
ggplot(DS.inflow1.m)+
  geom_boxplot(aes(x = variable, y = value))
## Density DS.inflow
# weir
ggplot(DS.inflow1, aes(x = in1.m_flow))+
  geom_density()
# Outlet structure
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

## Diagnostic Plots
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
  geom_smooth(method = lm, se = FALSE)+
  labs(y = "Dry Pond Outlet (cms)", x = "Weir (cms)")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

## box plots
ggplot(flow.short1.m)+
  geom_boxplot(aes(x = variable, y = value))
## Density
# Weir
ggplot(flow.short1, aes(x = in1.m_flow))+
  geom_density()
# Outlet Structure
ggplot(DS.inflow1, aes(x = dryout.m_flow))+
  geom_density()

## lm
## diagnostic plots
lm10.1 <- lm(dryout.m_flow ~ in1.m_flow, data = flow.short1)
summary(lm10.1)
par(mfrow=c(2,2))
plot(lm10.1)

## normality and autocorrelation corrections
# average values over 20-min non-rolling
weir <- aggregate(list(flow.short1$in1.m_flow), list(cut(as.POSIXlt(flow.short1$timestamp), "60 mins")), FUN = mean)
dryout <- aggregate(list(flow.short1$dryout.m_flow), list(cut(as.POSIXlt(flow.short1$timestamp), "60 mins")), FUN = mean)
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
# shapiro.test(flow.short1.corr$weir)
# shapiro.test(flow.short1.corr$log.weir)
# shapiro.test(flow.short1.corr$dryout)
# shapiro.test(flow.short1.corr$log.dryout)

##lm
lm10.2 <- lm(dryout ~ weir, data = flow.short1.corr)
summary(lm10.2)
par(mfrow=c(2,2))
plot(lm10.2)

## autocorrelation factor
acf(lm10.2$residuals)
runs.test(lm10.2$residuals)
dwtest(lm10.2)

# # Rectify autocorrelation
# DS.flow10.2 <- na.omit(flow.short1.corr[,])
# resid_linear <- lm10.2$residuals
# DS.flow10.2[, "resid_linear"] <- resid_linear
# DS.flow20.2 <- slide(DS.flow10.2, Var="resid_linear", NewVar = "lag1", slideBy = -1)
# DS.flow30.2 <- na.omit(DS.flow20.2)
# 
# lm20.1 <- lm(dryout ~ weir + lag1, data = DS.flow30.2[-c(32,33,9,10),])
# summary(lm20.1)
# par(mfrow=c(2,2))
# plot(lm20.1)

## scatter plot of final model
ggplot(flow.short1.corr, aes(x = weir, y = dryout))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(y = "Dry Pond Outlet (cms)", x = "Weir (cms)")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 18))

# Call:
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
acf(lm20.1$residuals)

#Root mean square error:
RSS1 <- c(crossprod(lm20.1$residuals))

MSE1 <- RSS1 / length(lm20.1$residuals)

RMSE1 <- sqrt(MSE1)
# View(RMSE1)
# Returns: 0.0156

## Repeat for base flow periods
## Create a INflow dataset
DS.baseflow <- (DS.flow) %>%
  select(timestamp,
         in1.m_flow,
         dryout.m_flow)%>%
  subset( timestamp >= as.POSIXct("2018-05-30 06:28:00") &
           timestamp <= as.POSIXct("2018-05-30 14:58:00") | timestamp >= as.POSIXct("2018-05-31 11:04:00") &
           timestamp <= as.POSIXct("2018-06-10 20:20:00") | timestamp >= as.POSIXct("2018-06-11 14:26:00") &
           timestamp <= as.POSIXct("2018-06-26 07:50:00") | timestamp >= as.POSIXct("2018-06-26 20:14:00") &
           timestamp <= as.POSIXct("2018-07-05 13:26:00") ) 
#View(DS.baseflow)

## Extract flow periods from dataset including ADP.index
DS1.baseflow <- RSC.hydro.m %>%
  subset(timestamp >= as.POSIXct("2018-05-30 06:28:00")) %>%
  select(timestamp,
         in1.m_flow,
         dryout.m_flow,
         ADP.index)
# View(DS1.baseflow)

DS2.baseflow <- DS1.baseflow %>%
  subset(ADP.index != 0 &
           ADP.index != 83 &
           ADP.index != 87 &
           ADP.index != 92)

## Summary of ADP
ADP.sum1 <- (DS1.baseflow) %>%
  group_by(ADP.index) %>%
  summarise(duation = difftime(max(timestamp), min(timestamp), units = "days")) 
# View(ADP.sum1)
# min(DS2.baseflow$in1.m_flow, na.rm = TRUE)
# returns: 0
# max(DS2.baseflow$in1.m_flow, na.rm = TRUE)
# returns: 0.001197896


## Diagnostic Plots
## Melt inflow Dataset 
DS.baseflow.m <- (DS.baseflow) %>%
  select(timestamp,
         in1.m_flow,
         dryout.m_flow)%>%
  melt(id = "timestamp")
#View(DS.baseflow.m)
## Plot events for calibration
ggplot(DS.baseflow.m, aes(x = timestamp))+
  geom_point(aes(y = value, color = variable, shape = variable))+
  scale_shape_manual(values = c(16, 1), labels = c("Weir", "Dry Pond Outlet"))+
  scale_color_manual(values = c("red", "black"), labels = c("Weir", "Dry Pond Outlet"))+
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "6 day")+
  labs(y = "Flow Rate (cms)", x = "Date")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 18))

## scatter plot DS.baseflow
ggplot(DS.baseflow, aes(x = in1.m_flow, y = dryout.m_flow))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(y = "Dry Pond Outlet (cms)", x = "Weir (cms)")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))
## box plots DS.inflow
ggplot(DS.baseflow.m)+
  geom_boxplot(aes(x = variable, y = value))
## Density DS.inflow
# weir
ggplot(DS.baseflow, aes(x = in1.m_flow))+
  geom_density()
# Outlet structure
ggplot(DS.baseflow, aes(x = dryout.m_flow))+
  geom_density()

## lm Baseflows
## diagnostic plots
lm100 <- lm(dryout.m_flow ~ in1.m_flow, data = DS.baseflow)
summary(lm100)
par(mfrow=c(2,2))
plot(lm100)

## normality and autocorrelation corrections
# average values over 20-min non-rolling
weir <- aggregate(list(DS.baseflow$in1.m_flow), list(cut(as.POSIXlt(DS.baseflow$timestamp), "20 hours")), FUN = mean)
dryout <- aggregate(list(DS.baseflow$dryout.m_flow), list(cut(as.POSIXlt(DS.baseflow$timestamp), "20 hours")), FUN = mean)
# add to new dataframe
DS.baseflow.corr <- data.frame(weir)
colnames(DS.baseflow.corr) <- c("timestamp", "weir")
# Repeat for dryout to new dataframe
scrap1 <- data.frame(dryout)
colnames(scrap1) <- c("timestamp", "dryout")
## Join dryout data
DS.baseflow.corr <- left_join(DS.baseflow.corr, scrap1, by = "timestamp") 
#View(DS.baseflow.corr)
## Remove na
DS.baseflow.corr <- na.omit(DS.baseflow.corr)
# mutate for appropriate normality corrections
DS.baseflow.corr <- DS.baseflow.corr %>%
  mutate(log.weir = log(weir + 0.01),
         log.dryout = log(dryout + 0.01))
#View(DS.baseflow.corr)

##lm
lm100.1 <- lm(dryout ~ weir, data = DS.baseflow.corr[,])
summary(lm100.1)
par(mfrow=c(2,2))
plot(lm100.1)

## autocorrelation factor
acf(lm100.1$residuals)

## scatter plot of final model
ggplot(DS.baseflow.corr, aes(x = weir, y = dryout))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(y = "Dry Pond Outlet (cms)", x = "Weir (cms)")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 18))

# # Rectify autocorrelation
# DS.flow100.2 <- na.omit(DS.baseflow.corr[,])
# resid_linear <- lm100.1$residuals
# DS.flow100.2[, "resid_linear"] <- resid_linear
# DS.flow20.1 <- slide(DS.flow100.2, Var="resid_linear", NewVar = "lag1", slideBy = -1)
# DS.flow20.1 <- slide(DS.flow20.1, Var="resid_linear", NewVar = "lag2", slideBy = -2)
# DS.flow30.1 <- na.omit(DS.flow20.1)
# 
# lm100.2 <- lm(dryout ~ weir + lag1, data = DS.flow30.1[-c(18,2242,2142),])
# summary(lm100.2)
# par(mfrow=c(2,2))
# plot(lm100.2)
# 
# 
# ## scatter plot of final model
# ggplot(DS.flow30.1[-c(18,2242,2142),], aes(x = weir + lag1 , y = dryout))+
#   geom_point()+
#   geom_smooth(method = lm, se = FALSE)+
#   labs(y = "Dry Pond Outlet (cms)", x = "Weir (cms)")+
#   theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))


# Call:
#   lm(formula = dryout ~ weir + lag1, data = DS.flow30.1[-c(18, 
#                                                            2242, 2142), ])
# 
# Residuals:
#   Min         1Q     Median         3Q        Max 
# -0.0022525 -0.0000739  0.0000111  0.0000783  0.0033121 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  6.124e-03  6.034e-06  1015.0   <2e-16 ***
#   weir        -1.129e+00  3.852e-02   -29.3   <2e-16 ***
#   lag1         9.352e-01  6.824e-03   137.0   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.0002614 on 2872 degrees of freedom
# Multiple R-squared:  0.876,	Adjusted R-squared:  0.8759 
# F-statistic: 1.014e+04 on 2 and 2872 DF,  p-value: < 2.2e-16


# ## autocorrelation factor
# acf(lm100.2$residuals)
# runs.test(lm100.2$residuals)
# dwtest(lm100.2)
# 
# #Root mean square error:
# RSS2 <- c(crossprod(lm100.2$residuals))
# 
# MSE2 <- RSS2 / length(lm100.2$residuals)
# 
# RMSE2 <- sqrt(MSE2)
# # View(RMSE2)
# # Returns: 0.0002612988
# 
# ## remove negative
# DS.noneg <- DS.flow30.1 %>%
#   subset(lag1 >= 0)
# 
# ## scatter plot without negative
# ggplot(DS.noneg[,], aes(x = weir + lag1 , y = dryout))+
#   geom_point()+
#   geom_smooth(method = lm, se = FALSE)+
#   labs(y = "Dry Pond Outlet (cms)", x = "Weir (cms)")+
#   theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))
# 
# lm100.3 <- lm(dryout ~ weir + lag1, data = DS.noneg[,])
# summary(lm100.3)
# par(mfrow=c(2,2))
# plot(lm100.3)

## Analysis with ADP > 1 day
## Diagnostic Plots
## Melt inflow Dataset 
DS2.baseflow.m <- (DS2.baseflow) %>%
  select(timestamp,
         in1.m_flow,
         dryout.m_flow)%>%
  melt(id = "timestamp")
#View(DS.baseflow.m)
## Plot events for calibration
ggplot(DS2.baseflow.m, aes(x = timestamp))+
  geom_point(aes(y = value, color = variable, shape = variable))+
  scale_shape_manual(values = c(16, 1), labels = c("Weir", "Dry Pond Outlet"))+
  scale_color_manual(values = c("red", "black"), labels = c("Weir", "Dry Pond Outlet"))+
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "6 day")+
  labs(y = "Flow Rate (cms)", x = "Date")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))
## scatter plot DS.baseflow
ggplot(DS2.baseflow, aes(x = in1.m_flow, y = dryout.m_flow))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(y = "Dry Pond Outlet (cms)", x = "Weir (cms)")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

## lm Baseflows
## diagnostic plots
lm100 <- lm(dryout.m_flow ~ in1.m_flow, data = DS2.baseflow)
summary(lm100)
par(mfrow=c(2,2))
plot(lm100)

## normality and autocorrelation corrections
# average values over 20-min non-rolling
weir <- aggregate(list(DS.baseflow$in1.m_flow), list(cut(as.POSIXlt(DS.baseflow$timestamp), "1 day")), FUN = mean)
dryout <- aggregate(list(DS.baseflow$dryout.m_flow), list(cut(as.POSIXlt(DS.baseflow$timestamp), "1 day")), FUN = mean)
# add to new dataframe
DS2.baseflow.corr <- data.frame(weir)
colnames(DS2.baseflow.corr) <- c("timestamp", "weir")
# Repeat for dryout to new dataframe
scrap24 <- data.frame(dryout)
colnames(scrap24) <- c("timestamp", "dryout")
## Join dryout data
DS2.baseflow.corr <- left_join(DS2.baseflow.corr, scrap24, by = "timestamp") 
#View(DS.baseflow.corr)
## Remove na
DS2.baseflow.corr <- na.omit(DS2.baseflow.corr)
# mutate for appropriate normality corrections
DS2.baseflow.corr <- DS2.baseflow.corr %>%
  mutate(log.weir = log(weir + 0.01),
         log.dryout = log(dryout + 0.01))
#View(DS.baseflow.corr)

##lm
lm100.1 <- lm(dryout ~ weir, data = DS2.baseflow.corr[-c(1),])
summary(lm100.1)
par(mfrow=c(2,2))
plot(lm100.1)

## autocorrelation factor
acf(lm100.1$residuals)
runs.test(lm100.1$residuals)
dwtest(lm100.1)

## scatter plot of final model
ggplot(DS2.baseflow.corr[-c(1),], aes(x = weir, y = dryout))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(y = "Dry Pond Outlet (cms)", x = "Weir (cms)")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

shapiro.test(DS2.baseflow.corr$weir)