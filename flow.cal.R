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
  mutate(weir = rollapply(in1.m_flow, 15, mean, fill = NA),
         dryout = rollapply(dryout.m_flow, 15, mean, fill = NA))%>%
  subset(#timestamp >= as.POSIXct("2018-05-28 02:28:00") & timestamp <= as.POSIXct("2018-05-30 06:28:00") |
         #timestamp >= as.POSIXct("2018-05-30 14:58:00") & timestamp <= as.POSIXct("2018-05-31 11:04:00") |
         #timestamp >= as.POSIXct("2018-06-10 20:20:00") & timestamp <= as.POSIXct("2018-06-11 14:26:00") |
         timestamp >= as.POSIXct("2018-06-25 23:50:00") & timestamp <= as.POSIXct("2018-06-27 12:14:00") |
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

##Plot relationship
ggplot(DS.inflow, aes(x = weir, y = dryout))+
  geom_point()+
  geom_abline(aes(intercept = 0, slope = 1))

## Creation of dataset for analysis & correction 
# DS.inflow for correction

# Log transform for normailty correction
DS.inflow <- DS.inflow %>%
  mutate(trans.in1 = log(in1.m_flow +0.01),
         trans.dryout = log(dryout.m_flow +0.01))
DS.inflow$trans.dryout[which(is.nan(DS.inflow$trans.dryout))] = NA
DS.inflow$trans.dryout[which(DS.inflow$trans.dryout==Inf)] = NA
DS.inflow$trans.in1[which(is.nan(DS.inflow$trans.in1))] = NA
DS.inflow$trans.in1[which(DS.inflow$trans.in1==Inf)] = NA
#View(DS.inflow)

# # rolling average 
# DS.inflow <- DS.inflow %>%
#   mutate(roll.tran.in = rollapply(trans.in1),
#          roll.tram.dry = rollapply(trans.dryout))
# ## Autocorrelation Correction



## Primary inlet diagnosis and correction
# Begin with linear model analysis
## linear regression of inflow methods
linear <- lm((dryout.m_flow) ~ (in1.m_flow), data = DS.inflow)
#linear <- lm(log(dryout.m_flow+0.01) ~ log(in1.m_flow+0.01), data = DS.flow.both)
summary(linear)


# Call:
#   lm(formula = (trans.dryout) ~ (trans.in1), data = DS.inflow)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.25545 -0.18603 -0.11058  0.00889  1.66106 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.714674   0.031023   23.04   <2e-16 ***
#   trans.in1   1.027940   0.007308  140.66   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3734 on 4234 degrees of freedom
# (122 observations deleted due to missingness)
# Multiple R-squared:  0.8237,	Adjusted R-squared:  0.8237 
# F-statistic: 1.978e+04 on 1 and 4234 DF,  p-value: < 2.2e-16


# Check model mean residuals -- should be near zero
mean(linear$residuals)
# Resutns:  2.340595e-18
# Equal variance check
par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
plot(linear)
# Autocorrelation check
acf(linear$residuals)
# Rectify autocorrelation
DS.flow1 <- na.omit(DS.inflow)
resid_linear <- linear$residuals
DS.flow1[, "resid_linear"] <- resid_linear
DS.flow2 <- slide(DS.flow1, Var="resid_linear", NewVar = "lag1", slideBy = -1)
DS.flow3 <- na.omit(DS.flow2)
linear2 <- lm((dryout.m_flow) ~ in1.m_flow + lag1, data = DS.flow3)

## Review model
summary(linear2)

# Returns:

# Call:
#   lm(formula = (dryout) ~ weir + lag1, data = DS.flow3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.108462 -0.000134 -0.000073 -0.000029  0.135456 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 9.896e-03  6.186e-05   160.0   <2e-16 ***
#   weir        1.980e+00  1.900e-03  1041.8   <2e-16 ***
#   lag1        9.807e-01  3.044e-03   322.1   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.003596 on 4133 degrees of freedom
# Multiple R-squared:  0.9965,	Adjusted R-squared:  0.9965 
# F-statistic: 5.942e+05 on 2 and 4133 DF,  p-value: < 2.2e-16


mean(linear2$residuals)
# returns: ] -1.353468e-19
acf(linear2$residuals)
# Below checks all assumptions of linear regression
#gvlma(quad)
# Equal variance check
par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
plot(linear2)

## Remove outliers denoted from previous model
linear3 <- lm((dryout.m_flow) ~ in1.m_flow + lag1, data = DS.flow3[-c(230883, 230884, 240069, 240088, 251228, 251458), ])
summary(linear3)
# Returns:
# Call:
#   lm(formula = (dryout.m_flow) ~ in1.m_flow + lag1, data = DS.flow3[-c(230883, 
#                                                                        230884, 240069, 240088, 251228, 251458), ])
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.118182 -0.000322 -0.000072  0.000068  0.113143 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 6.307e-03  2.622e-05   240.6   <2e-16 ***
#   in1.m_flow  1.787e+00  3.209e-03   557.0   <2e-16 ***
#   lag1        8.440e-01  3.571e-03   236.3   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.003931 on 22901 degrees of freedom
# Multiple R-squared:  0.9439,	Adjusted R-squared:  0.9439 
# F-statistic: 1.927e+05 on 2 and 22901 DF,  p-value: < 2.2e-16

# Check model mean residuals -- should be near zero
mean(linear3$residuals)
# Resutns:  -2.598854e-19
# Equal variance check
par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
plot(linear3)

## Remove outliers denoted from previous model
linear4 <- lm((dryout.m_flow) ~ in1.m_flow + lag1, data = DS.flow3[-c(230883, 230884, 240069, 240088, 251228, 251458), ])
summary(linear4)
# Returns:
# Call:
#   lm(formula = (dryout.m_flow) ~ in1.m_flow + lag1, data = DS.flow3[-c(230883, 
#                                                                        230884, 240069, 240088, 251228, 251458), ])
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.118182 -0.000322 -0.000072  0.000068  0.113143 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 6.307e-03  2.622e-05   240.6   <2e-16 ***
#   in1.m_flow  1.787e+00  3.209e-03   557.0   <2e-16 ***
#   lag1        8.440e-01  3.571e-03   236.3   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.003931 on 22901 degrees of freedom
# Multiple R-squared:  0.9439,	Adjusted R-squared:  0.9439 
# F-statistic: 1.927e+05 on 2 and 22901 DF,  p-value: < 2.2e-16

# Check model mean residuals -- should be near zero
mean(linear4$residuals)
# Resutns:   -2.598854e-19
# Equal variance check
par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
plot(linear4)

## log transform previous model
linear5 <- lm(log(dryout.m_flow) ~ in1.m_flow + lag1, data = DS.flow3[-c(230883, 230884, 240069, 240088, 251228, 251458), ])
summary(linear5)
# Returns:
# Call:
#   lm(formula = log(dryout.m_flow) ~ in1.m_flow + lag1, data = DS.flow3[-c(230883, 
#                                                                           230884, 240069, 240088, 251228, 251458), ])
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.2351 -0.0418  0.0524  0.1165  4.7036 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -5.143636   0.002177 -2363.1   <2e-16 ***
#   in1.m_flow  40.305574   0.266414   151.3   <2e-16 ***
#   lag1        36.786045   0.296475   124.1   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3263 on 22901 degrees of freedom
# Multiple R-squared:  0.6415,	Adjusted R-squared:  0.6414 
# F-statistic: 2.049e+04 on 2 and 22901 DF,  p-value: < 2.2e-16

# Check model mean residuals -- should be near zero
mean(linear5$residuals)
# Resutns:  -2.811963e-16
# Equal variance check
par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
plot(linear5)

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

