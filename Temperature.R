## Analysis of Morrisville RSC Temperature data from Providance Farm, Mason Farm Road
## monitoring period July 2017-June 2018
## data located ./Working
## Temperature Analysis

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

## user defined functions
# Temperature conversion F -> C
temp.FC <- function(F) { # give F returns C 
  temp.C <- (F - 32) / 1.8
  return(temp.C)
}

## Read file from ./Working folder
## units feet
DS <- read.csv("./Working/dataset.csv")
# View(DS)

# Format date time
DS$timestamp <- ymd_hms(DS$timestamp)
DS.flow$timestamp <- ymd_hms(DS.flow$timestamp)

## Selected needed elements
MRSC.temp <- DS %>%
  select(timestamp,
         rain.in,
         ends_with(".F")) 

## Convert unites
MRSC.temp.m <- MRSC.temp %>%
  transmute(timestamp = timestamp,
            rainfall.mm = rain.in * 25.4,
            atm.C = temp.FC(atm.F),
            in1.C = temp.FC(in1.F),
            in2.C = temp.FC(in2.F),
            out.C = temp.FC(out.F),
            well.C = temp.FC(well.F),
            dryout.C = temp.FC(dryout.F))
# View(MRSC.temp.m)

## Remove extra data; only rain and time stamp for delineation
MRSC.rain <- (MRSC.temp.m) %>%
  select(timestamp,
         rainfall.mm)
# View(MRSC.rain)

## Rainfall event delineation
# Exstract from Drizzle0.9.5
depth <- MRSC.rain$rainfall.mm
depth[depth == 0] <- NA
nalocf <- function(x) na.locf(x, maxgap = 359, na.rm = FALSE)
rain.index <- cumsum(diff(!is.na(c(NA, nalocf(depth)))) > 0) + nalocf(0*depth)

## Addend rain index to file to be delineated
MRSC.temp.m[,"rain.index"] <- rain.index

## Runoff event delineation
# Extend rain.idex for drawdown period of 12 hours in present case
# to change drawdown change DD; depends of data interval (DD.in.hours*60)/data.interval.in.minutes
runoff.del <- function(x, DD=362) {
  l <- cumsum(! is.na(x))
  c(NA, x[! is.na(x)])[replace(l, ave(l, l, FUN=seq_along) > DD, 0) + 1]
}
# vetor to process
k <- MRSC.temp.m$rain.index
# function operation
MRSC.temp.m$storm.index <- runoff.del(k)

## Replace NAs with zero
# rain index
MRSC.temp.m$rain.index <- (MRSC.temp.m$rain.index) %>%
  replace_na(0)
# View(MRSC.temp.m)
# storm index
MRSC.temp.m$storm.index <- (MRSC.temp.m$storm.index) %>%
  replace_na(0)
# View(MRSC.temp.m)

## Antecedant dry period analysis
## Similar to rainfall event delineation
# Exstract from Drizzle0.9.5 + modified
event <- MRSC.temp.m$storm.index
event[event != 0] <- NA
ADP.index <- cumsum(diff(!is.na(c(NA, (event)))) > 0) + (0*event)
# Add ADP index as new variable
MRSC.temp.m[, "ADP.index"] <- ADP.index
#Replace index NAs with zero
MRSC.temp.m$ADP.index[is.na(MRSC.temp.m$ADP.index)] <- 0 
# Confirm
# View(MRSC.temp.m)

##Normality of Temperature
# ggplot(data=MRSC.temp.m, aes(x = in1.C))+
#   geom_histogram(binwidth = 1)
# 
# ggplot(data=MRSC.temp.m, aes(x = in2.C))+
#   geom_histogram(binwidth = 1)
# 
# ggplot(data=MRSC.temp.m, aes(x = out.C))+
#   geom_histogram(binwidth = 1)
# 
# ggplot(data=MRSC.temp.m, aes(x = well.C))+
#   geom_histogram(binwidth = 1)
# 
# ggplot(data=MRSC.temp.m)+
#   geom_qq(aes(sample = in1.C))+
#   geom_abline(intercept = 0, slope = 1)
# 
# ggplot(data=MRSC.temp.m)+
#   geom_qq(aes(sample = in2.C))+
#   geom_abline(intercept = 0, slope = 1)
# 
# ggplot(data=MRSC.temp.m)+
#   geom_qq(aes(sample = out.C))+
#   geom_abline(intercept = 0, slope = 1)
# 
# ggplot(data=MRSC.temp.m)+
#   geom_qq(aes(sample = well.C))+
#   geom_abline(intercept = 0, slope = 1)

## Split into list of events
event.temps <- MRSC.temp.m %>%
  subset(storm.index != 0) %>%
  group_by(storm.index) %>%
  summarise(start = min(timestamp),
            Accumulation = sum(rainfall.mm),
            in1.med = median(in1.C, na.rm = TRUE),
            in1.max = max(in1.C, na.rm = TRUE),
            in2.med = median(in2.C, na.rm = TRUE),
            in2.max = max(in2.C, na.rm = TRUE),
            well.med = median(well.C, na.rm = TRUE),
            well.max = max(well.C, na.rm = TRUE),
            out.med = median(out.C, na.rm = TRUE),
            out.max = max(out.C, na.rm = TRUE),
            dryout.med = median(dryout.C, na.rm = TRUE),
            dryout.max = max(dryout.C, na.rm = TRUE)) %>%
  subset(!is.na(in1.med) &
           !is.na(in2.med) &
           !is.na(well.med) &
           !is.na(out.med) &
           Accumulation >= 2.24) 
# View(event.temps)

## monthly stats
month.stats <- event.temps %>%
  group_by(month=floor_date(start, "month")) %>%
  summarise(in1.med = median(in1.med, na.rm = TRUE),
            in1.max = max(in1.max, na.rm = TRUE),
            in2.med = median(in2.med, na.rm = TRUE),
            in2.max = max(in2.max, na.rm = TRUE),
            well.med = median(well.med, na.rm = TRUE),
            well.max = max(well.max, na.rm = TRUE),
            out.med = median(out.med, na.rm = TRUE),
            out.max = max(out.max, na.rm = TRUE))
# View(month.stats)

## Subset Event 26 for plotting
event.92 <- MRSC.temp.m %>%
  subset(storm.index == 92) %>%
  select(timestamp,
         atm.C,
         in1.C,
         in2.C,
         well.C,
         out.C) %>%
  melt(id = "timestamp")
# View(event.26)

# plot
ggplot(event.92)+
  geom_line(aes(x = timestamp, y = value, color = variable, linetype = variable))+
  scale_color_manual(values = c("black", "red", "purple", "blue", "green"),labels = c("Air", "IN1", "IN2", "WELL", "OUT"))+
  scale_linetype_manual(values = c(1,2,3,4,5), labels = c("Air", "IN1", "IN2", "WELL", "OUT"))+
  labs(y = "Temperature (°C)", x = "Date")+
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 18))

## statistics
# median in1 and in2
# wilcox.test(event.temps$in1.med, event.temps$in2.med, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# returns:
# Wilcoxon signed rank test
# 
# data:  event.temps$in1.med and event.temps$in2.med
# V = 344, p-value = 5.662e-07
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   0.7666667 1.3461111
# sample estimates:
#   (pseudo)median 
# 1.033889

# max in1 and in2
# wilcox.test(event.temps$in1.max, event.temps$in2.max, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# returns:
# Wilcoxon signed rank test
# 
# data:  event.temps$in1.max and event.temps$in2.max
# V = 285, p-value = 0.004279
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   0.3616667 2.1141667
# sample estimates:
#   (pseudo)median 
# 1.336111 

# median in1 and well
# wilcox.test(event.temps$in1.med, event.temps$well.med, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# returns:
# Wilcoxon signed rank test
# 
# data:  event.temps$in1.med and event.temps$well.med
# V = 230, p-value = 0.1731
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   -0.3036111  1.8613889
# sample estimates:
#   (pseudo)median 
# 0.7738889 

# max in1 and well
# wilcox.test(event.temps$in1.max, event.temps$well.max, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# returns:
# Wilcoxon signed rank test
# 
# data:  event.temps$in1.max and event.temps$well.max
# V = 321, p-value = 6.032e-05
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   1.860556 4.973889
# sample estimates:
#   (pseudo)median 
# 3.354444 

# median in2 and well
# wilcox.test(event.temps$in2.med, event.temps$well.med, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# returns:
# Wilcoxon signed rank test
# 
# data:  event.temps$in2.med and event.temps$well.med
# V = 155, p-value = 0.6171
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   -1.1633333  0.6705556
# sample estimates:
#   (pseudo)median 
# -0.3488889 

# max in2 and well
# wilcox.test(event.temps$in2.max, event.temps$well.max, alternative = "g", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# returns:
# Wilcoxon signed rank test
# 
# data:  event.temps$in2.max and event.temps$well.max
# V = 288, p-value = 0.001615
# alternative hypothesis: true location shift is greater than 0
# 95 percent confidence interval:
#   0.8502778       Inf
# sample estimates:
#   (pseudo)median 
# 1.951389 

# median in2 and out
# wilcox.test(event.temps$in2.med, event.temps$out.med, alternative = "l", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# returns:
# Wilcoxon signed rank test with continuity correction
# 
# data:  event.temps$in2.med and event.temps$out.med
# V = 17.5, p-value = 8.11e-05
# alternative hypothesis: true location shift is less than 0
# 95 percent confidence interval:
#   -Inf -0.4800211
# sample estimates:
#   (pseudo)median 
# -0.6968198

# max in2 and out
# wilcox.test(event.temps$in2.max, event.temps$out.max, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# returns:
# Wilcoxon signed rank test with continuity correction
# 
# data:  event.temps$in2.max and event.temps$out.max
# V = 134, p-value = 0.9152
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   -0.8716881  0.8408425
# sample estimates:
#   (pseudo)median 
# -0.09485565  

# median well and out
# wilcox.test(event.temps$well.med, event.temps$out.med, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# returns:
# Wilcoxon signed rank test
# 
# data:  event.temps$well.med and event.temps$out.med
# V = 146, p-value = 0.4678
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   -1.4050000  0.6033333
# sample estimates:
#   (pseudo)median 
# -0.2913889

# max well and out
# wilcox.test(event.temps$well.max,event.temps$out.max,   alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# returns:
# Wilcoxon signed rank test
# 
# data:  event.temps$well.max and event.temps$out.max
# V = 53, p-value = 0.001165
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   -3.3083333 -0.8622222
# sample estimates:
#   (pseudo)median 
# -2.105278 

# median in1 and out
# wilcox.test(event.temps$in1.med, event.temps$out.med, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# returns:
# Wilcoxon signed rank test with continuity correction
# 
# data:  event.temps$in1.med and event.temps$out.med
# V = 266, p-value = 0.0001053
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   0.2419189 0.5705846
# sample estimates:
#   (pseudo)median 
# 0.3611517 

# max in1 and out
# wilcox.test(event.temps$in1.max, event.temps$out.max, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# returns:
# Wilcoxon signed rank test with continuity correction
# 
# data:  event.temps$in1.max and event.temps$out.max
# V = 322, p-value = 1.882e-05
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   0.7205007 1.7794725
# sample estimates:
#   (pseudo)median 
# 1.29499 