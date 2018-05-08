## Analysis of Morrisville RSC data from Providance Farm, Mason Farm Road
## monitoring period July 2017-June 2018
## data location in TBD

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

## Create Time Series
## Use in matching observations
## 'by' 2-min interval
ts <- seq(ymd_hm("2017-07-12 0:00"), ymd_hm("2018-06-20 0:00"), by = 120) 
## Make data frame
ts.df <- data.frame(timestamp=ts)
##View(ts.df)

## Begin joining datasets
## Read file MRSC_rain
rain <- read.csv("./FlowLink/180507/MRSC_rain.csv")
## Reformat dates
rain$timestamp <- mdy_hm(rain$timestamp, tz = "GMT")
##View(rain)
## Join rainfall data
DS <- left_join(ts.df, rain, by = "timestamp") 
##View(DS)

## Read file MRSC_In1_level
in1.ft <- read.csv("./FlowLink/180507/MRSC_In1_level.csv")
## Reformat dates
in1.ft$timestamp <- mdy_hm(in1.ft$timestamp, tz = "GMT")
##View(in1.ft)
## Join in1.ft data
DS <- left_join(DS, in1.ft, by = "timestamp") 
##View(DS)

## Read file MRSC_In2_level
in2.ft <- read.csv("./FlowLink/180507/MRSC_In2_level.csv")
## Reformat dates
in2.ft$timestamp <- mdy_hm(in2.ft$timestamp, tz = "GMT")
##View(in2.ft)
## Join in2.ft data
DS <- left_join(DS, in2.ft, by = "timestamp") 
##View(DS)

## Read file MRSC_out_level
out.ft <- read.csv("./FlowLink/180507/MRSC_out_level.csv")
## Reformat dates
out.ft$timestamp <- mdy_hm(out.ft$timestamp, tz = "GMT")
##View(out.ft)
## Join out.ft data
DS <- left_join(DS, out.ft, by = "timestamp") 
##View(DS)

## Read file MRSC_out_velocity
out.velo <- read.csv("./FlowLink/180507/MRSC_out_velocity.csv")
## Reformat dates
out.velo$timestamp <- mdy_hm(out.velo$timestamp, tz = "GMT")
##View(out.velo)
## Join out.velo data
DS <- left_join(DS, out.velo, by = "timestamp") 
##View(DS)

## Binding Inlet 2 HOBO files
## create vector of dates when data was collected
## add additional collection dates to this vector as files are added to analysis
dates=c("170712", "170718", "170915", "171002", "171013", "171018", "171031", 
        "171110", "171206", "180108", "180122", "180131", "180215", "180304",
        "180328", "180405", "180423")
for (i in 1:length(dates)){
  # i=1
  date=dates[i]
  filename<-sprintf("./HOBO/RSC_INLET2_%s.csv",date)                            ##Specify file where data is located
  use<-read.table(file=filename,sep=",",header=TRUE,skip=2)                     ##Read the file
  use=use[,c(2:4)]                                                              ##Only columns 2-4
  colnames(use)=c("timestamp","in2.F","in2.hobo.ft")                            ##Name all columns
  assign(paste0("List",i),use)                                                  ##Assign names for each list
}
total=rbind(List1,List2,List3,List4,List5,List6,List7,List8,List9,List10,List11,List12,List13,List14,List15,List16,List17)          ##Row bind all of the fingerprint files
fact = total[match(unique(total$timestamp), total$timestamp),]    
in2 <- fact

## Binding ATM HOBO files
## create vector of dates when data was collected
## add additional collection dates to this vector as files are added to analysis
dates=c("170712", "170718", "170823", "170911", "170915", "171002", "171013", "171018", "171031", 
        "171110", "171206", "180108", "180122", "180131", "180215", "180304",
        "180328", "180405", "180423")
for (i in 1:length(dates)){
  # i=1
  date=dates[i]
  filename<-sprintf("./HOBO/RSC_ATM_%s.csv",date)                             ##Specify file where data is located
  use<-read.table(file=filename,sep=",",header=TRUE,skip=2)                   ##Read the file
  use=use[,c(2:3)]                                                            ##Only columns 2-3
  colnames(use)=c("timestamp","atm.F")                                        ##Name all columns
  assign(paste0("List",i),use)                                                ##Assign names for each list
}
total1=rbind(List1,List2,List3,List4,List5,List6,List7,List8,List9,List10,List11,List12,List13,List14,List15,List16,List17,List18,List19)          ##Row bind all of the fingerprint files
fact1 = total1[match(unique(total1$timestamp), total1$timestamp),]    
atm <- fact1

## Binding Well HOBO files
## create vector of dates when data was collected
## add additional collection dates to this vector as files are added to analysis
dates=c("171031", "171110", "171206", "180108", "180122", "180131", "180215", "180304",
        "180328", "180405", "180423")
for (i in 1:length(dates)){
  # i=1
  date=dates[i]
  filename<-sprintf("./HOBO/RSC_well_%s.csv",date)                            ##Specify file where data is located
  use<-read.table(file=filename,sep=",",header=TRUE,skip=2)                   ##Read the file
  use=use[,c(2:4)]                                                            ##Only columns 2-4
  colnames(use)=c("timestamp","well.F", "well.ft" )                           ##Name all columns
  assign(paste0("List",i),use)                                                ##Assign names for each list
}
total2=rbind(List1,List2,List3,List4,List5,List6,List7,List8,List9,List10,List11)          ##Row bind all of the fingerprint files
fact2 = total2[match(unique(total2$timestamp), total2$timestamp),]    
well <- fact2

## Binding In1 Temp HOBO files
## create vector of dates when data was collected
## add additional collection dates to this vector as files are added to analysis
dates=c("171018", "171031", "171110", "180223", "180304",
        "180328", "180405", "180423")
for (i in 1:length(dates)){
  # i=1
  date=dates[i]
  filename<-sprintf("./HOBO/RSC_In1_TEMP_%s.csv",date)                             ##Specify file where data is located
  use<-read.table(file=filename,sep=",",header=TRUE,skip=2)                        ##Read the file
  use=use[,c(2:3)]                                                                 ##Only columns 2-3
  colnames(use)=c("timestamp","in1.F")                                             ##Name all columns
  assign(paste0("List",i),use)                                                     ##Assign names for each list
}
total3=rbind(List1,List2,List3,List4,List5,List6,List7,List8)          ##Row bind all of the fingerprint files
fact3 = total3[match(unique(total3$timestamp), total3$timestamp),]    
in1.F <- fact3

## Binding Out Temp HOBO files
## create vector of dates when data was collected
## add additional collection dates to this vector as files are added to analysis
dates=c("171018", "171031", "171110", "180223", "180304",
        "180328", "180405", "180423")
for (i in 1:length(dates)){
  # i=1
  date=dates[i]
  filename<-sprintf("./HOBO/RSC_outlet_TEMP_%s.csv",date)                           ##Specify file where data is located
  use<-read.table(file=filename,sep=",",header=TRUE,skip=2)                         ##Read the file
  use=use[,c(2:3)]                                                                  ##Only columns 2-3
  colnames(use)=c("timestamp","out.F")                                              ##Name all columns
  assign(paste0("List",i),use)                                                      ##Assign names for each list
}
total4=rbind(List1,List2,List3,List4,List5,List6,List7,List8)          ##Row bind all of the fingerprint files
fact4 = total4[match(unique(total4$timestamp), total4$timestamp),]    
out.F <- fact4

## Reformat Bound data timestamps for join
## Reformat dates Inlet 2 hobo data
in2$timestamp <- mdy_hms(in2$timestamp, tz = "GMT")
##View(in2)
## Join in2 data
DS <- left_join(DS, in2, by = "timestamp") 
##View(DS)

## Reformat dates atm
atm$timestamp <- mdy_hms(atm$timestamp, tz = "GMT")
##View(atm)
## Join atm data
DS <- left_join(DS, atm, by = "timestamp") 
##View(DS)

## Reformat dates well
well$timestamp <- mdy_hms(well$timestamp, tz = "GMT")
##View(well)
## Join well data
DS <- left_join(DS, well, by = "timestamp") 
##View(DS)

## Reformat dates inlet 1 temp
in1.F$timestamp <- mdy_hms(in1.F$timestamp, tz = "GMT")
##View(in1.F)
## Join in1.F data
DS <- left_join(DS, in1.F, by = "timestamp") 
##View(DS)

## Reformat dates outlet temp
out.F$timestamp <- mdy_hms(out.F$timestamp, tz = "GMT")
##View(out.F)
## Join out.F data
DS <- left_join(DS, out.F, by = "timestamp") 
##View(DS)

## Reorder variables for visual preference
DS <- DS[,c(1,2,9,3,4,8,5,6,11,12,7,13,10)]
##View(DS)

## Select observations after day of first observation
DS <- subset.data.frame(DS, timestamp > "2017-07-12 12:00:00")

## Write .csv file for use in analysis
write.csv(DS, "./Working/dataset.csv")
