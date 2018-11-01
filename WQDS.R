## Analysis of Morrisville RSC WQ data from Providance Farm, Mason Farm Road
## monitoring period July 2017-June 2018
## data location in ./Working
## dataset preparation + analysis

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
## units TSS mg/L, all others ug/L
DS.wq <- read.csv("./Working/DS.wq_results_180827.csv")
# View(DS.wq)

## Rename columns
colnames(DS.wq) <- c("samp.date", 
                     "site",
                     "event",
                     "TKN",
                     "TKN.qc",
                     "NOx",
                     "NOx.qc",
                     "NH3N",
                     "NH3N.qc",
                     "TP",
                     "TP.qc",
                     "OP",
                     "OP.qc",
                     "TSS",
                     "TSS.est",
                     "TSS.qc")
# View(DS.wq)

##Format date time
DS.wq$samp.date <- mdy(DS.wq$samp.date)

## TSS correct
DS.wq <- DS.wq %>%
  mutate(TSS.extra = ifelse(is.na(TSS.est), TSS, TSS.est ))
# View(DS.wq)

## Summarize base flow concentrations per site
base.wq <- (DS.wq) %>%
  select(samp.date, 
         site, 
         event, 
         TKN, 
         NOx, 
         NH3N, 
         TP, 
         OP, 
         TSS,
         TSS.est) %>%
  mutate(TN = TKN + NOx,
         ON = TKN - NH3N,
         PBP = TP - OP,
         TSS.extra = ifelse(is.na(TSS.est), TSS, TSS.est )) %>%
  subset(event == "base")
# View(base.wq)
base.sum.wq <- (base.wq) %>%
  group_by(as.character(site)) %>%
  summarise_at(vars(-samp.date, -site, -event), funs(mean, median, max, min, var, sd), na.rm = TRUE)
# View(base.sum.wq)

## Test for normailty in pollutants
# shapiro.test(base.wq$TKN)
# shapiro.test(base.wq$NOx)
# shapiro.test(base.wq$NH3N)
# shapiro.test(base.wq$TP)
# shapiro.test(base.wq$OP)
# shapiro.test(base.wq$TSS)
# shapiro.test(storm.wq$TKN)
# shapiro.test(storm.wq$NOx)
# shapiro.test(storm.wq$NH3N)
# shapiro.test(storm.wq$TP)
# shapiro.test(storm.wq$OP)
# shapiro.test(storm.wq$TSS)


## Box plots of by site, analyte, and parameter
# Storm data set prep
storm.wq.melt <- (storm.wq) %>%
  select(site,
         TKN, 
         NOx, 
         NH3N, 
         TP, 
         OP, 
         TSS) %>%
  melt(id = "site")
#View(storm.wq.melt)

# baseflow data set prep
base.wq.melt <- (base.wq) %>%
  select(site,
         TKN, 
         NOx, 
         NH3N, 
         TP, 
         OP, 
         TSS) %>%
  melt(id = "site")
#View(base.wq.melt)

# Box plots
ggplot(data = storm.wq.melt, aes(x = variable))+
  ggtitle("Storm Flow")+
  geom_boxplot(aes(y = value, color = site))+
  labs(y = "Concentration (ug/L) or (mg/L)", x = "Pollutant")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

# Box plots
ggplot(data = base.wq.melt, aes(x = variable))+
  geom_boxplot(aes(y = value, color = site))+
  labs(y = "Concentration (ug/L) or (mg/L)", x = "Pollutant")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

## Stats base flow
# subset at sampling sites
base.in <- DS.wq %>%
  subset(site == "IN1" & event == "base")
# View(base.in)
base.out <- DS.wq %>%
  subset(site == "OUT" & event == "base")
# View(base.out)

## TKN analysis
base.TKN <- base.in %>%
  select(samp.date,
         TKN) 
# View(base.TKN)
base.TKN1 <- base.out %>%
  select(samp.date,
         TKN) 
# View(base.TKN1)
base.TKN <- left_join(base.TKN, base.TKN1, "samp.date")
# % reduction
base.TKN <- base.TKN %>%
  mutate(reduc = ((TKN.x - TKN.y) / TKN.x) * 100)
# View(base.TKN)
# median(base.TKN$reduc)
# return: 16.5

## NOx analysis
base.NOx <- base.in %>%
  select(samp.date,
         NOx) 
# View(base.NOx)
base.NOx1 <- base.out %>%
  select(samp.date,
         NOx) 
# View(base.NOx1)
base.NOx <- left_join(base.NOx, base.NOx1, "samp.date")
# % reduction
base.NOx <- base.NOx %>%
  mutate(reduc = ((NOx.x - NOx.y) / NOx.x) * 100)
# View(base.NOx)
# median(base.NOx$reduc)
# return: 44.0

## TN analysis
base.TN <- base.in %>%
  select(samp.date,
         TKN,
         NOx) %>%
  transmute(samp.date = samp.date,
            TN = TKN + NOx)
# View(base.TN)
base.TN1 <- base.out %>%
  select(samp.date,
         TKN,
         NOx) %>%
  transmute(samp.date = samp.date,
            TN = TKN + NOx)
# View(base.TN1)
base.TN <- left_join(base.TN, base.TN1, "samp.date")
# % reduction
base.TN <- base.TN %>%
  mutate(reduc = ((TN.x - TN.y) / TN.x) * 100)
# View(base.TN)
# median(base.TN$reduc)
# return: 22.3

## NH3N analysis
base.NH3N <- base.in %>%
  select(samp.date,
         NH3N) 
# View(base.NH3N)
base.NH3N1 <- base.out %>%
  select(samp.date,
         NH3N) 
# View(base.NH3N1)
base.NH3N <- left_join(base.NH3N, base.NH3N1, "samp.date")
# % reduction
base.NH3N <- base.NH3N %>%
  mutate(reduc = ((NH3N.x - NH3N.y) / NH3N.x) * 100)
# View(base.NH3N)
# median(base.NH3N$reduc)
# return: 13.8

## TP analysis
base.TP <- base.in %>%
  select(samp.date,
         TP) 
# View(base.TP)
base.TP1 <- base.out %>%
  select(samp.date,
         TP) 
# View(base.TP1)
base.TP <- left_join(base.TP, base.TP1, "samp.date")
# % reduction
base.TP <- base.TP %>%
  mutate(reduc = ((TP.x - TP.y) / TP.x) * 100)
# View(base.TP)
# median(base.TP$reduc)
# return: 2.6

## OP analysis
base.OP <- base.in %>%
  select(samp.date,
         OP) 
# View(base.OP)
base.OP1 <- base.out %>%
  select(samp.date,
         OP) 
# View(base.OP1)
base.OP <- left_join(base.OP, base.OP1, "samp.date")
# % reduction
base.OP <- base.OP %>%
  mutate(reduc = ((OP.x - OP.y) / OP.x) * 100)
# View(base.OP)
# median(base.OP$reduc)
# return: -4.1

## TSS analysis
base.TSS <- base.in %>%
  select(samp.date,
         TSS) 
# View(base.TSS)
base.TSS1 <- base.out %>%
  select(samp.date,
         TSS) 
# View(base.TSS1)
base.TSS <- left_join(base.TSS, base.TSS1, "samp.date")
# % reduction
base.TSS <- base.TSS %>%
  mutate(reduc = ((TSS.x - TSS.y) / TSS.x) * 100)
# View(base.TSS)
# median(base.TSS$reduc)
# return: 33.0

## TSS.extra analysis
base.TSS.extra <- base.in %>%
  select(samp.date,
         TSS.extra) 
# View(base.TSS.extra)
base.TSS.extra1 <- base.out %>%
  select(samp.date,
         TSS.extra) 
# View(base.TSS.extra1)
base.TSS.extra <- left_join(base.TSS.extra, base.TSS.extra1, "samp.date")
# % reduction
base.TSS.extra <- base.TSS.extra %>%
  mutate(reduc = ((TSS.extra.x - TSS.extra.y) / TSS.extra.x) * 100)
# View(base.TSS.extra)
# median(base.TSS.extra$reduc)
# return: 10.5

## ON analysis
base.ON <- base.in %>%
  select(samp.date,
         TKN,
         NH3N) %>%
  mutate(ON = TKN - NH3N)
# View(base.ON)
base.ON1 <- base.out %>%
  select(samp.date,
         TKN,
         NH3N) %>%
  mutate(ON = TKN - NH3N)
# View(base.ON1)
base.ON <- left_join(base.ON, base.ON1, "samp.date")
# % reduction
base.ON <- base.ON %>%
  mutate(reduc = ((ON.x - ON.y) / ON.x) * 100)
# View(base.ON)
# median(base.ON$reduc)
# return: 21.4

## PBP analysis
base.PBP <- base.in %>%
  select(samp.date,
         TP,
         OP) %>%
  mutate(PBP = TP - OP)
# View(base.PBP)
base.PBP1 <- base.out %>%
  select(samp.date,
         TP,
         OP) %>%
  mutate(PBP = TP - OP)
# View(base.PBP1)
base.PBP <- left_join(base.PBP, base.PBP1, "samp.date")
# % reductiPBP
base.PBP <- base.PBP %>%
  mutate(reduc = ((PBP.x - PBP.y) / PBP.x) * 100)
# View(base.PBP)
# median(base.PBP$reduc)
# return: 23.0

## base flow pollutant testing significance
# TKN
wilcox.test(base.TKN$TKN.x, base.TKN$TKN.y, alternative = "g", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns
# Wilcoxon signed rank test
# 
# data:  base.TKN$TKN.x and base.TKN$TKN.y
# V = 51, p-value = 0.006836
# alternative hypothesis: true location shift is greater than 0
# 95 percent confidence interval:
#   54.96   Inf
# sample estimates:
#   (pseudo)median
# 120.4

# NOx
wilcox.test(base.NOx$NOx.x, base.NOx$NOx.y, alternative = "t", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns
# Wilcoxon signed rank test
# 
# data:  base.NOx$NOx.x and base.NOx$NOx.y
# V = 49, p-value = 0.02734
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   5.93 325.76
# sample estimates:
#   (pseudo)median 
# 80.195 

# TN
wilcox.test(base.TN$TN.x, base.TN$TN.y, alternative = "t", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns
# Wilcoxon signed rank test
# 
# data:  base.TN$TN.x and base.TN$TN.y
# V = 52, p-value = 0.009766
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   108.755 470.640
# sample estimates:
#   (pseudo)median 
# 227.79 

# NH3N
wilcox.test(base.NH3N$NH3N.x, base.NH3N$NH3N.y, alternative = "t", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns
# Wilcoxon signed rank test
# 
# data:  base.NH3N$NH3N.x and base.NH3N$NH3N.y
# V = 31, p-value = 0.7695
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   -20.81  32.32
# sample estimates:
#   (pseudo)median 
# 6.25 

# TP
wilcox.test(base.TP$TP.x, base.TP$TP.y, alternative = "t", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns:
# Wilcoxon signed rank test
# 
# data:  base.TP$TP.x and base.TP$TP.y
# V = 29, p-value = 0.9219
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   -36.955  23.695
# sample estimates:
#   (pseudo)median 
# 2.615

# OP
wilcox.test(base.OP$OP.x, base.OP$OP.y, alternative = "t", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns:
# Wilcoxon signed rank test
# 
# data:  base.OP$OP.x and base.OP$OP.y
# V = 22, p-value = 0.625
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   -34.38  11.78
# sample estimates:
#   (pseudo)median 
# -5.56 

# TSS
wilcox.test(base.TSS$TSS.x, base.TSS$TSS.y, alternative = "g", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns
# Wilcoxon signed rank test
# 
# data:  base.TSS$TSS.x and base.TSS$TSS.y
# V = 45, p-value = 0.04199
# alternative hypothesis: true location shift is greater than 0
# 95 percent confidence interval:
#   0.14  Inf
# sample estimates:
#   (pseudo)median 
# 1.41 

# TSS.extra
wilcox.test(base.TSS.extra$TSS.extra.x, base.TSS.extra$TSS.extra.y, alternative = "g", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns
# Wilcoxon signed rank test
# 
# data:  base.TSS.extra$TSS.extra.x and base.TSS.extra$TSS.extra.y
# V = 48, p-value = 0.01855
# alternative hypothesis: true location shift is greater than 0
# 95 percent confidence interval:
#   0.19  Inf
# sample estimates:
#   (pseudo)median 
# 1.225 

# ON
wilcox.test(base.ON$ON.x, base.ON$ON.y, alternative = "g", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns
# Wilcoxon signed rank test
# 
# data:  base.ON$ON.x and base.ON$ON.y
# V = 52, p-value = 0.004883
# alternative hypothesis: true location shift is greater than 0
# 95 percent confidence interval:
#   48.125    Inf
# sample estimates:
#   (pseudo)median 
# 115.185

# PBP
wilcox.test(base.PBP$PBP.x, base.PBP$PBP.y, alternative = "g", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns
# Wilcoxon signed rank test
# 
# data:  base.PBP$PBP.x and base.PBP$PBP.y
# V = 38, p-value = 0.1611
# alternative hypothesis: true location shift is greater than 0
# 95 percent confidence interval:
#   -5.695    Inf
# sample estimates:
#   (pseudo)median 
# 8.01 

## Summarize storm flow concentrations per site
storm.wq <- (DS.wq) %>%
  select(samp.date, 
         site, 
         event, 
         TKN, 
         NOx, 
         NH3N, 
         TP, 
         OP, 
         TSS,
         TSS.est) %>%
  mutate(ON = TKN - NH3N,
         PBP = TP - OP) %>%
  subset(event == "storm")
# View(storm.wq)
storm.sum.wq <- (storm.wq) %>%
  group_by(as.character(site)) %>%
  summarise_at(vars(-samp.date, -site, -event), funs(mean, median, max, min, var, sd))
# View(storm.sum.wq)

## Gather Inlet concentrations
in.wq <- (DS.wq) %>%
  select(samp.date, 
         site, 
         event, 
         TKN, 
         NOx, 
         NH3N, 
         TP, 
         OP, 
         TSS) %>%
  subset(event == "storm" & 
         site == "IN1" | site == "IN2") %>% 
  mutate(TN = TKN + NOx,
         ON = TKN - NH3N,
         PBP = TP - OP) 
# View(in.wq)

## In.wq summary
in.wq.sum <- in.wq[-c(1),] %>%
  group_by(as.character(site)) %>%
  summarise_at(vars(-samp.date, -site, -event), funs(mean, median, max, min, var, sd))
# View(in.wq.sum)

## In.wq storm stats analysis
# subset at sampling sites
storm.in1 <- in.wq[-c(1),] %>%
  subset(site == "IN1") %>%
  mutate(ON = TKN - NH3N,
         PBP = TP - OP)
# View(storm.in1)
storm.in2 <- in.wq[-c(1),] %>%
  subset(site == "IN2") %>%
  mutate(ON = TKN - NH3N,
         PBP = TP - OP)
# View(storm.in2)

## TKN analysis
storm.in.TKN <- storm.in1 %>%
  select(samp.date,
         TKN) 
# View(storm.in.TKN)
storm.in.TKN1 <- storm.in2 %>%
  select(samp.date,
         TKN) 
# View(storm.in.TKN1)
storm.in.TKN <- left_join(storm.in.TKN, storm.in.TKN1, "samp.date")
# % reduction
storm.in.TKN <- storm.in.TKN %>%
  mutate(reduc = ((TKN.x - TKN.y) / TKN.x) * 100)
# View(storm.in.TKN)
# median(storm.in.TKN$reduc)
# return: 74.4

## NOx analysis
storm.in.NOx <- storm.in1 %>%
  select(samp.date,
         NOx) 
# View(storm.in.NOx)
storm.in.NOx1 <- storm.in2 %>%
  select(samp.date,
         NOx) 
# View(storm.in.NOx1)
storm.in.NOx <- left_join(storm.in.NOx, storm.in.NOx1, "samp.date")
# % reduction
storm.in.NOx <- storm.in.NOx %>%
  mutate(reduc = ((NOx.x - NOx.y) / NOx.x) * 100)
# View(storm.in.NOx)
# median(storm.in.NOx$reduc)
# return: 34.1

## TN analysis
storm.in.TN <- storm.in1 %>%
  select(samp.date,
         TKN,
         NOx) %>%
  transmute(samp.date = samp.date,
            TN = TKN + NOx)
# View(storm.in.TN)
storm.in.TN1 <- storm.in2 %>%
  select(samp.date,
         TKN,
         NOx) %>%
  transmute(samp.date = samp.date,
            TN = TKN + NOx)
# View(storm.in.TN1)
storm.in.TN <- left_join(storm.in.TN, storm.in.TN1, "samp.date")
# % reduction
storm.in.TN <- storm.in.TN %>%
  mutate(reduc = ((TN.x - TN.y) / TN.x) * 100)
# View(storm.in.TN)
# median(storm.in.TN$reduc)
# return: 67.8

## NH3N analysis
storm.in.NH3N <- storm.in1 %>%
  select(samp.date,
         NH3N) 
# View(storm.in.NH3N)
storm.in.NH3N1 <- storm.in2 %>%
  select(samp.date,
         NH3N) 
# View(storm.in.NH3N1)
storm.in.NH3N <- left_join(storm.in.NH3N, storm.in.NH3N1, "samp.date")
# % reduction
storm.in.NH3N <- storm.in.NH3N %>%
  mutate(reduc = ((NH3N.x - NH3N.y) / NH3N.x) * 100)
# View(storm.in.NH3N)
# median(storm.in.NH3N$reduc)
# return: 52.4

## TP analysis
storm.in.TP <- storm.in1 %>%
  select(samp.date,
         TP) 
# View(storm.in.TP)
storm.in.TP1 <- storm.in2 %>%
  select(samp.date,
         TP) 
# View(storm.in.TP1)
storm.in.TP <- left_join(storm.in.TP, storm.in.TP1, "samp.date")
# % reduction
storm.in.TP <- storm.in.TP %>%
  mutate(reduc = ((TP.x - TP.y) / TP.x) * 100)
# View(storm.in.TP)
# median(storm.in.TP$reduc)
# return: 73.4

## OP analysis
storm.in.OP <- storm.in1 %>%
  select(samp.date,
         OP) 
# View(storm.in.OP)
storm.in.OP1 <- storm.in2 %>%
  select(samp.date,
         OP) 
# View(storm.in.OP1)
storm.in.OP <- left_join(storm.in.OP, storm.in.OP1, "samp.date")
# % reduction
storm.in.OP <- storm.in.OP %>%
  mutate(reduc = ((OP.x - OP.y) / OP.x) * 100)
# View(storm.in.OP)
# median(storm.in.OP$reduc)
# return: 30.5

## TSS analysis
storm.in.TSS <- storm.in1 %>%
  select(samp.date,
         TSS) 
# View(storm.in.TSS)
storm.in.TSS1 <- storm.in2 %>%
  select(samp.date,
         TSS) 
# View(storm.in.TSS1)
storm.in.TSS <- left_join(storm.in.TSS, storm.in.TSS1, "samp.date")
# % reduction
storm.in.TSS <- storm.in.TSS %>%
  mutate(reduc = ((TSS.x - TSS.y) / TSS.x) * 100)
# View(storm.in.TSS)
# median(storm.in.TSS$reduc)
# return: 85.5

## ON analysis
storm.in.ON <- storm.in1 %>%
  select(samp.date,
         TKN,
         NH3N) %>%
  mutate(ON = TKN - NH3N)
# View(storm.in.ON)
storm.in.ON1 <- storm.in2 %>%
  select(samp.date,
         TKN,
         NH3N) %>%
  mutate(ON = TKN - NH3N)
# View(storm.in.ON1)
storm.in.ON <- left_join(storm.in.ON, storm.in.ON1, "samp.date")
# % reduction
storm.in.ON <- storm.in.ON %>%
  mutate(reduc = ((ON.x - ON.y) / ON.x) * 100)
# View(storm.in.ON)
# median(storm.in.ON$reduc)
# return: 74.4

## PBP analysis
storm.in.PBP <- storm.in1 %>%
  select(samp.date,
         TP,
         OP) %>%
  mutate(PBP = TP - OP)
# View(storm.in.PBP)
storm.in.PBP1 <- storm.in2 %>%
  select(samp.date,
         TP,
         OP) %>%
  mutate(PBP = TP - OP)
# View(storm.in.PBP1)
storm.in.PBP <- left_join(storm.in.PBP, storm.in.PBP1, "samp.date")
# % reductiPBP
storm.in.PBP <- storm.in.PBP %>%
  mutate(reduc = ((PBP.x - PBP.y) / PBP.x) * 100)
# View(storm.in.PBP)
# median(storm.in.PBP$reduc)
# return: 79.2

## storm flow inlet pollutant testing significance
# TKN
wilcox.test(storm.in.TKN$TKN.x, storm.in.TKN$TKN.y, alternative = "t", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns
# Wilcoxon signed rank test
# 
# data:  storm.in.TKN$TKN.x and storm.in.TKN$TKN.y
# V = 52, p-value = 0.009766
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   780.845 10354.065
# sample estimates:
#   (pseudo)median 
# 3103.845

# NOx
wilcox.test(storm.in.NOx$NOx.x, storm.in.NOx$NOx.y, alternative = "t", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns
# Wilcoxon signed rank test
# 
# data:  storm.in.NOx$NOx.x and storm.in.NOx$NOx.y
# V = 42, p-value = 0.1602
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   -160.375  351.905
# sample estimates:
#   (pseudo)median 
# 186.3 

# TN
wilcox.test(storm.in.TN$TN.x, storm.in.TN$TN.y, alternative = "t", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns
# Wilcoxon signed rank test
# 
# data:  storm.in.TN$TN.x and storm.in.TN$TN.y
# V = 54, p-value = 0.003906
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   1038.705 10138.545
# sample estimates:
#   (pseudo)median 
# 3392.15

# NH3N
wilcox.test(storm.in.NH3N$NH3N.x, storm.in.NH3N$NH3N.y, alternative = "t", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns
# Wilcoxon signed rank test
# 
# data:  storm.in.NH3N$NH3N.x and storm.in.NH3N$NH3N.y
# V = 48, p-value = 0.03711
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   20.55 315.48
# sample estimates:
#   (pseudo)median 
# 176.515 

# TP
wilcox.test(storm.in.TP$TP.x, storm.in.TP$TP.y, alternative = "t", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns:
# Wilcoxon signed rank test
# 
# data:  storm.in.TP$TP.x and storm.in.TP$TP.y
# V = 55, p-value = 0.001953
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   140.22 2702.74
# sample estimates:
#   (pseudo)median 
# 615.55 

# OP
wilcox.test(storm.in.OP$OP.x, storm.in.OP$OP.y, alternative = "t", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns:
# Wilcoxon signed rank test with continuity correction
# 
# data:  storm.in.OP$OP.x and storm.in.OP$OP.y
# V = 51.5, p-value = 0.01653
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   6.645048 48.064941
# sample estimates:
#   (pseudo)median 
# 23.81498  

# TSS
wilcox.test(storm.in.TSS$TSS.x, storm.in.TSS$TSS.y, alternative = "g", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns
# Wilcoxon signed rank test
# 
# data:  storm.in.TSS$TSS.x and storm.in.TSS$TSS.y
# V = 53, p-value = 0.00293
# alternative hypothesis: true location shift is greater than 0
# 95 percent confidence interval:
#   130.13    Inf
# sample estimates:
#   (pseudo)median 
# 303.07

# ON
wilcox.test(storm.in.ON$ON.x, storm.in.ON$ON.y, alternative = "g", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns
# Wilcoxon signed rank test
# 
# data:  storm.in.ON$ON.x and storm.in.ON$ON.y
# V = 53, p-value = 0.00293
# alternative hypothesis: true location shift is greater than 0
# 95 percent confidence interval:
#   1177.02     Inf
# sample estimates:
#   (pseudo)median 
# 2789.08 

# PBP
wilcox.test(storm.in.PBP$PBP.x, storm.in.PBP$PBP.y, alternative = "g", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns
# Wilcoxon signed rank test
# 
# data:  storm.in.PBP$PBP.x and storm.in.PBP$PBP.y
# V = 55, p-value = 0.0009766
# alternative hypothesis: true location shift is greater than 0
# 95 percent confidence interval:
#   283.025     Inf
# sample estimates:
#   (pseudo)median 
# 598.295

## combining inlet concentrations
# weighted average 0.32 * IN1 + 0.28 * IN2
in.tot <- left_join(storm.in1,storm.in2, by = "samp.date")
in.tot <- in.tot %>%
  transmute(samp.date = samp.date,
            TKN = 0.32 * TKN.x + 0.28 * TKN.y,
            NOx = 0.32 * NOx.x + 0.28 * NOx.y,
            NH3N = 0.32 * NH3N.x + 0.28 * NH3N.y,
            TN = 0.32 * TN.x + 0.28 * TN.y,
            TP = 0.32 * TP.x + 0.28 * TP.y,
            OP = 0.32 * OP.x + 0.28 * OP.y,
            TSS = 0.32 * TSS.x + 0.28 * TSS.y,
            ON = 0.32 * ON.x + 0.28 * ON.y,
            PBP = 0.32 * PBP.x + 0.28 * PBP.y)
# View(in.tot)

## In/Out wq
out.storm.wq <- storm.wq[-c(1,2),] %>%
  subset(site == "OUT") %>%
  select(-event, -TSS.est, -site) %>%
  mutate(TN = TKN + NOx,
         ON = TKN - NH3N,
         PBP = TP - OP)
# View(out.storm.wq)
tot.wq <- left_join(in.tot, out.storm.wq, by = "samp.date")
# View(tot.wq)

## In/Out wq summary
tot.wq.sum <- tot.wq %>%
  summarise_at(vars(-samp.date), funs(median, max, min, var, sd))
# View(tot.wq.sum)

## In/out % reduction
wq.reduc <- tot.wq %>%
  group_by(samp.date) %>%
  transmute(TKN.reduc = ((TKN.x - TKN.y) / TKN.x) * 100,
            NOx.reduc = ((NOx.x - NOx.y) / NOx.x) * 100,
            NH3N.reduc = ((NH3N.x - NH3N.y) / NH3N.x) * 100,
            TN.reduc = ((TN.x - TN.y) / TN.x) * 100,
            TP.reduc = ((TP.x - TP.y) / TP.x) * 100,
            OP.reduc = ((OP.x - OP.y) / OP.x) * 100,
            TSS.reduc = ((TSS.x - TSS.y) / TSS.x) * 100,
            ON.reduc = ((ON.x - ON.y) / ON.x) * 100,
            PBP.reduc = ((PBP.x - PBP.y) / PBP.x) * 100)
# View(wq.reduc)
# median(wq.reduc$TKN.reduc)
# returns: 4.0
# median(wq.reduc$NOx.reduc)
# returns: -77.3
# median(wq.reduc$NH3N.reduc)
# returns: 10.9
# median(wq.reduc$TN.reduc)
# returns: 3.43
# median(wq.reduc$TP.reduc)
# returns: 25.62
# median(wq.reduc$OP.reduc)
# returns: -52.5
# median(wq.reduc$TSS.reduc)
# returns: 69.21
# median(wq.reduc$ON.reduc)
# returns: 7.42
# median(wq.reduc$PBP.reduc)
# returns: 36.98

## storm flow in/out pollutant testing significance
# TKN
wilcox.test(tot.wq$TKN.x, tot.wq$TKN.y, alternative = "t", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns
# Wilcoxon signed rank test
# 
# data:  tot.wq$TKN.x and tot.wq$TKN.y
# V = 37, p-value = 0.375
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   -189.2462 2643.5184
# sample estimates:
#   (pseudo)median 
# 183.7802  

# NOx
wilcox.test(tot.wq$NOx.x, tot.wq$NOx.y, alternative = "t", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns
# Wilcoxon signed rank test
# 
# data:  tot.wq$NOx.x and tot.wq$NOx.y
# V = 9, p-value = 0.06445
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   -306.9928    8.3624
# sample estimates:
#   (pseudo)median 
# -233.998

# TN
wilcox.test(tot.wq$TN.x, tot.wq$TN.y, alternative = "t", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns
# Wilcoxon signed rank test
# 
# data:  tot.wq$TN.x and tot.wq$TN.y
# V = 31, p-value = 0.7695
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   -368.2368 2303.4728
# sample estimates:
#   (pseudo)median 
# 85.227 

# NH3N
wilcox.test(tot.wq$NH3N.x, tot.wq$NH3N.y, alternative = "t", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns
# Wilcoxon signed rank test
# 
# data:  tot.wq$NH3N.x and tot.wq$NH3N.y
# V = 32, p-value = 0.6953
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   -47.7020  71.6724
# sample estimates:
#   (pseudo)median 
# 12.798 

# TP
wilcox.test(tot.wq$TP.x, tot.wq$TP.y, alternative = "t", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns:
# Wilcoxon signed rank test
# 
# data:  tot.wq$TP.x and tot.wq$TP.y
# V = 46, p-value = 0.06445
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   -8.2444 609.9768
# sample estimates:
#   (pseudo)median 
# 93.5092 

# OP
wilcox.test(tot.wq$OP.x, tot.wq$OP.y, alternative = "t", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns:
# Wilcoxon signed rank test
# 
# data:  tot.wq$OP.x and tot.wq$OP.y
# V = 0, p-value = 0.001953
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   -40.8582 -13.1454
# sample estimates:
#   (pseudo)median 
# -24.1256 

# TSS
wilcox.test(tot.wq$TSS.x, tot.wq$TSS.y, alternative = "g", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns
# Wilcoxon signed rank test
# 
# data:  tot.wq$TSS.x and tot.wq$TSS.y
# V = 54, p-value = 0.001953
# alternative hypothesis: true location shift is greater than 0
# 95 percent confidence interval:
#   43.042    Inf
# sample estimates:
#   (pseudo)median 
# 95.0458

# ON
wilcox.test(tot.wq$ON.x, tot.wq$ON.y, alternative = "g", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns
# Wilcoxon signed rank test
# 
# data:  tot.wq$ON.x and tot.wq$ON.y
# V = 38, p-value = 0.1611
# alternative hypothesis: true location shift is greater than 0
# 95 percent confidence interval:
#   -100.2732       Inf
# sample estimates:
#   (pseudo)median 
# 162.5112 

# PBP
wilcox.test(tot.wq$PBP.x, tot.wq$PBP.y, alternative = "g", paired = TRUE, exact = TRUE, conf.int = TRUE, conf.level = 0.95 )
# returns
# Wilcoxon signed rank test
# 
# data:  tot.wq$PBP.x and tot.wq$PBP.y
# V = 51, p-value = 0.006836
# alternative hypothesis: true location shift is greater than 0
# 95 percent confidence interval:
#   35.3026     Inf
# sample estimates:
#   (pseudo)median 
# 112.35 

## Exceedence Probablility
# Extract from hendersonvill temperature analysis
# select variables from event summary table short
## TN
TN.ex <- tot.wq %>%
  select(starts_with("TN"))
colnames(TN.ex) <- c("storm.in", "storm.out")
TN.ex1 <- base.TN %>%
  select(starts_with("TN"))
colnames(TN.ex1) <- c("base.in", "base.out")
TN.ex <- bind_cols(TN.ex, TN.ex1)
# View(TN.ex) 
## sort columns
TN.ex <- TN.ex %>%
  mutate(In.st.sort = sort(storm.in, decreasing = TRUE, na.last = TRUE) / 1000,
         Out.st.sort = sort(storm.out, decreasing = TRUE, na.last = TRUE) / 1000,
         In.ba.sort = sort(base.in, decreasing = TRUE, na.last = TRUE) / 1000,
         Out.ba.sort = sort(base.out, decreasing = TRUE, na.last = TRUE) / 1000)
# View(TN.ex)
## Rank 
TN.ex <- TN.ex %>%
  mutate(In.st.rank = rank(desc(In.st.sort), na.last = TRUE),
         Out.st.rank = rank(desc(Out.st.sort), na.last = TRUE),
         In.ba.rank = rank(desc(In.ba.sort), na.last = TRUE),
         Out.ba.rank = rank(desc(Out.ba.sort), na.last = TRUE))
# View(TN.ex)
## Calculate probabiltiy
TN.ex <- TN.ex %>%
  mutate(In.st.prob = (In.st.rank) / (10 + 1),
         Out.st.prob = (Out.st.rank) / (10 + 1),
         In.ba.prob = (In.ba.rank) / (10 + 1),
         Out.ba.prob = (Out.ba.rank) / (10 + 1))
# View(TN.ex)
## Exceedance probability plots
ggplot(data = TN.ex)+
  geom_point(aes(x = In.ba.prob, y = log(In.ba.sort), shape = "Inlet TN"))+ 
  geom_point(aes(x = Out.ba.prob, y = log(Out.ba.sort), shape = "Outlet TN"))+
  geom_hline(aes(yintercept = log(0.99), color = "Good WQ"))+
  scale_shape_manual(values = c(15,16,0,1))+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 18))+
  labs(x = "Exceedance Probability", y = "Log(Concentration (mg/L))")

## TP
TP.ex <- tot.wq %>%
  select(starts_with("TP"))
colnames(TP.ex) <- c("storm.in", "storm.out")
TP.ex1 <- base.TP %>%
  select(starts_with("TP"))
colnames(TP.ex1) <- c("base.in", "base.out")
TP.ex <- bind_cols(TP.ex, TP.ex1)
# View(TP.ex) 
## sort columns
TP.ex <- TP.ex %>%
  mutate(In.st.sort = sort(storm.in, decreasing = TRUE, na.last = TRUE) / 1000,
         Out.st.sort = sort(storm.out, decreasing = TRUE, na.last = TRUE) / 1000,
         In.ba.sort = sort(base.in, decreasing = TRUE, na.last = TRUE) / 1000,
         Out.ba.sort = sort(base.out, decreasing = TRUE, na.last = TRUE) / 1000)
# View(TP.ex)
## Rank 
TP.ex <- TP.ex %>%
  mutate(In.st.rank = rank(desc(In.st.sort), na.last = TRUE),
         Out.st.rank = rank(desc(Out.st.sort), na.last = TRUE),
         In.ba.rank = rank(desc(In.ba.sort), na.last = TRUE),
         Out.ba.rank = rank(desc(Out.ba.sort), na.last = TRUE))
# View(TP.ex)
## Calculate probabiltiy
TP.ex <- TP.ex %>%
  mutate(In.st.prob = (In.st.rank) / (10 + 1),
         Out.st.prob = (Out.st.rank) / (10 + 1),
         In.ba.prob = (In.ba.rank) / (10 + 1),
         Out.ba.prob = (Out.ba.rank) / (10 + 1))
# View(TP.ex)
## Exceedance probability plots
ggplot(data = TP.ex)+
  geom_point(aes(x = In.ba.prob, y = log(In.ba.sort), shape = "Inlet TP"))+ 
  geom_point(aes(x = Out.ba.prob, y = log(Out.ba.sort), shape = "Outlet TP"))+
  geom_hline(aes(yintercept = log(0.11), color = "Good WQ"))+
  scale_shape_manual(values = c(15,16,0,1))+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 18))+
  labs(x = "Exceedance Probability", y = "Log(Concentration (mg/L))")

## TSS
TSS.ex <- tot.wq %>%
  select(starts_with("TSS"))
colnames(TSS.ex) <- c("storm.in", "storm.out")
TSS.ex1 <- base.TSS %>%
  select(starts_with("TSS"))
colnames(TSS.ex1) <- c("base.in", "base.out")
TSS.ex <- bind_cols(TSS.ex, TSS.ex1)
# View(TSS.ex) 
## sort columns
TSS.ex <- TSS.ex %>%
  mutate(In.st.sort = sort(storm.in, decreasing = TRUE, na.last = TRUE) ,
         Out.st.sort = sort(storm.out, decreasing = TRUE, na.last = TRUE) ,
         In.ba.sort = sort(base.in, decreasing = TRUE, na.last = TRUE) ,
         Out.ba.sort = sort(base.out, decreasing = TRUE, na.last = TRUE) )
# View(TSS.ex)
## Rank 
TSS.ex <- TSS.ex %>%
  mutate(In.st.rank = rank(desc(In.st.sort), na.last = TRUE),
         Out.st.rank = rank(desc(Out.st.sort), na.last = TRUE),
         In.ba.rank = rank(desc(In.ba.sort), na.last = TRUE),
         Out.ba.rank = rank(desc(Out.ba.sort), na.last = TRUE))
# View(TSS.ex)
## Calculate probabiltiy
TSS.ex <- TSS.ex %>%
  mutate(In.st.prob = (In.st.rank) / (10 + 1),
         Out.st.prob = (Out.st.rank) / (10 + 1),
         In.ba.prob = (In.ba.rank) / (10 + 1),
         Out.ba.prob = (Out.ba.rank) / (10 + 1))
# View(TSS.ex)
## Exceedance probability plots
ggplot(data = TSS.ex)+
  geom_point(aes(x = In.st.prob, y = log(In.st.sort), shape = "Inlet TSS"))+ 
  geom_point(aes(x = Out.st.prob, y = log(Out.st.sort), shape = "Outlet TSS"))+
  geom_hline(aes(yintercept = log(25.0), color = "TSS Target"))+
  scale_shape_manual(values = c(15,16,0,1))+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 18))+
  labs(x = "Exceedance Probability", y = "Log(Concentration (mg/L))")


## Efficiency Ratio v. accumulation regression
Accumulation <- c(0.71,0.5,1.13,1.01,0.84,0.7,0.77,1.74,0.93,1.65) * 25.4
tot.wq[,"Accumulation"] <- Accumulation

## WQ summary Statistics 
# median(tot.wq$Accumulation)
# range(tot.wq$Accumulation)

##lm TN
lmTN <- lm(log(TN.x/TN.y) ~ log(Accumulation), data = tot.wq)
summary(lmTN)
par(mfrow=c(2,2))
plot(lmTN)

## scatter plot of final model
ggplot(tot.wq, aes(x = log(Accumulation), y = log(TN.x/TN.y)))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(y = "Log(In/Out RC)", x = "Log(Accumulation (mm)) ")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))
 
##lm TP
lmTP <- lm(log(TP.x/TP.y) ~ log(Accumulation), data = tot.wq)
summary(lmTP)
par(mfrow=c(2,2))
plot(lmTP)

## scatter plot of final model
ggplot(tot.wq, aes(x = log(Accumulation), y = log(TP.x/TP.y)))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(y = "Log(In/Out RC)", x = "Log(Accumulation (mm)) ")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 18))

##lm TSS
lmTSS <- lm(log(TSS.x/TSS.y) ~ log(Accumulation), data = tot.wq)
summary(lmTSS)
par(mfrow=c(2,2))
plot(lmTSS)

## scatter plot of final model
ggplot(tot.wq, aes(x = log(Accumulation), y = log(TSS.x/TSS.y)))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(y = "Log(In/Out RC)", x = "Log(Accumulation (mm)) ")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        text =element_text(size = 18))

##lm PBP
lmPBP <- lm(log(PBP.x/PBP.y) ~ log(Accumulation), data = tot.wq)
summary(lmPBP)
par(mfrow=c(2,2))
plot(lmPBP)

## scatter plot of final model
ggplot(tot.wq, aes(x = log(Accumulation), y = log(PBP.x/PBP.y)))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(y = "Log(In/Out RC)", x = "Log(Accumulation (mm)) ")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 18))

##lm TKN
lmTKN <- lm(log(TKN.x/TKN.y) ~ log(Accumulation), data = tot.wq)
summary(lmTKN)
par(mfrow=c(2,2))
plot(lmTKN)

## scatter plot of final model
ggplot(tot.wq, aes(x = log(Accumulation), y = log(TKN.x/TKN.y)))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(y = "Log(In/Out RC)", x = "Log(Accumulation (mm)) ")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

##lm ON
lmON <- lm(log(ON.x/ON.y) ~ log(Accumulation), data = tot.wq)
summary(lmON)
par(mfrow=c(2,2))
plot(lmON)

## scatter plot of final model
ggplot(tot.wq, aes(x = log(Accumulation), y = log(ON.x/ON.y)))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(y = "Log(In/Out RC)", x = "Log(Accumulation (mm)) ")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

