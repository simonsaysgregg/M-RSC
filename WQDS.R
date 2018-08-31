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
  subset(event == "storm")
#View(storm.wq)
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
  mutate(TN = TKN + NOx) 
# View(in.wq)

## In.wq summary
in.wq.sum <- in.wq[-c(1),] %>%
  group_by(as.character(site)) %>%
  summarise_at(vars(-samp.date, -site, -event), funs(mean, median, max, min, var, sd))
# View(in.wq.sum)

## In.wq storm stats analysis
# subset at sampling sites
storm.in1 <- in.wq[-c(1),] %>%
  subset(site == "IN1")
# View(storm.in1)
storm.in2 <- in.wq[-c(1),] %>%
  subset(site == "IN2")
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

