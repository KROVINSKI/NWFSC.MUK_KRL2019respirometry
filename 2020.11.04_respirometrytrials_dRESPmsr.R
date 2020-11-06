##Hello World

#*********************************
## Version Check
#********************************* 
R.version



## Author: OA Lab, NWFSC
## Title: Respirometry Trails for Summer Krill 2019
## Date: October-November 2020

# R script below will subselect and plot temperature data for MOATs
# Overall goal is to determine if MOATs (per treatment) are true replicates


#*********************************
##Libraries
#********************************* 
library(stringr)
library(tidyverse)
library(plyr)
library(nlme)
library(tidyr)
library(purrr)
library(wql)
library(lubridate)
library(arsenal)
#for graphing
library(ggplot2)
library(stringr)
library(nlme)
library(RColorBrewer)
#statistical analysis
library(gdata)
library(rsq)
library(doBy)
#Rnotebooks 
library(gridExtra)
library(kableExtra)


#*********************************
## Outline Current (2020.10.30)
#*********************************

## 1.) Working Directory
## 2.) Creating the Out-files
## 3.) Reformatting directory to correct format 
## 4.) Analysis 

## 5.) Placeholder 
## 6.) Placeholder
## 7.) Placeholder
## 8.) Placeholder
## 9.) Placeholder
## 10.) Placeholder


#*********************************
## 1.) Setting Working Directory
#*********************************

#set working directory to the correct folder
setwd("/Users/katherinerovinski/GIT/NWFSC.MUK_KRL2019respirometry")

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |



#*********************************
## 2.) Creating the Out-file
#*********************************

#create a temporary blank file for everything to go into
# out.file <- ""

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |


#*********************************
## 3.) Creating the Dataframe, dRESP
#*********************************

## Method 1 - 1 file for the four trials
#get all the files in the defined working directory that are in csv format
dRESP <- read.csv(file = "KRILL_Resp_alltrials.csv", stringsAsFactors = FALSE)
dim(dRESP)

write.csv(dRESP, file = "2020.11.04_presenseSENSORONLY.alltrials.data.csv", row.names = FALSE)



# dRESPanimal represents just the animal, vial, MOATs, etc.  
dRESPanimal <- read.csv(file = "RespirometryTrials_all.Animal.Info.csv") 
dim(dRESPanimal)

write.csv(dRESPanimal, file = "2020.11.04_krillanimaldata.csv", row.names = FALSE)




#dRESPmsr represents the measurements made by the presense optical device for Dissolved Oxygen 
dRESPmsr <- merge(dRESP, dRESPanimal, by="SensorName")

write.csv(dRESPmsr, file = "2020.11.04_respirometrymeasurements.csv", row.names = FALSE)




dRESP$MOATS <- factor(dRESP$MOATS)
# Checking the names of the different levels
levels(dRESP$MOATS)


#*********************************
## 4.) Creating dateTime objects  
#*********************************

# 4.0 establish the date time object of the CSV |
# dRESPmsr$Date <- as.POSIXct(dRESPmsr$Date, format=("%dd-%mm-%yy))
# # dRESPmsr$Date <- as.POSIXct(dRESPmsr$Date, format=("%d-%m-%y))
# # dRESPmsr$Date <- as.POSIXct(dRESPmsr$Date, format=("%d-%m-%y))

#ReferenceTime <- as.POSIXct("2019-09-20 23:59:00")
class(ReferenceTime)

dRESPmsr$dateTime <- ""
dRESPmsr$dateTime <- as.POSIXct(paste(dRESPmsr$Date,
                                      dRESPmsr$Time), "%d-%B-%y %H:%M:%S", tz="UTC")



# QA check
dim(dRESPmsr)


dRESPmsr$Time <- as.POSIXct(dRESPmsr$dateTime, format="%H:%M:%S")

## Why does time not get stripped out?
# dRESPmsr$Time <- strptime(dRESPmsr$dateTime, format="%d/%m/%Y %H:%M:%S")





#*********************************
## 5.) Cleaning up observations  
#********************************


#Removing 
dRESPmsr <- subset(dRESPmsr, SensorName != "")

dRESPmsr$SensorName <- str_trim(dRESPmsr$SensorName, side = c("both"))
dRESPmsr$SensorName <- factor(dRESPmsr$SensorName)
dim(dRESPmsr)


dRESPmsr <- subset(dRESPmsr, Value != "---")



#*********************************
## 5.a) Plot to demonstrate the SensorName 
#*********************************

ggplot(dRESPmsr, aes(x = Time, y = Value, colour = SensorName)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm") + 
  #scale_y_continuous(breaks=seq(250,950,by=50))   +
  theme_bw()


#*********************************
## 6.) Vial Volume  
#*********************************

vialVol <- 28.06 #ml
dtotal$oxygen <- vialVol * dtotal$Value  
#nmol/vial; conversion for L to ml and umol to nmol cancels
View(dtotal)



#*********************************
## 7.) Creating a Krill ID with Trial in name    
#*********************************


## First Trial began at 2019-10-28 14:25:01
## First Trial ended at 2019-10-28 16:37:29


## Second Trial began at 2019-10-28 18:55:21
## Second Trial ended at 2019-10-28 21:16:45


## Third Trial began at 2019-10-29 14:06:21
## Third Trial ended at 2019-10-29 16:18:36


## Fourth Trial began at 2019-10-29 17:20:21
## Fourth Trial ended at 2019-10-29 19:32:01



dRESPmsr$TrialID <- ""

dRESPmsr <- dRESPmsr %>% mutate(TrialID=case_when(
  
## First Trial began at 2019-10-28 14:25:01
## First Trial ended at 2019-10-28 16:37:29
  
  ((Time >= as.POSIXct("2019-10-28 14:25:00", tz = "UTC")) 
   & (Time < as.POSIXct("2019-10-28 16:37:30", tz = "UTC"))) ~ "Trial01",

  
## Second Trial began at 2019-10-28 18:55:21
## Second Trial ended at 2019-10-28 21:16:45  
  
  ((Time >= as.POSIXct("2019-10-28 18:55:20", tz = "UTC")) 
   & (Time <= as.POSIXct("2019-10-28 21:16:50", tz = "UTC"))) ~ "Trial02",


## Third Trial began at 2019-10-29 14:06:21
## Third Trial ended at 2019-10-29 16:18:36


  ((Time >= as.POSIXct("2019-10-29 14:06:18", tz = "UTC")) 
   & (Time <= as.POSIXct("2019-10-29 16:18:40", tz = "UTC"))) ~ "Trial03",

## Fourth Trial began at 2019-10-29 17:20:21
## Fourth Trial ended at 2019-10-29 19:32:01

  
  ((Time >= as.POSIXct("2019-10-29 17:20:18", tz = "UTC")) 
   & (Time <= as.POSIXct("2019-10-29 19:40:01", tz = "UTC"))) ~ "Trial04",

  TRUE ~"other"
)) 

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

## new value 
## dRESPmsr$KrillID

dRESPmsr$KrillID <- ""

dRESPmsr$KrillID <- paste(dRESPmsr$TrialID, "_", dRESPmsr$SensorName, sep="")
#View(dRESPmsr)

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |


#*********************************
## 8.) DO trial 3 correction applied to all rounds   
#*********************************

# New values to correct the DO values for the temperature in Trial 3 in the dataframe dRESPmsr

dRESPmsr$percentDOassumpt <- ""
dRESPmsr$assumedSatDOmg <- ""
dRESPmsr$percentDO <- ""
dRESPmsr$obseveredSatDOmg <- ""
dRESPmsr$actualDOmg <- ""

dRESPmsr$percentDOassumpt <- as.numeric(dRESPmsr$percentDOassumpt)
dRESPmsr$assumedSatDOmg <- as.numeric(dRESPmsr$assumedSatDOmg)
dRESPmsr$percentDO <- as.numeric(dRESPmsr$percentDO)
dRESPmsr$obseveredSatDOmg <- as.numeric(dRESPmsr$obseveredSatDOmg)
dRESPmsr$actualDOmg <- as.numeric(dRESPmsr$actualDOmg)

#Correct Temperature
dRESPmsr$CorTemp <- 11

#Salinity Constant
dRESPmsr$SalinityConstant <- 30.3


# Numeric Corrections to DO value
dRESPmsr$Value <- as.numeric(dRESPmsr$Value)



dRESPmsr$assumedSatDOmg <- oxySol(dRESPmsr$CorTemp, 
                                  dRESPmsr$SalinityConstant)


dRESPmsr$percentDOassumpt <- dRESPmsr$Value / dRESPmsr$assumedSatDOmg

dRESPmsr$obseveredSatDOmg <- oxySol(dRESPmsr$CorTemp, dRESPmsr$SalinityConstant)

dRESPmsr$percentDO <- dRESPmsr$Value / dRESPmsr$assumedSatDOmg

dRESPmsr$actualDOmg <- dRESPmsr$percentDO * dRESPmsr$obseveredSatDOmg


#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |


#*********************************
## 9.) Table (prior to slope) & Plot Check  
#*********************************


write.table(dRESPmsr, file = "2020.11.04.dRESPmeasurements", sep=";", 
            row.names = TRUE)


dRESPmsr <-subset(dRESPmsr, actualDOmg != "---")
dRESPmsr$Time <- as.POSIXct(dRESPmsr$Time, format="%H:%M:%S")
ggplot(dRESPmsr, aes(x = Time, y = actualDOmg, colour = SensorName)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm") + 
  facet_wrap(~TrialID) +
  ylim(12)
  #scale_y_continuous(breaks=seq(250,950,by=50))   +
  theme_bw()


  
#*********************************
## 10.) Analysis, Respirometry - Creating the Slope  
#*********************************  
  
  
vialVol <- 28.06 #ml
dRESPmsr$oxygen <- vialVol * dRESPmsr$actualDOmg   
#nmol/vial; conversion for L to ml and umol to nmol cancels


#slope funcion of 2 vectors
slope <- function(y,x){
  return(lm(y~x, na.action = na.omit)$coefficients[2])
}
  

dRESPmsr$delta_t <- as.numeric(dRESPmsr$delta_t)

cSlopes <- by(dRESPmsr, dRESPmsr$KrillID, function(x){ slope(x$oxygen, x$delta_t)})


#creating a data frame instead of a list 
ds <- as.data.frame(sapply(cSlopes, I))
#having row names be a variable in a column to be able to pull it out for later merging 
ds$TrialID<- row.names(ds)

  
  
#*********************************
## 11.) Creating the ds dataframe
#*********************************


#add column to dslopes thats KrillID
#View(dref)
ds$KrillID <- row.names(ds)
#View(dslopes)
dtotal <- merge(dRESPmsr, ds, by = "KrillID")
View(dtotal)


# #get slopes and r^2 values from dtotal - just adjusted code from data analysis to fit into these df's
dtotal$KrillID <- factor(dtotal$KrillID)
nlevels(dtotal$KrillID)

## why do we have a different number of observations
## using the comparedf function from the arsenal package

comparedf(ds, dtotal)
summary(comparedf(ds, dtotal))



dtotal[is.na(dtotal$delta_t),]
unique(dtotal$KrillID)







#how best to run the lmlist over 
# group and then run a function tidyverse 
# group by krill ID and then run the function 

info <- lmList(oxygen ~ delta_t|KrillID, dtotal,na.action=na.omit)


dtotalGRP <- dtotal %>% group_by(KrillID)

info <- lmList(oxygen ~ delta_t|KrillID, dtotal,na.action=na.omit)


summary(lm(oxygen ~ delta_t, data= dtotal[dtotal$KrillID=="Trial04_KRLr4_59",]))$r.squared

lm(oxygen ~ delta_t, data= dtotal[dtotal$KrillID=="Trial04_KRLr4_68",])

lm(oxygen ~ delta_t, data= dtotal[dtotal$KrillID=="Trial04_KRLr4_69",])




dtotal[is.na(dtotal$delta_t),]
unique(dtotal$KrillID)
            
names(info)

view(dtotal[!dtotal$KrillID %in% names(info), ])

   
print(info)

# slopes <- coef(info)[2]
# #print(slopes)
# dslopes <- data.matrix(slopes)
# #View(dref)
# mode(dslopes[,1])
# dslopes <- as.data.frame(dslopes)
# dslopes$slope <- dslopes$delta_t
# dslopes$delta_t <- NULL
# #View(dslopes)


#now for R^2!!
Rsq <- sapply(info,function(x) summary(x)$r.squared)
t(Rsq) #transposes rows and columns
Rsq <- data.matrix(Rsq)
#View(Rsq)
dtotal <- cbind(ds, Rsq)
View(dtotal)



#*********************************
## 12.) Correcting the Slope & Summary Statistics   
#*********************************


#blank corrected slope and blank-size corrected slope
#see blankslopecorrection.R for more comments and detail
x <- subset(dtotal2, dtotal2$MOATS == 0)
row.names(x) <- NULL
#View(x)
x$slope
blankmeanslope <- tapply(x$slope, x$TrialID, mean)
print(blankmeanslope)
is.numeric(blankmeanslope)
#now we have a mean of the blanks in each separate trial!
#View(blankmeanslope)
as.data.frame(blankmeanslope)
#add a row that is the trial ID
blankmeanslope <- cbind(blankmeanslope, levels(dtotal2$TrialID))
#View(blankmeanslope) #good so far!! 20 july
blankmeanslope[,1] <- as.numeric(blankmeanslope[,1]) #make sure it stays numeric



#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |



#rename column 2 as TrialID so it matches up with the column in dtotal2
colnames(blankmeanslope)[2] <- "TrialID"
#View(blankmeanslope)
dtotal3 <- merge(dtotal2, blankmeanslope, by="TrialID")
#View(dtotal3) #all still good 20 july
dtotal3$blankmeanslope <- as.numeric(as.character(dtotal3$blankmeanslope))

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |


dtotal3$slope <- as.numeric(dtotal3$slope)

#20 july: the blanks themselves are an issue because they're getting the average of themselves and the other blanks taken away from them. what do? 
dtotal3$blankcorrslope <- (dtotal3$slope - dtotal3$blankmeanslope)
#View(dtotal3)

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |



####blank and size corrected slope
dtotal3$blanksizecorrslope <- dtotal3$blankcorrslope/dtotal3$Size
#View(dtotal3)
mode(dtotal3$blanksizecorrslope)
#y <- subset(dtotal3, dtotal3$blanksizecorrslope != "lnf")
#View(y)#dealing with the blank vials' size being zero
#View(dtotal3)
View(dtotal3)
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
} 


#*********************************
## 5.) Analysis, Respirometry - Saturation Calculations  
#*********************************
#saturation calculations!!!

#add columns for temp in kelvin, add temps and salinity to dtotal version 5 #added 7/14: dont i want saturation in mastersheetr? why am i making dtotal5 so long when i could just keep mastersheetr the long one?


#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |



dRESPmsr$TempK <- dRESPmsr$Temp + 273.15
#temps <- subset(dRESPmsr, select = c(KrillID, Temp, TempK, Salinity))

#the following lines of code are interested in dtotal 5- do we need seperate dataframes for seperate trials
#dtotal5 <- merge(temps, dtotal4, by="KrillID")
#dtotal5$MeasurementID <- 1:nrow(dtotal5)
#View(MasterSheetR)

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |



#calculate 100% saturation (mg/L)
saturationfun <- function(Temp, Salinity, Pressure){
  #Temp <- 11
  TempK <- Temp +273.15
  #Pressure = 1
  a <- -139.34411
  b <- 1.575701e+5
  c <- 6.642308e+7
  d <- 1.243800e+10
  e <- 8.621949e+11
  DOo <- exp(a + (b/TempK) - (c/TempK^2) + (d/TempK^3) - (e/TempK^4))
  #Salinity <- 30.3
  f <- 0.017674
  g <- 10.754
  h <- 2140.7
  Fs <- exp(-Salinity * (f - g/TempK + h/TempK^2))
  #Pressure <- 2
  i <- 11.8571
  j <- 3840.7
  k <- 216961
  u <- exp(i - j/TempK - k/TempK^2)
  l <- 0.000975
  m <- 1.43e-5
  n <- 6.436e-8
  theta <- l - m*Temp + n*Temp^2
  Fp <- ((Pressure - u)*(1-theta*Pressure))/((1-u)*(1-theta))
  totalDO <- Fp * DOo * Fs
  return(totalDO)
}

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |



#run saturation calcs on MasterSheetR
#satDO
dRESPmsr$solubility <- saturationfun(dRESPmsr$CorTemp, dRESPmsr$SalinityConstant, MdRESPmsr$patm/1000) 
dRESPmsr$solubilityumol  <- solubility*31.2627 #that number is just the conversion factor from mg to umol for O2
#View(dRESPr)

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |



#now make percent saturation column
## Master sheet is replaced by dRESPmsr
## Value is now referenced as actualDOmg
# MasterSheetR$Value <- as.numeric(MasterSheetR$Value) #warning: NAs introduced by coercion
dRESPmsr$percentsat <- (dRESPmsr$actualDOmg / dRESPmsr$solubilityumol)*100 
MasterSheetR <- na.omit(MasterSheetR) #get rid of rows that have NA values(just my fault for messing up data collection)
#View(MasterSheetR)

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |













