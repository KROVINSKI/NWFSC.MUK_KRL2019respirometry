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
library(plyr)
library(nlme)
library(tidyr)
library(purrr)
library(wql)
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
setwd("/Users/katherinerovinski/GIT/NWFSC.MUK_KRL_SMR2019/02. Respirometry")

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |



#*********************************
## 2.) Creating the Out-file
#*********************************

#create a temperaute blank file for everything to go into
out.file <- ""

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |



#*********************************
## 3.) Creating the Dataframe, creating the loop " dRESP
#*********************************

## Method 1 - 1 file for the four trials
#get all the files in the defined working directory that are in csv format
# dRESP <- read.csv(file = "KRILL_Resp_alltrials.csv", stringsAsFactors = FALSE)

## Method 2 - collecting the 4 files 
file.names <- dir(getwd(), pattern =".csv")
#print(file.names)
MasterSheetR = NULL
# 
dRESP <- read.table(file.names[1], sep = ";", row.names = NULL, skip = 1, 
                fill = TRUE,
                header = TRUE,
                fileEncoding="latin1",
                stringsAsFactors = FALSE)

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

## Kate Comments
## Loop below cycles thorough the fold to find desired file types based on extension
## why do we only select 53 columns?

#Paul Comments
#make a for loop (see data analysis 10 jun for comments on this code)
for(i in 1:length(file.names)){
  dRESP <- read.table(file.names[i], 
                  sep = ";", 
                  row.names = NULL, 
                  skip = 1, 
                  fill = TRUE, 
                  header = TRUE, 
                  fileEncoding="latin1", 
                  stringsAsFactors = FALSE)
  
  cnames <- colnames(dRESP)
  cnames <- cnames[2:length(cnames)]
  colnames(dRESP)<- cnames
  dRESP[,53] = NULL
  
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

  
## Kate Comment: line items, sensor names with a null/blank entry are removed
dRESP <- subset(dRESP, SensorName != "")

dRESP$SensorName <- str_trim(dRESP$SensorName, side = c("both"))
dRESP$SensorName <- factor(dRESP$SensorName)
dim(dRESP)
  
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
  
  
  
#  levels(d$SensorName) <- c(1:10)
  dRESP$TrialID <- substr(file.names[i], 0, nchar(file.names[1]) -4) 
#for some reason only takes first 10 characters instead of chopping off last 4. caused 7JUL - 07JUL problem
#d$Temp == 12 & d$Value >225 #what do i do with these lines
#d$Temp == 16 & d$Value > 206
  
  ## Why do we need the MasterSheetR?
  
  MasterSheetR <- rbind(MasterSheetR, dRESP)
}

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

## Do we need the master file - just to compress all the different trials 

write.table(MasterSheetR, file = "file", sep=";", 
            row.names = TRUE)
#MasterSheetR <- as.data.frame(MasterSheetR)
#View(MasterSheetR)
#mode(MasterSheetR$TrialID)

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |



levels(as.factor(MasterSheetR$TrialID))
#mode(MasterSheetR) 
levels(factor(dRESP$SensorName))
dim(dRESP)


#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |


#value = the mg/L dissolved oxygen
#Make value numeric, get rid of dashes, fix time and make a basic plot, just to get an overall view of data


dRESP$Value<-as.numeric(dRESP$Value)
dRESP <-subset(dRESP, Value != "---")
dRESP$Time <- as.POSIXct(dRESP$Time, format="%H:%M:%S")
ggplot(dRESP, aes(x = Time, y = Value, colour = SensorName)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm") + 
  #scale_y_continuous(breaks=seq(250,950,by=50))   +
  theme_bw()

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |


# updates to the mg/L
# saturated mg/L DO at observed temperature and assumed salinity
# the oxySol() function is form the wql package

# !!!!!!!!!!!!!!!!!!!!!!



#*********************************
## 4.) Analysis, Respirometry - Creating the Slope  
#*********************************


#call up treatment reference doc, make columns into factors so that you can merge them. 
dref <- read.csv("/Users/katherinerovinski/GIT/NWFSC.MUK_KRL_SMR2019/02. Respirometry/TreatSize/KRL_SMR_19_TreatSize_v2.csv")
View(dref)
dref$TrialID <- dref$FileName
dref$SensorName <- as.factor(dref$SensorName)
dref$MOATS <- as.factor(dref$MOATS)
dref$KrillID <- paste(dref$TrialID, "_", dref$SensorName, sep="")
dref$KrillID
#View(dref)

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |



#make columns in both mastersheetr and dref that are KrillID
MasterSheetR$KrillID
MasterSheetR$KrillID <- paste(MasterSheetR$TrialID, MasterSheetR$SensorName, sep="_")
View(MasterSheetR)
dref$KrillID <- paste(dref$TrialID, dref$SensorName, sep="_")
dref$KrillID <- str_trim(dref$KrillID, side = c("both"))
dref$KrillID
#View(dref)

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |



#merge dataframe dref and dataframe MasterSheetR into one big dataframe called dtotal
dtotal <- merge(MasterSheetR, dref, by="KrillID") 

dtotal$Value <- as.numeric(dtotal$Value)
#convert from O2 concentration to O2 quantity per vial 
#dtotal$Value is the concentration in the vial in umol/L

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |



#need to measure vials
vialVol <- 28.06 #ml
dtotal$oxygen <- vialVol * dtotal$Value   #nmol/vial; conversion for L to ml and umol to nmol cancels
View(dtotal)

uq

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |



# #get slopes and r^2 values from dtotal - just adjusted code from data analysis to fit into these df's
info <- lmList(oxygen ~ delta_t|KrillID, dtotal, na.action=na.omit)
print(info)
slopes <- coef(info)[2]
#print(slopes)
dslopes <- data.matrix(slopes)
#View(dref)
mode(dslopes[,1])
dslopes <- as.data.frame(dslopes)
dslopes$slope <- dslopes$delta_t
dslopes$delta_t <- NULL
#View(dslopes)


#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |



dtotal$Krill_Trial <- paste(dtotal$KrillID, dtotal$TrialID.x, sep = "_")

#slope funcion of 2 vectors
slope <- function(y,x){
  return(lm(y~x, na.action = na.omit)$coefficients[2])
}


#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |



#using newly created slope function to give it what columns to use 
cSlopes <- by(dtotal, dtotal$Krill_Trial, function(x){ slope(x$oxygen, x$delta_t)})
#creating a data frame instead of a list 
ds <- as.data.frame(sapply(cSlopes, I))
#having row names be a variable in a column to be able to pull it out for later merging 
ds$krilltrial<- row.names(ds)

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |



#add column to dslopes thats KrillID
#View(dref)
ds$KrillID <- row.names(ds)
#View(dslopes)
dtotal2 <- merge(dref, ds, by = "KrillID")
View(dtotal2)

#now for R^2!!
Rsq <- sapply(info,function(x) summary(x)$r.squared)
t(Rsq) #transposes rows and columns
Rsq <- data.matrix(Rsq)
#View(Rsq)
dtotal2 <- cbind(dtotal2, Rsq)
View(dtotal2)



#*********************************
## 5.) Analysis, Respirometry - Correcting the Slope  
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



MasterSheetR$TempK <- MasterSheetR$Temp + 273.15
#temps <- subset(MasterSheetR, select = c(KrillID, Temp, TempK, Salinity))
#dtotal5 <- merge(temps, dtotal4, by="KrillID")
#dtotal5$MeasurementID <- 1:nrow(dtotal5)
#View(MasterSheetR)

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |



#calculate 100% saturation (mg/L)
saturationfun <- function(Temp, Salinity, Pressure){
  #Temp <- 12
  TempK <- Temp +273.15
  #Pressure = 1
  a <- -139.34411
  b <- 1.575701e+5
  c <- 6.642308e+7
  d <- 1.243800e+10
  e <- 8.621949e+11
  DOo <- exp(a + (b/TempK) - (c/TempK^2) + (d/TempK^3) - (e/TempK^4))
  #Salinity <- 28.9
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
MasterSheetR$solubility <- saturationfun(MasterSheetR$Temp, MasterSheetR$Salinity, MasterSheetR$patm/1000) 
MasterSheetR$solubilityumol  <- solubility*31.2627 #that number is just the conversion factor from mg to umol for O2
#View(MasterSheetR)

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |



#now make percent saturation column
MasterSheetR$Value <- as.numeric(MasterSheetR$Value) #warning: NAs introduced by coercion
MasterSheetR$percentsat <- (MasterSheetR$Value / MasterSheetR$solubilityumol)*100 
MasterSheetR <- na.omit(MasterSheetR) #get rid of rows that have NA values(just my fault for messing up data collection)
#View(MasterSheetR)

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |


#*********************************
## 6.) Analysis, Respirometry - Distribution Plot  
#*********************************

#plot for distribution of blanksizecorrslope as box an whisker
mode(dtotal3$Temp)
#View(dtotal3)
babymaster <- data.frame(MasterSheetR$KrillID, MasterSheetR$Temp)
babymaster <- babymaster[!duplicated(babymaster[,c("MasterSheetR.KrillID", "MasterSheetR.Temp")]),]
babymaster$KrillID <- babymaster$MasterSheetR.KrillID
#View(babymaster)
dtotal3 <- merge(x=dtotal3, y=babymaster, by = "KrillID")
dtotal3$Temp <- dtotal3$MasterSheetR.Temp
#mode(dtotal3$KrillID)
#str(dtotal3)
dtotal4 <- subset(dtotal3, Treatment != "Blank")
dtotal4$Temperature <- as.factor(dtotal4$MasterSheetR.Temp)

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |



#fix trials where i had the presens set to wrong temp
dtotal4$Temperature[dtotal4$TrialID == "16JUN16_03"] <- 16
dtotal4$Temperature[dtotal4$TrialID == "13JUL16_02"] <- 16
dtotal4$Temperature[dtotal4$TrialID == "19JUN16_03"] <- 12
View(dtotal4)

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |



###make any changes to data = subset in any ways that you need to (take out trials that you messed up etc)
dtotal4 <- subset(dtotal4, Size > 2.5)
#dtotal4 <- subset(dtotal4, slope < 0) #can i do this?
dtotal4 <- subset(dtotal4, TrialID != c("19JUN16_01"))
dtotal4 <- subset(dtotal4, TrialID != c("22JUN16_03"))
#dtotal4 <- subset(dtotal4, TrialID != c("13JUL16_06"))
#dtotal4 <- subset(dtotal4, TrialID != c("23JUL16_01"))
dtotal4 <- subset(dtotal4, MasterSheetR.KrillID != "13JUL16_06_9") #took off wrong crab??
dtotal4 <- subset(dtotal4, MasterSheetR.KrillID != "07JUL16_04_1") #died in resp
View(dtotal4)
#fix respiration rates so they're how much oxygen USED instead of how much LEFT
dtotal4$blanksizecorrslope <- dtotal4$blanksizecorrslope * (-1)
summary(dtotal4$blanksizecorrslope)
dtotal4$blankcorrslope <- dtotal4$blankcorrslope * (-1)

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |



summaryForErrorBarsBSCS <- summarySE(subset(dtotal4, blanksizecorrslope > 0), measurevar="blanksizecorrslope", 
                                 groupvars=c("Treatment", "Temperature"), na.rm = TRUE)
View(summaryForErrorBarsBSCS)

summaryForErrorBarsBCS <- summarySE(subset(dtotal4, blankcorrslope > 0), measurevar="blankcorrslope", 
                                     groupvars=c( "Temperature"), na.rm = TRUE)
View(summaryForErrorBarsBCS)

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |



#mean O2 consumption per crab at 12C
# nmol/min/megalopae
(oxyPerCrab12C <- mean(subset(dtotal4, Temperature == 12 & blankcorrslope > 0)$blankcorrslope))

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |



#PLOT YAYYY
# blank corrected; remove resp rate < 0
ggplot(subset(dtotal4, blanksizecorrslope > 0), aes(Treatment, blanksizecorrslope)) + 
 # geom_boxplot(aes(colour = Temperature), lwd=1, position = position_dodge(.8)) +  #to split by treatment AND temp
  geom_jitter(aes(colour = Temperature), position = position_jitterdodge(), alpha = 0.5) +
  geom_errorbar(data = summaryForErrorBarsBSCS, aes(x = Treatment, colour = Temperature,
                                                ymin = blanksizecorrslope - se, ymax = blanksizecorrslope + se),
                position = position_dodge(0.8)) +
  geom_point(data = summaryForErrorBarsBSCS, aes(x = Treatment, y = blanksizecorrslope, 
                                             colour = Temperature), position = position_dodge(0.8), size = 5) +
  labs( x = "Treatment", y = "Oxygen Consumption Rate\n(nmol/minute/mm of carapace length)") +
  theme_bw(base_size = 16) +
  guides(colour = guide_legend(override.aes = list(size=1))) +
  #ylim(c(0,1.1)) + 
  scale_x_discrete(labels = c("High pH\nHigh DO", "High pH\nLow DO", "Low pH\nHigh DO", "Low pH\nLow DO")) + 
  scale_color_manual(values = c("blue", "red"), name= "Temperature",
                     breaks = c("12", "16"),
                     labels = c(expression("12"*~degree*C), expression("16"*~degree*C)))


str(sizeslopesub)
#size versus blank corrected respiration rate plot, separated by temperature
sizeslopesub <- subset(dtotal4, Size != 0)
#sizeslopesub <- subset(sizeslopesub, Temp == 12)#change to 16 to look at the graph for 16degreesC trials
ggplot(subset(dtotal4, blanksizecorrslope > 0) , aes(x=Size, y=blanksizecorrslope)) +
  geom_point(aes(colour = factor(Temp))) +
  geom_smooth(data = subset(dtotal4, Temp == 12 & blanksizecorrslope > 0), method = lm, se = FALSE, aes(Size, blanksizecorrslope), colour = "blue") +
  geom_smooth(data = subset(dtotal4, Temp == 16 & blanksizecorrslope > 0),method = lm, se = FALSE, aes(Size, blanksizecorrslope), colour = "red") +
  labs(x = "Carapace length\n(mm)", y= "Oxygen Consumption Rate\n(nmol/minute/mm of carapace length)") +
  theme_bw(base_size = 16) +
  scale_color_manual(values = c("blue", "red"))
  

summary(lmList(Size ~ blankcorrslope|Temp, sizeslopesub))$r.squared
summary(lmList(Size ~ blankcorrslope|Temp, sizeslopesub))$adj.r.squared

##################################
#STATISTICAL ANALYSIS

#make a table comparing medians of each treatment by temp
x <- as.matrix(summaryBy(blanksizecorrslope ~ Treatment + Temp, data = dtotal4, FUN = median))
x <- as.list(x[,3])
x <- matrix(x, nrow = 2, ncol = 4)
colnames(x) <- c("HH", "HL", "LH", "LL")
row.names(x) <- c("12C", "16C")
View(x)

#ANOVA
tapply(dtotal4$blanksizecorrslope, dtotal4$Temp, median)
tapply(dtotal4$blanksizecorrslope, dtotal4$Treatment, median)
int <- aov(data = dtotal4, blanksizecorrslope ~ Temp*Treatment)
summary(int) #Pr(>F) column, temp:treatment row = p-value. if it's big we don't have to worry about interaction?

addint <- aov(data = dtotal4, blanksizecorrslope ~ Temp + Treatment)
summary(addint) #p-value for temp is really small??? (1.91e-11)
#omnibus test (F test) to look at main effects and interactions


ano <- anova(lm(blanksizecorrslope ~ Treatment * Temp, subset(dtotal4, blanksizecorrslope > 0)))
print (ano)
summary(lm(blanksizecorrslope ~ Treatment * Temp, dtotal4))

pairwise.t.test(dtotal4$blanksizecorrslope, dtotal4$Treatment, p.adj = "none")
pairwise.t.test(dtotal4$blanksizecorrslope, dtotal4$Temp, p.adj = "none")

#Tukey post hoc test
TukeyHSD(aov(data = dtotal4, blanksizecorrslope ~ Treatment), conf.level = 0.95)
#TukeyHSD(aov(data = dtotal4, blanksizecorrslope ~ Temp), conf.level = 0.95) #doesnt work


#what am I doing here?


