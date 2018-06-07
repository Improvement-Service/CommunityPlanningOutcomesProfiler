library(tidyverse)
library(plyr)
library(shinythemes)
library(RColorBrewer)
library(DT)
library(data.table)
library(Unicode)
library(leaflet)
library(cowplot)

SpPolysDF<-read_rds("Files for Maps/Shapes.rds")
SpPolysIZ <- read_rds("Files for Maps/IZshapes.rds")
CPPdta <- read_csv("CPPcleandata.csv")
IGZdta <- read_csv("IGZcleandata.csv")
DZdta <- read_csv("DZcleandata.csv")

#Calculate percentiles for colours
povDecs <- c()
for(i in unique(SpPolysDF@data$council)){
  x <- ntile(SpPolysDF@data[SpPolysDF@data$council == i, 13], 7)
  povDecs <-c(povDecs,x)
}

tariffDecs <- c()
for(i in unique(SpPolysDF@data$council)){
  x <- ntile(SpPolysDF@data[SpPolysDF@data$council == i, 14], 7)
  tariffDecs <-c(tariffDecs,x)
}

posDecs <- c()
for(i in unique(SpPolysDF@data$council)){
  x <- ntile(SpPolysDF@data[SpPolysDF@data$council == i, 15], 7)
  posDecs <-c(posDecs,x)
}
t <- SpPolysDF@data$`Percentage of school leavers entering positive destinations`==100
posDecs[t] <- 7

benDecs <- c()
for(i in unique(SpPolysDF@data$council)){
  x <- ntile(SpPolysDF@data[SpPolysDF@data$council == i, 16], 7)
  benDecs <-c(benDecs,x)
}

crimeDecs <- c()
for(i in unique(SpPolysDF@data$council)){
  x <- ntile(SpPolysDF@data[SpPolysDF@data$council == i, 17], 7)
  crimeDecs <-c(crimeDecs,x)
}

admisDecs <- c()
for(i in unique(SpPolysDF@data$council)){
  x <- ntile(SpPolysDF@data[SpPolysDF@data$council == i, 18], 7)
  admisDecs <-c(admisDecs,x)
}

SpPolysDF@data <- cbind(SpPolysDF@data, povDecs, tariffDecs, posDecs,benDecs,crimeDecs, admisDecs)
rm(i, x, povDecs, tariffDecs, posDecs,benDecs,crimeDecs, admisDecs)

#Add column to IGZ data to specify whether a high value is positive
IGZdta <- IGZdta %>% mutate(`High is Positive?` = "Yes")
IGZdta$`High is Positive?`[IGZdta$Indicator %in% c("Child Poverty","Out of Work Benefits", "Crime Rate",
                                                   "Emergency Admissions", "Early Mortality", "Depopulation")] <- "No"

####Create CPP Scores and Type Scores for most recent years data
#Filter data to most recent years data
IGZ1617 <- filter(IGZdta, Year == "2016/17")


##CPP Score
#Group by Indicator and CPP and calculate CPP mean
IGZ1617 <- ddply(IGZ1617,. (CPP, Indicator), transform, CPPMean = (mean(value)))

#Calculate score minus group mean
IGZ1617$Differences <- IGZ1617$value - IGZ1617$CPPMean

#Group by CPP and Indicator and calculate Standard Deviation
IGZ1617 <- ddply(IGZ1617,. (CPP, Indicator), transform, StandardDeviation = (sd(value)))

#Calculate difference divided by standard deviation
IGZ1617$ZScore <- IGZ1617$Differences/IGZ1617$StandardDeviation

#If high is bad multiply Z score by minus 1
IGZ1617$CPPScore <- IGZ1617$ZScore
IGZ1617$CPPScore[IGZ1617$High.is.Positive. =="No"] <- (IGZ1617$CPPScore[IGZ1617$High.is.Positive. =="No"])*-1
IGZ1617 <- select(IGZ1617, c(-CPPMean, -Differences, -StandardDeviation, -ZScore))

##Type Score
#Group by Indicator and Typology Group and calculate Type mean
IGZ1617 <- ddply(IGZ1617,. (Typology_Group, Indicator), transform, TypeMean = (mean(value)))

#Calculate score minus group mean
IGZ1617$Differences <- IGZ1617$value - IGZ1617$TypeMean

#Group by Typology Group and Indicator and calculate Standard Deviation
IGZ1617 <- ddply(IGZ1617,. (Typology_Group, Indicator), transform, StandardDeviation = (sd(value)))

#Calculate difference divided by standard deviation
IGZ1617$ZScore <- IGZ1617$Differences/IGZ1617$StandardDeviation

#If high is bad multiply Z score by minus 1
IGZ1617$TypeScore <- IGZ1617$ZScore
IGZ1617$TypeScore[IGZ1617$High.is.Positive. =="No"] <- (IGZ1617$TypeScore[IGZ1617$High.is.Positive. =="No"])*-1
IGZ1617 <- select(IGZ1617, c(-TypeMean, -Differences, -StandardDeviation, -ZScore))

####Create CPP Change Scores and Typology Scores for the change from start to finish year

#filter data to start and finish year
IGZChange <- filter(IGZdta, Year %in% c("2006/07","2016/17"))

#Group data by IGZ and Indicator and calculate change from start year to finish year
IGZChange <- ddply(IGZChange,. (InterZone, Indicator), transform, Change = (last(value)/first(value)-1))

#If High is bad multiply change value by minus 1
IGZChange$Change[IGZChange$High.is.Positive. == "No"] <- (IGZChange$Change[IGZChange$High.is.Positive. == "No"])*-1

##Calculate overall Z Score for change value

#Filter data so that change value is only included once per IGZ
IGZChange <- filter(IGZChange, Year == "2016/17")

#Group by indicator and calculate mean of change value
IGZChange <- ddply(IGZChange,. (Indicator), transform, OverallMean = (mean(Change)))

#Calculate change score minus overall mean
IGZChange$Differences <- IGZChange$Change - IGZChange$OverallMean

#Group by indicator and calculate Standard Deviation
IGZChange <- ddply(IGZChange,. (Indicator), transform, StandardDeviation = (sd(Change)))

#Calculate difference divided by Standard Deviation
IGZChange$OverallZScore <- IGZChange$Differences/IGZChange$StandardDeviation
IGZChange <- select(IGZChange, c(-OverallMean, -Differences, -StandardDeviation))

##Calculate CPP Change Score

#Group by Indicator and CPP and calculate mean of overall Z score
IGZChange <- ddply(IGZChange,. (CPP, Indicator), transform, CPPMean = (mean(OverallZScore)))

#Calculate overallZscore minus CPPmean
IGZChange$Differences <- IGZChange$OverallZScore - IGZChange$CPPMean

#Group by Indicator and CPP and calculate Standard Deviation
IGZChange <- ddply(IGZChange,. (CPP, Indicator), transform, StandardDeviation = (sd(OverallZScore)))

#Calculate differences divided by Standard Deviation
IGZChange$CPPChangeScore <- IGZChange$Differences/IGZChange$StandardDeviation
IGZChange <- select(IGZChange, c(-CPPMean, -Differences, -StandardDeviation))

##Calculate Typology Change Score

#Group by Indicator and Typology group and calculate mean of overall Z score
IGZChange <- ddply(IGZChange,. (Typology_Group, Indicator), transform, TypeMean = (mean(OverallZScore)))

#Calculate overallZscore minus Typemean
IGZChange$Differences <- IGZChange$OverallZScore - IGZChange$TypeMean

#Group by Indicator and Typology Group and calculate Standard Deviation
IGZChange <- ddply(IGZChange,. (Typology_Group, Indicator), transform, StandardDeviation = (sd(OverallZScore)))

#Calculate differences divided by Standard Deviation
IGZChange$TypeChangeScore <- IGZChange$Differences/IGZChange$StandardDeviation
IGZChange <- select(IGZChange, c(-TypeMean, -Differences, -StandardDeviation))

##For IGZs add Z score column to SpDF
decs <- c()
decs <- ddply(IGZ1617, .(InterZone, CPP), summarise, combCPP = sum(CPPScore)) %>%
  ddply(., .(CPP), mutate, CPPDec = ntile(combCPP, n = 7)) %>% 
  ddply(.,.(CPP), mutate, CPPRank = frank(combCPP)) %>% select(InterZone, CPPDec,CPPRank)
SpPolysIZ@data <- left_join(SpPolysIZ@data, decs, by = "InterZone") %>% select(-rank_decs, -`rank-min`)
names(SpPolysIZ@data)[c(13,14)] <- c("rank_decs", "rank-min")

saveRDS(SpPolysDF, "Files for Maps/Shapes_decs.rds")
saveRDS(SpPolysIZ, "Files for Maps/IZshapes_decs.rds")
write_csv(IGZChange,"IGZChangeTypology.csv")
write_csv(IGZ1617,"IGZ1617Typology.csv")


##########
#Compute improvement rates and store data for plots on page 1

#add new column to data so that line type can be specified
CPPdtaCurrent <- CPPdta %>% mutate(Grouping=  paste(CPP, Type))  

#add new column to show rate of improvement 
CPPdtaCurrent <- filter(CPPdtaCurrent, Type != "Projected")
#CPPdtaCurrent <- ddply(CPPdtaCurrent, .(CPP, Indicator), transform, Improvement_Rate = ((last(value)/first(value)-1)*100))
CPPdtaCurrent <- setDT(CPPdtaCurrent)[, Improvement_Rate :=(last(value)/first(value)-1)*100,by = list(CPP, Indicator)]
#add new column to show whether a high value represents a positive outcome
CPPdtaCurrent <- CPPdtaCurrent %>% mutate(`High is Positive?` = "Yes")
CPPdtaCurrent$`High is Positive?`[CPPdtaCurrent$Indicator %in% c("Dwelling Fires", "Unplanned Hospital Attendances",
                                                                 "Fuel Poverty", "Fragility", "Carbon Emissions",
                                                                 "Child Poverty", "Out Of Work Benefits",
                                                                 "Crime Rate", "Emergency Admissions",
                                                                 "Early Mortality")] <- "No"

write_csv(CPPdtaCurrent, "ImpRateCPP.csv")
