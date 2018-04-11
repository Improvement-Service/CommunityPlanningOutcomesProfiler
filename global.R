library(shiny)
library(shinydashboard)
library(tidyverse)
library(plyr)
library(shinythemes)
library(RColorBrewer)
library(DT)
library(data.table)
library(Unicode)



CPPdta <- read_csv("CPPcleandata.csv")
IGZdta <- read_csv("IGZcleandata.csv")
DZdta <- read_csv("DZcleandata.csv")

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

#Square this difference
IGZ1617$SquaredDiff <- IGZ1617$Differences*IGZ1617$Differences

#Group by indicator and CPP and sum these squared differences 
IGZ1617 <- ddply(IGZ1617,. (CPP, Indicator), transform, SumSquares = (sum(SquaredDiff)))

#Group by CPP and count number of IGZ's within CPP
IGZ1617 <- ddply(IGZ1617,. (CPP,Indicator), transform, N = (length(InterZone)))

#Calculate sums of squares divided by N -1 
IGZ1617$Nminus1 <- IGZ1617$N - 1
IGZ1617$Variance <- IGZ1617$SumSquares/IGZ1617$Nminus1

#Calculate square root of variance
IGZ1617$StandardDeviation <- sqrt(IGZ1617$Variance)

#Calculate difference divided by standard deviation
IGZ1617$ZScore <- IGZ1617$Differences/IGZ1617$StandardDeviation

#If high is bad multiply Z score by minus 1 
IGZ1617 <- filter(IGZ1617, High.is.Positive. == "No") %>%
            mutate(CPPScore = ZScore * -1)
IGZ1617 <- select(IGZ1617, c(-CPPMean, -Differences, -SquaredDiff, -SumSquares, -N, -Nminus1,
                             -Variance, -StandardDeviation, -ZScore))

##Type Score
#Group by Indicator and Typology Group and calculate Type mean
IGZ1617 <- ddply(IGZ1617,. (Typology_Group, Indicator), transform, TypeMean = (mean(value)))

#Calculate score minus group mean
IGZ1617$Differences <- IGZ1617$value - IGZ1617$TypeMean

#Square this difference
IGZ1617$SquaredDiff <- IGZ1617$Differences*IGZ1617$Differences

#Group by indicator and Typology and sum these squared differences 
IGZ1617 <- ddply(IGZ1617,. (Typology_Group, Indicator), transform, SumSquares = (sum(SquaredDiff)))

#Group by Typology and count number of IGZ's within typology group
IGZ1617 <- ddply(IGZ1617,. (Typology_Group,Indicator), transform, N = (length(InterZone)))

#Calculate sums of squares divided by N -1 
IGZ1617$Nminus1 <- IGZ1617$N - 1
IGZ1617$Variance <- IGZ1617$SumSquares/IGZ1617$Nminus1

#Calculate square root of variance
IGZ1617$StandardDeviation <- sqrt(IGZ1617$Variance)

#Calculate difference divided by standard deviation
IGZ1617$ZScore <- IGZ1617$Differences/IGZ1617$StandardDeviation

#If high is bad multiply Z score by minus 1 
IGZ1617 <- filter(IGZ1617, High.is.Positive. == "No") %>%
  mutate(TypeScore = ZScore * -1)
IGZ1617 <- select(IGZ1617, c(-TypeMean, -Differences, -SquaredDiff, -SumSquares, -N, -Nminus1,
                             -Variance, -StandardDeviation, -ZScore))
