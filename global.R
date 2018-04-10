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

#Group by Indicator and CPP and calculate CPP mean
IGZ1617 <- ddply(IGZ1617,. (CPP, Indicator), transform, CPPMean = (mean(value)))

#Calculate score minus group mean
IGZ1617$Differences <- IGZ1617$value - IGZ1617$CPPMean

#Square this difference
IGZ1617$SquaredDiff <- IGZ1617$Differences*IGZ1617$Differences

#Group by indicator and CPP and sum these squared differences 
IGZ1617 <- ddply(IGZ1617,. (CPP, Indicator), transform, SumSquares = (sum(Differences)))

#Group by CPP and count number of IGZ's within CPP
IGZ1617 <- ddply(IGZ1617,. (CPP,Indicator), transform, N = (length(InterZone)))

