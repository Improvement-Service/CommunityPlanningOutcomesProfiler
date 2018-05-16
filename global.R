library(shiny)
library(shinydashboard)
library(tidyverse)
library(plyr)
library(shinythemes)
library(RColorBrewer)
library(DT)
library(data.table)
library(Unicode)
library(leaflet)


SpPolysDF<-read_rds("Files for Maps/Shapes.rds")
SpPolysIZ <- read_rds("Files for Maps/IZshapes.rds")
CPPdta <- read_csv("CPPcleandata.csv")
IGZdta <- read_csv("IGZcleandata.csv")
DZdta <- read_csv("DZcleandata.csv")

##create deciles for colours
CPPMapDta <- SpPolysDF@data
##convert to numeric
CPPMapDta[[15]] <- as.numeric(CPPMapDta[[15]])
CPPMapDta[[14]] <- as.numeric(CPPMapDta[[14]])
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