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

