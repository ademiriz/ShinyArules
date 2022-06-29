library(shiny)
library (arules)
library (arulesSequences)
library (arulesViz)
library(shinydashboard)
library(visNetwork)
library(grid)
library(Matrix)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(RODBC)
library(dplyr)
library( geosphere )
library(shinyjs)
library(arulesViz)
library(stringr)
library(grid)
library(plotly)
library(visNetwork)


library(readxl)
tranTableDF <- read_excel("SampleTranx.xlsx")
#View(tranTableDF)

 
 tranTableDF<- split(tranTableDF$Item, tranTableDF$TransactionNo)
 ts <- as(tranTableDF, "transactions")
 
 ## calling Shiny App to visualize association rules
 source("arulesInterfaceTRranData.R") 
 arulesApp(ts)
