library(shiny)
library(shinythemes)
library(rsconnect)
library(plotly)
library(ggplot2)
library("sf")
library(dplyr)
library(viridis)

source("ui.R",local = TRUE)
source("server.R",local = TRUE)

shinyApp(
  ui = ui, 
  server = server,
  session
)

deployApp()