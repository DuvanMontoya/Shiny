# main.R

library(shiny)
library(dplyr)
library(ggplot2)

rm(list=ls())
APP_FOLDER <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(APP_FOLDER)
list.files()

source("global.R")

server <- tryCatch({
  source("server.R")$value
}, error=function(e){
  message("Failed to load 'server.R'. Using a dummy server.")
  function(input, output, session){
    output$dummy<-renderText("Dummy server running. Please debug 'server.R'.")
  }
})

ui <- tryCatch({
  source("ui.R")$value
}, error=function(e){
  stop("Failed to load 'ui.R'. Please debug 'ui.R' before running the app.")
})

shinyApp(ui=ui, server=server)