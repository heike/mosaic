library(shiny)
load("happy2.RData")
library(plyr)
library(wesanderson)
happy[happy=="NA"] <- NA


# Define UI for application that creates a mosaic plot
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Dynamic Mosaic Plot"),
  
  sidebarLayout(
    
    # sidebar with options for controlling parameters
    sidebarPanel(
      selectizeInput('group', "Select variables to compare:", NULL, multiple = TRUE, options = list(maxItems = 3)),
      uiOutput("values"),
      selectizeInput(
        'ds', 'Direction', choices = c("horizontal"=1, "vertical"=2, "horizontal"=3, "vertical"=4, "horizontal"=5, "vertical"=6),
        multiple = TRUE, options = list(maxItems = 3)
      )
   ),
   
   # plotting 
    mainPanel(
      textOutput("funct"),
      plotOutput("mosaicplot")
   )
  )
)) 