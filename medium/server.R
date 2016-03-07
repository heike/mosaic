library(shiny)
load("happy2.RData")
library(plyr)
library(wesanderson)
happy[happy=="NA"] <- NA
happy<-subset(happy, select=c(happy, sex, marital, degree, finrela, health))


shinyServer(function(input, output, session) {

  names<-c("happy", "sex", "marital", "degree", "finrela", "health") #want to use colnames ?
  d<- data.frame(value=names, label=names)
  updateSelectizeInput(session, 'group', choices = d, server=TRUE)
  output$values<- renderText({
    input$group
  })
    
  output$funct <- renderText({
    paste("f(", input$group,')')
  })
  output$mosaicplot <- renderPlot({
    data=happy
  
     # which divider is selected by user
    # default divider is mosaic
   v1<-get(input$group[1])
    
    #print(v1)
    prodplot(happy, ~ v1, mosaic(), na.rm=T)+ aes(fill=happy) +scale_fill_manual(values=wes_palette(n=3, name="Moonrise2"))
    
        
      })
   
    
    
}) 
