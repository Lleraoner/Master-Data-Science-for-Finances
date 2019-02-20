library(shiny)
library(ggplot2)
library(MASS)
library(magrittr)
library(dplyr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  resultado <- reactiveValues()
  
  output$image <- renderUI({
  
    list(
      #tags$iframe(width = 560, height = 315, src = paste0("https://www.youtube.com/embed/", isolate(input$idvideo))),
      tags$img(src = 'http://i63.tinypic.com/2rynh9h.jpg', width = "150", height = "225")
      #tags$head(includeScript('script.js'))
    )
  })
  
  
  output$guesscorrelation <- renderUI({
    
      box(h3('Selecciona la correlacion correcta'),
          textInput("choose", "Your Guess", value = 'Your gues is...'),
          actionButton('generar', 'Lets see', icon = icon('500px'))
      )
  
  })
  
  
  
  output$paintmodel <- renderUI({
    
    box(h3('Pinta la correlacion correcta'),
        textInput("pinta", "Your Paint", value = 'You have to paint a ... corelation'),
        actionButton('pintar', 'Check', icon = icon('500px'))
        # y aqui habrá que hacer un prit de tu pintada es tal con un error de tal
    )
    
  })
  
  output$cambiomodelo <- renderUI({
    
    box(h3('Comprueba la correlacion correcta'),
        actionButton('check', 'Check', icon = icon('500px'))
        # y aqui habrá que hacer un prit de tu pintada es tal con un error de tal
    )
    
  })
  
  
  output$compruebamediana <- renderUI({
    
    box(h3('Comprueba la mediana'),
        textInput("xmedian", "X median", value = 'Your value for x median is ...'),
        textInput("ymedian", "Y median", value = 'Your value for y median is ...'),
        
        actionButton('checkmedian', 'Check medians', icon = icon('500px'))
        # y aqui habrá que hacer un prit de tu mediana comprobada es de tal y la respuesta correcta es tal...
    )
    
  })
  
  output$referencias <- renderUI({
    
    list(
      tags$img(src = 'http://i63.tinypic.com/2rynh9h.jpg', width = "150", height = "225")
    )
  })
}) 