library(shiny)
library(ggplot2)
library(gginnards)
library(tibble)
library(magrittr)
library(stringr)

shinyServer(function(input, output){
  
  variables <- reactiveValues(tamanio = 0, 
                              distribucion = 0, 
                              orden = 0,
                              x = 0,
                              y = 0,
                              plotInicial = 0,
                              x3 = 0,
                              y3 = 0,
                              z = 0)
  
  react <- eventReactive(input$generar, {
    variables$tamanio <- isolate(input$tamanio)
    variables$distribucion <- isolate(input$correlacion)
    variables$orden <- isolate(input$orden)
    
    meanA <- -0.5
    meanB <- 0.5
    sdA <- 1
    sdB <- 2
    
    # Generate from independent standard normals
    variables$x <- rnorm(variables$tamanio, 0, 1)
    variables$y <- rnorm(variables$tamanio, 0, 1)
    
    # Transform
    x2 <- variables$x # could be avoided
    y2 <- variables$distribucion*variables$x + sqrt(1 - variables$distribucion^2)*variables$y
    
    # Fix up means and standard deviations
    variables$x3 <- meanA + sdA*x2
    variables$y3 <- meanB + sdB*y2

    
    variables$z <- as.data.frame(cbind(variables$x3, variables$y3))
    
    variables$plotInicial <- ggplot(variables$z) + geom_point(aes(x = variables$x3, y = variables$y3)) + xlab('X') + ylab('Y')
    variables$plotInicial 
  })
  
  observeEvent(input$modelo, {
    variables$orden <- isolate(input$orden)
    
    #variables$plotInicial + 
    #append_layers(variables$plotInicial, stat_smooth(aes(x = variables$x3, y = variables$y3), method = "lm", formula = y ~ poly(x, input$orden), se = F, size = 0.25))
  
    output$grafica <- renderPlot(variables$plotInicial + stat_smooth(aes(x = variables$x3, y = variables$y3), method = "lm", formula = y ~ poly(x, variables$orden), se = F, size = 0.25))
    })
  
  
  output$grafica <- renderPlot({
    react()
    #ggplot(variables$z) + geom_point(aes(x = variables$x3, y = variables$y3))
  })
  
  
})