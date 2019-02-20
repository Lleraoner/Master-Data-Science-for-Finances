library(shiny)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  resultads <- reactiveValues()
  
  output$grafico_barras <- renderPlot({
    ggplot(mpg, aes(x = cyl)) + geom_histogram(fill = input$color)
  })
  
  #Esta es una opción, se observaría el evento y se actualizaría la gráfica en el valor reactivo
  observeEvent(input$buton, {
    randomX <- as.data.frame(rnorm(input$elementos))
    randomY <- as.data.frame(rnorm(input$elementos))
    dataframe <- cbind(randomX, randomY)
    
    names(dataframe) <- c('ejeX', 'ejeY')
    resultads$grafics <- ggplot(dataframe) + geom_point(aes(ejeX, ejeY))
  }) 
  
  output$muestra_aleatoria <- renderPlot({
    
    #Poniendo aqui el input se actualizaría directamente el resultado, sería otra opción.
    #input$buton
    
    randomX <- as.data.frame(rnorm(input$elementos))
    randomY <- as.data.frame(rnorm(input$elementos))
    dataframe <- cbind(randomX, randomY)
    
    names(dataframe) <- c('ejeX', 'ejeY')
    
    resultads$grafics <- ggplot(dataframe) + geom_point(aes(ejeX, ejeY))
    resultads$grafics
    
  })
  
})

 