library(shiny)
library(ggplot2)
library(MASS)
library(magrittr)
library(dplyr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  resultado <- reactiveValues()
  
  output$gif <- renderUI({
    #Llamamos a todo al pulsar este botón
    #react()
    
    #Todo lo que se escriba aque se va a completar en la ui en la parte de abajo de ui htmlvideo
    #Se pueden devolver todas las cosas que se quieran
    #Para devolver muchas cosas se tiene que crear una lista si o si
    list(
      #tags$iframe(width = 560, height = 315, src = paste0("https://www.youtube.com/embed/", isolate(input$idvideo))),
      tags$iframe(src = 'https://giphy.com/embed/l1J9qrAVNHt1bDi2A', width = "480", height = "362")
      #tags$head(includeScript('script.js'))
    )
  })
  
  
  output$cambiomodelo <- renderUI({
    if (input$tipo == 1) {
      box(h3('Datos relativos a la gráfica polinomial'),
      numericInput('tamanio', 'Tamaño de la muestra', min = 100, max = 25000, step = 100, value = 500),
      numericInput('correlacion', 'Correlacion', min = -1, max = 1, step = 0.1, value = 0),
      actionButton('generar', 'Generar muestra', icon = icon('500px')),
      numericInput('orden', 'Orden polinomial', min = 1, step = 1, value = 1),
      actionButton('modelo', 'Añadir modelo', icon = icon('500px'))
      )
    }
    else if (input$tipo == 2){
      box('Datos relativos al K-Means',
      selectInput('x', 'Eje X', choices = names(iris[-5]), multiple = F),
      selectInput('y', 'Eje Y', choices = names(iris[-5]), multiple = F),
      numericInput('cluster', label = 'Numero de clusteres', min = 1, step = 1, value = 1),
      actionButton('modeloKmeans', 'Generar modelo', icon = icon('500px')) 
      )
    }
    
  })
  
  observeEvent(input$generar, {
    datos <- as.data.frame(mvrnorm(input$tamanio, c(0,0),
                                   Sigma = matrix(c(1, input$correlacion, input$correlacion, 1), nrow = 2)))
    colnames(datos) <- c("x", "y")
    resultado$grafica <- ggplot(datos, aes(x = x, y = y)) + geom_point()
  })
  
  
  observeEvent(input$modelo, {
    
    req(resultado$grafica)
    
    orden <- input$orden
    resultado$grafica <- resultado$grafica + geom_smooth(method = "lm", formula = y ~ poly(x, orden))
  })
  
  
  miModelo <- reactive({
    input$modeloKmeans
    iris %>%
      select_(isolate(input$x), isolate(input$y)) %>%
      kmeans(isolate(input$cluster))
  })
  
  observeEvent(input$modeloKmeans, {
    resultado$grafica <- ggplot(iris, aes_string(x = isolate(input$x),
                            y = isolate(input$y),
                            color = as.factor(miModelo()$cluster))) +
      geom_point()
  })
  
  
  output$modelo <- renderPlot({
    resultado$grafica
  })
  
})