library(shiny)
library(ggplot2)
library(MASS)
library(magrittr)
library(dplyr)

points <- list(x=vector("numeric", 0), y=vector("numeric", 0))
df <- data.frame(x = seq(20,150, length.out = 10) + rnorm(10)*8,
                 y = seq(20,150, length.out = 10) + rnorm(10)*8)
df$y[1] = df$y[1] + 80


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  resultado <- reactiveValues()
  resultado$cor <- 0
  
  output$menu <- renderMenu({
    sidebarMenu(menuItem("Menu item", icon = icon("calendar")))
  })
  
  output$image <- renderUI({
  
    list(
      #tags$iframe(width = 560, height = 315, src = paste0("https://www.youtube.com/embed/", isolate(input$idvideo))),
      tags$img(src = 'http://i63.tinypic.com/2rynh9h.jpg', width = "150", height = "225")
      #tags$head(includeScript('script.js'))
    )
    
    fluidRow(align = 'center', 
             h2('Shiny Final Proyect'),
             h3('Game Guess The correlation'),
             h4('Luis Llera Garcia'),
             tags$img(src = 'https://imagizer.imageshack.com/img924/4103/xS5bv7.png', width = "450", height = "200"),
             p('This aim of this project is to build a game with 3 parts:'),
             hr(),
             p('1- The first one is the game Guess the correlation, the user has to guess a correlation value between -1 and 1 by only watching a graph.'),
             hr(),
             p('2- In the second game the user has to draw a random correlation by clicking the graph'),
             hr(),
             p('3- The final part consists of estimating the median for X and Y axis.'))
  })
  
  
  output$guesscorrelation <- renderUI({
    column(12, align="center",
      h4('Choose estimated correlation'),
      numericInput("choose", "Your Guess", value = 0, min = -1, max = 1, step = 0.01),
      actionButton('estimar', 'Let\'s see', icon = icon('500px')),
      verbatimTextOutput('correlationtTxt'),
      actionButton('reload', 'Reload graph')
    )
  
  })
  
  
  output$paintmodel <- renderUI({
    
    column(12, align = 'center', h3('Click on graph to draw '),
        verbatimTextOutput("pinta"),
        actionButton('checkPaint', 'Check', icon = icon('500px')),
        verbatimTextOutput('resultPaint')
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
    
    column(12, align = 'center', 
           h3('Guess the median for X and Y axis'),
           numericInput("xmedian", "Median for X", value = 0, step = 0.01),
           numericInput("ymedian", "Median for Y", value = 0, step = 0.01),
           actionButton('checkMedian', 'Check medians', icon = icon('500px')),
           actionButton('reloadMedian', 'Reload chart'),
           verbatimTextOutput('medianTxtX'),
           verbatimTextOutput('medianTxtY')
        # y aqui habrá que hacer un prit de tu mediana comprobada es de tal y la respuesta correcta es tal...
    )
    
  })
  
  
  
  ##### BOTONES #####
  observeEvent(input$estimar, {
    correlation <- cor(resultado$df[1], resultado$df[2])
    
    resultado$cor <- correlation
    
    resultado$text = ''
    
    if (round(resultado$cor, 2) == round(input$choose, 2)) 
      resultado$text <- paste('Congratulations, the correlation is: ', round(resultado$cor, 2))
    else
      resultado$text <- paste('Sorry, your choice was: ', input$choose, ' but the correct one is: ', round(resultado$cor, 2))
    
  })
  
  observeEvent(input$checkPaint, {
    df <- as.data.frame(points)
    cor <- cor(df$x, df$y)
    
    resultado$textPaint = ''
    
    if (round(cor, 2) == round(resultado$corPaint, 2))
      resultado$textPaint <- paste('Congratulations, the correlation is: ', round(resultado$corPaint, 2))
    else
      resultado$textPaint <- paste('Sorry, your choice was: ', round(cor, 2), ' but the correct one is: ', resultado$corPaint)
  })
  
  observeEvent(input$reloadPaint, {
    points <- as.data.frame(points)
    points <- points[1,]
    points$x <- 0
    points$y <- 0
    
  })
  
  observeEvent(input$checkMedian, {
    
    medX <- median(resultado$dfMedian[,1])
    medY <- median(resultado$dfMedian[,2])
    
    resultado$textMedX = ''
    resultado$textMedY = ''
    
    if (round(input$xmedian, 2) == round(medX, 2))
      resultado$textMedX <- paste('Congratulations, the median of X is: ', round(medX, 2))
    else
      resultado$textMedX <- paste('Sorry, your choice was: ', round(input$xmedian, 2), ' but the correct median of X is: ', round(medX, 2))
  
    if (round(input$ymedian, 2) == round(medY, 2))
      resultado$textMedY <- paste('Congratulations, the median of Y is: ', round(medY, 2))
    else
      resultado$textMedY <- paste('Sorry, your choice was: ', round(input$ymedian, 2), ' but the correct median of Y is: ', round(medY, 2))
    
    
    })
  
  ###### TEXTOS ######
  output$correlationtTxt <- renderText({
    resultado$text
  })
  
  output$pinta <- renderText({
    corP <- round(runif(1, min = -1, max = 1), 2)
    paste("You have to paint a correlation of: ", value = corP)
    resultado$corPaint <- corP
  })
  
  output$resultPaint <- renderText({
    resultado$textPaint
  })
  
  output$medianTxtX <- renderText({
    resultado$textMedX
  })
  
  output$medianTxtY <- renderText({
    resultado$textMedY
  })
  
  ###### GRAFICAS ######
  output$modelo <- renderPlot({
    
    input$reload
    
    randomX <- as.data.frame(rnorm(300))
    randomY <- as.data.frame(rnorm(300))
    dataframe <- cbind(randomX, randomY)
    
    resultado$df <- dataframe
    
    names(dataframe) <- c('ejeX', 'ejeY')
    
    ggplot(dataframe) + geom_point(aes(ejeX, ejeY))
  })
  
  output$modelopintado <- renderPlot({
    input$paintclick
    input$reloadPaint
    plot(points$x, points$y, xlim = c(-5, 5), ylim = c(-5, 5))
  })
  
  observeEvent(input$paintclick, {
    points$x <<- c(points$x, isolate(input$paintclick$x))
    points$y <<- c(points$y, isolate(input$paintclick$y))
  })
  
  output$graficomediana <- renderPlot({
    
    input$reloadMedian
    
    randomX <- as.data.frame(rnorm(300))
    randomY <- as.data.frame(rnorm(300))
    dataframe <- cbind(randomX, randomY)
    
    resultado$dfMedian <- dataframe
    
    names(dataframe) <- c('ejeX', 'ejeY')
    
    ggplot(dataframe) + geom_point(aes(ejeX, ejeY))
  })
}) 