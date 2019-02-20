#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinydashboard)


shinyUI(
  dashboardPage(
    dashboardHeader(title = h4('Guess The Correlation')),
    dashboardSidebar(
      sidebarMenu(
        #Cuidado con los tabname
        menuItem('Introducction', tabName = 'image', icon = icon('camera-retro')),
        menuItem('Guess the correlation', tabName = 'guess', icon = icon('chart-bar')),
        menuItem('Paint the correlation', tabName = 'paint', icon = icon('chart-bar')),
        menuItem('Change the correlation', tabName = 'change', icon = icon('chart-bar')),
        menuItem('Guess the median', tabName = 'median', icon = icon('chart-bar')),
        menuItem('Conclusion And References', tabName = 'referencias', icon= icon('chart-bar'))
      )
    ),
    dashboardBody(tabItems(
    
        tabItem(tabName = 'image', h1('Shiny Final Proyect'),h4('Game Guess The correlation'),p('Luis Llera Garcia'),
              uiOutput("image")),
      
        tabItem(tabName = 'guess',
        h2('Correlation Chart'),
        box(plotOutput('modelo')),
        box(
          h2('Game Guess the Correlation'),
          br()
          ),
          uiOutput('guesscorrelation')),
     
       tabItem(tabName = 'paint',
              h2('Paint the correlation Chart'),
              box(plotOutput('modelopintado')),
              box(
                h2('Game Paint the Correlation'),
                br()
              ),
              uiOutput('paintmodel')),
     # el de change no se lo que hay que ahcer solo se que hay un boton que te he puesto en el server
       tabItem(tabName = 'change',
              h2('Change Chart'),
              box(plotOutput('modelocambiado')),
              box(
                h2('Game change the Correlation'),
                br(),
                h3("Selecciona la correlacion correcta")
              ),
              uiOutput('cambiomodelo')),
      
      tabItem(tabName = 'median',
              h2('Median Chart'),
              box(plotOutput('graficomediana')),
              box(
                h2('Game Guess the median'),
                br(),
                h3("Selecciona la mediana correcta")
                ##Aqui tenemos que ponerle texto
              ),
              uiOutput('compruebamediana')),
      
      tabItem(tabName = 'referencias', h2('Conclusion y Referencias'),p('FERRO MARICÓN'),uiOutput("referencias")
                  ))
         
      
    ))
  )

