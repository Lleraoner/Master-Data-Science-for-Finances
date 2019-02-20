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
    dashboardHeader(title = 'Mi primer dashboard'),
    dashboardSidebar(
      sidebarMenu(
        #Cuidado con los tabname
        menuItem('GIF', tabName = 'gif', icon = icon('camera-retro')),
        menuItem('Modelo', tabName = 'modelo', icon = icon('chart-bar'))
      )
    ),
    dashboardBody(tabItems(
      tabItem(tabName = 'gif', h2('Gif Stranger Things'), uiOutput("gif")),
      tabItem(
        tabName = 'modelo',
        h2('Modelo'),
        box(plotOutput('modelo')),
        box(
          h2('Tipo'),
          br(),
          h3("Selecciona el tipo de gráfica que se va a usar"),
          selectInput(
            'tipo',
            label = 'Tipo de gráfica',
            choices = c('Polinomial' = 1, 'K-Means' = 2),
            multiple = F
          ),
          uiOutput('cambiomodelo')
        )
      )
    ))
  )
)


#<img src="smiley.gif" alt="Smiley face" width="42" height="42">

#Practica lunes shiny

#Shiny dashboard
#Tiene 2 items: GIF - Aparecerá un gif animado a nuestra elección || etiqueta img para gifs
#El segundo item va a ser Modelo - gráfica de la normal bivariada. El tamaño, correlacion y tal estará dentro de un BOX de shinydashboard
#Habrá un selectinput al principio del box que tendra dos opciones: polinomico y KMeans, por defecto polinomico, esto se hace con UI
#Si se elige KMeans se pondrá numero de cluster, x e y con IRIS
