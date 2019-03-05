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
  dashboardPage(skin = "purple",
                
    dashboardHeader(title = h4('Guess The Correlation')),
   
    dashboardSidebar(
      sidebarMenu(
        #Cuidado con los tabname
        menuItem('Introduction', tabName = 'image', icon = icon('camera-retro')),
        menuItem('Guess the correlation', tabName = 'guess', icon = icon('chart-bar')),
        menuItem('Paint the correlation', tabName = 'paint', icon = icon('chart-bar')),
        #menuItem('Change the correlation', tabName = 'change', icon = icon('chart-bar')),
        menuItem('Guess the median', tabName = 'median', icon = icon('chart-bar')) 
     
        
      )
    ),
    dashboardBody(tabItems(
      
        tabItem(tabName = 'image', uiOutput("image")),
      
        tabItem(tabName = 'guess',
              h2('Correlation Chart'),
              plotOutput('modelo'),
              uiOutput('guesscorrelation')),
     
       tabItem(tabName = 'paint',
              h2('Paint the correlation Chart'),
              plotOutput('modelopintado', click = 'paintclick'),
              uiOutput('paintmodel')),
      
      tabItem(tabName = 'median',
              h2('Guess the median'),
              plotOutput('graficomediana'),
              uiOutput('compruebamediana'))
      )
         
      
    ))
  )

