library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(readxl)
library(plotly)


files <- list.files("Modules",full.names=TRUE,pattern="\\.R")
sapply(files, source)



ui <- dashboardPage(
  dashboardHeader(title = "Neutralisation Analyser"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("1. Upload Data", tabName = "upload_data"),
      menuItem("2. Heatmaps", tabName = "heatmap"),
      menuItem("3. Analysis Settings", tabName = "analysis_settings"),
      menuItem("4. Channel Selection", tabName = "virus_selection"),
      menuItem("5. Controls", tabName = "controls"),
      menuItem("6. Neutralisation", tabName = "neutralisation"),
      menuItem("7. EC50", tabName = "ec50")
    )
    
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'upload_data',
              fluidPage(
                fileUI('file_data', 'Load your Excel file :)'),
                tableUI("mytable")
              )
              
      ),
      
      tabItem(tabName = 'heatmap',
              fluidPage(
                heatmapUI('heatmap')
              )
              
      ),
      
      
      tabItem(tabName = "analysis_settings",
              fluidPage(
                analysisUI("settings")
      )
      ),
      
      tabItem(tabName = "virus_selection",
              fluidPage(
                multivirusUI("test")
              )
     ),
      
      tabItem(tabName = "controls",
              fluidPage(
                controlUI("control")
      )
      ),
      
      tabItem(tabName = "neutralisation",
              fluidPage(
                neutralisationUI("neutralisation")
              )
      )
    )
  )
)

server <- function(input, output, session) { 

  
  
  
  
  
  file_data <- fileServer("file_data") #creates our reactive dataframe
  
  data_uploaded <- tableServer("mytable",  reactive(file_data())) # creates and editable table and stores it as another variable (so file_data() is pure for reset)

  heatmapServer('heatmap', reactive(data_uploaded$data)) #Creates Heatmaps with reactive variables depending on the dataframe
  
  data_updated <- analysisServer('settings', reactive(data_uploaded$data))
  
  multivirusServer("test", reactive(data_updated))
  
  controlServer('control', reactive(data_updated)
  
  neutralisationServer('neutralisation', reactive(data_updated)

      
}

shinyApp(ui, server)


