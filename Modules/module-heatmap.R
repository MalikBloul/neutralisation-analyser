# Module UI function

controlUI <- function(id){
  
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(
   #inputs go here: 
    fluidRow( 
      box(width =5, 
          uiOutput('control_plot_colour')),
          
      
      fluidRow( box ( title = "Plot", width = 9 , plotOutput('control_plot'))),
  )
  )
}


controlServer <- function(id, dataset, ui_control_column = reactive("hello")){
  moduleServer(
    id,
    function(input, output, session) {
      ns<- session$ns
      #code goes here
      
      output$control_plot_colour <- renderUI ({
        
        selectInput(ns('control_plot_colour'), "Choose Your Fill:", choices = c("Plate", ui_control_column()), selected =  "Plate")
        
      })
      
  #     selectInput('chosen_y', "Choose what you want to look at:", choices = c("PercentageGFPpos", "AllcellsSelectedNumberofObjects")),
  #     selectInput('chosen_x', "Choose what you want to be on x-axis:", choices = c("CellType", "Plate"))),
  # 
  # box(width = 3, checkboxGroupInput('chosen_celltypes', "Select controls to include on plot:", choices = data %>% pull(CellType) %>% unique(), inline = TRUE)),
      
    }
  )
}