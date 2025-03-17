# Module UI function

controlUI <- function(id){
  
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(
         fluidRow(
           box(
             uiOutput(ns('control_plot_colour')),
             textOutput(ns("ui_control_column")),
             uiOutput(ns("chosen_controls")),
             uiOutput(ns("chosen_y")),
             uiOutput(ns("chosen_x"))
             
           )
         ),
         fluidRow(
           
           box(
             uiOutput(ns("show_plot")),  
             plotOutput(ns("control_plot"))
           ),
           box(
             
             plotOutput(ns("control_plot_quality"))
           )
           
         )
  )
}


controlServer <- function(id, dataset){
  moduleServer(
    id,
    function(input, output, session) {
      ns<- session$ns
      #code goes here
      
      output$control_plot_colour <- renderUI ({
        
        req(dataset()$control_column)
        req(dataset()$data)
        
        selectInput(ns('control_plot_colour'), "Choose Your Fill:", choices = c("Plate", isolate(dataset()$control_column)), selected =  "Plate")
        
      })
      
      output$chosen_controls <- renderUI ({
        req(dataset()$data)
        
        if(dataset()$analysis_type == "Monoclonal Antibody"){
          
          req(dataset()$control_values)
          choices = dataset()$control_values
          checkboxGroupInput(ns('chosen_controls'), "Select controls to include on plot:", choices = choices, inline = TRUE, selected = choices)
          
        } else{
           
           choices = dataset()$data %>% filter(is_control == 1) %>% pull(!!rlang::sym(dataset()$control_column)) %>% unique()
           checkboxGroupInput(ns('chosen_controls'), "Select controls to include on plot:", choices = choices, inline = TRUE, selected = choices)
          
        }
      })
      
      
      output$ui_control_column <- renderText({
        
        # dataset() %>% colnames()
        
        
      })
      
      output$chosen_y <- renderUI({
        req(input$chosen_controls)
        selectInput(ns('chosen_y'), "Choose what you want to look at:", choices = c("PercentageGFPpos", "AllcellsSelectedNumberofObjects"))
        
      })
      
      
      output$chosen_x <- renderUI({
        req(input$chosen_y)
        selectInput(ns('chosen_x'), "Choose what you want to look at:", choices = c("Plate", isolate(dataset()$control_column)))
        
      })
      
      output$show_plot <- renderUI({
        # req(input$chosen_x)
        # actionButton(ns("show_plot"), "Show")
      })

      output$control_plot <- renderPlot({
        req(dataset()$data)
       
       
        dataset()$data %>% 
          filter(is_control == 1, get(dataset()$control_column) %in% input$chosen_controls) %>%
          ggplot(aes(x = get(input$chosen_x), y = get(input$chosen_y), color = get(input$control_plot_colour))) +
          stat_summary(geom = "col", fun = mean, position = 'dodge', width = 0.5) +
          stat_summary(geom = "errorbar", fun.data = function(y) c(ymin=mean(y)-sd(y),ymax=mean(y)+sd(y)), position = 'dodge', width=0.5) +
          theme_classic() + xlab(input$chosen_x) + ylab(input$chosen_y) + labs(color = input$control_plot_colour)
      })
      
      
      output$control_plot_quality <- renderPlot({
        
        req(dataset()$data)
        
        dataset()$data %>% 
          filter(is_control == 1, get(dataset()$control_column) %in% input$chosen_controls) %>%
          ggplot(aes(x = PercentageGFPpos, y = AllcellsSelectedNumberofObjects, color = get(input$control_plot_colour))) +
          geom_point() + geom_smooth(method = lm) + facet_wrap(~get(dataset()$control_column), scales = "free") + theme_classic() + labs(color = input$control_plot_colour)
      })
    }
  )
}