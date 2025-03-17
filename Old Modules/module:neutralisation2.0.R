



# Module UI function

neutralisationUI <- function(id){
  
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(
         fluidRow(
           box(
             uiOutput(ns('neutralisation_plot_colour')),
             uiOutput(ns('neutralisation_plot_shape')),
             uiOutput(ns("chosen_samples")),
             uiOutput(ns("chosen_y")),
             uiOutput(ns("chosen_plate")),
             uiOutput(ns("wrapped")),
             textOutput(ns("text")),
             uiOutput(ns("show_plot"))
           ),
           box(
             
             DT::dataTableOutput(ns("results_table"))
             
           )
         ),
         fluidRow( style = "background-color: grey;height:1000px",
                   
           
            plotOutput(ns("neutralisation_plot"), width = "100%", height = "100%")
           
         )
  )
}

neutralisationServer <- function(id, dataset, list_of_viruses){
  moduleServer(
    id,
    function(input, output, session) {
      
      ns<- session$ns
      #code goes here
      
      output$neutralisation_plot_shape <- renderUI ({ #this is the fill of the plot it needs to be optional or set to default 
        
        
        req(dataset()$data)
        
        selectInput(ns('neutralisation_plot_shape'), "Choose Your Shape:", choices = c("Plate", isolate(dataset()$control_column), isolate(dataset()$compound_column)), selected =  "Plate")
        
      })
      
      output$neutralisation_plot_colour <- renderUI ({ #this is the fill of the plot it needs to be optional or set to default 
      
        req(dataset()$data)
        
        selectInput(ns('neutralisation_plot_colour'), "Choose Your Fill:", choices = c("Plate", isolate(dataset()$control_column), isolate(dataset()$compound_column)), selected =  "Plate")
        
      })
      
      output$chosen_samples <- renderUI ({
        req(dataset()$data)
        
        if(dataset()$analysis_type == "Monoclonal Antibody"){
          
          req(dataset()$control_values)
          choices = dataset()$control_values
          checkboxGroupInput(ns('chosen_samples'), "Select samples/viruses to include on plot:", choices = choices, inline = TRUE, selected = isolate(choices))
          
        } else if(dataset()$analysis_type == "Titration"){
          
          choices = dataset()$data %>% filter(is_titrated == 1) %>% pull(dataset()$control_column) %>% unique()
          checkboxGroupInput(ns('chosen_samples'), "Select samples/viruses to include on plot:", choices = choices, inline = TRUE, selected = isolate(choices))
       
        } else { 
        
        
           choices = dataset()$data %>% filter(is_sample == 1) %>% pull(dataset()$control_column) %>% unique()
           checkboxGroupInput(ns('chosen_samples'), "Select samples/viruses to include on plot:", choices = choices, inline = TRUE, selected = isolate(choices))
          
        }
      })
      
     ### NEEDS UPDATING !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
      ctrlvalue <- reactive({
        req(dataset()$data)

        if(dataset()$analysis_type == "Sera" & dataset()$control_applyall == TRUE){
          j = dataset()$data%>% filter(is_control == 1) %>% summarise(mean = mean(PercentageGFPpos)) %>% as.numeric()
          return(j)
          
        } else {
         return(NULL)
        }
      })
        
        
      output$wrapped <- renderUI({
        req(dataset()$data)
        
        
        checkboxInput(ns("wrapped"), 'Would you like a separate plot for each virus/sample?')
      })
      
      
        
       wrapper <- reactive({
         
         if(input$wrapped == TRUE){
           
           return(dataset()$control_column)
           
         } else{
           
           return(NULL)
           
         }
         
       })
        
        
    
      output$text <- renderText({
        
        # dataset()$data%>% colnames()
      if(is.null(ctrlvalue())){
        print('control value is null')
      }
        
      })
      
      output$chosen_y <- renderUI({
        req(dataset()$data)
        
        ###Needs a function that recreate the 
        
        for(item in list_of_viruses()){
          
          columns <- selected_data$data %>% colnames()
          similar_columns = grep(item[1], columns)  
          
          
          choices = columns[similar_columns]
        }
        
        
        
        selectInput(ns('chosen_y'), "Choose what do you want to look at:", choices = isolate(c("PercentageGFPpos", "infection", 'neutralisation')))
        
      })
      
      
      output$chosen_plate <- renderUI({
        req(dataset()$data)
        checkboxGroupInput(ns('chosen_plate'), "Choose which plates you want to look at:", choices = dataset()$data %>% pull(Plate) %>% unique())
        
      })
      
      output$show_plot <- renderUI({
        
        actionButton(ns("show_plot"), "Show")
      })
      
      
      output$neutralisation_plot <- renderPlot({
        req(dataset()$data)
        
        # plotly::ggplotly(n_plot())
        n_plot()
        
        
            })

      
      
      n_plot <- eventReactive(input$show_plot, {
        req(dataset()$data)
        
        #need an if statement for if the box is checked then need to calculate ctrl for all samples as an input with maybe like avg, N() and SD. 
        #will also require a download button and a table with summary of the data. 
        
        # dataset()$data %>%
        #   filter(is_sample == 1, !!rlang::sym(dataset()$control_column) %in% input$chosen_samples) %>%
        #   group_by(Plate, !!rlang::sym(dataset()$control_column)) %>%
        #   do(infcalc(.data, ctrlvalue)) %>%
        #   ungroup() %>%
        #   group_by(Plate, !!rlang::sym(dataset()$control_column), Concentration) %>%
        #   filter(Concentration != 0, Plate == input$chosen_plate) %>%
        #   summarise(avg = mean(!!rlang::sym(input$chosen_y)), n =n(), upper = avg + sd(!!rlang::sym(input$chosen_y)), lower = avg - sd(!!rlang::sym(input$chosen_y))) %>%
        #   ggplot(aes(x = Concentration, y = avg,  colour = !!rlang::sym(dataset()$control_column))) +
        #   geom_point() +
        #   geom_line() + 
        #   scale_x_log10() +
        #   geom_errorbar(aes(ymax = upper, ymin = lower), width = 0.5) + 
        #   theme(legend.position = 'top') + 
        #   geom_hline( yintercept = 0, linetype = 'dashed') +
        #   theme_classic()
        
       
        
        p = dataset()$data %>%
          filter( is_sample == 1, !!rlang::sym(dataset()$control_column) %in% input$chosen_samples) %>%
          group_by(Plate, !!rlang::sym(dataset()$control_column)) %>%
          do(infcalc(.data, ctrlvalue())) %>%
          ungroup() %>%
          filter(Concentration != 0, Plate %in% input$chosen_plate) %>%
          ggplot(aes(x = Concentration, y = !!rlang::sym(input$chosen_y), colour = !!rlang::sym(input$neutralisation_plot_colour), shape = !!rlang::sym(input$neutralisation_plot_shape))) +
          stat_summary(geom = 'point', fun = mean, size = 2) + 
          stat_summary(geom = 'line', fun = mean) +
          stat_summary(geom = "errorbar", fun.data = function(y) c(ymin=mean(y)-sd(y), ymax=mean(y)+sd(y)), width=0.1) +
          scale_x_log10() +
          theme(legend.position = 'top') + 
          geom_hline( yintercept = 0, linetype = 'dashed') +
          facet_wrap({{wrapper()}}) +
          theme(aspect.ratio = 1) +
          theme_linedraw()
          
        return(p)
      })
      
      
      
     table_data <- eventReactive(input$show_plot, {
        
        
        if(input$neutralisation_plot_colour == input$neutralisation_plot_shape){
          
          df <- dataset()$data %>%
            filter(is_sample == 1, !!rlang::sym(dataset()$control_column) %in% input$chosen_samples) %>%
            group_by(Plate, !!rlang::sym(dataset()$control_column)) %>%
            do(infcalc(.data, ctrlvalue())) %>%
            ungroup() %>%
            filter(Plate %in% input$chosen_plate) %>% 
            group_by(!!rlang::sym(dataset()$control_column), Concentration) %>%
            summarise( Average = mean(!!rlang::sym(input$chosen_y)) %>% round(4), SD = sd(!!rlang::sym(input$chosen_y)), n = n())
          
          return(df)
          
        } else {
          
          df <- dataset()$data%>%
            filter(!!rlang::sym(dataset()$control_column) %in% input$chosen_samples) %>%
            group_by(Plate, !!rlang::sym(dataset()$control_column)) %>%
            do(infcalc(.data, ctrlvalue())) %>%
            ungroup() %>%
            filter(Plate %in% input$chosen_plate) %>% 
            group_by(Plate, !!rlang::sym(dataset()$control_column),!!rlang::sym(dataset()$compound_column), Concentration) %>%
            summarise(Average = mean(!!rlang::sym(input$chosen_y)) %>% round(4), SD = sd(!!rlang::sym(input$chosen_y)), n = n())
          
          return(df)
        }
      })
      
      
      
      output$results_table <- DT::renderDT(server = FALSE,{
        
       DT::datatable(table_data(), extensions = "Buttons" , options = list(scrollX = TRUE, 
                                                  searching = FALSE,
                                                  dom = 'Bfrtip',
                                                  buttons = c("excel", "pdf"), 
                                                  info = FALSE))
        
      })
    }
  )
}