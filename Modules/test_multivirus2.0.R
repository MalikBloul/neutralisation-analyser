
multivirusUI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    box(
      h1("Virus Selection"),
      uiOutput(ns("n_viruses")),
      uiOutput(ns("virus_inputs")),
      uiOutput(ns("submit")),
      
    ),
    box(
      DT::dataTableOutput(ns("table"))
      
    )
   
  
  )
}


multivirusServer <- function(id, dataset){
  moduleServer(
    id,
    function(input, output, session){
      ns<- session$ns
      
      data <- reactive(dataset())
      
      output$n_viruses <- renderUI({
        
        selectInput(ns("n_viruses"), "How many viruses do you have?", choices = c("",1,2,3))
        
      })
      
      output$submit <- renderUI({
        req(input$n_viruses)
        actionButton(ns("submit"), label = "Submit", icon = icon("play"))
      })
      
      
      output$table <- renderDT({
        req(selected_data$data)
        
        DT::datatable(selected_data$data, options = list(scrollX = TRUE))
      })
      
      
      output$viruses_input1 <- renderUI({
        req(input$n_viruses)
        
        choices = c("",data()$data %>% dplyr::select(contains("Percentage")) %>% names())
        lapply(1:input$n_viruses, function(i){
          
          tagList(
            fluidRow(
              column(2,h4(strong(paste("Virus", i)))),
              column(5, selectInput(inputId =  paste0(ns("viruses_input1"), i), em(paste("Which column is this virus in?")), choices = choices)),
              column(5, textInput(inputId =  paste0(ns("viruses_input2"), i), em(paste("What is the name of this virus?"))))
              
            )
            
          )
        })
      })
      
      list_object = reactiveValues(lists = NULL)
      selected_data = reactiveValues(data = NULL)
      
      observeEvent(input$submit,{
        
        list_object$lists <- list()
        
        lapply(1:input$n_viruses, function(i){
          
          x = input[[paste0("viruses_input1", i)]]
          y = input[[paste0("viruses_input2", i)]]
          list_object$lists[[i]] <- c(x,y)
          print(list_object$lists[[i]])
          
        }) 
     
           selected_data$data <- data()$data %>% filter( is_sample == 1, !!rlang::sym(mono_column_name_virus) %in% mono_column_name_virus_included) %>%
             group_by(Plate, !!rlang::sym(mono_column_name_virus)) %>%
             do(infcalc(.data, ctrlval = NULL, list_of_viruses =  list_object$lists)) %>%
             ungroup()
        

  })
        
      
      output$text <- renderPrint({
        for(item in data()$lists ){
          print(item)
        }
})
      
      
      output$virus_inputs <- renderUI({
        req(data)
        tagList(
          
          uiOutput(ns("viruses_input1")),
          uiOutput(ns("viruses_input2")),
          uiOutput(ns("text"))
          
        )
      })
      
      #return(selected_data)
    }
  )
}



