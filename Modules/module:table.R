library(DT)

tableUI <- function(id){
  
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(
    h4("Uploaded Data:"),
    DT::dataTableOutput(ns("mytable"))
    
  )
   
  
}

tableServer <- function(id, dataset, returnVal = TRUE){
  moduleServer(
    id,
    function(input, output, session){
      
      v<- reactiveValues(data = NULL)
      
      observeEvent(dataset(),{
        
        v$data <- dataset()
      })
      
      # v <- reactiveValues(data = dataset())
      
      
      observeEvent(input$mytable_cell_edit, {
        info = input$mytable_cell_edit
        data = v$data
        v$data <- DT::editData(data, info, resetPaging = FALSE)

      })
      
      output$mytable <- renderDT({
        
        DT::datatable(v$data, options = list(scrollX = TRUE), editable = TRUE)
        
      })
      
      if(returnVal == TRUE){
        return(v)
      }
    
    }
  )
}
