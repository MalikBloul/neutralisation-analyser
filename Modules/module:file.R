



# Module UI function

fileUI <- function(id, label = "Choose Excel File") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(
    fileInput(ns('file_data'), label, accept = '.xlsx'),
    checkboxInput(ns("use_test_data"), "Use test data instead", value = FALSE)
  )
}


fileServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    userFile <- reactive({
      # If no file is selected, don't do anything
      validate(need(input$file_data, message = FALSE))
      input$file_data
    })
    
    
    dataframe <- reactive({
      sheet <- excel_sheets(userFile()$datapath) #give the name of the plates
      data_frame = lapply(setNames(sheet, c(1:length(sheet))), function(x)
        read_excel(userFile()$datapath, sheet = x))
      
      data = bind_rows(data_frame, .id = "Plate")
      
      data$Concentration <- round(data$Concentration, digits = 2)
      
      data$neutralisation <- NA
      
      data <- data %>%
        rename_with( ~ gsub(" ", "", .x)) %>%
        rename_with( ~ gsub("-", "", .x)) %>%
        rename_with( ~ gsub("`", "", .x)) %>%
        rename_with( ~ gsub("%", "Percentage", .x))
      
      return(data)
    })
    
    
    
    
    return(dataframe)
  })
}