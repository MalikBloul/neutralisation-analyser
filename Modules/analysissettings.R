# Module UI function

analysisUI <- function(id){
  
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(
   #inputs go here: 
    
    uiOutput(ns('analysis_type')),
    uiOutput(ns('titration_inputs')),
    uiOutput(ns("mono_control_type")),
    uiOutput(ns("mono_control_inputs")),
    uiOutput(ns("sera_inputs"))
  )
}


analysisServer <- function(id,  dataset){
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      #code goes here
      
      #What values we want to pass onto the next module 
      v <- reactiveValues(data = NULL, control_column = NULL, control_values = NULL, analysis_type = NULL, control_applyall = FALSE, compound_column = NULL, list_of_viruses = NULL)
      
      observeEvent(dataset(),{
        
        v$data <- dataset()
      })
      
      observeEvent(input$analysis_type,{
        
        v$analysis_type <- input$analysis_type
        
      })
      
      output$analysis_type <- renderUI({
        
        selectInput(ns('analysis_type'), 'What type of analysis would you like to do?', c("Titration", "Monoclonal Antibody", "Sera"))
        
      })
      
      #Titration ==============================================================================================================================================================================================================================================================================================================================================================================
      
      #which column to take the values from 
      
      output$titration_column_name <- renderUI ({
        req(dataset())
        req(input$analysis_type == "Titration")
        
        selectInput(ns('titration_column_name'), 'Which column are your viruses in?', choices = c('',isolate(v$data %>% colnames())), selected = NULL)
        
      })
      
      
      #From the column that the user selected what viruses are we looking at for a titration (there may not always be multiple viruses to look at) 
      
      output$titration_column_viruses <- renderUI ({
        req(dataset())
        req(input$analysis_type == "Titration")
        req(input$titration_column_name)
        
        checkboxGroupInput(ns('titration_column_viruses'), 'Which viruses would you like to include in the titration analysis?', choices = isolate(v$data %>% pull(get(input$titration_column_name)) %>% unique()), selected =  NULL)
        
      })
      
      
      ##Titration Settings Stored here:
      output$titration_inputs <- renderUI({
        req(dataset())
        req(input$analysis_type == "Titration")
        tagList(
          uiOutput(ns("titration_column_name")),
          uiOutput(ns("titration_column_viruses")),
          uiOutput(ns("titration_control_applybutton")),
          uiOutput(ns("titration_control_resetbutton")),
          DT::dataTableOutput(ns("table"))
        )
      })
      
      #apply and reset
      output$titration_control_applybutton <- renderUI({
        
        req(input$titration_column_viruses)
        
        actionButton(ns("titration_control_applybutton"), "Apply")
        
        
      })
      
      ##Reset
      
      output$titration_control_resetbutton <- renderUI({
        
        req(input$titration_column_viruses)
        actionButton(ns("titration_control_resetbutton"), "Reset")
        
      })
      
      output$table <- renderDT({
        
        DT::datatable(v$data, options = list(scrollX = TRUE))
      })
      
      
      ##Observe titration buttons being pushed
      
      
      observeEvent(input$titration_control_applybutton, {
        
        print("click")
        print(input$titration_column_name)
        
        v$data <- v$data %>% mutate(is_titrated = if_else(get(input$titration_column_name) %in% input$titration_column_viruses, 1,0))
        v$control_column <- input$titration_column_name
        v$control_values <- input$titration_column_viruses
        
      })
      
      observeEvent(input$titration_control_resetbutton, {
        
        v$data <- dataset()
        v$control_column <- NULL
        v$control_values <- NULL
        v$control_applyall <- FALSE
        v$compound_column = NULL
      })
      


      #Monoclonal Antibodies =================================================================================================================================================================================================================================================================================================================

      #Is the monoclonal antibody control defined by concentration or a label (Probably best to always select concentration, label more so for sera)

      output$mono_control_type <- renderUI ({

        req(dataset())
        req(input$analysis_type == "Monoclonal Antibody")

        selectInput(ns('mono_control_type'), 'What defines your Monoclonal Antibody control?', choices = c('', 'label', 'concentration', selected = NULL))

      })

      ##Need a selection box where you define what the label of the control is (if labelled)

      output$mono_control_name <- renderUI({

        req(dataset())
        req(input$analysis_type == "Monoclonal Antibody")
        req(input$mono_control_type == 'label')

        textInput(ns('mono_control_name'), label = "Name of your control")


      })

      #Need to know which column the label is in (compound, etc)
      output$mono_control_column <- renderUI({

        req(dataset())
        req(input$analysis_type == "Monoclonal Antibody")
        req(input$mono_control_type)
        selectInput(ns('mono_control_column'), 'Which column are your control labels in?', choices = c('', isolate(v$data %>% colnames())), selected = NULL)

      })

      #Need to work out which column your viruses are in, might also need what column are your antibodies in for further down?
      output$mono_column_name_virus <- renderUI ({
        req(dataset())

        req(input$analysis_type == "Monoclonal Antibody")

        req(input$mono_control_type)

        selectInput(ns('mono_column_name_virus'), 'Which column are your viruses in?', choices = c('',isolate(v$data %>% colnames())), selected = NULL)


      })


      #Which viruses the user wants to include in the analysis, will make all the selection and everything donw the track for graphs a bit more streamlined.


      output$mono_column_name_virus_included <- renderUI ({
        req(dataset())
        req(input$analysis_type == "Monoclonal Antibody")
        req(input$mono_column_name_virus)


        choices = isolate(v$data %>% pull(get(input$mono_column_name_virus)) %>% unique())

        checkboxGroupInput(ns('mono_column_name_virus_included'), 'Which viruses would you like to include in the monoclonal antibody analysis?', choices = choices, selected = isolate(choices), inline = TRUE)

      })
      
      
      #We need to indicate which column our compound is in
      
      output$mono_compound_column <- renderUI({
       
        req(input$mono_column_name_virus_included)
        
        selectInput(ns("mono_compound_column"), "Which column are your antibodies defined in?", choices = isolate(dataset() %>% colnames()))
        
        
      })

      #Now need a button that you can confirm your selection


      output$control_type_text <- renderText({
        input$mono_column_name_virus_included

      })


      #Putting all the html outputs in a single output for simplicity

      output$mono_control_inputs <- renderUI({
        req(dataset())
        req(input$analysis_type == "Monoclonal Antibody")
        req(input$mono_control_type)

        if (req(input$mono_control_type) == "label"){
          tagList(
            
            fluidRow(
              box(
              uiOutput(ns("mono_control_name")),
              uiOutput(ns("mono_control_column")),
              uiOutput(ns("mono_column_name_virus")),
              uiOutput(ns("mono_column_name_virus_included")),
              uiOutput(ns("mono_compound_column")),
              uiOutput(ns("mono_control_applybutton")),
              uiOutput(ns("mono_control_resetbutton")),
              textOutput(ns('control_type_text')),
              textOutput(ns('success')),
              
              )
            ),
            
            fluidRow(
          
              DT::dataTableOutput(ns("table"))
              
            )
           
            

          )
        } else if(req(input$mono_control_type) == "concentration"){
          tagList(
            uiOutput(ns("mono_column_name_virus")),
            uiOutput(ns("mono_column_name_virus_included")),
            uiOutput(ns("mono_compound_column")),
            uiOutput(ns("mono_control_applybutton")),
            uiOutput(ns("mono_control_resetbutton")),
            textOutput(ns('control_type_text')),
            DT::dataTableOutput(ns("table"))
          )
        }
      })

      #Buttons to apply/reset changes
      ##Apply

      output$mono_control_applybutton <- renderUI({

        req(input$mono_column_name_virus)

        actionButton(ns("mono_control_applybutton"), "Apply")


      })

      ##Reset

      output$mono_control_resetbutton <- renderUI({

        req(input$mono_column_name_virus)
        actionButton(ns("mono_control_resetbutton"), "Reset")

      })


      ##Record an action

      observeEvent(input$mono_control_applybutton, {

        if(req(input$mono_control_type) == "label"){

          v$data <- v$data %>% mutate(is_control = if_else(grepl(input$mono_control_name, !!rlang::sym(input$mono_control_column)), 1, 0 ), is_sample = if_else(!!rlang::sym(input$mono_column_name_virus) %in% input$mono_column_name_virus_included, 1,0))
          v$control_column <- input$mono_column_name_virus
          v$control_values <- input$mono_column_name_virus_included

        } else if(req(input$mono_control_type) == "concentration"){

          v$data <- v$data %>% mutate(is_control = if_else(Concentration == 0, 1, 0 ), is_sample = if_else(!!rlang::sym(input$mono_column_name_virus) %in% input$mono_column_name_virus_included, 1,0))
          v$control_column <- input$mono_column_name_virus
          v$control_values <- input$mono_column_name_virus_included
          v$compound_column <- input$mono_compound_column
        }
      })

      observeEvent(input$mono_control_resetbutton, {

        v$data <- dataset()
        v$control_column <- NULL
        v$control_values <- NULL
        v$control_applyall <- FALSE
        v$compound_column <- NULL

      })
      
      
      output$sera_control_column <- renderUI({
        
        req(dataset())
        req(input$analysis_type == "Sera")
        
        selectInput(ns('sera_control_column'), 'Which column are your serum control labels in?', choices = c('', isolate(v$data %>% colnames())), selected = NULL)  
        
      })
      
      output$sera_control_name <- renderUI({
        
        req(dataset())
        req(input$analysis_type == "Sera")
        req(input$sera_control_column)
        
        textInput(ns('sera_control_name'), label = "Name of your control or letter that defines control (case sensitive)")
        
        
      })
      
      output$sera_column_name_virus <- renderUI ({
        req(dataset())
        req(input$analysis_type == "Sera")
        req(input$sera_control_column)
        
        selectInput(ns('sera_column_name_virus'), 'Which column are your samples in?', choices = c('',isolate(v$data %>% colnames())), selected = NULL)
        
      })
      
      
      
      output$sera_column_name_virus_included <- renderUI ({
        req(dataset())
        req(input$analysis_type == "Sera")
        req(input$sera_column_name_virus)
        choices = isolate(v$data %>% pull(get(input$sera_column_name_virus)) %>% unique())
        
        checkboxGroupInput(ns('sera_column_name_virus_included'), 'Which samples would you like to include in this sera neutralisation analysis?', choices = choices, selected = isolate(choices), inline = TRUE)
        
      })
      
      
      
      # Are the controls localised to one plate or is there a control on each plate. 
      
      output$sera_control_plate_independent <- renderUI({
        req(input$sera_control_name)
        checkboxInput(ns("sera_control_plate_independent"), "Tick if these control samples are used to calculate neutralisation on all plates.")
        
      })
      
      
      
      
      #apply and reset
      output$sera_control_applybutton <- renderUI({
        
        req(input$sera_column_name_virus_included)
        
        actionButton(ns("sera_control_applybutton"), "Apply")
        
        
      })
      
      ##Reset
      
      output$sera_control_resetbutton <- renderUI({
        
        req(input$sera_column_name_virus_included)
        actionButton(ns("sera_control_resetbutton"), "Reset")
        
      })
      
      
      #Observe button push
      
      observeEvent(input$sera_control_applybutton, {
        
        v$data <- v$data %>% mutate(is_control = if_else(grepl(input$sera_control_name, get(input$sera_control_column)), 1, 0 ), is_sample = if_else(get(input$sera_column_name_virus) %in% input$sera_column_name_virus_included, 1,0))
        v$control_column <- input$sera_control_column
        v$control_values <- input$sera_column_name_virus_included
        v$control_applyall <- input$sera_control_plate_independent
      })
      
      observeEvent(input$sera_control_resetbutton, {
        
        v$data <- dataset()
        v$control_column <- NULL
        v$control_values <- NULL
        v$control_applyall <- FALSE
        v$compound_column = NULL
      })
      
      # Output to contain all the html
      
      output$sera_inputs <- renderUI({
        req(dataset())
        req(input$analysis_type == "Sera")
        
        tagList(
          
          uiOutput(ns("sera_control_column")),
          uiOutput(ns("sera_control_name")),
          uiOutput(ns("sera_control_plate_independent")),
          uiOutput(ns("sera_column_name_virus")),
          uiOutput(ns("sera_column_name_virus_included")),
          uiOutput(ns("sera_control_applybutton")),
          uiOutput(ns("sera_control_resetbutton")),
          DT::dataTableOutput(ns("table"))
          
          
        )
        
      })

      return(v)

    }
  )
}