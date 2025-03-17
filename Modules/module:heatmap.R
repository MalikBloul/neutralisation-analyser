# Module UI function

heatmapUI <- function(id){
  
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(
    plotOutput(ns('heatmap_plot')),
    uiOutput(ns('plate_selection')),
    uiOutput(ns('fill_selection'))
   
  )
}


heatmapServer <- function(id, dataset){
  moduleServer(
    id,
    function(input, output, session) {
      

      ns<- session$ns
      
    
    
      output$plate_selection <- renderUI({
        req(dataset())
        selectInput(ns('plate_selection'), "Plate", choices =  c(dataset() %>% pull(Plate) %>% unique(), "All"))
                                                                  
      })
      
      
      output$fill_selection <- renderUI({ #selects fill
        req(dataset())

          selectInput(ns('fill_selection'), 'Fill', choices = dataset() %>% colnames())
          
      })



      output$heatmap_plot <- renderPlot({

        req(dataset())


        if(input$plate_selection != 'All'){

         dataset() %>%
            filter(Plate %in% input$plate_selection) %>%
            mutate(Row = as.factor(Row), Column = as.factor(Column)) %>%
            ggplot(aes(Column, Row)) +
            geom_tile(
              aes(fill = get(input$fill_selection)),
              color = 'black',
              lwd = 0.2,
              linetype = 1
            ) +
            scale_y_discrete(limits = rev) +
            coord_fixed(1) +
            labs(title = paste( "Plate", input$plate_selection), fill = paste(input$fill_selection))


          # ggplotly(p)
        } else if(input$plate_selection == 'All'){

          dataset() %>%
            mutate(Row = as.factor(Row), Column = as.factor(Column)) %>%
            ggplot(aes(Column, Row)) +
            geom_tile(
              aes(fill = get(input$fill_selection)),
              color = 'black',
              lwd = 0.2,
              linetype = 1
            ) +
            scale_y_discrete(limits = rev) +
            coord_fixed(1) +
            labs(title = paste( "Plate", input$plate_selection), fill = paste(input$fill_selection)) +
            facet_wrap(~Plate)
        }
        # ggplotly(p)
      })

    }
  )
}