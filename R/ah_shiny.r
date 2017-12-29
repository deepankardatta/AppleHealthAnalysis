library(shiny)
library(ggplot2)
library(purrr)
library(dplyr)

ah_shiny <- function(health_data) {

  ah_type_list <- levels(health_data$type)

  require(shiny)
  shinyApp(
    ui = fluidPage(
      titlePanel("Apple Health Analysis Shiny Dashboard"),

      sidebarLayout(
        sidebarPanel(

          #dateRangeInput start

          dateRangeInput( inputId = "date_range",
                          label = "Date range: ",
                          start = NULL,
                          end = NULL,
                          min = NULL,
                          max = NULL,
                          format = "yyyy-mm-dd",
                          startview = "month",
                          weekstart = 1,
                          language = "en",
                          separator = " to "),

          # END

          #se

          selectInput( inputId = "health_variable",
                       label = "Choose a health variable to display",
                       choices = ah_type_list, # End of choices
                       selected = NULL,
                       multiple = FALSE,
                       selectize = TRUE)

          #END



          ),
        mainPanel(
          textOutput("selected_var"),
          plotOutput("ah_plot")
          )
      )
    ),
    server = function(input, output) {

      output$selected_var <- renderText({
        paste("You have selected", input$health_variable)
      })

      data_to_plot <- reactive({

        health_data %>% filter( type == input$health_variable )

      })

      output$ah_plot <- renderPlot({
        g <- ggplot(data_to_plot(), aes(x=date, y=value)) +
          geom_line() +
          theme_bw() +
          labs(x="Date",
               y=input$health_variable,
               title=paste0("From ", input$date_range[1], " to ", input$date_range[2]))
        g
      })
    }
  )
}

