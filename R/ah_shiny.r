library(shiny)
library(ggplot2)
library(purrr)
library(dplyr)

ah_shiny <- function(health_data) {

  # Makes a list of the type factors, aka the health data variables measured
  ah_type_list <- levels(health_data$type)

  require(shiny)
  shinyApp(
    ui = fluidPage(
      titlePanel("Apple Health Analysis Shiny Dashboard"),

      sidebarLayout(
        sidebarPanel(

          #dateRangeInput start

          # A decision was made not to make this a reactice element (yet!)
          # This is a design decision: it seems more useful to have the date
          # ranges set for global data, than re-do the date ranges for each
          # individual health variable when selected

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

          #Health data variable selection drop box

          selectInput( inputId = "health_variable",
                       label = "Choose a health variable to display",
                       choices = ah_type_list, # End of choices
                       selected = "HeartRate",
                       multiple = FALSE,
                       selectize = TRUE)

          #END

          # Could also consider putting a file input and export box here

          ),

        mainPanel( # Start of Shiny dashboard output
          textOutput("selected_var"),
          plotOutput("ah_plot")
          )
      )
    ),
    server = function(input, output) {

      # Test as per Shiny examples
      output$selected_var <- renderText({
        paste("You have viewing a plot of ", input$health_variable,
              " from ", input$date_range[1], " to ", input$date_range[2])
      })

      # Use the Shiny reactive function to filter to the variable the user wants
      data_to_plot <- reactive({

        health_data %>% filter( type == input$health_variable )

      })

      # The plot generation output
      output$ah_plot <- renderPlot({
        ah_plot <- ggplot(data_to_plot(), aes(x=date, y=value)) +
          geom_line() +
          theme_bw() +
          labs(x="Date", y=input$health_variable)
        ah_plot
      })
    }
  )
}

