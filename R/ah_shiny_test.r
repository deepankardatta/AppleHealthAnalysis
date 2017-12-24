library(shiny)
library(ggplot2)
library(dplyr)
library(magrittr)
library(lubridate)

#shiny app modified from here:
# https://jeffjjohnston.github.io/rstudio/rmarkdown/2016/04/28/explore-your-apple-watch-heart-rate-data.html

ui <- shinyUI(fluidPage(

  titlePanel("My Apple Watch heart rate data"),

  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date_range", "Date range: ",
                     format="mm-dd-yyyy",
                     min=min(as.Date(heart_rate_data$date)),
                     max=max(as.Date(heart_rate_data$date)),
                     start=max(as.Date(heart_rate_data$date)) - days(1),
                     end=max(as.Date(heart_rate_data$date))
      )
    ),
    mainPanel(
      plotOutput("heart_rate_plot"),
      tableOutput("heart_rate_table")
    )
  )
))

server <- shinyServer(function(input, output) {

  heart_rate_ranged <- reactive({
    date_ranges <- input$date_range[1:2] %>% as.Date
    date_start  <- min(date_ranges)
    date_end    <- max(date_ranges)

    heart_rate_data %>%
      filter(date >= date_start & date <= date_end)
  })

  output$heart_rate_plot <- renderPlot({
    g <- ggplot(heart_rate_ranged(), aes(x=date, y=value)) +
      geom_line() +
      theme_bw() +
      labs(x="",
           y="Heart rate (bpm)",
           title=paste0("From ", input$date_range[1], " to ", input$date_range[2]))
    g
  })

  output$heart_rate_table <- renderTable({
    heart_rate_ranged() %>%
      summarize(earliest_record    = as.character(min(date)),
                latest_record      = as.character(max(date)),
                highest_heart_rate = max(value),
                lowest_heart_rate  = min(value),
                measurements       = prettyNum(n(), big.mark=","))
  })
})

shinyApp(ui = ui, server = server)
