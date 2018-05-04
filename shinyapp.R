#load packages
library(shinydashboard)
library(shiny)
library(leaflet)

## ui.R ##
ui <- fluidPage(
  titlePanel("Dynamically generated user interface components"),
  fluidRow(
    
    column(3, wellPanel(
      selectInput("input_type", "Restaurant type",
                  c("existing restaurant", "new restaurant"
                  )
      )
    )),
    
    column(3, wellPanel(
      # This outputs the dynamic UI component
      uiOutput("ui")
    )),
    
    column(3,
           tags$p("Input type:"),
           verbatimTextOutput("input_type_text"),
           tags$p("Dynamic input value:"),
           verbatimTextOutput("dynamic_value")
    )
  )
)

server <- function(input, output) {
  
  output$ui <- renderUI({
    if (is.null(input$input_type))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type,
           
           "existing restaurant" = checkboxGroupInput("dynamic", "What variables do we want to look at?(check all apply)",
                                                choices = c("Delivery" = "delivery",
                                                            "Parking" = "parking",
                                                            "Reservation" = "reservation")
           ),
           "new restaurant" = checkboxGroupInput("dynamic", "What variables do we want to look at?(check all apply)",
                                                 choices = c("Delivery" = "delivery",
                                                             "Parking" = "parking",
                                                             "Reservation" = "reservation")
           ),
           
           "radioButtons" = radioButtons("dynamic", "Dynamic",
                                         choices = c("Option 1" = "option1",
                                                     "Option 2" = "option2"),
                                         selected = "option2"
           ),
           "selectInput" = selectInput("dynamic", "Dynamic",
                                       choices = c("Option 1" = "option1",
                                                   "Option 2" = "option2"),
                                       selected = "option2"
           ),
           "selectInput (multi)" = selectInput("dynamic", "Dynamic",
                                               choices = c("Option 1" = "option1",
                                                           "Option 2" = "option2"),
                                               selected = c("option1", "option2"),
                                               multiple = TRUE
           ),
           "date" = dateInput("dynamic", "Dynamic"),
           "daterange" = dateRangeInput("dynamic", "Dynamic")
    )
  })
  
  output$input_type_text <- renderText({
    input$input_type
  })
  
  output$dynamic_value <- renderPrint({
    str(input$dynamic)
  })
  
}

shinyApp(ui, server)