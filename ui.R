library(shiny)
library(shinythemes)

ui <- shinyUI(fluidPage(
  theme = shinytheme(theme = "sandstone"),
  
  tabPanel("Swimmer_Comparison",
           h1("Swimmer Attributes"),
           sidebarLayout(
             sidebarPanel(
               pickerInput(
                 "Datasource", "Select Data to Use",
                 list("Example Data", "Upload Data"),
                 selected = "Example Data"
               ),
               uiOutput("file_upload"),
               
               selectInput(inputId = "sex",
                           label = "Sex of Swimmer", 
                           choices = c("Male", "Female")),
               selectInput(inputId =  "event", 
                           label = "Event of Swimmer", 
                           choices = c("50y Freestyle","100y Freestyle","200y Freestyle","500y Freestyle",
                                       "1650y Freestyle","100y Backstroke",
                                       "200y Backstroke","100y Breaststroke","200y Breaststroke",
                                       "100y Butterfly","200y Butterfly","200y IM","400y IM")),
               pickerInput(inputId = "place",
                           label = "Place of Swimmer",
                           choices = c("First", "Second", "Third", "Fourth", "Fifth", "Sixth", "Seventh", "Eighth"),
                           options = list((`actions-box` = TRUE), multiple = T, seletected = "First")),
               textInput("stroke_count",
                         "What is your Swimmer's stroke count every lap?", 
                         "10,11"),
               textInput("kick_count", 
                         "What is your Swimmer's underwater kick count every lap?", 
                         "7,9"),
               textInput("tempo", 
                         "What is your Swimmer's tempo every lap?", 
                         ".94, .87"),
               textInput("time", 
                         "What is your Swimmer's time (in seconds) every lap?", 
                         "9.36,8.64"),
               submitButton(text = "Submit")
             ),
             mainPanel(
               plotOutput(outputId = "graph_out", height = "auto")
             )
           )
  )
))

server <- function(input, output) {
  output$select_data <- renderUI({
    req(input$Datasource)
    if (input$Datasource == "Upload Data") {
      fileInput(
        "file", "Upload data",
        accept = c(
          ".csv"
        )
      )
    } 
  })
}

shinyApp(ui = ui, server = server)
