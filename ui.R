library(shiny)
library(shinythemes)
library(shinyWidgets)
ui <- shinyUI(fluidPage(
  theme = shinytheme(theme = "lumen"),
  tabsetPanel(
  tabPanel("Top Swimmers",
           h1("Select Swimmers"),
           sidebarLayout(
             sidebarPanel(
               pickerInput(
                 "Datasource", "Select Data to Use",
                 list("2023 NCAA Data", "Upload Data"),
                 selected = "2023 NCAA Data"
               ),
               uiOutput("file_upload"),
               selectizeInput(inputId = "plot",
                           label = "What plot(s) would you like to view?",
                           choices = c("Stroke Count", "Kick Count", "Tempo", "Time"),
                           multiple = T, selected = c("Stroke Count","Kick Count", "Tempo", "Time")),
               selectInput(inputId = "sex",
                           label = "Sex of Swimmer", 
                           choices = c("Male", "Female")),
               selectInput(inputId =  "event", 
                           label = "Event of Swimmer", 
                           choices = c("50y Freestyle","100y Freestyle","200y Freestyle","500y Freestyle",
                                       "1650y Freestyle","100y Backstroke",
                                       "200y Backstroke","100y Breaststroke","200y Breaststroke",
                                       "100y Butterfly","200y Butterfly","200y IM","400y IM"),
                           selected = "200y Freestyle"),
               submitButton(text = "Submit")
             ),
             mainPanel(
               plotOutput(outputId = "graph_out1", height = "auto")
             )
           )
  ),
  tabPanel("Swimmer Comparison",
           h1("Swimmer Attributes"),
           sidebarLayout(
             sidebarPanel(
               selectizeInput(inputId = "place",
                           label = "Place of Top Swimmer",
                           choices = c("First", "Second", "Third", "Fourth", "Fifth", "Sixth", "Seventh", "Eighth"),
                           multiple = T, selected = "First"),
               selectizeInput(inputId = "plot2",
                           label = "What comparison(s) are you looking for?",
                           choices = c("Stroke Count", "Kick Count", "Tempo", "Time"),
                           multiple = T, selected = c("Stroke Count","Kick Count", "Tempo", "Time")),
               textInput("stroke_count",
                         "What is your Swimmer's stroke count every lap?", 
                         "9,13,13,12,13,13,14,16"),
               textInput("kick_count", 
                         "What is your Swimmer's underwater kick count every lap?", 
                         "4,3,3,4,3,5,5,7"),
               textInput("tempo", 
                         "What is your Swimmer's tempo every lap?", 
                         "1.2, 1.4,1.35,1.22,1.24,1.26,1.37,1.49"),
               textInput("time", 
                         "What is your Swimmer's time (in seconds) every lap?", 
                         "9.89,11.24,11.49,12.02,12.04,12.42,11.39,11.07"),
               submitButton(text = "Submit") 
             ),
             mainPanel(
               plotOutput(outputId = "graph_out", height = "auto")
)
)
)
)
)
)
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
