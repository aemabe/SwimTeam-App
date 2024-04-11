
shinyServer(function(input, output, session) {
  
  output$file_upload <- renderUI({
    if (input$Datasource == "Upload Data") {
      fileInput("file", "Upload Data", accept = c(".csv"))
    } else {
      NULL
    }
  })
  
  df_example <- reactive({
    if (input$Datasource == "Example Data") {
      if(input$sex == "Male"){
      swim11<-read.csv("swim11.csv")
      message(glue::glue('[{format(Sys.time(),"%F %T")}] >',
                         'Loading of the sample data'))
      return(swim11)} else {
        swim13<-read.csv("swim13.csv")
        message(glue::glue('[{format(Sys.time(),"%F %T")}] >',
                           'Loading of the sample data'))
        return(swim13)
      }
    } else if (input$Datasource == "Upload Data" && !is.null(input$file)) {
      infile <- input$file
      upload <- as.data.frame(read_csv(infile$datapath))
      upload[] <- lapply(upload, function(x) gsub("\u00A0", "", x))
      upload[] <- lapply(upload, type.convert, as.is = TRUE)
      message(glue::glue('[{format(Sys.time(),"%F %T")}] >',
                         'Uploading of an external file'))
      upload <- upload[!apply(is.na(upload), 1, all),]
      return(upload)
    } else {
      return(NULL)
  }
    })
  
  
dataInput <- reactive({
  req(input$sex, input$event, input$stroke_count, input$kick_count, input$tempo, input$time)
  
  sex <- input$sex
  event <- input$event
  Stroke.Count <- as.numeric(unlist(strsplit(input$stroke_count, ",")))
  Lap.Number <- as.numeric(1:length(Stroke.Count))
  Kick.Count <- as.numeric(unlist(strsplit(input$kick_count, ",")))
  tempo <- as.numeric(unlist(strsplit(input$tempo, ",")))
  time <- as.numeric(unlist(strsplit(input$time, ",")))
  
  return(data.frame(Swimmer = sex, Event = event, Stroke.Count = Stroke.Count, 
                    Lap.Number = Lap.Number, Kick.Count = Kick.Count, 
                    Tempo = tempo, Time = time))
})


  place_numeric <- reactive({
    switch(input$place,
           "First" = 1,
           "Second" = 2,
           "Third" = 3,
           "Fourth" = 4,
           "Fifth" = 5,
           "Sixth" = 6,
           "Seventh" = 7,
           "Eighth" = 8)
  }) 
  
  filtered_data <- reactive({
    if (!is.null(df_example())) {
      df_example_filtered <- subset(df_example(), Event == input$event & Swimmer == place_numeric())
      return(df_example_filtered)
    } else {
      return(NULL)
    }
  })
  # Define a function to create the plot
  create_plot <- function(dataInput, df_example, y_var, variable_name) {
    ggplot() +
      geom_line(data = dataInput, aes(x = Lap.Number, y = !!sym(y_var), color = "User Input")) +
      geom_point(data = dataInput, aes(x = Lap.Number, y = !!sym(y_var), color = "User Input")) +
      geom_line(data = df_example, aes(x = Lap.Number, y = !!sym(y_var), color = "Example Data", linetype = "Example Data")) +
      geom_point(data = df_example, aes(x = Lap.Number, y = !!sym(y_var), color = "Example Data")) +
      scale_color_manual(values = c("lightblue", "deeppink3")) +
      scale_linetype_manual(values = c("solid")) +
      labs(title = paste("Men", unique(dataInput$Event), "-", variable_name),
           x = "Lap",
           y = variable_name,
           color = "Source") +
      theme_minimal()
  }
  
  # Render combined plot
  output$graph_out <- renderPlot({
    if (!is.null(dataInput()) && !is.null(filtered_data())) {
      plot_list <- lapply(c("Stroke.Count", "Kick.Count", "Tempo", "Time"), function(var) {
        create_plot(dataInput(), filtered_data(), var, var)
      })
      
      # Combine individual plots into a single plot with shared axes and legends
      combined_plot <- wrap_plots(plotlist = plot_list, ncol = 1)
      
      return(combined_plot)
    }
  }, height = function(){
    session$clientData$output_graph_out_width
  })
  
  
})