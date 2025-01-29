
shinyServer(function(input, output, session) {
  
  output$file_upload <- renderUI({
    if (input$Datasource == "Upload Data") {
      fileInput("file", "Upload Data", accept = c(".csv"))
    } else {
      NULL
    }
  })
  
  df_example <- reactive({
    if (input$Datasource == "2023 NCAA Data") {
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
      print(infile$datapath)
      upload <- as.data.frame(read_csv(infile$datapath))
      upload[] <- lapply(upload, function(x) gsub("\u00A0", "", x))
      upload[] <- lapply(upload, type.convert, as.is = TRUE)
      message(glue::glue('[{format(Sys.time(),"%F %T")}] >',
                         'Uploading of an external file'))
      upload <- upload[!apply(is.na(upload), 1, all),]
      rename_col <- function(data){
        names(data) <- gsub("Lap Number", "Lap.Number", names(data))
        names(data) <- gsub("Stroke Count", "Stroke.Count", names(data))
        names(data) <- gsub("Kick Count", "Kick.Count", names(data))
        return(data)
      }
      upload <- rename_col(upload)
      return(upload)
    } else {
      return(NULL)
  }
    })
  
  filtered_data1 <- reactive({
    if (!is.null(df_example())) {
      df_example_filtered <- subset(df_example(), Event == input$event)
      return(df_example_filtered)
    } else {
      return(NULL)
    }
  })
  
  plot_numeric <- reactive({
    if (!is.null(input$plot)) {
      numeric_places1 <- c()
      for (plot in input$plot) {
        numeric_places1 <- c(numeric_places1, switch(plot,
                                                   "Stroke Count" = "Stroke.Count",
                                                   "Kick Count" = "Kick.Count",
                                                   "Tempo" = "Tempo",
                                                   "Time" = "Time"))
      }
      return(unique(numeric_places1))
    } else {
      return(NULL)
    }
  })
  
  plot1_numeric <- reactive({
    if (!is.null(input$plot2)) {
      numeric_places2 <- c()
      for (plot in input$plot2) {
        numeric_places2 <- c(numeric_places2, switch(plot,
                                                    "Stroke Count" = "Stroke.Count",
                                                    "Kick Count" = "Kick.Count",
                                                    "Tempo" = "Tempo",
                                                    "Time" = "Time"))
      }
      return(unique(numeric_places2))
    } else {
      return(NULL)
    }
  })
  
  create_plot1 <- function(data, y_var, variable_name) {
    ggplot(data = data, aes(x=Lap.Number, y=!!sym(y_var), color = factor(Swimmer))) +
      geom_line() + geom_point(aes(shape = factor(Swimmer))) +
      scale_shape_manual(values=1:8) + labs(title = paste(input$sex, unique(data$Event), "-", variable_name),
                                            x="Lap",
                                            y=variable_name,
                                            color = "Swimmer",
                                            shape = "Swimmer") + theme_minimal()
  }
  
  # Render combined plot
  output$graph_out1 <- renderPlot({
    if (!is.null(filtered_data1())) {
      plot_list1 <- lapply(plot_numeric(), function(var) {
        create_plot1(filtered_data1(), var, var)
      })
      
      # Combine individual plots into a single plot with shared axes and legends
      combined_plot1 <- wrap_plots(plotlist = plot_list1, ncol = 1, guides = "collect")
      
      return(combined_plot1)
    }
  }, height = function(){
    session$clientData$output_graph_out_width
  })   
  
dataInput <- reactive({
  #req(input$event, input$stroke_count, input$kick_count, input$tempo, input$time)
  
  sex <- input$sex
  event <- input$event
  Stroke.Count <- as.numeric(unlist(strsplit(input$stroke_count, ",")))
  Lap.Number <- as.numeric(1:length(Stroke.Count))
  Kick.Count <- as.numeric(unlist(strsplit(input$kick_count, ",")))
  tempo <- as.numeric(unlist(strsplit(input$tempo, ",")))
  time <- as.numeric(unlist(strsplit(input$time, ",")))
  Swimmer <- "swimmer input"
  
  return(data.frame(Swimmer = Swimmer, Event = event, Stroke.Count = Stroke.Count, 
                    Lap.Number = Lap.Number, Kick.Count = Kick.Count, 
                    Tempo = tempo, Time = time))
})


place_numeric <- reactive({
  if (!is.null(input$place)) {
    numeric_places <- c()
    for (place in input$place) {
      numeric_places <- c(numeric_places, switch(place,
                                                 "First" = 1,
                                                 "Second" = 2,
                                                 "Third" = 3,
                                                 "Fourth" = 4,
                                                 "Fifth" = 5,
                                                 "Sixth" = 6,
                                                 "Seventh" = 7,
                                                 "Eighth" = 8))
    }
    return(unique(numeric_places))
  } else {
    return(NULL)
  }
})
  
  filtered_data <- reactive({
    if (!is.null(df_example())) {
      df_example_filtered <- subset(df_example(), Event == input$event & Swimmer %in% place_numeric())
      return(df_example_filtered)
    } else {
      return(NULL)
    }
  })
  # Define a function to create the plot
  create_plot <- function(dataInput, df_example, y_var, variable_name) {
    ggplot() +
      geom_line(data = dataInput, aes(x = Lap.Number, y = !!sym(y_var), color = factor(Swimmer))) +
      geom_point(data = dataInput, aes(x = Lap.Number, y = !!sym(y_var), color = factor(Swimmer))) +
      geom_line(data = df_example, aes(x = Lap.Number, y = !!sym(y_var), color = factor(Swimmer))) +
      geom_point(data = df_example, aes(x = Lap.Number, y = !!sym(y_var), color = factor(Swimmer))) +
      labs(title = paste(input$sex, unique(dataInput$Event), "-", variable_name),
           x = "Lap",
           y = variable_name,
           color = "Swimmer") +
      theme_minimal()
  }
  
  # Render combined plot
  output$graph_out <- renderPlot({
    if (!is.null(dataInput()) && !is.null(filtered_data())) {
      plot_list <- lapply(plot1_numeric(), function(var) {
        create_plot(dataInput(), filtered_data(), var, var)
      })
      
      # Combine individual plots into a single plot with shared axes and legends
      combined_plot <- wrap_plots(plotlist = plot_list, ncol = 1, guides = "collect")
      
      return(combined_plot)
    }
  }, height = function(){
    session$clientData$output_graph_out_width
  })
  
  
})