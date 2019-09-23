library(shiny)

# Plot output for File Import and Display App
plot_color_scatter <- function(input, output, reactive_dataset) {
  
  # Check that all the data is present
  # Using inputs simply because the table could be present, but not complete
  validate(
    need(input$datafile, "Please upload the data file(s)."),
    need(input$colorfile, "Please upload the color file(s).")
  )

  output$plot <- renderPlot(
    plot(reactive_dataset$table()$X, 
         log(reactive_dataset$table()$Y), 
         xlab = "X", ylab = "ln(Y)", 
         col = tolower(reactive_dataset$table()$Color), 
         pch = 16))
}

# Merge the color file into the dataset
merge_color_file <- function(input, reactive_dataset) {
  
  # Open file
  color_data <- read.csv(input$colorfile$datapath)
  
  # Add in color to full dataset via temp storage
  temp_data <- merge(reactive_dataset$table(), color_data, by = "SampleID")
  reactive_dataset$table <- reactive({temp_data})
}

# Main UI function
ui <- navbarPage(
  
  # Full app elements ---------------------------------------------------------
  
  # Allow for fluid page elements
  fluid = TRUE,
  
  # Main title
  title = "Fun with Shiny",
  
  # Simple Walking App UI ----------------------------------------------------
  
  tabPanel(
    title = "Simple Walking App",
  
    # Title
    fluidRow(
      column(12, 
             h1("Simple Walking App"))
    ),
    
    # Explanation of first attempt at something simple
    fluidRow(
      column(10,
             p("This is a very simple, first app. The following buttons",
               "should allow walking and resting,",
               "showing the distance walked and how much stamina is left.", 
               "Walking is impossible without stamina. Resting replenishes",
               "stamina."))
    ),
    
    # Buttons for walking and resting
    # Side-by-side if app is "wide" enough
    fluidRow(
      # Walk button
      column(1, 
             actionButton(inputId = "walk", label = "Walk")),
      # Rest button
      column(1,
             actionButton(inputId = "rest", label = "Rest"))
    ),
    
    # Output message with distance
    fluidRow(
      column(6,
             textOutput(outputId = "distance"))
    ),
    
    # Output message with stamina
    fluidRow(
      column(6, 
             textOutput(outputId = "stamina"))
    )
  ),

  # File Import & Display App -------------------------------------------------
  
  tabPanel(
    title = "File Import & Display App",
    
    # Title
    fluidRow(
      column(12, 
             h1("File Import & Display App"))
    ),
    
    # Explanation of first attempt at something simple
    fluidRow(
      column(10,
             p("This app takes in csv files, combines the information,",
               "and produces both a graphic and statistics on the",
               "information based on desired colors."),
             h3("Input Data Files"),
             p("There can be multiple input data files, but each is,",
               "expected to have the following column names: SampleID,",
               "X, Y."),
             h3("Color Data File"),
             p("Only one color data file is currently allowed. This file",
               "is expected to have the following column names: SampleID,",
               "Color. The SampleIDs from the color data file should",
               "match up with the SampleIDs from the input data files.")) 
    ),
    
    # File input for data
    fluidRow(
      column(4, 
             fileInput(inputId = "datafile", 
                       label = "Input data files", 
                       multiple = TRUE))
    ),
    
    # File input for color data
    fluidRow(
      column(4,
             fileInput(inputId = "colorfile", 
                       label = "Input Color Data File"))
    ),
    
    # Plot output
    fluidRow(
      column(6,
             plotOutput(outputId = "plot"))
    ),
    
    # Color input box
    fluidRow(
      column(4, 
             textInput(inputId = "color", 
                       label = "Input colors, separated by spaces"))
    ),
    
    # Button for color input action
    fluidRow(
      column(1,
             column(1, 
                    actionButton(inputId = "getstats", 
                                 label = "Get Statistics")))
    ),
    
    # Stats output based on color options
    fluidRow(
      column(10,
             tableOutput(outputId = "stats"))
    )
  )
)

server <- function(input, output) {

  # Beginning of Simple Walking App -------------------------------------------
  
  # Reactive values list for holding current stats for distance and stamina
  stats <- reactiveValues(distance = reactive(0), stamina = reactive(10))
  
  # Print the initial stats in the app
  output$distance <- renderText({
    paste0("Distance: ", stats$distance(), " steps")
    })
  output$stamina <- renderText({
    paste0("Stamina: ", stats$stamina(), " units")
    }) 
  
  # Action for walk button
  # If there is still stamina:
  #   increment distance and decrement stamina 
  #   print new values to app
  # else: 
  #   do nothing/ignore attempt to walk
  observeEvent(input$walk, {
    if (stats$stamina() > 0) {
      # Get old/current value of distance
      prev.distance <- stats$distance()
      # Increment distance value
      stats$distance <- reactive({prev.distance + 1})
      
      # Get old/current value of stamina
      prev.stamina <- stats$stamina()
      # Decrement stamina value
      stats$stamina <- reactive({prev.stamina - 1})
      
      # Print distance and stamina in app
      output$distance <- renderText({paste0("Distance: ", 
                                            stats$distance(), 
                                            " steps")})
      output$stamina <- renderText({paste0("Stamina: ", 
                                           stats$stamina(), 
                                           " units")})  
    } # else do nothing since no stamina left
  })
  
  # Action for rest button
  # Simply increment stamina and print new value to app
  # Note: Had this as percentage, but was annoying to test 100 clicks.
  #   Changed to units so it was easier. 
  observeEvent(input$rest, {
    # Get old/current value of stamina
    prev.stamina <- stats$stamina()
    # Increment stamina value
    stats$stamina <- reactive({prev.stamina + 1})
    
    # Print new stamina value in app
    output$stamina <- renderText({paste0("Stamina: ", 
                                         stats$stamina(), 
                                         " units")})  
  })
  
  # End Simple Walking App ----------------------------------------------------
  
  # Beginning of File Upload & Display App ------------------------------------
  
  # Reactive Value for the data
  data <- reactiveValues()
  
  # Action to take when data input files are uploaded
  # Should open each file and merge into a single table
  observeEvent(input$datafile, {
    
    # Open all the data files and combine into a single table
    # Assume all are of same style and will have no conflicts with how 
    #   I am merging these
    # Need to check number of files to ensure that there are no errors 
    #   if only one file is uploaded
    if (dim(input$datafile)[1] == 1) {
      # Only one file uploaded
      data$table <- reactive(read.csv(input$datafile$datapath))
    } else { # Should be 1 or more files, never less
      # Open first file into temporary storage
      main_temp_data <- read.csv(input$datafile$datapath[1])
      # Go through files starting with the second one and merge each 
      # into the main temp data
      for (i in 2:dim(input$datafile)[1]) {
        temp_data <- read.csv(input$datafile$datapath[i])
        main_temp_data <- merge(main_temp_data, temp_data, all = TRUE)
      }
      
      # Give data the full table 
      data$table <- reactive(main_temp_data)
      
      # Check if color data is available; if yes, merge into full table
      req(input$colorfile)
      
      merge_color_file(input, data)
      plot_color_scatter(input, output, data)
    }
    
  })
  
  # Action to take when color input file is uploaded
  # Should merge the color data into the full table from the data input upload
  # Assumes the data input files have been uploaded and correctly merged 
  #   before the color file is added
  observeEvent(input$colorfile, {
    
    # Make sure the data has been uploaded first
    validate(
      need(input$datafile, "Please upload data file(s) before the color file.")
    )
    
    # data$table should exist since created when input$datafile is uploaded
    merge_color_file(input, data)

    # Create scatterplot
    plot_color_scatter(input, output, data)

  })
  
  # Action to take when the getStats button is pressed
  # Should get color list from the color textbox and then give a 
  #   stats table display based on colors chosen
  observeEvent(input$getstats, {
    # Only do an action if colors have been put in the color textbox
    if (isTruthy(input$color)) {
      # Get the colors from the text input, separate them into a list of colors
      stat_colors <- as.list(strsplit(input$color, " "))[[1]]
      
      # Dataframe to hold the statistic values
      stats <- data.frame(matrix(nrow = length(stat_colors), ncol = 4))
      colnames(stats) <- c("Color", 
                           "Number of Observations", 
                           "Mean", 
                           "Standard Deviation")
      
      # Iterate through each color given, calculate statistics; save to table
      for (i in 1:length(stat_colors)) {
        # Save the current color to the table
        stats$Color[i] <- stat_colors[[i]]
        # Get indices from input data that correspond to the current color
        # Drop both to lowercase temporarily to account for case differences
        color.indices <- which(tolower(stats$Color[i]) == 
                                 tolower(data$table()$Color))
        # Check the number of indices found. If at least one index found, 
        #   do statistics and save to table,
        #   else save defaults to table.
        if (length(color.indices) > 0) {
          # Get the number of observations for the color and save to table
          stats$`Number of Observations`[i] <- length(color.indices)
          # Calculate the mean of Y values for the current color; save to table
          stats$Mean[i] <- mean(data$table()$Y[color.indices])
          # Calcuate Y value standard deviation for current color; save to table
          stats$`Standard Deviation`[i] <- sd(data$table()$Y[color.indices])
        } else {
          # Must not have been a color in the dataset. Give defaults to allow 
          #   user to see that the color wasn't present, rather than the 
          #   program having just skipped it
          # No observations/indices found; default = 0
          stats$`Number of Observations`[i] <- 0
          # Cannot calculate mean of no values; default = NA
          stats$Mean[i] <- NA
          # Cannot calculate standard deviation of no values; default = NA
          stats$`Standard Deviation`[i] <- NA
        }
      }
      # Send the table to the stats table output for display
      output$stats <- renderTable(stats)
    }
  })
  
  # End of File Upload & Display App ------------------------------------------
  
}

shinyApp(ui = ui, server = server)