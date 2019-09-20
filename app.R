library(shiny)

ui <- navbarPage(
  
  ###### Full app elements ######
  
  # Allow for fluid page elements
  fluid = TRUE,
  
  # Main title
  title = "Fun with Shiny",
  
  ###### Simple Walking App UI ######
  
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
             p("This is a very simple, first app. The following buttons should allow walking and resting,",
               "showing the distance walked and how much stamina is left.", 
               "Walking is impossible without stamina. Resting replenishes stamina."))
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

  ###### File Import & Display App ######
  
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
             p("This app takes in csv files, combines the information, and produces both a graphic and statistics on the information based on desired colors."),
             h3("Input Data Files"),
             p("There can be multiple input data files, but each is expected to have the following column names: SampleID, X, Y."),
             h3("Color Data File"),
             p("Only one color data file is currently allowed. This file is expected to have the following column names: SampleID, Color.",
               "The SampleIDs from the color data file should match up with the SampleIDs from the input data files."),
             h4("Note:"),
             p("The file upload may not allow for changing/adding/removing files after first upload.",
                "Additionally, the color file is expected to be uploaded after data files.")) 
    ),
    
    # File input for data
    fluidRow(
      column(4, 
             fileInput(inputId = "datafile", label = "Input data files", multiple = TRUE))
    ),
    
    # File input for color data
    fluidRow(
      column(4,
             fileInput(inputId = "colorfile", label = "Input Color Data File"))
    ),
    
    # Plot output
    fluidRow(
      column(6,
             plotOutput(outputId = "plot"))
    ),
    
    # Color input box
    fluidRow(
      column(4, 
             textInput(inputId = "color", label = "Input colors, separated by spaces"))
    ),
    
    # Button for color input action
    fluidRow(
      column(1,
             column(1, 
                    actionButton(inputId = "getstats", label = "Get Statistics")))
    ),
    
    # Stats output based on color options
    fluidRow(
      column(10,
             tableOutput(outputId = "stats"))
    )
  )
)

server <- function(input, output) {

  ###### Beginning of Simple Walking App ######
  
  # Reactive values list for holding current stats for distance and stamina
  stats <- reactiveValues(distance = reactive(0), stamina = reactive(10))
  
  # Print the initial stats in the app
  output$distance <- renderText({paste0("Distance: ", stats$distance(), " steps")})
  output$stamina <- renderText({paste0("Stamina: ", stats$stamina(), " units")}) 
  
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
      output$distance <- renderText({paste0("Distance: ", stats$distance(), " steps")})
      output$stamina <- renderText({paste0("Stamina: ", stats$stamina(), " units")})  
    } # else do nothing since no stamina left
  })
  
  # Action for rest button
  # Simply increment stamina and print new value to app
  # Note: Originally had this as percentage, but was annoying to test 100 clicks.
  #   Changed to units so it was easier. 
  observeEvent(input$rest, {
    # Get old/current value of stamina
    prev.stamina <- stats$stamina()
    # Increment stamina value
    stats$stamina <- reactive({prev.stamina + 1})
    
    # Print new stamina value in app
    output$stamina <- renderText({paste0("Stamina: ", stats$stamina(), " units")})  
  })
  
  ###### End Simple Walking App ######
  
  ###### Beginning of File Upload & Display App ######
  
  # Reactive Value for the data
  data <- reactiveValues()
  
  # Action to take when data input files are uploaded
  # Should open each file and merge into a single table
  observeEvent(input$datafile, {
    
    # Open all the data files and combine into a single table
    # Assume all are of same style and will have no conflicts with how I am merging these
    # Need to check number of files to ensure that there are no errors if only one file is uploaded
    if (dim(input$datafile)[1] == 1) {
      # Only one file uploaded
      data$table <- reactive(read.csv(input$datafile$datapath))
    } else { # If observeEvent activated, then there should be 1 or more files, never less
      # Open first file into temporary storage
      mainTempData <- read.csv(input$datafile$datapath[1])
      # Go through files starting with the second one and merge each into the main temp data
      for (i in 2:dim(input$datafile)[1]) {
        tempData <- read.csv(input$datafile$datapath[i])
        mainTempData <- merge(mainTempData, tempData, all = TRUE)
      }
      
      # Give data the full table 
      data$table <- reactive(mainTempData)
    }
    
  })
  
  # Action to take when color input file is uploaded
  # Should merge the color data into the full table from the data input upload
  # Currently assumes the data input files have been uploaded and correctly merged before the color file is added
  observeEvent(input$colorfile, {
    
    # Open file
    colorData <- read.csv(input$colorfile$datapath)
    
    # Add in color to full dataset via temp storage
    tempData <- merge(data$table(), colorData, by="SampleID")
    data$table <- reactive(tempData)

    
    #data$table <- reactive(read.csv(input$file$datapath))
    output$plot <- renderPlot(
      plot(data$table()$X, log(data$table()$Y), xlab="X", ylab="ln(Y)", col=tolower(data$table()$Color), pch=16))
  })
  
  # Action to take when the getStats button is pressed
  # Should get color list from the color textbox and then give a stats table display based on colors chosen
  observeEvent(input$getstats, {
    # Only do an action if colors have been put in the color textbox
    if (isTruthy(input$color)) {
      # Get the colors from the text input, separate them into a list of colors
      statColors <- as.list(strsplit(input$color, " "))[[1]]
      
      # Dataframe to hold the statistic values
      stats <- data.frame(matrix(nrow=length(statColors), ncol=4))
      colnames(stats) <- c("Color", "Number of Observations", "Mean", "Standard Deviation")
      
      # Iterate through each color given, calculate statistics and save to table
      for (i in 1:length(statColors)) {
        # Save the current color to the table
        stats$Color[i] <- statColors[[i]]
        # Get indices from input data that correspond to the current color
        # Drop both to lowercase temporarily to account for case differences
        color.indices <- which(tolower(stats$Color[i]) == tolower(data$table()$Color))
        # Check the number of indices found. If at least one index found, do statistics and save to table,
        # else save defaults to table.
        if (length(color.indices) > 0) {
          # Get the number of observations for the color and save to table
          stats$`Number of Observations`[i] <- length(color.indices)
          # Calculate the mean of Y values for the current color and save to table
          stats$Mean[i] <- mean(data$table()$Y[color.indices])
          # Calcuate Y value standard deviation for current color and save to table
          stats$`Standard Deviation`[i] <- sd(data$table()$Y[color.indices])
        } else {
          # Must not have been a color in the dataset. Give defaults to allow user to see that the
          # color wasn't present, rather than the program having just skipped it
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
  
  ###### End of File Upload & Display App ######
  
}

shinyApp(ui = ui, server = server)