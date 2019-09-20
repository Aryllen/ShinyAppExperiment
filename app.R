library(shiny)

ui <- navbarPage(
  # Allow for fluid page elements
  fluid = TRUE,
  
  # Main title
  title = "Fun with Shiny",
  
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
             p("Let's try something really simple. The following buttons should allow walking and resting,",
               " showing the distance walked and how much stamina is left.", 
               " Walking is impossible without stamina. Resting replenishes stamina."))
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

  tabPanel(
    title = "File Import & Display App",
    
    # Title
    fluidRow(
      column(12, 
             h1("Nicole's Second Shiny App"))
    ),
    
    # Explanation of first attempt at something simple
    fluidRow(
      column(10,
             p("This app takes in four files, combines the information, and produces a graphic and stats on the information."))
    ),
    
    # File input
    fluidRow(
      column(4, 
             fileInput(inputId = "file", label = "Input data"))
    ),
    
    # Plot output
    fluidRow(
      column(6,
             plotOutput(outputId = "plot"))
    ),
    
    # Color input
    fluidRow(
      column(3, 
             textInput(inputId = "color", label = "Input colors, separated by spaces")),
      
      column(1, 
             actionButton(inputId = "getstats", label = "Get Statistics"))
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
  
  # Action to take when an input file is uploaded
  observeEvent(input$file, {
    data$table <- reactive(read.csv(input$file$name))
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