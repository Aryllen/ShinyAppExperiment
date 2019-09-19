library(shiny)

ui <- fluidPage(

  # Title
  fluidRow(
    column(12, 
           h1("Nicole's First Shiny App"))
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
)

server <- function(input, output) {

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
}

shinyApp(ui = ui, server = server)