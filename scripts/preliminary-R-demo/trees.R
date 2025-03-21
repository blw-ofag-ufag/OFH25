library(shiny)
library(ggplot2)
library(plotly)

# Define the UI
ui <- fluidPage(
  titlePanel("Select Points to Add"),
  fluidRow(
    column(
      width = 8,  # Wide column for the plot
      plotlyOutput("interactivePlot", height = "800px")
    ),
    column(
      width = 4,  # Narrow column for controls and selected points
      h3("Controls"),
      actionButton("reset", "Reset Points"),
      h3("Selected Points"),
      verbatimTextOutput("selectedPoints")
    )
  )
)

startpoints = read.table("positions.csv")

# Define the server logic
server <- function(input, output, session) {
  # Initial data
  D <- reactiveVal(startpoints)
  
  # Define the grid resolution
  resolution <- 100
  x_seq <- seq(0, 1, length.out = resolution)
  y_seq <- seq(0, 1, length.out = resolution)
  grid <- expand.grid(x = x_seq, y = y_seq)
  
  # Function to compute distances
  compute_distances <- function(grid, points) {
    grid$min_dist <- apply(grid, 1, function(p) {
      z = sqrt((points$x - p[1])^2 + (points$y - p[2])^2)
      min(mean(head(sort(z),5)),abs(1-min(z)))
    })
    return(grid)
  }
  
  # Render plot
  output$interactivePlot <- renderPlotly({
    # Compute distances based on current points
    updated_grid <- compute_distances(grid, D())
    
    # Create the ggplot
    p <- ggplot(updated_grid, aes(x = x, y = y, fill = min_dist)) +
      geom_tile() +
      geom_point(data = D(), aes(x = x, y = y), size = 3, inherit.aes = FALSE) +
      scale_fill_gradient2(low = "red", mid = "yellow", high = "green", 
                           midpoint = max(updated_grid$min_dist) / 2) +
      coord_fixed() +
      theme_void() +
      theme(legend.position = "none")
    
    # Convert ggplot to plotly for interactivity
    ggplotly(p, source = "select_points")
  })
  
  # Capture clicked points
  observeEvent(event_data("plotly_click", source = "select_points"), {
    clicked <- event_data("plotly_click", source = "select_points")
    if (!is.null(clicked)) {
      # Add the new point to the data
      new_point <- data.frame(x = clicked$x, y = clicked$y)
      D(rbind(D(), new_point))
    }
  })
  
  # Display selected points
  output$selectedPoints <- renderPrint({
    D()
  })
  
  # Reset button
  observeEvent(input$reset, {
    D(startpoints)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
