library(shiny)
library(ggplot2)
library(plotly)
library(raster)

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

startpoints <- read.table("positions.csv", header = TRUE) * 100

# Define the server logic
server <- function(input, output, session) {
  
  # Initial data
  D <- reactiveVal(startpoints)
  
  # Define the grid resolution
  resolution <- 100
  grid <- expand.grid(x = seq(0, 100, length.out = resolution),
                      y = seq(0, 100, length.out = resolution))
  
  # Updated utility function that accepts a vector p with named components x and y
  U <- function(p, trees) {
    
    # compute the distance from p to each tree position
    d <- sqrt((trees$x - p["x"])^2 + (trees$y - p["y"])^2)
    
    # sort d to have the closest distances first
    d <- sort(d)
    
    # pick the first n:5 elements of d (where n in [1,5])
    d <- head(d, 5)
    
    # calculate the mean distance among the five closest trees
    mu <- mean(d)
    
    # the utility has a upper limit
    min(mu, 50)
  }
  
  # Function to compute distances for each point in the grid
  compute_distances <- function(grid, trees, U) {
    grid$utility <- apply(grid, 1, U, trees = trees)
    return(grid)
  }
  
  # Temporary visualization of the computed grid values
  # G <- compute_distances(grid, startpoints, U)
  # colnames(G) <- c("x", "y", "z")
  # x_vals <- sort(unique(G$x))
  # y_vals <- sort(unique(G$y))
  # z_matrix <- matrix(G$z, nrow = length(y_vals), ncol = length(x_vals), byrow = TRUE)
  # image(x_vals, y_vals, z_matrix, col = terrain.colors(100),
  #       xlab = "x", ylab = "y", main = "Image Plot of z")
  # points(startpoints)
  
  # Render interactive plot with plotly
  output$interactivePlot <- renderPlotly({
    updated_grid <- compute_distances(grid, D(), U)
    
    p <- ggplot(updated_grid, aes(x = x, y = y, fill = utility)) +
      geom_tile() +
      geom_point(data = D(), aes(x = x, y = y), size = 3, inherit.aes = FALSE) +
      scale_fill_gradient2(low = "red", mid = "yellow", high = "green", 
                           midpoint = max(updated_grid$utility) / 2) +
      coord_fixed() +
      theme_void() +
      theme(legend.position = "none")
    
    ggplotly(p, source = "select_points")
  })
  
  # Capture clicked points and add them to the data
  observeEvent(event_data("plotly_click", source = "select_points"), {
    clicked <- event_data("plotly_click", source = "select_points")
    if (!is.null(clicked)) {
      new_point <- data.frame(x = clicked$x, y = clicked$y)
      D(rbind(D(), new_point))
    }
  })
  
  # Display selected points
  output$selectedPoints <- renderPrint({
    D()
  })
  
  # Reset button to restore the initial points
  observeEvent(input$reset, {
    D(startpoints)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
