library(shiny)
library(ggplot2)
library(plotly)

# 1) READ & PREPARE THE DATA ---------------------------------------------------

# Make sure "positions.csv" has two columns of coordinates. Here, we rename them:
startpoints <- read.table("positions.csv", header = TRUE)
colnames(startpoints) <- c("x", "y")
startpoints <- startpoints * 100

# 2) DEFINE THE UI ------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Select Points to Add"),
  
  fluidRow(
    column(
      width = 8,  
      plotlyOutput("interactivePlot", height = "800px")
    ),
    column(
      width = 4,  
      h3("Controls"),
      actionButton("reset", "Reset Points"),
      # Let user toggle between Utility and Marginal Utility
      radioButtons("layerChoice", "Choose layer to display:",
                   choices = c("Utility" = "utility",
                               "Marginal Utility" = "marginal"),
                   selected = "utility"),
      h3("Selected Points"),
      verbatimTextOutput("selectedPoints")
    )
  ),
  
  # Show total utility at bottom-right
  absolutePanel(
    bottom = 10, right = 10,
    h4("Total Utility"),
    verbatimTextOutput("totalUtility")
  )
)

# 3) DEFINE SERVER LOGIC ------------------------------------------------------
server <- function(input, output, session) {
  
  # ---- 3A) "TREES" REACTIVE VALUE ----
  D <- reactiveVal(startpoints)
  
  # ---- 3B) BUILD A REGULAR GRID ----
  resolution <- 20
  grid <- expand.grid(
    x = seq(0, 100, length.out = resolution),
    y = seq(0, 100, length.out = resolution)
  )
  
  # ---- 3C) POINT-LEVEL UTILITY FUNCTION FOR THE GRID ----
  # U(p, trees): "utility" for a single point p, based on its distance to existing trees.
  U <- function(p, trees) {
    px <- as.numeric(p["x"])
    py <- as.numeric(p["y"])
    d  <- sqrt((trees$x - px)^2 + (trees$y - py)^2)
    d  <- d[!is.na(d)]
    d  <- sort(d)
    if (length(d) == 0) return(0)
    d  <- head(d, 5)
    mu <- mean(d)
    min(mu, 50)
  }
  
  # ---- 3D) TREE-SPECIFIC UTILITY (EXCLUDING SELF) ----
  # This is used to compute how "useful" each tree is in the context of all the others.
  compute_tree_utility <- function(p, trees) {
    px <- as.numeric(p["x"])
    py <- as.numeric(p["y"])
    d  <- sqrt((trees$x - px)^2 + (trees$y - py)^2)
    d  <- d[!is.na(d)]
    d  <- d[d > 0]  # exclude self-distance (0)
    if (length(d) == 0) return(0)
    d  <- sort(d)
    d  <- head(d, 5)
    mu <- mean(d)
    min(mu, 50)
  }
  
  # ---- 3E) TOTAL UTILITY FOR A SET OF TREES ----
  compute_total_utility <- function(trees) {
    if(nrow(trees) == 0) return(0)
    sum(apply(trees, 1, function(row) {
      compute_tree_utility(row, trees)
    }))
  }
  
  # ---- 3F) BASELINE TREE UTILITIES AND TOTAL UTILITY ----
  # For each tree, compute tree-specific utility and store it in the "utility" column.
  tree_utilities <- reactive({
    trees <- D()
    if(nrow(trees) == 0) {
      return(data.frame(x = numeric(0), y = numeric(0), utility = numeric(0)))
    }
    util <- apply(trees, 1, function(row) {
      compute_tree_utility(row, trees)
    })
    trees$utility <- round(util, 1)
    trees
  })
  
  # ---- 3G) SUM OF ALL TREE UTILITIES ----
  totalUtility <- reactive({
    compute_total_utility(D())
  })
  
  # ---- 3H) GRID: UTILITY VS. MARGINAL UTILITY ----
  # We'll build a single data frame that has both `utility` and `marginal_utility` columns.
  #    - utility: the same background as before (U for each grid cell)
  #    - marginal_utility: how much total utility would change if we add a new tree at that cell
  compute_grid_values <- reactive({
    trees <- D()
    
    # 1) Utility layer
    g <- grid
    g$utility <- apply(g, 1, U, trees = trees)
    
    # 2) Marginal utility
    baseline <- compute_total_utility(trees)
    
    # For each grid cell, add a tree at (x,y), re-calc total, subtract baseline
    g$marginal_utility <- apply(g[, c("x","y")], 1, function(pt) {
      # pt is c(x=?, y=?)
      # "Add" new tree
      Tplus <- rbind(trees, pt)
      # The difference in total utility
      compute_total_utility(Tplus) - baseline
    })
    g
  })
  
  # ---- 3I) OUTPUT: PLOTLY PLOT ----
  output$interactivePlot <- renderPlotly({
    # We get the grid with both layers
    data_grid <- compute_grid_values()
    # We get the existing trees with their own per-tree utility
    tree_util <- tree_utilities()
    
    # Decide which layer to show
    layer_choice <- input$layerChoice
    if(layer_choice == "marginal") {
      fill_col <- "marginal_utility"
      fill_vals <- data_grid$marginal_utility
    } else {
      fill_col <- "utility"
      fill_vals <- data_grid$utility
    }
    
    # Build ggplot with the chosen fill
    p <- ggplot(data_grid, aes(x = x, y = y, fill = .data[[fill_col]])) +
      geom_tile() +
      geom_point(data = tree_util, aes(x = x, y = y),
                 size = 3, inherit.aes = FALSE) +
      scale_fill_gradient2(
        low = "red", mid = "yellow", high = "green",
        midpoint = max(fill_vals, na.rm = TRUE)/2
      ) +
      coord_fixed() +
      theme_void() +
      theme(legend.position = "none")
    
    pl <- ggplotly(p, source = "select_points")
    
    # Annotate each tree with its utility (the same regardless of layer choice).
    annots <- lapply(seq_len(nrow(tree_util)), function(i) {
      list(
        x = tree_util$x[i],
        y = tree_util$y[i],
        text = as.character(tree_util$utility[i]),
        showarrow = FALSE,
        xanchor = "center",
        yanchor = "bottom",
        yshift = 10,  # offset the label upward
        font = list(color = "black", size = 10)
      )
    })
    
    layout(pl, annotations = annots)
  })
  
  # ---- 3J) OUTPUT: TOTAL UTILITY ----
  output$totalUtility <- renderPrint({
    totalUtility()
  })
  
  # ---- 3K) USER CLICKS -> ADD NEW TREE ----
  observeEvent(event_data("plotly_click", source = "select_points"), {
    click <- event_data("plotly_click", source = "select_points")
    if (!is.null(click)) {
      new_point <- data.frame(x = as.numeric(click$x), y = as.numeric(click$y))
      D(rbind(D(), new_point))
    }
  })
  
  # ---- 3L) SHOW SELECTED POINTS ----
  output$selectedPoints <- renderPrint({
    D()
  })
  
  # ---- 3M) RESET ----
  observeEvent(input$reset, {
    D(startpoints)
  })
}

# 4) RUN THE SHINY APP --------------------------------------------------------
shinyApp(ui = ui, server = server)
