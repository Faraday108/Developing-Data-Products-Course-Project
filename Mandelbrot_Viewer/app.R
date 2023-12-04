source("mandelbrot_functions.R")

library(shiny)
library(gt)

ui <- fluidPage(tags$head(
  # Note the wrapping of the string in HTML()
  # Sets the formatting: background, text style and color, and 
  # horizontal rule color. 
  tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
      body {
        background-color: black;
        color: black;
      }
      h2 {
        font-family: 'Yusei Magic', sans-serif;
      }
      hr {
      border-top: 1px solid #000000;
      }
      .shiny-input-container {
        color: #474747;
      }"))
),

# Application title
titlePanel(h1("Mandelbrot Viewer", style = "color: white;")),

# Sidebar to change and view conditions of plot. 
sidebarLayout(
  sidebarPanel(
    h3("Instructions"),
    p("1. Click and drag on the plot to create a selection box"),
    p("2. Double click on box to zoom in on selected box"),
    hr(), 
    h3("Range of selected box"),
    tableOutput("zoominfo"), 
    hr(), 
    h3("Color setup"), 
    p("You can change the color of the simulation to any of the palettes 
      in RColorBrewer. The available palettes can be subsetted to colorblind friendly."),
    checkboxInput("colblind", "Colorblind Only", value = FALSE),
    selectInput("col", "Color Selection", 
                choices = row.names(brewer.pal.info), 
                selected = "RdBu")
  ),
  
  # Show a plot of the generated mandelbrot set
  mainPanel(
    plotOutput("Mandelbrot",
               dblclick = "plot_dblclick", 
               brush = brushOpts(
                 id = "plot_brush", 
                 resetOnNew = TRUE
               ))
  )
)
)

# Define server logic. 
server <- function(input, output, session) {
  # Initial values of mandelbrot set plot. 
  param <- reactiveValues(xmin = -2, xmax = .5, 
                          ymin = -1.2, ymax = 1.2, 
                          r = 2.5/2.4, 
                          width = 500, 
                          height = 480)
  # Further implementation could allow user to change default size
  # Left at 500 pixels to avoid excessive computation time. 
  size <- 500
  
  # Subset color options if colorblind friendly is selected. 
  outVar <- reactive({
    if(input$colblind) {
      row.names(brewer.pal.info[brewer.pal.info$colorblind == TRUE,])
    } else {
      row.names(brewer.pal.info)
    }
  })
  
  # Update color selection if colorblind friendly is selected. 
  observe({
    updateSelectInput(session, "col",
                      choices = outVar(), 
                      selected = input$col
    )})
  
  # Rendering of mandelbrot set, allowed to update if color selection changes
  output$Mandelbrot <- renderPlot(width = size, height = size, bg="transparent", {
    input$col
    brew_max_col <- brewer.pal.info[input$col, "maxcolors"]
    plt() + scale_fill_gradientn(colours = 
                                   c("black", brewer.pal(brew_max_col, input$col)))
  })
  
  # Reactive plot that updates when new range is chosen with brush
  plt <- reactive({
    # Include progress bar
    progress <- shiny::Progress$new()
    progress$set(message = "Computing Mandelbrot set", value = 0)
    on.exit(progress$close())
    
    updateProgress <- function(value = NULL, detail = NULL) {
      if(is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 3
      }
      progress$set(value = value, detail = detail)
    }
    
    # Run mandelbrot function with new brush x and y limits
    vector_mandelbrot2(xmin = param$xmin, 
                       xmax = param$xmax, 
                       ymin = param$ymin, 
                       ymax = param$ymax, 
                       xpixel = size, ypixel = size, 
                       updateProgress = updateProgress, 
                       plt_color = isolate(input$col))
  })
  
  # Event to change the x-values, y-values, and width/height of image
  observeEvent(input$plot_dblclick,{
    brush <- input$plot_brush
    tmp <- param
    
    # This checks if the double click event was paired with a brush selection
    if (!is.null(brush)) {
      tmp$xmin <- brush$xmin
      tmp$xmax <- brush$xmax
      tmp$ymin <- brush$ymax
      tmp$ymax <- brush$ymin
      tmp$r <- (brush$xmax - brush$xmin) / (brush$ymax - brush$ymin)
      
      if (tmp$r >= 1) {
        # is the image wide?
        tmp$width <- 500
        tmp$height <- 500/tmp$r
      } else {
        # is the image tall?
        tmp$width <- 500*tmp$r
        tmp$height <- 500
      }
      
      param <- tmp
    }
  })
  
  # Update the zoom table with the current brush selection. 
  output$zoominfo <- renderTable({
    if(!is.null(input$plot_brush)){
      data.frame("Range" = c("xmin", "xmax", "ymin", "ymax"),
                 "brush" = c(input$plot_brush$xmin, 
                             input$plot_brush$xmax, 
                             paste(input$plot_brush$ymax, "i"), 
                             paste(input$plot_brush$ymin, "i"))) %>%
        gt()} else {
          # Default NA values when nothing is selected. 
          data.frame("Range" = c("xmin", "xmax", "ymin", "ymax"), 
                     "Brush" = c(NA, NA, NA, NA))
        }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
