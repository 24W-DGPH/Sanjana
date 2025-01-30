# Load required libraries
library(shiny)
library(ggplot2)
library(DT)
library(colourpicker)
library(readxl)
library(viridis)  # For vibrant color scales


# Define UI
ui <- fluidPage(
  titlePanel("Advanced GFR Data Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      # Dynamic column selection for X-axis
      uiOutput("column_select_ui_x"),
      
      # Dynamic column selection for Y-axis (for scatter plot)
      uiOutput("column_select_ui_y"),
      
      # Slider for histogram binwidth
      sliderInput("binwidth", "Histogram Binwidth", min = 1, max = 50, value = 10),
      
      # Color picker for histogram fill
      colourInput("histogram_color", "Histogram Fill Color", value = "magenta"),
      
      # Color picker for scatter plot points
      colourInput("scatter_color", "Scatter Plot Base Color", value = "blue"),
      
      # Instructions
      tags$hr(),
      tags$p("Instructions:"),
      tags$ul(
        tags$li("Select a column from the dropdown to visualize."),
        tags$li("Customize the histogram binwidth and scatter plot color.")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Histogram", h3("Histogram of Selected Column"), plotOutput("histogram_plot")),
        tabPanel("Scatter Plot", h3("Scatter Plot of Two Columns"), plotOutput("scatter_plot")),
        tabPanel("Boxplot", h3("Boxplot of Selected Column"), plotOutput("boxplot_plot")),
        tabPanel("Data Table", h3("Data Table"), DTOutput("data_table"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Load data with error handling
  data <- reactive({
    tryCatch({
      readxl::read_excel("Sanjana_cleaneddata.xlsx")
    }, error = function(e) {
      showNotification("Error: Could not load the dataset!", type = "error")
      return(NULL)
    })
  })
  
  # Dynamic UI for X-axis column selection
  output$column_select_ui_x <- renderUI({
    req(data())  # Ensure data is loaded
    selectInput("selected_column_x", "Select Column (X-axis)", choices = names(data()))
  })
  
  # Dynamic UI for Y-axis column selection
  output$column_select_ui_y <- renderUI({
    req(data())  # Ensure data is loaded
    selectInput("selected_column_y", "Select Column (Y-axis)", choices = names(data()))
  })
  
  # Histogram with vibrant color
  output$histogram_plot <- renderPlot({
    req(input$selected_column_x)  # Ensure a column is selected
    ggplot(data(), aes(x = .data[[input$selected_column_x]])) +
      geom_histogram(binwidth = input$binwidth, 
                     fill = input$histogram_color, 
                     color = "black", alpha = 0.8) +  # Black border and slight transparency
      labs(title = paste("Histogram of", input$selected_column_x),
           x = input$selected_column_x, y = "Frequency") +
      theme_minimal()
  })
  
  # Scatter Plot with improved contrast
  output$scatter_plot <- renderPlot({
    req(input$selected_column_x, input$selected_column_y)  # Ensure two columns are selected
    ggplot(data(), aes(x = .data[[input$selected_column_x]], 
                       y = .data[[input$selected_column_y]], 
                       color = .data[[input$selected_column_y]])) +  # Color mapped to Y values
      geom_point(size = 4, stroke = 1, color = input$scatter_color, alpha = 0.9) +  # Larger points with stroke
      scale_color_viridis(option = "B", direction = -1) +  # Vibrant color scale
      labs(title = paste("Scatter Plot of", input$selected_column_x, "vs", input$selected_column_y),
           x = input$selected_column_x, y = input$selected_column_y) +
      theme_minimal()
  })
  
  # Boxplot with vibrant colors
  output$boxplot_plot <- renderPlot({
    req(input$selected_column_x)  # Ensure a column is selected
    ggplot(data(), aes(x = "", y = .data[[input$selected_column_x]])) +
      geom_boxplot(fill = "darkorange", color = "black", outlier.colour = "red", outlier.size = 3) +  # Highlight outliers
      labs(title = paste("Boxplot of", input$selected_column_x),
           y = input$selected_column_x) +
      theme_minimal()
  })
  
  # Data Table
  output$data_table <- renderDT({
    req(data())  # Ensure data is loaded
    datatable(data(), options = list(pageLength = 10))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
