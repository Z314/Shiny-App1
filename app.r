##
# Add your Google Sheet ID at line 58
# R version: 4.4.2
##

# Load required libraries
library(shiny)       # For building the Shiny app
library(googlesheets4) # For interacting with Google Sheets
library(plotly)      # For creating interactive plots
library(dplyr)       # For data manipulation

# Function to load data from Google Sheets (in CSV format)
load_google_sheet <- function(sheet_id, sheet_name = "Sheet1") {
  # Construct the URL for exporting the sheet data in CSV format
  url <- sprintf("https://docs.google.com/spreadsheets/d/%s/export?format=csv&sheet=%s", sheet_id, sheet_name)
  
  # Attempt to fetch and read the data
  tryCatch({
    data <- read.csv(url)  # Read the CSV data into a data frame
    return(data)           # Return the data frame
  }, error = function(e) { # Handle errors during data fetching
    stop("Failed to fetch Google Sheet data.")  # Display an error message
  })
}

# Function to convert a column with date strings into Date objects
# Assumes the input date format is dd/mm/yyyy
convert_to_date <- function(column) {
  as.Date(column, format = "%d/%m/%Y")
}

# Define the Shiny app UI (User Interface)
ui <- fluidPage(
  # Application title
  titlePanel("Data from Google Sheet"),
  
  # Layout with a sidebar and main panel
  sidebarLayout(
    # Sidebar for user input
    sidebarPanel(
      # Dropdown to select the X-axis column
      selectInput("x_column", "Select X-axis column", choices = NULL),
      # Dropdown to select the Y-axis column
      selectInput("y_column", "Select Y-axis column", choices = NULL)
    ),
    
    # Main panel for displaying the plot and debug output
    mainPanel(
      plotlyOutput("data_plot"),        # Output area for the interactive plot
      verbatimTextOutput("debug_output") # Output area for debug information (optional)
    )
  )
)

# Define the server logic for the app
server <- function(input, output, session) {
  # Google Sheet details (replace with your actual sheet ID and name)
  sheet_id <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
  sheet_name <- "Sheet1"
  
  # Reactive expression to load and preprocess data
  data <- reactive({
    # Load data from the Google Sheet
    df <- load_google_sheet(sheet_id, sheet_name)
    # Convert the Date column to Date format
    df <- df %>% mutate(Date = convert_to_date(Date))
    return(df)  # Return the processed data frame
  })
  
  # Dynamically update the column selection inputs
  observe({
    df <- data()  # Access the loaded data
    # Update choices for the X-axis and Y-axis column dropdowns
    updateSelectInput(session, "x_column", choices = names(df))
    updateSelectInput(session, "y_column", choices = names(df))
  })
  
  # Render the interactive plot based on user selections
  output$data_plot <- renderPlotly({
    req(input$x_column, input$y_column)  # Ensure both X and Y columns are selected
    df <- data()  # Get the data frame
    
    x_col <- input$x_column  # Selected X-axis column
    y_col <- input$y_column  # Selected Y-axis column
    
    # Filter out rows with missing values in the selected columns
    df <- df %>% filter(!is.na(.data[[x_col]]) & !is.na(.data[[y_col]]))
    
    # Convert X-axis column to Date format if necessary
    if (!inherits(df[[x_col]], "Date")) {
      df[[x_col]] <- convert_to_date(df[[x_col]])
    }
    
    # Determine the range of dates for the X-axis
    date_range <- range(df[[x_col]], na.rm = TRUE)
    
    # Create the initial plot with lines
    p <- plot_ly(
      data = df,
      x = ~df[[x_col]],  # X-axis data
      y = ~df[[y_col]],  # Y-axis data
      type = 'scatter',  # Scatter plot type
      mode = 'lines',    # Lines only (no markers)
      line = list(color = 'blue', width = 2),  # Line style
      name = "Measurement"  # Legend label
    ) %>%
      layout(
        title = paste(y_col, "vs", x_col),  # Plot title
        xaxis = list(
          title = x_col,               # X-axis label
          tickformat = "%d/%m/%Y",     # Date format for ticks
          rangeslider = list(visible = TRUE), # Add a range slider
          type = "date",               # Specify date axis type
          range = date_range           # Set the X-axis range
        ),
        yaxis = list(title = y_col)    # Y-axis label
      )
    
    # Filter data within the selected date range
    plot_range <- list(min = min(df[[x_col]]), max = max(df[[x_col]]))
    df_filtered <- df %>% filter(Date >= plot_range$min & Date <= plot_range$max)
    
    # Add a mean line if there is data in the filtered range
    if (nrow(df_filtered) > 0) {
      mean_y <- mean(df_filtered[[y_col]], na.rm = TRUE)
      p <- p %>%
        add_trace(
          y = rep(mean_y, length(df_filtered[[x_col]])),  # Mean line values
          x = df_filtered[[x_col]],                      # X-axis values for mean line
          type = 'scatter',
          mode = 'lines',                                # Line only for mean
          line = list(color = 'red', width = 2),         # Red line style
          name = "Mean"                                  # Legend label for mean line
        )
    }
    
    p  # Return the final plot object
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
