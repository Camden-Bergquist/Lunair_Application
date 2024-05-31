library(shiny)
library(tidyverse)

# UI definition
ui <- fluidPage(
  titlePanel("Data Cleaner"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      uiOutput("time_slider"),
      uiOutput("column_checkboxes")
    ),
    mainPanel(
      tableOutput("contents")
    )
  )
)

# Function defined outside the server
extract_and_convert_to_milliseconds <- function(datetime_string) {
  time_string <- str_extract(datetime_string, "\\d{2}:\\d{2}:\\d{2}\\.\\d{3}")
  parts <- str_split(time_string, "[:.]")[[1]]
  hours <- as.numeric(parts[1])
  minutes <- as.numeric(parts[2])
  seconds <- as.numeric(parts[3])
  milliseconds <- as.numeric(parts[4])
  
  total_milliseconds <- (hours * 3600000) + (minutes * 60000) + (seconds * 1000) + milliseconds
  return(total_milliseconds)
}

# Server definition
server <- function(input, output, session) {
  input_file <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, sep = ";")
  })
  
  output_file <- reactive({
    df <- input_file() |>
      rename(
        "Start_Time" = "StartTime..MM.dd.yyyy.HH.mm.ss.fff.",
        "End_Time" = "EndTime..MM.dd.yyyy.HH.mm.ss.fff."
      ) |>
      select(-End_Time) |>
      mutate(
        Start_Time_MS = sapply(Start_Time, extract_and_convert_to_milliseconds),
        Time_Elapsed_MS = Start_Time_MS - Start_Time_MS[1],
        # Handles the time overflowing past midnight by adding 24 hours in milliseconds to the total before subtracting.
        Time_Elapsed_MS = ifelse(Time_Elapsed_MS < 0, Start_Time_MS + (24 * 60 * 60 * 1000) - Start_Time_MS[1], Time_Elapsed_MS)
      )
    
    # Ensure there are no grouping issues by summarising properly
    df <- df |>
      group_by(Time_Elapsed_MS, Channel) |>
      summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop')
    
    # Pivot the data
    df <- df |>
      pivot_wider(names_from = Channel, values_from = Value, values_fill = NA)
    
    return(df)
  })
  
  # Create the slider UI dynamically based on the data
  output$time_slider <- renderUI({
    df <- output_file()
    min_time <- min(df$Time_Elapsed_MS, na.rm = TRUE)
    max_time <- max(df$Time_Elapsed_MS, na.rm = TRUE)
    
    sliderInput("timeRange", "Select Time Range (ms):", 
                min = min_time, max = max_time, 
                value = c(min_time, max_time))
  })
  
  # Create checkboxes for relevant value columns
  output$column_checkboxes <- renderUI({
    checkboxGroupInput("columns", "Select Columns to Display:",
                       choices = c("IMUAccelerometerX", "IMUAccelerometerY", "IMUAccelerometerZ",
                                   "IMUGyroscopeX", "IMUGyroscopeY", "IMUGyroscopeZ", 
                                   "TransthoracicImpedance", "EventLeadImpedanceStart", 
                                   "StimulationWaveform", "EventTherapyOn", "EventTherapyOff"),
                       selected = c("IMUAccelerometerX", "IMUAccelerometerY", "IMUAccelerometerZ",
                                    "IMUGyroscopeX", "IMUGyroscopeY", "IMUGyroscopeZ", 
                                    "TransthoracicImpedance"))
  })
  
  # Filter the data based on the slider input and selected columns
  filtered_data <- reactive({
    req(input$timeRange)
    df <- output_file()
    df <- df |> filter(Time_Elapsed_MS >= input$timeRange[1], Time_Elapsed_MS <= input$timeRange[2])
    selected_columns <- c("Time_Elapsed_MS", input$columns)
    df <- df |> select(all_of(selected_columns))
    return(df)
  })
  
  output$contents <- renderTable({
    filtered_data() |> 
      mutate_if(is.numeric, as.integer) |> 
      head(n = 15)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
