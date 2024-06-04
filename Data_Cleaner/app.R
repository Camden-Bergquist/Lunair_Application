library(shiny)
library(tidyverse)
library(shinydashboard)
library(DT)

# UI definition
ui <- dashboardPage(
  dashboardHeader(title = "Data Analysis Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "data_tab", icon = icon("table")),
      menuItem("Graphs", tabName = "graphs_tab", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "data_tab",
              fluidPage(
                titlePanel("Select Parameters:"),
                sidebarLayout(
                  sidebarPanel(
                    uiOutput("time_slider"),
                    uiOutput("column_checkboxes"),
                    uiOutput("upload_button"),
                    uiOutput("save_button")
                  ),
                  mainPanel(
                    uiOutput("preview_title"),
                    DTOutput("contents")  # Updated to DTOutput
                  )
                )
              )
      ),
      # Second tab content
      tabItem(tabName = "graphs_tab",
              fluidPage(
                titlePanel("Graphs"),
                # Placeholder for future graph outputs
                p("Graph outputs will be displayed here.")
              )
      )
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
  # Show modal dialog to upload file at the start
  showModal(modalDialog(
    title = "Upload CSV File",
    fileInput("file1", "Upload CSV File",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
    ),
    easyClose = FALSE,
    footer = NULL
  ))
  
  input_file <- reactive({
    req(input$file1)
    removeModal()  # Close the modal when a file is uploaded
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
    
    # Ensure there are no grouping issues by summarizing properly
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
    req(input$file1)  # Only render if file is loaded
    df <- output_file()
    min_time <- min(df$Time_Elapsed_MS, na.rm = TRUE)
    max_time <- max(df$Time_Elapsed_MS, na.rm = TRUE)
    
    sliderInput("timeRange", "Select Time Range (ms):", 
                min = min_time, max = max_time, 
                value = c(min_time, max_time),
                step = 10)  # Set step to 10ms
  })
  
  # Create checkboxes for relevant value columns
  output$column_checkboxes <- renderUI({
    req(input$file1)  # Only render if file is loaded
    checkboxGroupInput("columns", "Select Columns to Include:",
                       choices = c("IMUAccelerometerX", "IMUAccelerometerY", "IMUAccelerometerZ",
                                   "IMUGyroscopeX", "IMUGyroscopeY", "IMUGyroscopeZ", 
                                   "TransthoracicImpedance", "EventLeadImpedanceStart", 
                                   "StimulationWaveform", "EventTherapyOn", "EventTherapyOff"),
                       selected = c("IMUAccelerometerX", "IMUAccelerometerY", "IMUAccelerometerZ",
                                    "IMUGyroscopeX", "IMUGyroscopeY", "IMUGyroscopeZ", 
                                    "TransthoracicImpedance"))
  })
  
  # Create the upload new file button
  output$upload_button <- renderUI({
    req(input$file1)  # Only render if file is loaded
    actionButton("upload_new_file", "Upload New File")
  })
  
  # Create the save button dynamically based on the data
  output$save_button <- renderUI({
    req(input$file1)  # Only render if file is loaded
    downloadButton("downloadData", "Save File")
  })
  
  # Create the preview title dynamically based on the data
  output$preview_title <- renderUI({
    req(input$file1)  # Only render if file is loaded
    h3("Preview:")
  })
  
  # Filter the data based on the slider input and selected columns
  filtered_data <- reactive({
    req(input$timeRange)
    df <- output_file()
    df <- df |> filter(Time_Elapsed_MS >= input$timeRange[1], Time_Elapsed_MS <= input$timeRange[2])
    selected_columns <- c("Time_Elapsed_MS", input$columns)
    df <- df |> select(all_of(selected_columns))
    
    # Filter out rows where all selected columns are NA
    df <- df |> filter(rowSums(is.na(select(df, -Time_Elapsed_MS))) < (ncol(df) - 1))
    
    return(df)
  })
  
  output$contents <- renderDT({
    datatable(filtered_data(),
              options = list(scrollX = TRUE))  # Enable horizontal scrolling
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("cleaned_data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # Observe the upload new file button to show the modal again
  observeEvent(input$upload_new_file, {
    showModal(modalDialog(
      title = "Upload CSV File",
      fileInput("file1", "Upload CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      easyClose = FALSE,
      footer = NULL
    ))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
