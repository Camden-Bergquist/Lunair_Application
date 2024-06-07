library(shiny)
library(tidyverse)
library(shinydashboard)
library(DT)
library(plotly)
library(shinycssloaders)

# UI definition
ui <- dashboardPage(
  dashboardHeader(title = "Data Tool"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Manipulation", tabName = "data_tab", icon = icon("table")),
      menuItem("Data Visualization", tabName = "graphs_tab", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper {
          height: 100vh !important;
        }
        .content {
          height: 100vh !important;
        }
        .box-body {
          height: calc(100vh - 100px) !important;
        }
      "))
    ),
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
                    withSpinner(DTOutput("contents"))  # Added spinner
                  )
                )
              )
      ),
      # Second tab content
      tabItem(tabName = "graphs_tab",
              fluidPage(
                titlePanel("Data Visualization"),
                selectInput("graph_type", "Select Graph:", choices = c("Accelerometer", "Gyroscope", "Stimulus")),
                div(style = "height: 100%;", withSpinner(plotlyOutput("plotly_graph", height = "100%")))  # Added spinner
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
  
  # Set maximum file upload size (currently 64MB)
  options(shiny.maxRequestSize = 64 * 1024^2)
  
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
    withProgress(message = 'Loading file...', value = 0, {
      df <- input_file()
      incProgress(0.2)
      
      df <- df |>
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
      
      incProgress(0.4)
      
      # Ensure there are no grouping issues by summarizing properly
      df <- df |>
        group_by(Time_Elapsed_MS, Channel) |>
        summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop')
      
      incProgress(0.6)
      
      # Pivot the data
      df <- df |>
        pivot_wider(names_from = Channel, values_from = Value, values_fill = NA)
      
      incProgress(0.8)
      
      df
    })
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
  
  # Render Plotly plot in the "Graphs" tab based on selection
  output$plotly_graph <- renderPlotly({
    req(input$file1)  # Ensure the file is uploaded
    
    df <- output_file()
    
    if (input$graph_type == "Accelerometer") {
      plot_data <- df |> 
        select(Time_Elapsed_MS, IMUAccelerometerX, IMUAccelerometerY, IMUAccelerometerZ) |>
        filter(!is.na(Time_Elapsed_MS) & !is.na(IMUAccelerometerX) & !is.na(IMUAccelerometerY) & !is.na(IMUAccelerometerZ))
      
      plot_ly(plot_data, x = ~Time_Elapsed_MS, height = 600) |>
        add_lines(y = ~IMUAccelerometerX, name = 'Accelerometer X', line = list(color = 'blue2', width = 2), opacity = 0.5) |>
        add_lines(y = ~IMUAccelerometerY, name = 'Accelerometer Y', line = list(color = 'purple', width = 2), opacity = 0.5) |>
        add_lines(y = ~IMUAccelerometerZ, name = 'Accelerometer Z', line = list(color = 'green', width = 2), opacity = 0.5) |>
        layout(
          title = 'Accelerometer Data',
          xaxis = list(title = 'Time Elapsed (ms)', automargin = TRUE, autorange = TRUE),
          yaxis = list(title = 'Acceleration', range = c(-36000, 36000), automargin = TRUE, autorange = TRUE),
          template = 'plotly_white',
          autosize = TRUE,
          margin = list(t = 50, b = 50, l = 50, r = 50)
        )
    } else if (input$graph_type == "Gyroscope") {
      plot_data <- df |> 
        select(Time_Elapsed_MS, IMUGyroscopeX, IMUGyroscopeY, IMUGyroscopeZ) |>
        filter(!is.na(Time_Elapsed_MS) & !is.na(IMUGyroscopeX) & !is.na(IMUGyroscopeY) & !is.na(IMUGyroscopeZ))
      
      plot_ly(plot_data, x = ~Time_Elapsed_MS, height = 600) |>
        add_lines(y = ~IMUGyroscopeX, name = 'Gyroscope X', line = list(color = 'blue2', width = 2), opacity = 0.5) |>
        add_lines(y = ~IMUGyroscopeY, name = 'Gyroscope Y', line = list(color = 'purple', width = 2), opacity = 0.5) |>
        add_lines(y = ~IMUGyroscopeZ, name = 'Gyroscope Z', line = list(color = 'green', width = 2), opacity = 0.5) |>
        layout(
          title = 'Gyroscope Data',
          xaxis = list(title = 'Time Elapsed (ms)', automargin = TRUE, autorange = TRUE),
          yaxis = list(title = 'Rotation', range = c(-36000, 36000), automargin = TRUE, autorange = TRUE),
          template = 'plotly_white',
          autosize = TRUE,
          margin = list(t = 50, b = 50, l = 50, r = 50)
        )
    } else if (input$graph_type == "Stimulus") {
      # Check if columns exist before plotting
      plot_ly(height = 600) %>%
        {if ("StimulationWaveform" %in% colnames(df)) {
          ImpWav_filtered_waveform <- df |> filter(!is.na(StimulationWaveform))
          add_lines(., x = ~ImpWav_filtered_waveform$Time_Elapsed_MS, y = ~ImpWav_filtered_waveform$StimulationWaveform,
                    name = 'Stimulation Waveform', line = list(color = 'gold', opacity = 0.5), yaxis = 'y1')
        } else .} %>%
        {if ("TransthoracicImpedance" %in% colnames(df)) {
          ImpWav_filtered_impedance <- df |> filter(!is.na(TransthoracicImpedance)) |>
            # Convert to ohms.
            mutate(TransthoracicImpedance = ((TransthoracicImpedance * 0.04176689) - 8.5538812))
          add_lines(., x = ~ImpWav_filtered_impedance$Time_Elapsed_MS, y = ~ImpWav_filtered_impedance$TransthoracicImpedance,
                    name = 'Transthoracic Impedance (Ohms)', line = list(color = 'green', opacity = 0.5), yaxis = 'y2')
        } else .} %>%
        layout(
          yaxis = list(
            title = 'Stimulation Waveform',
            range = c(0, 8000),
            autorange = TRUE,
            automargin = TRUE
          ),
          yaxis2 = list(
            title = 'Transthoracic Impedance (Ohms)',
            overlaying = 'y',
            side = 'right',
            standoff = 15,
            range = c(0, 8000),
            autorange = TRUE,
            automargin = TRUE
          ),
          xaxis = list(title = 'Time Elapsed (ms)', automargin = TRUE, autorange = TRUE),
          legend = list(orientation = 'h'),
          autosize = TRUE,
          margin = list(t = 50, b = 50, l = 50, r = 50)
        )
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
