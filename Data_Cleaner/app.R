library(shiny)
library(dplyr)
library(stringr)

ui <- fluidPage(
  titlePanel("CSV File Upload"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      )
    ),
    mainPanel(
      tableOutput("contents")
    )
  )
)

# Function defined outside the server:

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

server <- function(input, output) {
  input_file <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, sep = ";")
  })
  
  output_file <- reactive({
    input_file() |>
      rename(
        "Start_Time" = "StartTime..MM.dd.yyyy.HH.mm.ss.fff.",
        "End_Time" = "EndTime..MM.dd.yyyy.HH.mm.ss.fff."
      ) |>
      select(-End_Time) |>
      mutate(Start_Time_MS = sapply(Start_Time, extract_and_convert_to_milliseconds),
             Time_Elapsed_MS = Start_Time_MS - min(Start_Time_MS))
  })
  
  standardized_start_time <- reactive({
    min(output_file()$Start_Time_MS)
  })
  
  output$contents <- renderTable({
    output_file()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
