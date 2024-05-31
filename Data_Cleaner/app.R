library(shiny)
library(dplyr)
library(stringr)

ui <- fluidPage(
  titlePanel("Data Cleaner"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload CSV File",
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
      mutate(
        Start_Time_MS = sapply(Start_Time, extract_and_convert_to_milliseconds),
        Time_Elapsed_MS = Start_Time_MS - Start_Time_MS[1],
        # Handles the time overflowing past midnight by adding 24 hours in milliseconds to the total before subtracting.
        Time_Elapsed_MS = ifelse(Time_Elapsed_MS < 0, Start_Time_MS + (24 * 60 * 60 * 1000) - Start_Time_MS[1], Time_Elapsed_MS)
      )
  })
  
  output$contents <- renderTable({
    output_file() |> 
      select(Channel, Time_Elapsed_MS, Value) |>
      head(n = 15)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
