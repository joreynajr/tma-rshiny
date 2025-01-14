library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)

# File paths
files <- c(
  "C:/Users/ehsanbakhshh/Desktop/TMA/TNBC1.xlsx",
  "C:/Users/ehsanbakhshh/Desktop/TMA/TNBC2.xlsx",
  "C:/Users/ehsanbakhshh/Desktop/TMA/TNBC3.xlsx"
)

# Load and prepare data
tnbc_data <- data.frame()
for (file in files) {
  tryCatch({
    data <- read_excel(file, sheet = 1, guess_max = 1000) %>%
      janitor::clean_names()
    
    # Debugging: Print column names
    cat("Column names in", file, ":\n", colnames(data), "\n")
    
    # Select and standardize columns
    if ("patient_id" %in% colnames(data) & "race" %in% colnames(data) & "sample_id" %in% colnames(data)) {
      data <- data %>%
        select(patient_id, race, sample_id) %>%
        distinct(patient_id, .keep_all = TRUE)  # Ensure each Patient_ID is unique
      tnbc_data <- bind_rows(tnbc_data, data)
    } else {
      cat("Required columns not found in", file, "\n")
    }
  }, error = function(e) cat("Error reading file:", file, "-", e$message, "\n"))
}

if (nrow(tnbc_data) == 0) stop("No valid data found in the provided files")

# Shiny UI
ui <- fluidPage(
  titlePanel("TNBC Patient Demographic Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("analyze", "Analyze Race Distribution"),
      downloadButton("download_data", "Download Patient Data")
    ),
    
    mainPanel(
      plotOutput("race_plot"),
      verbatimTextOutput("race_summary")
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  
  # Reactive data
  patient_data <- reactive({
    tnbc_data
  })
  
  # Generate plot
  output$race_plot <- renderPlot({
    req(input$analyze)
    
    data <- patient_data()
    
    ggplot(data, aes(x = race)) +
      geom_bar(fill = "skyblue", color = "black", alpha = 0.7) +
      labs(
        title = "Race Distribution of TNBC Patients",
        x = "Race",
        y = "Number of Patients"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  # Race summary
  output$race_summary <- renderText({
    req(input$analyze)
    
    data <- patient_data()
    black_patients <- sum(data$race == "Black", na.rm = TRUE)
    total_patients <- nrow(data)
    percentage_black <- (black_patients / total_patients) * 100
    
    paste(
      "Total Patients:", total_patients, "\n",
      "Black Patients:", black_patients, "\n",
      "Percentage of Black Patients:", round(percentage_black, 2), "%"
    )
  })
  
  # Download data
  output$download_data <- downloadHandler(
    filename = function() {
      "tnbc_patient_race_data.csv"
    },
    content = function(file) {
      write.csv(patient_data(), file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
