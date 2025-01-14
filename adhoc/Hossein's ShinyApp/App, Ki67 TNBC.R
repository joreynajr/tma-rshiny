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
    if ("patient_id" %in% colnames(data) & "ki67" %in% colnames(data) & "grade" %in% colnames(data) & "sample_id" %in% colnames(data)) {
      data <- data %>%
        select(patient_id, ki67, grade, sample_id) %>%
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
  titlePanel("TNBC Ki67 and Tumor Grade Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("analyze", "Analyze Ki67 Distribution"),
      downloadButton("download_data", "Download Patient Data")
    ),
    
    mainPanel(
      plotOutput("ki67_plot"),
      verbatimTextOutput("ki67_summary"),
      plotOutput("grade_ki67_plot")
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  
  # Reactive data
  patient_data <- reactive({
    tnbc_data
  })
  
  # Generate Ki67 distribution plot
  output$ki67_plot <- renderPlot({
    req(input$analyze)
    
    data <- patient_data()
    
    ggplot(data, aes(x = ki67)) +
      geom_bar(fill = "skyblue", color = "black", alpha = 0.7) +
      labs(
        title = "Ki67 Distribution Across TNBC Samples",
        x = "Ki67 Levels",
        y = "Number of Patients"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  # Generate Ki67 and Grade association plot
  output$grade_ki67_plot <- renderPlot({
    req(input$analyze)
    
    data <- patient_data()
    
    ggplot(data, aes(x = grade, fill = ki67)) +
      geom_bar(position = "fill", color = "black", alpha = 0.7) +
      scale_y_continuous(labels = scales::percent) +
      labs(
        title = "Tumor Grade vs. Ki67 Levels",
        x = "Tumor Grade",
        y = "Percentage",
        fill = "Ki67 Levels"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  # Ki67 summary
  output$ki67_summary <- renderText({
    req(input$analyze)
    
    data <- patient_data()
    total <- nrow(data)
    low <- sum(data$ki67 == "Low", na.rm = TRUE)
    intermediate <- sum(data$ki67 == "Intermediate", na.rm = TRUE)
    high <- sum(data$ki67 == "High", na.rm = TRUE)
    
    paste(
      "Total Patients:", total, "\n",
      "Low Ki67:", low, "\n",
      "Intermediate Ki67:", intermediate, "\n",
      "High Ki67:", high, "\n",
      "Percentage Low:", round((low / total) * 100, 2), "%\n",
      "Percentage Intermediate:", round((intermediate / total) * 100, 2), "%\n",
      "Percentage High:", round((high / total) * 100, 2), "%"
    )
  })
  
  # Download data
  output$download_data <- downloadHandler(
    filename = function() {
      "tnbc_patient_ki67_data.csv"
    },
    content = function(file) {
      write.csv(patient_data(), file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)