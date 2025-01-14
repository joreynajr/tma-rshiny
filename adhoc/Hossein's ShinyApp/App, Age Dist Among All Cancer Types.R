library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(DBI)
library(RSQLite)

# File Paths
file_paths <- list.files(path = "C:/Users/ehsanbakhshh/Desktop/TMA", pattern = "\\.xlsx$", full.names = TRUE)

# Combine Data
combined_data <- data.frame()
for (file in file_paths) {
  if (grepl("^~\\$", basename(file))) next
  tryCatch({
    data <- read_excel(file, sheet = 1)
    cancer_type <- gsub(".*_(.*)\\.xlsx$", "\\1", basename(file))
    data$cancer_type <- cancer_type
    age_column <- intersect(colnames(data), c("Age_in_disease", "Age", "AgeDx"))
    if (length(age_column) > 0) {
      data <- data %>%
        rename(Age_in_disease = !!age_column[1]) %>%
        mutate(Age_in_disease = as.numeric(Age_in_disease))
      combined_data <- bind_rows(combined_data, 
                                 data %>% select(Patient_ID, Age_in_disease, cancer_type))
    }
  }, error = function(e) message("Error reading file: ", file, " - ", e$message))
}

if (nrow(combined_data) == 0) stop("No valid data found in the provided files")

# Save Data to SQL Database
con <- dbConnect(SQLite(), "cancer_data.sqlite")
dbWriteTable(con, "CancerData", combined_data, overwrite = TRUE)

# Shiny UI
ui <- fluidPage(
  titlePanel("Cancer Data Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("cancer_type", "Select Cancer Type:", 
                  choices = unique(combined_data$cancer_type), 
                  selected = unique(combined_data$cancer_type)[1]),
      downloadButton("download_summary", "Download Age Summary")
    ),
    
    mainPanel(
      plotOutput("age_distribution"),
      dataTableOutput("age_summary_table")
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  filtered_data <- reactive({
    combined_data %>% filter(cancer_type == input$cancer_type)
  })
  
  output$age_distribution <- renderPlot({
    ggplot(filtered_data(), aes(x = factor(Age_in_disease))) +
      geom_bar(fill = "skyblue", color = "black") +
      geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
      labs(
        title = paste("Number of Patients by Age for", input$cancer_type),
        x = "Age in Disease",
        y = "Number of Patients"
      ) +
      scale_x_discrete(labels = function(x) as.character(x)) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  age_summary <- reactive({
    filtered_data() %>%
      group_by(Age_in_disease) %>%
      summarise(Number_of_Patients = n(), .groups = "drop")
  })
  
  output$age_summary_table <- renderDataTable({
    age_summary()
  })
  
  output$download_summary <- downloadHandler(
    filename = function() {
      paste(input$cancer_type, "_age_summary.csv", sep = "")
    },
    content = function(file) {
      write.csv(age_summary(), file, row.names = FALSE)
    }
  )
}

# Run the App
shinyApp(ui = ui, server = server)