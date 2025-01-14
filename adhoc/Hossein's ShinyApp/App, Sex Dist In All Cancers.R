library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(DBI)
library(RSQLite)

# File paths and custom labels
files <- c(
  "C:/Users/ehsanbakhshh/Desktop/TMA/LungCancer3.xlsx", 
  "C:/Users/ehsanbakhshh/Desktop/TMA/LungCancer2.xlsx", 
  "C:/Users/ehsanbakhshh/Desktop/TMA/LungCancer1.xlsx",
  "C:/Users/ehsanbakhshh/Desktop/TMA/HGSOC.xlsx",
  "C:/Users/ehsanbakhshh/Desktop/TMA/TNBC3.xlsx",
  "C:/Users/ehsanbakhshh/Desktop/TMA/ColonCancer2.xlsx",
  "C:/Users/ehsanbakhshh/Desktop/TMA/ColonCancer1.xlsx",
  "C:/Users/ehsanbakhshh/Desktop/TMA/HCC3.xlsx",
  "C:/Users/ehsanbakhshh/Desktop/TMA/HCC1.xlsx",
  "C:/Users/ehsanbakhshh/Desktop/TMA/HCC2.xlsx",
  "C:/Users/ehsanbakhshh/Desktop/TMA/TNBC1.xlsx",
  "C:/Users/ehsanbakhshh/Desktop/TMA/TNBC2.xlsx"
)

custom_labels <- c(
  "LungCancer3.xlsx" = "Lung Cancer 3", 
  "LungCancer2.xlsx" = "Lung Cancer 2", 
  "LungCancer1.xlsx" = "Lung Cancer 1",
  "HGSOC.xlsx" = "HGSOC",
  "TNBC3.xlsx" = "TNBC3",
  "ColonCancer2.xlsx" = "Colon Cancer 2",
  "ColonCancer1.xlsx" = "Colon Cancer 1",
  "HCC3.xlsx" = "HCC 3",
  "HCC1.xlsx" = "HCC 1",
  "HCC2.xlsx" = "HCC 2",
  "TNBC1.xlsx" = "TNBC 1",
  "TNBC2.xlsx" = "TNBC 2"
)

# Prepare data
gender_data <- data.frame()
for (file in files) {
  file_name <- basename(file)
  disease_type <- custom_labels[file_name]
  
  tryCatch({
    data <- read_excel(file, sheet = 1) %>%
      mutate(across(everything(), as.character))
    if ("Sex" %in% colnames(data) && "Patient_ID" %in% colnames(data)) {
      data_subset <- data %>%
        distinct(Patient_ID, .keep_all = TRUE) %>%  # Aggregate by Patient_ID
        mutate(
          disease_type = disease_type,
          Sex = case_when(
            Sex == "F" ~ "Female",
            Sex == "M" ~ "Male",
            TRUE ~ NA_character_
          )
        ) %>%
        filter(!is.na(Sex))
      gender_data <- bind_rows(gender_data, data_subset)
    }
  }, error = function(e) cat("Error reading file:", file, "\n"))
}

if (nrow(gender_data) > 0) {
  gender_summary <- gender_data %>%
    group_by(disease_type, Sex) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(disease_type) %>%
    mutate(percentage = (count / sum(count)) * 100)
}

# Save to SQL Database
con <- dbConnect(SQLite(), dbname = "gender_data.sqlite")
dbWriteTable(con, "GenderSummary", gender_summary, overwrite = TRUE)

# Shiny UI
ui <- fluidPage(
  titlePanel("Gender Distribution by Cancer Type"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("cancer_type", "Select Cancer Type:", 
                  choices = unique(gender_summary$disease_type), 
                  selected = unique(gender_summary$disease_type)[1]),
      downloadButton("download_data", "Download Gender Summary")
    ),
    
    mainPanel(
      plotOutput("gender_plot"),
      dataTableOutput("gender_table")
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  filtered_data <- reactive({
    gender_summary %>%
      filter(disease_type == input$cancer_type)
  })
  
  output$gender_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = disease_type, y = percentage, fill = Sex)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(title = paste("Gender Distribution:", input$cancer_type),
           x = "Cancer Type", y = "Percentage",
           fill = "Gender") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$gender_table <- renderDataTable({
    filtered_data()
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("gender_summary_", input$cancer_type, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
