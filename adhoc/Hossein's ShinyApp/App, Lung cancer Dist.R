library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(DBI)
library(RSQLite) # Replace with your SQL driver if not using SQLite

# File paths
file1 <- "C:/Users/ehsanbakhshh/Desktop/TMA/LungCancer1.xlsx"
file2 <- "C:/Users/ehsanbakhshh/Desktop/TMA/LungCancer2.xlsx"
file3 <- "C:/Users/ehsanbakhshh/Desktop/TMA/LungCancer3.xlsx"

# Load Excel sheets
data1 <- read_excel(file1, sheet = 1)
data2 <- read_excel(file2, sheet = 1)
data3 <- read_excel(file3, sheet = 1)

# Merge data
merged_data <- full_join(data1, data2, by = "Sample_ID") %>%
  full_join(data3, by = "Sample_ID")

# Aggregate by Patient_ID
aggregated_data <- merged_data %>%
  group_by(Patient_ID) %>%
  summarise(
    Sex = first(Sex[!is.na(Sex)]),
    Race = first(Race[!is.na(Race)]),
    Age_in_disease = mean(Age_in_disease, na.rm = TRUE),
    Smoking = first(Smoking.x[!is.na(Smoking.x)]),
    preop_chemo = first(preop_chemo[!is.na(preop_chemo)]),
    postop_chemo = first(postop_chemo[!is.na(postop_chemo)]),
    psite_desc = first(psite_desc[!is.na(psite_desc)]),
    ajccpathgrp = first(ajccpathgrp[!is.na(ajccpathgrp)]),
    ajccclingrp = first(ajccclingrp[!is.na(ajccclingrp)]),
    Mutations = first(Mutations[!is.na(Mutations)])
  ) %>%
  ungroup()

# Save aggregated data to SQLite database
con <- dbConnect(SQLite(), dbname = "lung_cancer_data.sqlite")
dbWriteTable(con, "lung_cancer", aggregated_data, overwrite = TRUE)

# Shiny UI
ui <- fluidPage(
  titlePanel("Lung Cancer Data Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("plot_type", "Choose Plot Type:",
                  choices = c("Sex Distribution", "Race Distribution", "Age Distribution",
                              "Tobacco Use", "Pre-operation Chemo", "Post-operation Chemo",
                              "Tumor Location", "Stage Distribution", "Grade Distribution", 
                              "Mutation Distribution")),
      actionButton("update", "Update Plot")
    ),
    
    mainPanel(
      plotOutput("plot"),
      dataTableOutput("data_table")
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  data <- reactive({
    dbReadTable(con, "lung_cancer")
  })
  
  output$data_table <- renderDataTable({
    data()
  })
  
  output$plot <- renderPlot({
    req(input$update)  # Wait for user to click the "Update" button
    plot_data <- data()
    
    ggplot(plot_data, aes_string(x = switch(input$plot_type,
                                            "Sex Distribution" = "Sex",
                                            "Race Distribution" = "Race",
                                            "Age Distribution" = "Age_in_disease",
                                            "Tobacco Use" = "Smoking",
                                            "Pre-operation Chemo" = "preop_chemo",
                                            "Post-operation Chemo" = "postop_chemo",
                                            "Tumor Location" = "psite_desc",
                                            "Stage Distribution" = "ajccpathgrp",
                                            "Grade Distribution" = "ajccclingrp",
                                            "Mutation Distribution" = "Mutations"))) +
      geom_bar(fill = "skyblue") +
      geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
      labs(title = input$plot_type, x = input$plot_type, y = "Number of Patients") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
