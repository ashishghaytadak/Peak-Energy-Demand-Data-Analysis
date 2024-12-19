library(shiny)
library(dplyr)
library(arrow)  # For reading Parquet files
library(caret)  # For confusionMatrix
library(rsconnect)

# UI Definition
ui <- fluidPage(
  titlePanel("Peak Demand Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file2", "Upload Peak Demand Change Dataset (peak_demand_change.parquet):"),
      
      numericInput("n_rows", "Number of rows to display:", value = 5, min = 1),
      
      actionButton("update", "Update Dataset")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Dataset Preview", 
                 h4("Peak Demand Change Dataset Preview:"), 
                 tableOutput("previewData2")),
        
        tabPanel("Summary", verbatimTextOutput("summary")),
        
        tabPanel("Confusion Matrix",  
                 verbatimTextOutput("confMatrix"), 
                 
                 h4("How to Interpret the Confusion Matrix"), 
                 
                 p("The confusion matrix shows how well the model is performing. Here's how to interpret the numbers:"), 
                 
                 p("1. **True Positives (TP)**: These are the cases where the model correctly predicted a positive outcome."), 
                 
                 p("2. **True Negatives (TN)**: These are the cases where the model correctly predicted a negative outcome."), 
                 
                 p("3. **False Positives (FP)**: These are the cases where the model incorrectly predicted a positive outcome."), 
                 
                 p("4. **False Negatives (FN)**: These are the cases where the model incorrectly predicted a negative outcome."), 
                 
                 p("5. **Accuracy**: The proportion of correct predictions, calculated as (TP + TN) / total samples."), 
                 
                 p("A good model should have a high **accuracy**, and a low number of **FP** and **FN**.")
        ),
        
        tabPanel("Predictions", tableOutput("predictions"))
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Load peak_demand_change.parquet with error handling
  peakDemandData <- reactive({
    req(input$file2)
    
    tryCatch({
      read_parquet(input$file2$datapath)
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
      NULL
    })
  })
  
  # Preview first n rows of peak_demand_change 
  output$previewData2 <- renderTable({
    req(peakDemandData())
    head(peakDemandData(), input$n_rows)
  })
  
  # Summary of the peak_demand_change dataset 
  output$summary <- renderPrint({
    req(peakDemandData())
    summary(peakDemandData())
  })
  
  # Generate predictions (for demonstration, use a simple approach) 
  predictedData <- reactive({
    req(peakDemandData())
    
    # Simulate a prediction for demonstration purposes, as we don't have a pre-trained model 
    peakDemandData() %>% 
      mutate(predicted_energy_usage = current_energy_usage + runif(n(), min = -10, max = 10)) # Simulating a prediction 
  })
  
  # Display predictions 
  output$predictions <- renderTable({
    req(predictedData())
    predictedData() %>% select(current_energy_usage, predicted_energy_usage)
  })
  
  # Confusion matrix based on current_energy_usage (actual) and predicted_energy_usage (predicted) 
  output$confMatrix <- renderPrint({
    req(predictedData())
    
    # Check if both actual and predicted columns exist 
    if ("current_energy_usage" %in% colnames(predictedData()) && "predicted_energy_usage" %in% colnames(predictedData())) {
      
      # Convert to binary (e.g., classify as high/low energy usage for confusion matrix) 
      actual <- ifelse(predictedData()$current_energy_usage > median(predictedData()$current_energy_usage), "High", "Low")
      predicted <- ifelse(predictedData()$predicted_energy_usage > median(predictedData()$predicted_energy_usage), "High", "Low")
      
      # Generate confusion matrix 
      confusion <- confusionMatrix(factor(predicted), factor(actual))
      print(confusion)
      
    } else {
      print("The dataset must contain 'current_energy_usage' and 'predicted_energy_usage' columns for generating a confusion matrix.")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)