library(shiny)
library(car)    # For VIF calculation
library(dplyr)  # For data manipulation
library(DT)     # For displaying data tables

# Define UI for Module 1: Data Upload and Multicollinearity Check
mod1_multicollinearity_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Module 1: Data Upload and Multicollinearity Check"),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("datafile"), "Upload CSV File", accept = ".csv"),
        checkboxInput(ns("header"), "Dataset has a header row", TRUE),
        numericInput(ns("vifThreshold"), "VIF Threshold", value = 5, min = 1, step = 0.1),
        numericInput(ns("detThreshold"), "Determinant Threshold", value = 0.00001, min = 0.000001, step = 0.00001),
        actionButton(ns("runVIF"), "Run Multicollinearity Check")
      ),
      mainPanel(
        h3("Uploaded Dataset"),
        DTOutput(ns("dataTable")),
        hr(),
        h3("Multicollinearity Check Results"),
        verbatimTextOutput(ns("vifResults")),
        verbatimTextOutput(ns("detResults")),
        hr(),
        h3("Summary and Interpretation"),
        textOutput(ns("vifInterpretation")),
        downloadButton(ns("downloadCleaned"), "Download Cleaned Dataset")
      )
    )
  )
}

# Define server logic for Module 1 using moduleServer
mod1_multicollinearity_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    dataset <- reactiveVal()
    cleaned_dataset <- reactiveVal()
    
    observeEvent(input$datafile, {
      req(input$datafile)
      data <- read.csv(input$datafile$datapath, header = input$header)
      dataset(data)
      output$dataTable <- renderDT({
        datatable(data)
      })
    })
    
    # Function to check multicollinearity using correlation matrix determinant
    check_multicollinearity_det <- function(data, threshold) {
      cor_matrix <- cor(data)
      det_value <- det(cor_matrix)
      
      removed_vars <- c()
      remaining_vars <- colnames(data)
      
      while (det_value < threshold && length(remaining_vars) > 1) {
        cor_sums <- colSums(abs(cor_matrix)) - 1
        worst_var <- names(which.max(cor_sums))
        
        removed_vars <- c(removed_vars, worst_var)
        remaining_vars <- setdiff(remaining_vars, worst_var)
        
        if (length(remaining_vars) > 1) {
          cor_matrix <- cor(data[, remaining_vars, drop = FALSE])
          det_value <- det(cor_matrix)
        }
      }
      
      return(list(
        original_det = det(cor(data)),
        final_det = det_value,
        removed = removed_vars,
        remaining = remaining_vars
      ))
    }
    
    observeEvent(input$runVIF, {
      req(dataset())
      data <- dataset()
      
      numeric_data <- data %>% select_if(is.numeric)
      
      if (ncol(numeric_data) < 2) {
        output$vifResults <- renderPrint({
          "The dataset needs at least two numeric variables for multicollinearity analysis."
        })
        return()
      }
      
      numeric_data <- numeric_data %>% select_if(~ var(., na.rm = TRUE) > 0)
      
      tryCatch({
        first_var <- names(numeric_data)[1]
        other_vars <- names(numeric_data)[-1]
        formula_str <- paste(first_var, "~", paste(other_vars, collapse = " + "))
        model <- lm(as.formula(formula_str), data = numeric_data)
        vif_values <- vif(model)
        
        det_results <- check_multicollinearity_det(numeric_data, input$detThreshold)
        
        output$vifResults <- renderPrint({
          cat("VIF Analysis Results:\n")
          cat("--------------------\n")
          print(vif_values)
          high_vif_vars <- names(vif_values)[vif_values > input$vifThreshold]
          if (length(high_vif_vars) > 0) {
            cat("\nVariables with VIF >", input$vifThreshold, ":", paste(high_vif_vars, collapse = ", "), "\n")
          } else {
            cat("\nNo variables have a VIF above the threshold.\n")
          }
        })
        
        output$detResults <- renderPrint({
          cat("\nCorrelation Matrix Determinant Analysis:\n")
          cat("------------------------------------\n")
          cat("Original Determinant:", det_results$original_det, "\n")
          cat("Final Determinant:", det_results$final_det, "\n")
          if (length(det_results$removed) > 0) {
            cat("\nVariables Removed:", paste(det_results$removed, collapse = ", "), "\n")
          } else {
            cat("\nVariables Removed: None\n")
          }
          cat("Remaining Variables:", paste(det_results$remaining, collapse = ", "), "\n")
        })
        
        cleaned_data <- numeric_data[, det_results$remaining, drop = FALSE]
        cleaned_dataset(cleaned_data)
        
        output$vifInterpretation <- renderText({
          paste(
            "Analysis completed successfully using both VIF and determinant methods.\n",
            "- Original variables:", ncol(numeric_data), "\n",
            "- Variables retained:", length(det_results$remaining), "\n",
            "- Variables removed:", length(det_results$removed), "\n",
            "Determinant threshold:", format(input$detThreshold, scientific = FALSE), "\n",
            "VIF threshold:", input$vifThreshold, "\n",
            "This process helps identify and remove variables that contribute to multicollinearity, which can distort the results of regression analyses. The VIF method identifies variables with high collinearity, while the determinant method iteratively removes variables to improve the matrix's determinant value."
          )
        })
        
      }, error = function(e) {
        output$vifResults <- renderPrint({
          paste("Error in calculation:", e$message)
        })
      })
    })
    
    output$downloadCleaned <- downloadHandler(
      filename = function() {
        paste("cleaned_dataset_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(cleaned_dataset(), file, row.names = FALSE)
      }
    )
  })
}
