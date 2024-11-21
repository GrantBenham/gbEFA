#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

# Import required libraries
library(shiny)
library(rmarkdown)
library(knitr)
library(kableExtra)
library(plotly)
library(htmlwidgets)
library(ggplot2)
library(reshape2)
library(psych)
library(dplyr)
library(htmltools)
library(base64enc)

# Define UI for Module 4: Factor Analysis
mod4_efa_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Exploratory Factor Analysis"),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("data_file"), "Upload CSV File", accept = ".csv"),
        checkboxInput(ns("header"), "Dataset has a header row", TRUE),
        
        uiOutput(ns("nfactors_ui")),  # Dynamic UI for nfactors
        
        selectInput(ns("extraction"), "Factor Extraction Method",
                  choices = c(
                    "Principal Axis Factoring" = "pa", 
                    "Minimum Residual" = "minres"
                  ),
                  selected = "pa"),
        
        selectInput(ns("rotation"), "Rotation Method",
                  choices = c("promax", "varimax", "oblimin", "quartimax"),
                  selected = "promax"),
        
        selectInput(ns("correlation"), "Correlation Matrix Type",
                  choices = c("spearman", "pearson", "polychoric"),
                  selected = "spearman"),
        
        helpText("Note: Polychoric correlations are recommended for ordinal data (e.g., Likert scales)"),
        
        numericInput(ns("loading_threshold"), "Loading Threshold",
                   value = 0.3, min = 0, max = 1, step = 0.05),
        
        numericInput(ns("communality_threshold"), "Communality Threshold",
                   value = 0.2, min = 0, max = 1, step = 0.05),
        
        numericInput(ns("scale_min"), "Scale Minimum Value", 
                     value = 1, min = 0, max = 10),
        numericInput(ns("scale_max"), "Scale Maximum Value",
                     value = 10, min = 1, max = 10),
        helpText("Scale range values will update based on your data"),
        
        actionButton(ns("run_efa"), "Run Analysis",
                   class = "btn-primary"),
        
        hr(),
        
        downloadButton(ns("export_results"), "Export Results")
      ),
      mainPanel(
        uiOutput(ns("results_panel")),
        uiOutput(ns("warning_panel"))
      )
    )
  )
}

# Define server logic for Module 4
mod4_efa_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Add reactive for data
    data <- reactive({
      req(input$data_file)
      read.csv(input$data_file$datapath)
    })
    
    # Dynamic UI for nfactors
    output$nfactors_ui <- renderUI({
      req(data())
      n_vars <- ncol(data())
      
      # More conservative maximum factors calculation
      max_factors <- min(
        floor((n_vars - sqrt(n_vars))/2),  # Traditional formula
        floor(n_vars/3),                   # One-third rule
        n_vars - 1                         # Must be less than number of variables
      )
      
      # Ensure at least 1 factor but no more than conservative maximum
      max_factors <- max(1, min(max_factors, 5))  # Cap at 5 factors for stability
      
      div(
        numericInput(session$ns("nfactors"), 
                    "Number of Factors",
                    value = min(3, max_factors), 
                    min = 1,
                    max = max_factors),
        helpText(sprintf(
          "Maximum recommended factors: %d\n
          Based on your data having %d variables.\n
          Note: Maximum is limited to ensure stable results.",
          max_factors, n_vars
        ))
      )
    })
    
    # Reactive values to store analysis results
    results <- reactiveVal(NULL)
    
    # Validate inputs
    validate_inputs <- function(data, input) {
      if(ncol(data) < 2) {
        return("Dataset must contain at least 2 variables")
      }
      if(nrow(data) < 3 * ncol(data)) {
        return("Sample size should be at least 3 times the number of variables")
      }
      
      n_vars <- ncol(data)
      
      # Use the same conservative maximum factors calculation
      max_factors <- min(
        floor((n_vars - sqrt(n_vars))/2),  # Traditional formula
        floor(n_vars/3),                   # One-third rule
        n_vars - 1                         # Must be less than number of variables
      )
      
      # Cap at 5 factors for stability
      max_factors <- max(1, min(max_factors, 5))
      
      if(input$nfactors > max_factors) {
        return(sprintf(
          "Number of factors (%d) is too high for %d variables.\n
          Maximum recommended factors: %d\n\n
          Having too many factors can lead to:\n
          • Convergence problems<br>
          • Unstable results<br>
           Unreliable factor structure<br><br>
          Please reduce the number of factors.",
          input$nfactors, n_vars, max_factors
        ))
      }
      
      if(input$nfactors < 1) {
        return("Number of factors must be at least 1")
      }
      return(NULL)
    }
    
    # Observe run button click
    observeEvent(input$run_efa, {
      req(input$data_file)
      
      data <- read.csv(input$data_file$datapath)
      
      validation_error <- validate_inputs(data, input)
      if(!is.null(validation_error)) {
        showNotification(
          HTML(validation_error),
          type = "error",
          duration = NULL,  # Makes notification stay until closed
          closeButton = TRUE
        )
        return()
      }
      
      tryCatch({
        withProgress(message = 'Running EFA Analysis', value = 0, {
          # Data loading
          incProgress(0.1, detail = "Loading data...")
          data <- read.csv(input$data_file$datapath)
          
          # Initial checks
          incProgress(0.2, detail = "Performing initial checks...")
          
          # Calculate maximum recommended factors based on number of variables
          n_vars <- ncol(data)
          max_factors <- floor((n_vars - sqrt(n_vars))/2)
          
          # Validate number of factors
          if (input$nfactors > max_factors) {
            showNotification(
              HTML(sprintf(
                "<strong>Factor Analysis Error</strong><br>
                The number of factors (%d) is too high for %d variables.<br>
                Maximum recommended factors: %d<br><br>
                Having too many factors can lead to:<br>
                • Convergence problems<br>
                • Unstable results<br>
                • Unreliable factor structure<br><br>
                Please reduce the number of factors.",
                input$nfactors, n_vars, max_factors
              )),
              type = "error",
              duration = NULL,  # Stays visible until dismissed
              closeButton = TRUE
            )
            stop("Too many factors requested")
          }
          
          # Running EFA
          incProgress(0.4, detail = "Running factor analysis...")
          result <- perform_iterative_efa(
            data = data,
            nfactors = input$nfactors,
            rotation = input$rotation,
            correlation_type = input$correlation,
            extraction_method = input$extraction,
            loading_threshold = input$loading_threshold,
            communality_threshold = input$communality_threshold,
            filename = basename(input$data_file$name)
          )
          
          # Finalizing
          incProgress(0.8, detail = "Generating report...")
          results(result)
          
          incProgress(1, detail = "Complete")
        })
      }, error = function(e) {
        if (grepl("maximum iteration exceeded", e$message)) {
          showNotification(
            HTML(paste0(
              "<strong>Factor Analysis Error</strong><br>",
              "Analysis failed to converge after 500 iterations.<br>",
              "This usually means either:<br>",
              "• Too many factors requested<br>", 
              "• High multicollinearity in data<br>",
              "Try reducing the number of factors or removing highly correlated variables."
            )),
            type = "error",
            duration = NULL,
            closeButton = TRUE
          )
        } else if (grepl("subscript out of bounds", e$message)) {
          showNotification(
            HTML(paste0(
              "<strong>Matrix Error</strong><br>",
              "Subscript out of bounds error.<br>",
              "This usually means the correlation matrix is not positive definite.<br>",
              "Try:<br>",
              "• Reducing number of factors<br>",
              "• Removing highly correlated variables<br>",
              "• Using a different extraction method"
            )),
            type = "error",
            duration = NULL,
            closeButton = TRUE
          )
        } else {
          showNotification(
            HTML(paste0(
              "<strong>Analysis Error</strong><br>",
              "An unexpected error occurred:<br>",
              e$message
            )),
            type = "error",
            duration = NULL,
            closeButton = TRUE
          )
        }
        stop(e$message)
      })
    })
    
    # Render results panel
    output$results_panel <- renderUI({
      req(results())
      
      # Add debug output
      cat("\nDEBUG: Rendering results panel")
      cat("\nDEBUG: Results class:", class(results()))
      cat("\nDEBUG: Results names:", paste(names(results()), collapse=", "))
      
      # Safely extract html_output
      html_output <- if(is.list(results()) && !is.null(results()$html_output)) {
        results()$html_output
      } else {
        cat("\nDEBUG: No valid html_output found in results")
        "Error: No results to display"
      }
      
      # Create HTML output
      HTML(html_output)
    })
    
    # Render warning panel
    output$warning_panel <- renderUI({
      req(results())
      
      # Add debug output
      cat("\nDEBUG: Rendering warning panel")
      cat("\nDEBUG: Results class:", class(results()))
      
      warnings_list <- list()
      
      # Safely check for correlation warnings
      if(is.list(results()) && !is.null(results()$correlation_warnings)) {
        cat("\nDEBUG: Found correlation warnings")
        warnings_list <- c(warnings_list, results()$correlation_warnings)
      }
      
      # Safely check for other warnings
      if(is.list(results()) && !is.null(results()$warnings)) {
        cat("\nDEBUG: Found general warnings")
        if(is.character(results()$warnings)) {
          if(any(grepl("Heywood", results()$warnings))) {
            warnings_list <- c(warnings_list, 
              "An ultra-Heywood case was detected. This means that one or more variables 
               have communalities greater than 1, which is theoretically impossible. 
               The results should be interpreted with extreme caution.")
          }
          
          if(any(grepl("factor scores", results()$warnings))) {
            warnings_list <- c(warnings_list,
              "The factor score estimates may be unreliable. This could be due to 
               various issues including multicollinearity or poor model fit.")
          }
        }
      }
      
      # Create warning panel if there are warnings
      if(length(warnings_list) > 0) {
        cat("\nDEBUG: Rendering", length(warnings_list), "warnings")
        div(
          class = "alert alert-warning",
          role = "alert",
          tags$h4("Analysis Warnings:"),
          tags$ul(
            lapply(warnings_list, function(w) {
              tags$li(HTML(w))
            })
          )
        )
      } else {
        cat("\nDEBUG: No warnings to display")
        NULL
      }
    })
    
    # Handle download
    output$export_results <- downloadHandler(
      filename = function() {
        paste0("EFA_Report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
      },
      content = function(file) {
        # Create temporary Rmd file
        tempReport <- file.path(tempdir(), "report.Rmd")
        
        # Write the report template
        writeLines(
          create_report_template(results(), "html"),
          tempReport
        )
        
        # Render with HTML options
        rmarkdown::render(tempReport,
                         output_format = rmarkdown::html_document(
                           css = system.file("rmd/css/custom.css", package = "kableExtra")
                         ),
                         output_file = file,
                         quiet = TRUE)
      }
    )
    
    # Reactive values for scale range
    scale_range <- reactiveVal(list(min = 1, max = 7))  # Default fallback values
    
    # Update scale range when data is loaded
    observe({
      req(data())
      numeric_data <- data() %>% select_if(is.numeric)
      if(ncol(numeric_data) > 0) {
        actual_min <- floor(min(numeric_data, na.rm = TRUE))
        actual_max <- ceiling(max(numeric_data, na.rm = TRUE))
        
        # Update the UI inputs
        updateNumericInput(session, "scale_min", 
          value = actual_min,
          label = sprintf("Scale Minimum Value (observed: %d)", actual_min))
        updateNumericInput(session, "scale_max", 
          value = actual_max,
          label = sprintf("Scale Maximum Value (observed: %d)", actual_max))
      }
    })
  })
}

# Replace the entire create_report_template function with:
create_report_template <- function(results, format) {
  # Add format-specific YAML headers
  yaml_header <- 'gbEFA shinyapp by Dr. Grant Benham'
  
  # Combine YAML header with results
  paste0(yaml_header, "\n\n", results$html_output)
}

# Modified perform_iterative_efa function
perform_iterative_efa <- function(data, nfactors, rotation, correlation_type = "spearman",
                                  extraction_method = "pa", loading_threshold = 0.3,
                                  communality_threshold = 0.2, filename = "dataset") {
  
  # Add at start of function to prevent plot generation
  void_dev <- function() {
    # Close any existing devices
    while (dev.cur() > 1) dev.off()
    # Open null device
    pdf(file = NULL)
  }
  void_dev()  # Activate the null device
  
  # Initialize all variables that might be needed in error returns
  ENABLE_DIAGNOSTICS <- TRUE
  iteration <- 1
  iteration_results <- list()
  iteration_removals <- list()
  initial_item_count <- ncol(data)
  factor_items <- vector("list", nfactors)
  factor_alphas <- numeric(nfactors)
  factor_omegas <- numeric(nfactors)
  html_output_all <- ""
  last_successful_efa <- NULL
  original_variable_order <- colnames(data)
  
  # Initialize reliability measures
  cronbach_alpha <- NA
  mcdonalds_omega <- NA
  
  # Try to calculate initial reliability measures
  tryCatch({
    cronbach_alpha <- psych::alpha(data)$total$raw_alpha
    mcdonalds_omega <- suppressWarnings(
      suppressMessages(
        psych::omega(data, plot=FALSE, digits=3)$omega.tot
      )
    )
  }, error = function(e) {
    # If reliability calculations fail, keep NA values
  })
  
  # Calculate maximum recommended factors based on number of variables
  n_vars <- ncol(data)
  max_factors <- floor((n_vars - sqrt(n_vars))/2)
  
  # Validate number of factors
  if (nfactors > max_factors) {
    showNotification(
      HTML(sprintf(
        "<strong>Factor Analysis Error</strong><br>
        The number of factors (%d) is too high for %d variables.<br>
        Maximum recommended factors: %d<br><br>
        Having too many factors can lead to:<br>
        • Convergence problems<br>
        • Unstable results<br>
        • Unreliable factor structure<br><br>
        Please reduce the number of factors.",
        nfactors, n_vars, max_factors
      )),
      type = "error",
      duration = NULL,  # Stays visible until dismissed
      closeButton = TRUE
    )
    stop("Too many factors requested")
  }
  
  # Initialize HTML output accumulator
  html_output_all <- ""
  
  # Initialize tracking variables
  iteration_removals <- list()
  initial_item_count <- ncol(data)
  factor_items <- vector("list", nfactors)
  factor_alphas <- numeric(nfactors)
  factor_omegas <- numeric(nfactors)
  
  # Capture the original order of variables
  original_variable_order <- colnames(data)
  
  # Initialize warnings collector
  correlation_warnings <- character(0)
  
  # Calculate correlation matrix based on type
  spearman_corr <- tryCatch({
    cat("\nDEBUG: Starting correlation matrix calculation")
    cat("\nDEBUG: Correlation type:", correlation_type)
    
    corr_matrix <- if(correlation_type == "polychoric") {
      cat("\nDEBUG: Attempting polychoric correlation")
      
      # Check if data is suitable for polychoric correlation
      non_integer_check <- !all(apply(data, 2, function(x) all(x == round(x))))
      cat("\nDEBUG: Data contains non-integer values:", non_integer_check)
      
      if(non_integer_check) {
        correlation_warnings <- c(correlation_warnings, 
          "Data contains non-integer values. Polychoric correlation requires ordinal data. Using Spearman correlation instead.")
        cat("\nDEBUG: Falling back to Spearman correlation due to non-integer values")
        cor(as.matrix(data), method = "spearman")
      } else {
        # Check for sufficient distinct values
        distinct_values <- sapply(data, function(x) length(unique(x)))
        min_distinct <- min(distinct_values)
        cat("\nDEBUG: Minimum distinct values:", min_distinct)
        
        if(min_distinct < 2) {
          correlation_warnings <- c(correlation_warnings,
            "Some variables have less than 2 distinct values. Using Spearman correlation instead.")
          cat("\nDEBUG: Falling back to Spearman correlation due to insufficient distinct values")
          cor(as.matrix(data), method = "spearman")
        } else {
          # Attempt polychoric correlation
          cat("\nDEBUG: Computing polychoric correlation matrix")
          tryCatch({
            cat("\nDEBUG: Data dimensions for polychoric:", dim(data)[1], "x", dim(data)[2])
            poly_result <- suppressWarnings({
              result <- psych::polychoric(as.matrix(data), correct = 0, smooth = TRUE)
              if(is.list(result) && !is.null(result$rho)) {
                result$rho
              } else if(is.matrix(result)) {
                result
              } else {
                cat("\nDEBUG: Invalid polychoric result type:", class(result))
                cor(as.matrix(data), method = "spearman")
              }
            })
            
            if(!is.matrix(poly_result) || any(is.na(poly_result))) {
              cat("\nDEBUG: Polychoric correlation failed - invalid result")
              correlation_warnings <- c(correlation_warnings,
                "Polychoric correlation calculation failed. Using Spearman correlation instead.")
              cor(as.matrix(data), method = "spearman")
            } else {
              cat("\nDEBUG: Polychoric correlation matrix computed successfully")
              cat("\nDEBUG: Polychoric matrix dimensions:", dim(poly_result)[1], "x", dim(poly_result)[2])
              poly_result
            }
          }, error = function(e) {
            cat("\nDEBUG: Error in polychoric calculation:", e$message)
            correlation_warnings <- c(correlation_warnings,
              paste("Polychoric correlation failed:", e$message, "Using Spearman correlation instead."))
            cor(as.matrix(data), method = "spearman")
          })
        }
      }
    } else {
      cat("\nDEBUG: Using standard correlation method:", correlation_type)
      if(!correlation_type %in% c("pearson", "kendall", "spearman")) {
        correlation_warnings <- c(correlation_warnings,
          paste("Invalid correlation type:", correlation_type, ". Using Spearman correlation instead."))
        correlation_type <- "spearman"
      }
      cor(as.matrix(data), method = correlation_type)
    }
    
    # Ensure proper matrix attributes
    if(is.matrix(corr_matrix)) {
      colnames(corr_matrix) <- colnames(data)
      rownames(corr_matrix) <- colnames(data)
      attr(corr_matrix, "method") <- correlation_type
      attr(corr_matrix, "warnings") <- correlation_warnings
      corr_matrix
    } else {
      stop("Failed to compute correlation matrix")
    }
  }, error = function(e) {
    cat("\nDEBUG: Error in correlation calculation:", e$message)
    correlation_warnings <- c(correlation_warnings,
      paste("Error computing correlation matrix:", e$message, "Using Spearman correlation instead."))
    result <- cor(as.matrix(data), method = "spearman")
    attr(result, "method") <- "spearman"
    attr(result, "warnings") <- correlation_warnings
    result
  })
  
  # Add debug output after correlation calculation
  cat("\nDEBUG: Correlation matrix computed")
  cat("\nDEBUG: Matrix class:", class(spearman_corr))
  cat("\nDEBUG: Matrix dimensions:", dim(spearman_corr)[1], "x", dim(spearman_corr)[2])
  cat("\nDEBUG: Matrix method:", attr(spearman_corr, "method"))
  
  # Continue with EFA using the correlation matrix
  # Initialize iteration count
  iteration <- 1
  total_vars <- ncol(spearman_corr)  # Use correlation matrix dimensions
  
  # Initialize iteration results storage
  iteration_results <- list()
  
  # Add helper functions from the notebook
  plot_correlation_heatmap <- function(corr_matrix) {
    melted_corr <- melt(corr_matrix)
    p <- ggplot(melted_corr, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile() +
      geom_text(aes(label = sprintf("%.2f", value)), size = 3) +  # Changed back to size for text
      scale_fill_gradient2(low = "#67001F", mid = "white", high = "#053061",
                           limits = c(-1, 1)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title = element_blank()) +
      coord_fixed()
    
    temp_file <- tempfile(fileext = ".png")
    ggsave(temp_file, p, width = 10, height = 10)
    
    sprintf('<img src="data:image/png;base64,%s" style="width:800px;height:800px;">', 
            base64enc::base64encode(temp_file))
  }
  
  create_univariate_table <- function(stats) {
    stats_df <- as.data.frame(t(stats))
    colnames(stats_df) <- c("Skewness", "Kurtosis")
    
    skew_colors <- sapply(stats_df$Skewness, function(x) {
      if(abs(x) > 2) return("red")
      if(abs(x) > 1) return("darkorange")
      return("black")
    })
    
    kurt_colors <- sapply(stats_df$Kurtosis, function(x) {
      if(abs(x) > 3) return("red")
      if(abs(x) > 1) return("darkorange")
      return("black")
    })
    
    stats_df_html <- stats_df
    stats_df_html$Skewness <- sprintf("<span style='color: %s'>%.3f</span>", 
                                      skew_colors, stats_df$Skewness)
    stats_df_html$Kurtosis <- sprintf("<span style='color: %s'>%.3f</span>", 
                                      kurt_colors, stats_df$Kurtosis)
    
    tbl <- kable(stats_df_html, format = "html", escape = FALSE) %>%
      kable_styling(full_width = FALSE, 
                    position = "left",
                    bootstrap_options = "bordered") %>%
      row_spec(0:nrow(stats_df), extra_css = "border: none !important") %>%
      column_spec(1:2, extra_css = "border: none !important")
    
    # Create summary text
    summary_text <- paste0(
      "<p><strong>Note on normality thresholds:</strong></p>",
      "<p><strong>Skewness:</strong><br>",
      "• Values in orange (|skew| > 1) indicate moderate asymmetry<br>",
      "• Values in red (|skew| > 2) indicate severe asymmetry<br>",
      "• Values close to 0 indicate symmetry</p>",
      "<p><strong>Excess Kurtosis:</strong><br>",
      "The values shown are excess kurtosis (kurtosis - 3), centered around 0:<br>",
      "• Positive values indicate heavier tails than normal distribution (leptokurtic)<br>",
      "• Negative values indicate lighter tails than normal distribution (platykurtic)<br>",
      "• Values close to 0 indicate normal-like tail weight (mesokurtic)<br>",
      "• Values in orange (|excess kurtosis| > 1) indicate moderate deviation from normality<br>",
      "• Values in red (|excess kurtosis| > 3) indicate severe deviation from normality</p>"
    )
    
    list(table = tbl, summary = summary_text)
  }
  
  # Add Mardia's test function
  mardia_test <- function(data) {
    n <- nrow(data)
    p <- ncol(data)
    
    # Calculate centered data
    center_data <- scale(data, center = TRUE, scale = FALSE)
    
    # Calculate Mardia's coefficients
    m_skew <- psych::mardia(data)$b1p
    m_kurt <- psych::mardia(data)$b2p
    
    # Calculate test statistics and p-values
    skew_stat <- (n/6) * m_skew
    kurt_stat <- (m_kurt - p * (p + 2)) / sqrt(8 * p * (p + 2) / n)
    
    # Calculate p-values
    skew_p <- pchisq(skew_stat, df = p * (p + 1) * (p + 2) / 6, lower.tail = FALSE)
    kurt_p <- 2 * pnorm(abs(kurt_stat), lower.tail = FALSE)
    
    list(
      skewness = m_skew,
      kurtosis = m_kurt,
      skew_p = skew_p,
      kurt_p = kurt_p
    )
  }
  
  # Add case_when function if not using dplyr
  case_when <- function(...) {
    args <- list(...)
    for (i in seq_along(args)) {
      if (length(args[[i]]) == 2) {
        test <- args[[i]][[1]]
        if (any(test, na.rm = TRUE)) {
          return(args[[i]][[2]])
        }
      }
    }
    return(NULL)
  }
  
  # Initialize storage for iteration results at the start of perform_iterative_efa
  iteration_results <- list()
  
  # Main EFA iteration loop
  repeat {
    # Add debugging
    cat("\n-------- DEBUG INFO --------\n")
    cat(sprintf("Current iteration: %d\n", iteration))
    cat(sprintf("Number of variables: %d\n", ncol(spearman_corr)))
    cat(sprintf("Number of factors requested: %d\n", nfactors))
    cat("Variable names:", paste(colnames(spearman_corr), collapse=", "), "\n")
    cat("--------------------------------\n")
    
    # Check if we have enough variables for stable factor analysis
    if(ncol(spearman_corr) <= nfactors) {
      warning_html <- paste0(
        "<div style='color: red; font-weight: bold; margin: 20px 0;'>",
        "<h4>⚠️ Analysis Halted</h4>",
        sprintf("Iteration %d: Number of variables (%d) must be greater than number of factors (%d).<br>", 
                iteration, ncol(spearman_corr), nfactors),
        "The analysis cannot proceed with the current factor structure.<br>",
        "Last successful iteration results are shown above.",
        "</div>"
      )
      
      # Create final HTML output
      html_output_all <- paste(
        paste(iteration_results[1:(iteration-1)], collapse = "<hr>"),
        warning_html,
        sep = "<hr>"
      )
      
      # Return results from last successful iteration
      return(list(
        efa_result = last_successful_efa,
        iteration_removals = iteration_removals[1:(iteration-1)],
        factor_items = factor_items,
        factor_alphas = factor_alphas,
        factor_omegas = factor_omegas,
        final_alpha = cronbach_alpha,
        final_omega = mcdonalds_omega,
        html_output = html_output_all,
        warnings = "Insufficient variables for requested factors",
        correlation_warnings = correlation_warnings
      ))
    }
    
    # Before EFA calculation
    if(ENABLE_DIAGNOSTICS) {
      cat(sprintf("\nPre-EFA Diagnostics (Iteration %d):", iteration))
      cat(sprintf("\n- Matrix determinant: %.6f", det(spearman_corr)))
      cat(sprintf("\n- Matrix condition number: %.3f", kappa(spearman_corr)))
      cat("\n")
    }
    
    # Store the current EFA result before attempting next iteration
    last_successful_efa <- NULL
    
    # Run EFA using the correlation matrix with enhanced error handling
    result <- tryCatch({
      cat("DEBUG: Starting tryCatch block\n")
      
      # Check if we have enough variables for stable solution
      if(ncol(spearman_corr) < (nfactors * 3)) {
        stop(sprintf(
          "Insufficient variables (%d) remaining to extract %d factors.<br><br>
          <strong>Why this happened:</strong><br>
          • Each factor needs at least 3 items for reliable measurement<br>
          • After removing problematic items, only %d variables remain<br>
          • This can support at most %d factors<br><br>
          <strong>Recommendations:</strong><br>
          1. Reduce the number of factors to %d or fewer, or<br>
          2. Adjust your thresholds to retain more variables",
          ncol(spearman_corr), nfactors, ncol(spearman_corr), 
          floor(ncol(spearman_corr)/3), floor(ncol(spearman_corr)/3)
        ))
      }
      
      # Try the EFA
      cat("DEBUG: About to attempt FA calculation\n")
      efa_result <- withCallingHandlers({
        fa(r = spearman_corr, 
           nfactors = nfactors, 
           rotate = rotation, 
           fm = extraction_method,
           max.iter = 500)
      }, warning = function(w) {
        cat("DEBUG: Warning caught:", w$message, "\n")
        invokeRestart("muffleWarning")
      })
      
      cat("DEBUG: FA calculation completed\n")
      
      # After FA calculation debugging
      cat("DEBUG: Starting post-processing\n")
      cat(sprintf("DEBUG: EFA result class: %s\n", class(efa_result)))
      cat(sprintf("DEBUG: EFA result names: %s\n", paste(names(efa_result), collapse=", ")))
      
      # Try to process the results with more detailed error catching
      tryCatch({
        cat("DEBUG: Attempting to extract loadings and communalities\n")
        # Extract outputs safely
        loadings <- try(efa_result$loadings)
        if(inherits(loadings, "try-error")) {
          cat("DEBUG: Error extracting loadings\n")
          stop("Failed to extract loadings")
        }
        cat(sprintf("DEBUG: Loadings dimensions: %d x %d\n", nrow(loadings), ncol(loadings)))
        
        communalities <- try(efa_result$communality)
        if(inherits(communalities, "try-error")) {
          cat("DEBUG: Error extracting communalities\n")
          stop("Failed to extract communalities")
        }
        cat(sprintf("DEBUG: Communalities length: %d\n", length(communalities)))
        
        # Store successful result before further processing
        cat("DEBUG: Storing successful result\n")
        last_successful_efa <- efa_result
        
        # Return both the EFA result and processed data
        cat("DEBUG: Returning successful result\n")
        list(
          success = TRUE,
          efa_result = efa_result,
          loadings = loadings,
          communalities = communalities
        )
      }, error = function(e) {
        cat("DEBUG: Error in post-processing:", e$message, "\n")
        cat("DEBUG: Error class:", class(e), "\n")
        cat("DEBUG: Stack trace:\n")
        print(sys.calls())
        
        # If post-processing fails, create error message and stop
        html_output_all <<- paste0(
          "<div style='color: red; font-weight: bold; margin: 20px 0;'>",
          "<h4>⚠️ Analysis Halted</h4>",
          "Factor analysis completed but results processing failed.<br>",
          "Error details: ", e$message, "<br>",
          if(extraction_method == "ml") {
            paste0(
              "Maximum Likelihood estimation failed. This usually means:<br>",
              "1. Your correlation matrix is not positive definite<br>",
              "2. The model failed to converge<br>",
              "Consider using Principal Axis Factoring ('pa') or Minimum Residual ('minres') instead."
            )
          } else {
            "This usually means too many factors were requested for the current number of variables."
          },
          "<br>Current variables: ", ncol(spearman_corr), "<br>",
          "Factors requested: ", nfactors, "<br>",
          "Consider reducing the number of factors.",
          "</div>"
        )
        
        # Return error status with more details
        list(
          success = FALSE,
          error = "post_processing_failed",
          error_details = e$message,
          html_output = html_output_all
        )
      })
    }, error = function(e) {
      cat("\nDEBUG: Main error handler triggered\n")
      cat("DEBUG: Error message:", e$message, "\n")
      
      # Create user-friendly error message based on error type
      error_msg <- if(grepl("Insufficient variables", e$message)) {
        # Use the custom message we created above
        e$message
      } else if(grepl("subscript out of bounds", e$message)) {
        sprintf(
          "Factor analysis failed to find a stable solution with %d factors.<br><br>
          <strong>Why this happened:</strong><br>
          • The analysis couldn't find enough distinct factors in your data<br>
          • This often occurs when requesting too many factors<br>
          • Your data might naturally contain fewer underlying factors<br><br>
          <strong>Recommendations:</strong><br>
          1. Try reducing the number of factors<br>
          2. Examine the scree plot in Module 2 to determine the optimal number of factors<br>
          3. Consider theoretical reasons for the number of factors you expect",
          nfactors
        )
      } else if(grepl("maximum iteration exceeded", e$message)) {
        sprintf(
          "Factor analysis failed to converge after 500 iterations.<br><br>
          <strong>Why this happened:</strong><br>
          • The algorithm couldn't find a stable solution with %d factors<br>
          • This usually means either:<br>
          &nbsp;&nbsp;- Too many factors requested<br>
          &nbsp;&nbsp;- High multicollinearity in data<br><br>
          <strong>Recommendations:</strong><br>
          1. Try reducing the number of factors<br>
          2. Check for highly correlated variables<br>
          3. Consider using a different rotation method",
          nfactors
        )
      } else {
        sprintf(
          "An unexpected error occurred during factor analysis.<br><br>
          Error details: %s<br><br>
          <strong>Recommendations:</strong><br>
          1. Try reducing the number of factors<br>
          2. Check your data for anomalies<br>
          3. Consider using different analysis parameters",
          e$message
        )
      }
      
      html_output_all <<- paste0(
        "<div style='color: red; font-weight: bold; margin: 20px 0;'>",
        "<h4>⚠��� Analysis Halted</h4>",
        error_msg,
        "</div>"
      )
      
      list(
        success = FALSE,
        error = "fa_failed",
        html_output = html_output_all
      )
    })
    
    # Check result status
    if(!result$success) {
      cat("DEBUG: Analysis failed, returning results\n")
      return(list(
        efa_result = last_successful_efa,
        html_output = result$html_output,
        warnings = "Analysis failed - too many factors for current variables"
      ))
    }
    
    # If we get here, continue with successful results
    efa_result <- result$efa_result
    loadings <- result$loadings
    communalities <- result$communalities
    
    # Post-EFA diagnostics
    if(ENABLE_DIAGNOSTICS) {
      cat(sprintf("\nPost-EFA Diagnostics:"))
      cat(sprintf("\n- Max communality: %.3f", max(communalities)))
      cat(sprintf("\n- Factor score calculation status: %s", 
                  !any(is.na(efa_result$scores))))
      cat(sprintf("\n- Heywood check: %s", 
                  any(communalities > 1.0)))
      cat("\n")
    }
    
    cat("DEBUG: Starting pattern matrix creation\n")
    
    # Ensure consistent factor ordering
    factor_names <- paste0("PA", 1:nfactors)
    cat(sprintf("DEBUG: Factor names created: %s\n", paste(factor_names, collapse=", ")))
    
    # Check loadings column names
    cat(sprintf("DEBUG: Current loadings column names: %s\n", paste(colnames(loadings), collapse=", ")))
    
    tryCatch({
      # Add debug output for column names
      cat("DEBUG: Original loadings column names:", paste(colnames(loadings), collapse=", "), "\n")
      
      # Handle ML extraction method differently
      if(extraction_method == "ml") {
        # For ML, we need to ensure the loadings matrix is properly formatted
        loadings_matrix <- as.matrix(loadings)
        # Rename columns to PA format
        colnames(loadings_matrix) <- paste0("PA", 1:ncol(loadings_matrix))
        # Update the loadings object
        loadings <- loadings_matrix
      } else {
        # Get variance explained for each factor
        variance_info <- efa_result$Vaccounted[2,]  # Row 2 contains proportion of variance
        
        # Reorder columns based on variance explained
        if(length(variance_info) == ncol(loadings)) {
          # Get order of factors by descending variance
          order_by_variance <- order(variance_info, decreasing = TRUE)
          # Reorder loadings columns
          loadings <- loadings[, order_by_variance, drop = FALSE]
        }
        
        # Always rename columns to PA format regardless of extraction method
        colnames(loadings) <- paste0("PA", 1:ncol(loadings))
      }
      
      cat("DEBUG: Final loadings column names:", paste(colnames(loadings), collapse=", "), "\n")
      
      cat("DEBUG: Converting loadings to matrix\n")
      # Calculate crossloading info - ensure matrix conversion
      loadings_matrix <- as.matrix(unclass(loadings))
      if(!is.matrix(loadings_matrix)) {
        loadings_matrix <- as.matrix(loadings)
      }
      
      # Ensure rownames are preserved
      if(is.null(rownames(loadings_matrix))) {
        rownames(loadings_matrix) <- rownames(loadings)
      }
      
      cat(sprintf("DEBUG: Loadings matrix dimensions: %d x %d\n", nrow(loadings_matrix), ncol(loadings_matrix)))
      
      cat("DEBUG: Identifying items with low communality\n")
      # Identify items with low communality - with safety check
      low_communalities <- if(!is.null(communalities)) {
        names(which(communalities < communality_threshold))
      } else {
        character(0)
      }
      cat(sprintf("DEBUG: Found %d items with low communality\n", length(low_communalities)))
      
      cat("DEBUG: Identifying items with low loadings\n")
      # Identify items with low loadings - with additional checks
      low_loading_items <- tryCatch({
        low_loading_mask <- apply(abs(loadings_matrix) < loading_threshold, 1, all)
        if(any(low_loading_mask)) {
          rownames(loadings)[low_loading_mask]
        } else {
          character(0)
        }
      }, error = function(e) {
        cat("DEBUG: Error in low loadings calculation:", e$message, "\n")
        character(0)
      })
      cat(sprintf("DEBUG: Found %d items with low loadings\n", length(low_loading_items)))
      
      cat("DEBUG: Calculating crossloadings\n")
      crossloading_info <- list()
      cross_loading_items <- character(0)
      
      # Check each row for crossloadings with error handling
      tryCatch({
        for(i in 1:nrow(loadings_matrix)) {
          var_name <- rownames(loadings_matrix)[i]
          above_threshold <- which(abs(loadings_matrix[i,]) >= loading_threshold)
          if(length(above_threshold) > 1) {
            crossloading_info[[var_name]] <- above_threshold
            cross_loading_items <- c(cross_loading_items, var_name)
          }
        }
      }, error = function(e) {
        cat("DEBUG: Error in crossloading calculation:", e$message, "\n")
      })
      cat(sprintf("DEBUG: Found %d crossloading items\n", length(cross_loading_items)))
      
      cat("DEBUG: Combining items to remove\n")
      # Combine items to remove with safety checks
      items_to_remove <- unique(c(
        if(length(low_loading_items) > 0) low_loading_items else character(0),
        if(length(cross_loading_items) > 0) cross_loading_items else character(0),
        if(length(low_communalities) > 0) low_communalities else character(0)
      ))
      
      # Sort items based on original order if there are any items to remove
      if(length(items_to_remove) > 0 && !is.null(original_variable_order)) {
        items_to_remove <- items_to_remove[order(match(items_to_remove, original_variable_order))]
      }
      cat(sprintf("DEBUG: Total items to remove: %d\n", length(items_to_remove)))
      
      # Store items removed in this iteration
      iteration_removals[[iteration]] <- items_to_remove
      
    }, error = function(e) {
      cat("DEBUG: Error in pattern matrix creation:", e$message, "\n")
      cat("DEBUG: Error class:", class(e), "\n")
      cat("DEBUG: Stack trace:\n")
      print(sys.calls())
      
      # Initialize empty values for error case
      items_to_remove <- character(0)
      iteration_removals[[iteration]] <- character(0)
    })
    
    cat("DEBUG: Pattern matrix creation completed\n")
    
    # Generate HTML output for this iteration
    html_output_iteration <- paste0(
      "<h3>EFA Iteration ", iteration, "</h3>",
      "<h4>SUMMARY</h4>",
      "<p><strong>Current Analysis Status:</strong><br>",
      "• Number of variables in analysis: ", ncol(spearman_corr), "<br>",
      "• Variables being analyzed: ", paste(colnames(spearman_corr), collapse=", "), "</p>",
      
      "<p><strong>Matrix Health Indicators:</strong><br>",
      "• Matrix determinant: ", format(det(spearman_corr), digits = 6), 
      "<br><em>Interpretation:</em> ", 
      if(det(spearman_corr) < 0.00001) {
        "Very low determinant indicates potential multicollinearity issues. This suggests some variables may be too highly correlated, which could affect factor stability."
      } else if(det(spearman_corr) < 0.0001) {
        "Low determinant suggests some correlation redundancy. While not critical, this indicates moderate levels of correlation between variables."
      } else if(det(spearman_corr) < 0.001) {
        "Acceptable determinant value, though some moderate correlations exist. This is common and generally not problematic."
      } else {
        "Good determinant value indicating healthy levels of correlation without excessive multicollinearity."
      },
      "<br><br>",
      
      "• Matrix condition number: ", format(kappa(spearman_corr), digits = 3),
      "<br><em>Interpretation:</em> ", 
      if(kappa(spearman_corr) > 30) {
        "High condition number indicates potential computational instability. Results should be interpreted with caution as the matrix may be close to singular."
      } else if(kappa(spearman_corr) > 15) {
        "Moderate condition number suggests some matrix instability. While not critical, this indicates the presence of some strong correlations."
      } else if(kappa(spearman_corr) > 10) {
        "Acceptable condition number indicating reasonable matrix stability. This is common in factor analysis and generally not problematic."
      } else {
        "Excellent condition number indicating a very stable correlation matrix. This suggests reliable factor analysis computations."
      },
      "</p>",
      
      "<p><strong>Issues Detected:</strong><br>",
      "• Variables with no significant loadings: [", length(low_loading_items), "] ", 
      paste(low_loading_items, collapse = ", "), "<br>",
      if(length(low_loading_items) > 0) {
        paste0("<em>Note:</em> These variables show no loadings above the threshold of ", 
               loading_threshold, " on any factor.<br>")
      },
      
      "• Variables with low communality: [", length(low_communalities), "] ", 
      paste(low_communalities, collapse = ", "), "<br>",
      if(length(low_communalities) > 0) {
        paste0("<em>Note:</em> These variables have communalities below the threshold of ", 
               communality_threshold, ", indicating poor explanation by the factor solution.<br>")
      },
      
      "• Cross-loading variables: [", length(cross_loading_items), "] ", 
      paste(cross_loading_items, collapse = ", "), "<br>",
      if(length(cross_loading_items) > 0) {
        "These variables load significantly on multiple factors, which may indicate complexity or poor factor differentiation.<br>"
      },
      
      "<p><strong>Action for Next Iteration:</strong><br>",
      if(length(items_to_remove) > 0) {
        paste0("The following ", length(items_to_remove), " variables will be removed: ", 
               paste(items_to_remove, collapse = ", "), "<br>",
               "<em>Reason:</em> These items show one or more issues that suggest they do not fit well in the current factor solution.")
      } else {
        "No items need to be removed. This is the final solution."
      },
      "</p>"
    )
    
    # Create pattern matrix table with formatting
    display_loadings <- as.data.frame(matrix("", nrow = nrow(loadings), 
                                             ncol = ncol(loadings) + 3))
    colnames(display_loadings) <- c("Variable", factor_names, 
                                    "Communalities", "Uniquenesses")
    display_loadings$Variable <- rownames(loadings)
    
    # Format loadings with color coding
    display_matrix <- apply(loadings_matrix, 2, function(x) sprintf("%.3f", x))
    
    # Apply grey text for below threshold values
    for(i in 1:nrow(loadings_matrix)) {
      for(col in 1:ncol(display_matrix)) {
        if(abs(loadings_matrix[i, col]) < loading_threshold) {
          display_matrix[i, col] <- paste0("<span style='color:lightgrey'>", 
                                         display_matrix[i, col], "</span>")
        }
      }
    }
    
    display_loadings[, 2:(ncol(loadings) + 1)] <- display_matrix
    display_loadings$Communalities <- sprintf("%.3f", communalities)
    display_loadings$Uniquenesses <- sprintf("%.3f", 1 - communalities)
    
    # Create base loadings table
    loadings_table <- kable(display_loadings, format = "html", 
                            row.names = FALSE, 
                            escape = FALSE) %>%
      kable_styling(full_width = FALSE, 
                    position = "left",
                    bootstrap_options = c("striped", "bordered")) %>%
      row_spec(0:nrow(display_loadings), extra_css = "border: 1px solid black;") %>%
      column_spec(1:ncol(display_loadings), border_left = TRUE, border_right = TRUE)
    
    # Highlight variable names in red for items to be removed
    if(length(items_to_remove) > 0) {
      loadings_table <- loadings_table %>%
        column_spec(1, background = ifelse(display_loadings$Variable %in% items_to_remove, "red", "white"))
    }
    
    # Highlight crossloading cells in light blue
    if(length(cross_loading_items) > 0) {
      cat("\nDEBUG: Starting crossloading highlighting\n")
      
      # Create a matrix to store all background colors
      background_matrix <- matrix("white", nrow = nrow(display_loadings), 
                                ncol = ncol(loadings))
      
      for(var_name in cross_loading_items) {
        row_idx <- which(display_loadings$Variable == var_name)
        cat(sprintf("\nDEBUG: Processing %s (row %d)\n", var_name, row_idx))
        
        # Get the actual loadings for this variable
        var_loadings <- loadings_matrix[row_idx,]
        cat("DEBUG: Raw loadings:", paste(round(var_loadings, 3), collapse=", "), "\n")
        
        # Find significant loadings
        significant_cols <- which(abs(var_loadings) >= loading_threshold)
        cat("DEBUG: Significant columns:", paste(significant_cols, collapse=", "), "\n")
        
        # Store background colors for this row
        for(col in significant_cols) {
          background_matrix[row_idx, col] <- "lightblue"
          cat(sprintf("DEBUG: Marking column %d for highlighting\n", col))
        }
      }
      
      # Apply all highlighting at once
      for(col in 1:ncol(loadings)) {
        loadings_table <- loadings_table %>%
          column_spec(col + 1, background = background_matrix[, col])
      }
      
      cat("\nDEBUG: Finished crossloading highlighting\n")
    }
    
    # Highlight low communality cells in orange
    if(length(low_communalities) > 0) {
      loadings_table <- loadings_table %>%
        column_spec(ncol(loadings) + 2,  # Communalities column
                    background = ifelse(display_loadings$Variable %in% low_communalities, 
                                        "orange", "white"))
    }
    
    # Add legend
    legend <- paste0(
      "<strong>Legend:</strong><br>",
      "<span style='background-color: red;'>Red background:</span> Variables to be removed in next iteration<br>",
      "<span style='background-color: yellow;'>Yellow background:</span> Variables with no loadings at or above threshold<br>",
      "<span style='color: lightgrey;'>Light grey text:</span> Loadings below the set threshold<br>",
      "<span style='background-color: orange;'>Orange background:</span> Low communality variables<br>",
      "<span style='background-color: lightblue;'>Light blue background:</span> Crossloading values<br>",
      "<em>Note: Factor loadings are evaluated using exact values, though displayed rounded to 3 decimal places.</em>"
    )
    
    # Get variance explained for each factor
    variance_info <- efa_result$Vaccounted[2,]  # Row 2 contains proportion of variance
    variance_text <- paste0(
      "<p><strong>Proportion of Variance Explained by Each Factor:</strong><br>",
      paste0("Factor ", 1:length(variance_info), ": ", 
             sprintf("%.1f%%", variance_info * 100), collapse = "<br>"),
      "<br>Total: ", sprintf("%.1f%%", sum(variance_info) * 100), "</p>"
    )
    
    # Add factor correlation matrix if using oblique rotation
    correlation_text <- if(rotation %in% c("promax", "oblimin")) {
      # Get factor correlations
      factor_cors <- efa_result$Phi
      
      # Create correlation matrix table with highlighting
      factor_cor_matrix <- matrix(NA, nrow = ncol(factor_cors), ncol = ncol(factor_cors))
      colnames(factor_cor_matrix) <- paste0("PA", 1:ncol(factor_cors))
      rownames(factor_cor_matrix) <- paste0("PA", 1:ncol(factor_cors))
      
      # Format correlations with color coding
      for(i in 1:ncol(factor_cors)) {
        for(j in 1:ncol(factor_cors)) {
          value <- factor_cors[i,j]
          color <- if(abs(value) > 0.7) {
            "red"  # Strong correlations
          } else if(abs(value) > 0.5) {
            "darkorange"  # Moderate correlations
          } else {
            "black"  # Weak correlations
          }
          factor_cor_matrix[i,j] <- sprintf("<span style='color: %s'>%.3f</span>", color, value)
        }
      }
      
      # Create table
      cor_table <- kable(factor_cor_matrix, format = "html", escape = FALSE) %>%
        kable_styling(full_width = FALSE, position = "left", bootstrap_options = "bordered")
      
      paste0(
        "<h4>Factor Correlation Matrix</h4>",
        "<p><strong>Interpretation Guide:</strong><br>",
        "• Correlations > 0.7 (red): Strong relationship - factors might be redundant<br>",
        "• Correlations 0.5-0.7 (orange): Moderate relationship - expected in oblique rotation<br>",
        "• Correlations < 0.5 (black): Weak relationship - factors are relatively distinct</p>",
        cor_table
      )
    } else {
      # For orthogonal rotation (varimax, quartimax)
      "<p><em>Note: Factor correlations not shown as orthogonal rotation enforces uncorrelated factors.</em>"
    }
    
    # Modify the html_output_iteration concatenation to include fit indices:
    html_output_iteration <- paste0(
      html_output_iteration,
      "<br>",
      legend,
      "<br><br>",
      loadings_table,
      "<br>",
      variance_text,
      "<br>",
      correlation_text,
      "<br>"
    )
    
    # In the main loop, store each iteration's HTML
    iteration_results[[iteration]] <- html_output_iteration
    
    # If final iteration, add additional statistics
    if (length(items_to_remove) == 0) {
      cat("\nDEBUG: Starting final statistics calculation\n")
      
      # If final iteration, suppress all warnings for the entire block
      suppressWarnings({
        cat("DEBUG: Re-running final EFA\n")
        # Re-run final EFA with the same correlation method
        efa_result <- fa(r = spearman_corr, 
                          nfactors = nfactors, 
                          rotate = rotation, 
                          fm = extraction_method)
        
        # After initial EFA calculation, reorder factors by variance explained
        loadings <- efa_result$loadings
        variance_explained <- efa_result$Vaccounted[2,]  # Row 2 contains proportion of variance
        factor_order <- order(variance_explained, decreasing = TRUE)
        
        # Reorder loadings columns by variance explained
        loadings <- loadings[, factor_order]
        
        # Rename columns to match new order
        colnames(loadings) <- paste0("PA", 1:ncol(loadings))
        
        # Update the EFA result with reordered loadings
        efa_result$loadings <- loadings
        
        # Also reorder other relevant components of efa_result
        if(!is.null(efa_result$Phi)) {
          efa_result$Phi <- efa_result$Phi[factor_order, factor_order]
        }
        efa_result$Vaccounted <- efa_result$Vaccounted[, factor_order]
        
        cat("DEBUG: Extracting final outputs\n")
        # Re-extract outputs
        loadings <- efa_result$loadings
        communalities <- efa_result$communality
        factor_correlations <- efa_result$Phi
        
        cat("DEBUG: Starting factor-specific statistics\n")
        # Calculate factor-specific statistics
        tryCatch({
          for (f in 1:nfactors) {
            cat(sprintf("DEBUG: Processing factor %d\n", f))
            
            # Safely extract factor loadings
            factor_loadings <- try({
              if(f <= ncol(loadings)) {
                loadings[, f]
              } else {
                stop("Factor has no loadings")
              }
            })
            
            if(inherits(factor_loadings, "try-error")) {
              cat(sprintf("DEBUG: Error extracting loadings for factor %d\n", f))
              factor_items[[f]] <- character(0)
              factor_alphas[f] <- NA
              factor_omegas[f] <- NA
              next
            }
            
            # Get items for this factor
            primary_items <- character(0)
            for(item in rownames(loadings)) {
              item_loadings <- abs(loadings[item,])
              # Find the maximum loading for this item
              max_loading <- max(item_loadings)
              # Check if it meets threshold and belongs to current factor
              if(max_loading >= loading_threshold && which.max(item_loadings) == f) {
                primary_items <- c(primary_items, item)
              }
            }
            
            factor_items[[f]] <- primary_items
            
            # Calculate reliability only if we have enough items
            if(length(primary_items) >= 3) {
              cat(sprintf("DEBUG: Factor %d has %d primary items: %s\n", 
                          f, length(primary_items), paste(primary_items, collapse=", ")))
              factor_data <- data[, primary_items, drop=FALSE]
              
              # Calculate reliabilities
              tryCatch({
                alpha_result <- psych::alpha(factor_data)
                omega_result <- suppressWarnings(
                  suppressMessages(
                    psych::omega(factor_data, plot=FALSE, digits=3)
                  )
                )
                factor_alphas[f] <- alpha_result$total$raw_alpha
                factor_omegas[f] <- omega_result$omega.tot
                
                cat(sprintf("DEBUG: Factor %d reliability calculated: alpha=%.3f, omega=%.3f\n",
                            f, factor_alphas[f], factor_omegas[f]))
              }, error = function(e) {
                cat(sprintf("DEBUG: Error calculating reliability for factor %d: %s\n", f, e$message))
                factor_alphas[f] <- NA
                factor_omegas[f] <- NA
              })
            } else {
              cat(sprintf("DEBUG: Factor %d has insufficient items (%d) for reliability\n", 
                          f, length(primary_items)))
              factor_alphas[f] <- NA
              factor_omegas[f] <- NA
            }
          }
        }, error = function(e) {
          cat("DEBUG: Error in factor statistics calculation:", e$message, "\n")
          warning_html <- paste0(
            "<div style='color: orange; margin: 10px 0;'>",
            "<strong>Warning:</strong> Some factors could not be fully analyzed. ",
            "This often means fewer meaningful factors exist in the data than requested.",
            "</div>"
          )
          html_output_all <<- paste0(warning_html, html_output_all)
        })
        
        # Continue with existing code but add debug statements around critical operations
        cat("DEBUG: Starting overall reliability calculation\n")
        final_data <- data[, rownames(spearman_corr)]
        cat(sprintf("DEBUG: Final data dimensions: %d x %d\n", nrow(final_data), ncol(final_data)))
        
        tryCatch({
          cronbach_alpha <- psych::alpha(final_data)$total$raw_alpha
          mcdonalds_omega <- suppressWarnings(
            suppressMessages(
              psych::omega(final_data, plot=FALSE, digits=3)$omega.tot
            )
          )
          cat(sprintf("DEBUG: Overall reliability calculated: alpha=%.3f, omega=%.3f\n", 
                     cronbach_alpha, mcdonalds_omega))
        }, error = function(e) {
          cat("DEBUG: Error calculating overall reliability:", e$message, "\n")
          cronbach_alpha <- NA
          mcdonalds_omega <- NA
        })

        # Keep all existing code but add debug statements around matrix operations
        cat("DEBUG: Starting correlation matrix calculations\n")
        full_data <- data
        cat(sprintf("DEBUG: Full data dimensions: %d x %d\n", nrow(full_data), ncol(full_data)))
        
        tryCatch({
          # Store the correlation method being used
          current_method <- attr(spearman_corr, "method")
          cat(sprintf("DEBUG: Using correlation method: %s\n", current_method))
          
          # Calculate final correlation matrix using the same method as before
          full_corr <- if(current_method == "polychoric") {
            cat("DEBUG: Computing final polychoric correlation matrix\n")
            poly_result <- suppressWarnings(
              psych::polychoric(as.matrix(full_data), correct = 0, smooth = TRUE)
            )
            if(is.list(poly_result) && !is.null(poly_result$rho)) {
              poly_result$rho
            } else {
              poly_result
            }
          } else {
            cat(sprintf("DEBUG: Computing final %s correlation matrix\n", current_method))
            cor(full_data, method = current_method)
          }
          
          cat("DEBUG: Correlation matrix calculated successfully\n")
          
          # Calculate determinant
          corr_det <- det(full_corr)
          cat(sprintf("DEBUG: Correlation determinant: %.6f\n", corr_det))
          
          # Store the correlation matrix and determinant in the parent environment
          assign("full_corr", full_corr, inherits = TRUE)
          assign("corr_det", corr_det, inherits = TRUE)
          
        }, error = function(e) {
          cat("DEBUG: Error in correlation calculations:", e$message, "\n")
          # Fall back to Spearman correlation if there's an error
          cat("DEBUG: Falling back to Spearman correlation for final calculations\n")
          full_corr <- cor(full_data, method = "spearman")
          corr_det <- det(full_corr)
          # Store the fallback results in the parent environment
          assign("full_corr", full_corr, inherits = TRUE)
          assign("corr_det", corr_det, inherits = TRUE)
        })

        # Continue with rest of existing code...
      })  # End of suppressWarnings block
      
      # Calculate overall reliability statistics
      final_data <- data[, rownames(spearman_corr)]
      cronbach_alpha <- psych::alpha(final_data)$total$raw_alpha
      mcdonalds_omega <- suppressWarnings(
        suppressMessages(
          psych::omega(final_data, plot=FALSE, digits=3)$omega.tot
        )
      )
      
      # Get correlation matrix for ALL loaded variables
      cat("DEBUG: Starting final correlation matrix calculation\n")
      full_data <- data  # Use the original loaded dataset
      
      # Get the correlation method safely
      current_method <- if(!is.null(attr(spearman_corr, "method"))) {
        attr(spearman_corr, "method")
      } else {
        correlation_type  # Fall back to the original correlation_type
      }
      
      cat(sprintf("DEBUG: Using correlation method: %s\n", current_method))
      
      # Calculate final correlation matrix
      full_corr <- tryCatch({
        if(current_method == "polychoric") {
          cat("DEBUG: Computing final polychoric correlation matrix\n")
          poly_result <- suppressWarnings(
            psych::polychoric(as.matrix(full_data), correct = 0, smooth = TRUE)
          )
          if(is.list(poly_result) && !is.null(poly_result$rho)) {
            poly_result$rho
          } else {
            poly_result
          }
        } else {
          cat(sprintf("DEBUG: Computing final %s correlation matrix\n", current_method))
          cor(as.matrix(full_data), method = ifelse(current_method %in% c("pearson", "kendall", "spearman"), 
                                                   current_method, "spearman"))
        }
      }, error = function(e) {
        cat("DEBUG: Error in correlation calculations:", e$message, "\n")
        cat("DEBUG: Falling back to Spearman correlation\n")
        cor(as.matrix(full_data), method = "spearman")
      })
      
      # Calculate correlation matrix determinant
      corr_det <- det(full_corr)
      cat(sprintf("DEBUG: Correlation determinant: %.6f\n", corr_det))
      
      # Calculate univariate normality statistics
      univariate_stats <- apply(full_data, 2, function(x) {
        c(skew = psych::skew(x), kurtosis = psych::kurtosi(x))
      })
      
      # Calculate Mardia's test using full dataset
      mvn_results <- mardia_test(full_data)
      
      # KMO test on full correlation matrix
      kmo_result <- psych::KMO(full_corr)
      
      # Bartlett's test on full correlation matrix
      bartlett_test <- psych::cortest.bartlett(full_corr, n = nrow(full_data))
      
      # Format Bartlett's p-value
      bartlett_pvalue <- if(bartlett_test$p.value < 1e-10) {
        "< .0000000001"
      } else {
        format(round(bartlett_test$p.value, 10), nsmall = 10)
      }
      
      # Create correlation heatmap using full correlation matrix
      heatmap_html <- plot_correlation_heatmap(full_corr)
      
      # Create correlation matrix table using full correlation matrix
      corr_matrix_table <- kable(round(full_corr, 3), format = "html",
                                 row.names = TRUE,
                                 escape = FALSE) %>%
        kable_styling(full_width = FALSE, 
                      position = "left",
                      bootstrap_options = "bordered") %>%
        row_spec(0:nrow(full_corr), extra_css = "border: none !important") %>%
        column_spec(1:ncol(full_corr), extra_css = "border: none !important") %>%
        column_spec(1, extra_css = "font-weight: bold !important;") %>%
        row_spec(0, extra_css = "font-weight: bold !important;")
      
      # Create univariate normality table
      univariate_results <- create_univariate_table(univariate_stats)
      
      # Create reliability table with variance explained
      reliability_table_html <- "<table class='table table-bordered' style='width: auto !important;'>
<tr>
  <th></th>
  <th>Variables</th>
  <th>% of Variance Explained</th>
  <th>Cronbach's alpha</th>
  <th>McDonald's omega</th>
</tr>"

      # Get variance explained
      variance_explained <- efa_result$Vaccounted[2,] * 100

      # Use the factor assignments we already calculated
      for(f in 1:length(factor_items)) {
        # Get the items for this factor
        items <- factor_items[[f]]
        
        # Get reliability statistics we already calculated
        alpha_value <- factor_alphas[f]
        omega_value <- factor_omegas[f]
        
        # Add row to table
        reliability_table_html <- paste0(
          reliability_table_html,
          sprintf("<tr><td>PA%d</td>", f),
          sprintf("<td>%s</td>", paste(items, collapse=", ")),
          sprintf("<td>%.1f</td>", variance_explained[f]),
          sprintf("<td>%s</td>", if(!is.na(alpha_value)) sprintf("%.3f", alpha_value) else "NA"),
          sprintf("<td>%s</td></tr>", if(!is.na(omega_value)) sprintf("%.3f", omega_value) else "NA")
        )
      }

      reliability_table_html <- paste0(reliability_table_html, "</table>")
      
      # Add individual MSA values table - remove duplicate column
      individual_msa <- data.frame(
        Variable = names(kmo_result$MSAi),
        MSA = round(kmo_result$MSAi, 3)
      )
      
      # Add interpretation column after creating the data frame
      individual_msa$Interpretation <- sapply(individual_msa$MSA, function(x) {
        if(x >= 0.90) "Marvelous"
        else if(x >= 0.80) "Meritorious"
        else if(x >= 0.70) "Middling"
        else if(x >= 0.60) "Mediocre"
        else if(x >= 0.50) "Miserable"
        else "Unacceptable"
      })
      
      individual_msa_table <- kable(individual_msa, format = "html", 
                                    row.names = FALSE) %>%  # Added row.names = FALSE to remove the duplicate column
        kable_styling(full_width = FALSE, 
                       position = "left",
                       bootstrap_options = c("striped", "bordered")) %>%
        row_spec(0:nrow(individual_msa), extra_css = "border: 1px solid black;") %>%
        column_spec(1:ncol(individual_msa), border_left = TRUE, border_right = TRUE)
      
      # Generate SPSS syntax
      generate_spss_syntax <- function(factor_items, loadings_matrix, var_names, scale_min, scale_max) {
        # Extract prefix from first variable name (everything before the first number)
        prefix <- gsub("[0-9].*$", "", var_names[1])
        
        # Initialize syntax string with header
        syntax <- "<pre style='background-color: #f8f9fa; padding: 15px; border-radius: 5px;'>"
        syntax <- paste0(syntax, "<h4>SPSS syntax code for identified factors</h4>\n")
        syntax <- paste0(syntax, "* SPSS syntax for computing factor scores based on final EFA solution.\n")
        syntax <- paste0(syntax, "* IMPORTANT: Please review this syntax carefully before use.\n")
        syntax <- paste0(syntax, "* Note: This syntax assumes that any theoretically reverse-coded items\n")
        syntax <- paste0(syntax, "* have already been reverse-coded in your dataset prior to EFA.\n\n")
        syntax <- paste0(syntax, "* Process explanation:\n")
        syntax <- paste0(syntax, "* 1. For items loading positively: Used directly in factor computation\n")
        syntax <- paste0(syntax, "* 2. For items loading negatively: First reverse-coded, then used in computation\n")
        syntax <- paste0(syntax, "* 3. All factor scores are computed as mean scores (sum divided by number of items)\n\n")
        
        # For each factor
        for(f in seq_along(factor_items)) {
          if(length(factor_items[[f]]) > 0) {
            # Get loadings for this factor
            factor_vars <- factor_items[[f]]
            factor_loadings <- loadings_matrix[factor_vars, f]
            
            # Identify negative loadings
            neg_vars <- names(factor_loadings[factor_loadings < 0])
            if(length(neg_vars) > 0) {
              syntax <- paste0(syntax, 
                sprintf("* The following variables loaded negatively on %sFACTOR%d:\n* %s\n",
                prefix, f, paste(neg_vars, collapse=", ")))
              
              # Add reverse coding commands for this factor
              reverse_value <- scale_max + scale_min
              syntax <- paste0(syntax, "\n* Reverse coding negatively loading items:\n")
              for(var in neg_vars) {
                syntax <- paste0(syntax, 
                  sprintf("COMPUTE %s_R = %d - %s.\n", 
                  var, reverse_value, var))
              }
              syntax <- paste0(syntax, "\n")
            }
            
            # Create COMPUTE statement using reversed versions for negative items
            var_terms <- sapply(factor_vars, function(v) {
              if(v %in% neg_vars) paste0(v, "_R") else v
            })
            
            compute_stmt <- sprintf("COMPUTE %sFACTOR%d = (%s) / %d.\n",
                                  prefix, f,
                                  paste(var_terms, collapse=" + "),
                                  length(factor_vars))
            
            syntax <- paste0(syntax, compute_stmt)
          }
        }
        
        syntax <- paste0(syntax, "EXECUTE.\n")  # Append EXECUTE command here
        syntax <- paste0(syntax, "</pre>")
        return(syntax)
      }
      
      # Generate SPSS syntax
      spss_syntax <- generate_spss_syntax(
        factor_items, 
        loadings_matrix = as.matrix(efa_result$loadings),
        var_names = colnames(data),
        scale_min = input$scale_min,
        scale_max = input$scale_max
      )
      
      # Create final HTML summary
      final_summary_html <- paste0(
        "<h2>Overall Summary</h2>",
        
        "<p><strong>Dataset Information:</strong><br>",
        "• Dataset name: ", filename, "<br>",
        "• Number of rows of data: ", nrow(data), "<br>",
        "• Report generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "</p>",
        
        "<p><strong>Analysis Settings:</strong><br>",
        "• Number of Factors: ", nfactors, "<br>",
        "• Rotation Method: ", rotation, "<br>",
        "• Factor Method: ", extraction_method, "<br>",
        "• Correlation Method: ", correlation_type, 
        if(correlation_type == "polychoric") {
          " (Note: Polychoric correlations are specifically designed for ordinal data like Likert scales)"
        } else "",
        "<br>",
        "• Loading Threshold: ", loading_threshold, "<br>",
        "• Communality Threshold: ", communality_threshold, "</p>",
        
        "<p><strong>Analysis Results:</strong><br>",
        "• Total Iterations: ", iteration, "<br>",
        "• Initial Variables: ", initial_item_count, "<br>",
        "• Final Variables: ", nrow(spearman_corr), "</p>",
        
        "<h3>Reliability Statistics</h3>",
        "<p><strong>Overall Scale Reliability:</strong><br>",
        "• Cronbach's alpha: ", round(cronbach_alpha, 3), "<br>",
        "• McDonald's omega: ", round(mcdonalds_omega, 3), "</p>",
        
        "<h4>Factor-Specific Reliability:</h4>",
        reliability_table_html,
        "<br>",  # Add spacing
        spss_syntax,
        
        "<h3>Correlation Matrix Analysis</h3>",
        "<h4>Correlation Heatmap</h4>",
        heatmap_html,
        
        "<h4>Correlation Matrix</h4>",
        corr_matrix_table,
        
        "<h4>Correlation Matrix Determinant</h4>",
        "<p>Determinant: ", format(corr_det, digits = 8, scientific = FALSE),
        "<br><em>Interpretation:</em> ", 
        if(corr_det < 0.00001) {
          "Very low determinant indicates potential multicollinearity issues. This suggests some variables may be too highly correlated, which could affect factor stability."
        } else if(corr_det < 0.0001) {
          "Low determinant suggests some correlation redundancy. While not critical, this indicates moderate levels of correlation between variables."
        } else if(corr_det < 0.001) {
          "Acceptable determinant value, though some moderate correlations exist. This is common and generally not problematic."
        },
        "</p>",
        
        "<h3>Normality Tests</h3>",
        "<h4>Univariate Normality</h4>",
        univariate_results$table,
        univariate_results$summary,
        
        "<h4>Multivariate Normality Test (Mardia's Test)</h4>",
        "<p><strong>Note:</strong> This test is performed on the original dataset values, before any correlation matrix transformations.</p>",
        "<p><strong>Results:</strong><br>",
        "Mardia's Multivariate Skewness: ", round(mvn_results$skewness, 5), "<br>",
        "Mardia's Multivariate Kurtosis: ", round(mvn_results$kurtosis, 5), "<br>",
        "Skewness p-value: ", format(mvn_results$skew_p, digits = 5, scientific = FALSE), "<br>",
        "Kurtosis p-value: ", format(mvn_results$kurt_p, digits = 5, scientific = FALSE), "</p>",
        
        "<p><strong>Interpretation:</strong><br>",
        if(mvn_results$skew_p < 0.05 || mvn_results$kurt_p < 0.05) {
          paste0(
            "• Your data significantly deviates from multivariate normality (p < 0.05).<br>",
            "• This is common in real-world data and has implications for your factor analysis:<br>",
            "&nbsp;&nbsp;1. You should use the Spearman correlation matrix (currently selected: ", correlation_type, ")<br>",
            "&nbsp;&nbsp;2. Consider robust estimation methods like Principal Axis Factoring (currently selected: ", extraction_method, ")<br>",
            "&nbsp;&nbsp;3. Be cautious with Maximum Likelihood estimation as it assumes multivariate normality<br>",
            "• Your current settings are ", 
            if(correlation_type == "spearman" && extraction_method != "ml") {
              "appropriate for non-normal data."
            } else {
              "not optimal for non-normal data. Consider adjusting them as suggested above."
            }
          )
        } else {
          paste0(
            "• Your data does not significantly deviate from multivariate normality (p ≥ 0.05).<br>",
            "• This means you can confidently use either Pearson or Spearman correlations<br>",
            "• All factor extraction methods, including Maximum Likelihood, are appropriate"
          )
        },
        "</p>",
        
        "<h3>Sampling Adequacy Tests</h3>",
        "<h4>KMO (Kaiser-Meyer-Olkin) Test</h4>",
        "<p><strong>What is KMO?</strong><br>",
        "The KMO test measures the proportion of variance among your variables that might be common variance. ",
        "It indicates the suitability of your data for factor analysis by examining patterns of correlations.</p>",
        
        "<p><strong>Overall Results:</strong><br>",
        "Overall MSA: ", round(kmo_result$MSA, 3),
        "<br><strong>Interpretation:</strong> ", 
        if(kmo_result$MSA >= 0.90) {
          "Marvelous - Your data is exceptionally suited for factor analysis. The variables share a very high proportion of common variance, suggesting a strong underlying factor structure."
        } else if(kmo_result$MSA >= 0.80) {
          "Meritorious - Your data is very well suited for factor analysis. The variables share a substantial amount of common variance, indicating clear underlying factors."
        } else if(kmo_result$MSA >= 0.70) {
          "Middling - Factor analysis should yield reliable factors. While not optimal, there is sufficient common variance to proceed with the analysis."
        } else if(kmo_result$MSA >= 0.60) {
          "Mediocre - Factor analysis results should be interpreted with caution. The common variance between variables is relatively low, which may affect factor stability."
        } else if(kmo_result$MSA >= 0.50) {
          "Miserable - Factor analysis may not yield reliable results. The variables share minimal common variance, suggesting weak underlying factors."
        } else {
          "Unacceptable - Your data is not suitable for factor analysis. There is insufficient common variance between variables to identify meaningful factors."
        },
        "</p>",
        
        "<p><strong>Individual Variable MSA Values:</strong><br>",
        "These values show how well each variable fits with the factor analysis model:</p>",
        individual_msa_table,
        "<p><em>Note: Consider removing variables with unacceptable MSA values (< 0.50) before proceeding with factor analysis.</em></p>",
        
        "<h4>Bartlett's Test of Sphericity</h4>",
        "<p><strong>Note:</strong> This test is performed on the ", correlation_type, " correlation matrix.</p>",
        "<p><strong>Results:</strong><br>",
        "Chi-Square: ", format(bartlett_test$chisq, digits = 10), "<br>",
        "Degrees of Freedom: ", bartlett_test$df, "<br>",
        "p-value: ", bartlett_pvalue, "</p>",
        
        "<p><strong>What does this test tell us?</strong><br>",
        "Bartlett's test examines whether your correlation matrix is significantly different from an identity matrix ",
        "(a matrix where variables correlate perfectly with themselves but have zero correlation with other variables).</p>",
        
        "<p><strong>Interpretation:</strong><br>",
        if(bartlett_test$p.value < 0.05) {
          paste0(
            " The test is significant (p < 0.05)<br>",
            "• This indicates your variables are sufficiently correlated for factor analysis<br>",
            "• You can proceed with your factor analysis"
          )
        } else {
          paste0(
            "• The test is not significant (p ≥ 0.05)<br>",
            "• This suggests your variables may not be sufficiently correlated<br>",
            "• Factor analysis may not be appropriate for your data<br>",
            "• Consider examining your correlation matrix for patterns of relationships"
          )
        },
        "</p>"
      )
      
      # Create final HTML with summary at top
      html_output_all <- paste0(
        final_summary_html,
        "<hr><h2>Detailed Iteration Results</h2>",
        paste(iteration_results, collapse = "<hr>")
      )
      
      break
    }
    
    # Append this iteration's HTML to the overall output
    html_output_all <- paste0(html_output_all, html_output_iteration)
    
    # Remove identified items for next iteration
    spearman_corr <- spearman_corr[!rownames(spearman_corr) %in% items_to_remove, 
                                 !colnames(spearman_corr) %in% items_to_remove]
    iteration <- iteration + 1
  }  # End of repeat loop
  
  # Add error handling for final statistics calculation
  tryCatch({
    # Calculate final statistics
    final_loadings <- efa_result$loadings
    final_items <- colnames(spearman_corr)
    
    # Debug output
    cat("DEBUG: Calculating final statistics\n")
    cat("DEBUG: Number of final items:", length(final_items), "\n")
    cat("DEBUG: Final loadings dimensions:", dim(final_loadings)[1], "x", dim(final_loadings)[2], "\n")
    
    # Calculate factor statistics with more robust error checking
    factor_stats <- lapply(1:ncol(final_loadings), function(f) {
        tryCatch({
            items <- final_items[abs(final_loadings[,f]) >= loading_threshold]
            cat(sprintf("DEBUG: Factor %d has %d items: %s\n", 
                       f, length(items), paste(items, collapse=", ")))
            
            if(length(items) >= 3) {  # Only calculate reliability if 3 or more items
                factor_data <- data[, items, drop=FALSE]
                alpha <- psych::alpha(factor_data)$total$raw_alpha
                omega <- suppressWarnings(
                    suppressMessages(
                        psych::omega(factor_data, plot=FALSE, digits=3)$omega.tot
                    )
                )
                return(list(
                    items = items,
                    alpha = alpha,
                    omega = omega,
                    valid = TRUE
                ))
            } else {
                cat(sprintf("DEBUG: Factor %d has insufficient items (%d) for reliability calculation\n", 
                           f, length(items)))
                return(list(
                    items = items,
                    alpha = NA,
                    omega = NA,
                    valid = FALSE
                ))
            }
        }, error = function(e) {
            cat(sprintf("DEBUG: Error processing factor %d: %s\n", f, e$message))
            return(list(
                items = character(0),
                alpha = NA,
                omega = NA,
                valid = FALSE
            ))
        })
    })
    
    # Safely extract results
    factor_items <- lapply(factor_stats, function(x) x$items)
    factor_alphas <- sapply(factor_stats, function(x) x$alpha)
    factor_omegas <- sapply(factor_stats, function(x) x$omega)
    
    # Calculate overall statistics only for factors with valid reliability
    valid_items <- unique(unlist(
        lapply(factor_stats[sapply(factor_stats, `[[`, "valid")], `[[`, "items")
    ))
    
    if(length(valid_items) >= 3) {
        cat(sprintf("DEBUG: Calculating overall reliability with %d valid items\n", 
                   length(valid_items)))
        final_data <- data[, valid_items, drop=FALSE]
        cronbach_alpha <- psych::alpha(final_data)$total$raw_alpha
        mcdonalds_omega <- suppressWarnings(
            suppressMessages(
                psych::omega(final_data, plot=FALSE, digits=3)$omega.tot
            )
        )
    } else {
        cat("DEBUG: Insufficient valid items for overall reliability calculation\n")
        cronbach_alpha <- NA
        mcdonalds_omega <- NA
    }
    
    cat("DEBUG: Final statistics calculated successfully\n")
    
}, error = function(e) {
    # Log the error and return NA values
    cat("ERROR in final statistics calculation:", conditionMessage(e), "\n")
    factor_items <- vector("list", nfactors)
    factor_alphas <- rep(NA, nfactors)
    factor_omegas <- rep(NA, nfactors)
    cronbach_alpha <- NA
    mcdonalds_omega <- NA
})
  
  # At the end of the function, restore default device
  dev.off()
  
  # Before returning results, ensure all components are properly initialized
  if(is.null(efa_result)) efa_result <- list()
  if(is.null(iteration_removals)) iteration_removals <- list()
  if(is.null(factor_items)) factor_items <- vector("list", nfactors)
  if(is.null(factor_alphas)) factor_alphas <- rep(NA, nfactors)
  if(is.null(factor_omegas)) factor_omegas <- rep(NA, nfactors)
  if(is.null(cronbach_alpha)) cronbach_alpha <- NA
  if(is.null(mcdonalds_omega)) mcdonalds_omega <- NA
  if(is.null(html_output_all)) html_output_all <- "No results generated"
  if(is.null(correlation_warnings)) correlation_warnings <- character(0)
  
  # Create the final results list
  final_results <- list(
    efa_result = efa_result,
    iteration_removals = iteration_removals,
    factor_items = factor_items,
    factor_alphas = factor_alphas,
    factor_omegas = factor_omegas,
    final_alpha = cronbach_alpha,
    final_omega = mcdonalds_omega,
    html_output = html_output_all,
    warnings = character(0),
    correlation_warnings = correlation_warnings
  )
  
  # Add debug output before returning
  cat("\nDEBUG: Returning final results")
  cat("\nDEBUG: Results class:", class(final_results))
  cat("\nDEBUG: Results names:", paste(names(final_results), collapse=", "))
  
  # At the end of the function, properly close devices
  tryCatch({
    while (dev.cur() > 1) dev.off()
  }, error = function(e) {
    cat("\nDEBUG: Error closing graphics devices:", e$message, "\n")
  })
  
  return(final_results)
}  # End of perform_iterative_efa function

