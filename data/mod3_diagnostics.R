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
library(MVN)

# Add helper functions
format_small_value <- function(x) {
  if(x < 1e-10) {
    "< .0000000001"
  } else {
    format(round(x, 10), nsmall = 10)
  }
}

plot_correlation_heatmap <- function(corr_matrix) {
  melted_corr <- melt(corr_matrix)
  p <- ggplot(melted_corr, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    geom_text(aes(label = sprintf("%.2f", value)), size = 3) +
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

# Define UI for Module 3: Data Diagnostics
mod3_diagnostics_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Data Diagnostics for EFA"),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("data_file"), "Upload CSV File", accept = ".csv"),
        checkboxInput(ns("header"), "Dataset has a header row", TRUE),
        actionButton(ns("run_diagnostics"), "Run Diagnostics",
                     class = "btn-primary"),
        hr(),
        downloadButton(ns("export_diagnostics"), "Save Diagnostics Report")
      ),
      mainPanel(
        uiOutput(ns("diagnostics_output"))
      )
    )
  )
}

# Define server logic for Module 3
mod3_diagnostics_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values to store diagnostics results
    diagnostics <- reactiveVal(NULL)
    
    # Observe run diagnostics button click
    observeEvent(input$run_diagnostics, {
      req(input$data_file)
      
      # Show progress
      withProgress(message = 'Running Diagnostics', value = 0, {
        
        # Read the data
        data <- read.csv(input$data_file$datapath, header = input$header)
        
        # Ensure numeric data
        numeric_data <- data %>% select_if(is.numeric)
        
        # Basic information
        basic_info <- list(
          n_rows = nrow(numeric_data),
          n_cols = ncol(numeric_data),
          variable_names = names(numeric_data)
        )
        
        # Missing data analysis
        missing_analysis <- sapply(numeric_data, function(x) sum(is.na(x)))
        missing_summary <- list(
          has_missing = any(missing_analysis > 0),
          total_missing = sum(missing_analysis),
          vars_with_missing = names(missing_analysis[missing_analysis > 0]),
          missing_counts = missing_analysis[missing_analysis > 0]
        )
        
        # Normality tests
        univariate_stats <- apply(numeric_data, 2, function(x) {
          c(skew = psych::skew(x), kurtosis = psych::kurtosi(x))
        })
        
        mvn_results <- mardia_test(numeric_data)
        
        # Correlation analysis
        corr_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
        corr_det <- det(corr_matrix)
        
        # Sampling adequacy tests
        kmo_result <- psych::KMO(corr_matrix)
        bartlett_test <- psych::cortest.bartlett(corr_matrix, n = nrow(numeric_data))
        
        # Store all results
        diagnostics(list(
          basic_info = basic_info,
          missing_analysis = missing_summary,
          univariate_stats = univariate_stats,
          mvn_results = mvn_results,
          corr_matrix = corr_matrix,
          corr_det = corr_det,
          kmo_result = kmo_result,
          bartlett_test = bartlett_test
        ))
        
        # Update progress
        incProgress(1)
      })
    })
    
    # Render all diagnostics output
    output$diagnostics_output <- renderUI({
      req(diagnostics())
      
      results <- diagnostics()
      
      # Generate HTML output to render
      create_report_template(results)
    })
    
    # Function to create report HTML content
    create_report_template <- function(results) {
      # Check if results$missing_analysis exists and is not NULL
      missing_section <- if(is.null(results$missing_analysis)) {
        "<p>Missing data analysis not available.</p>"
      } else if(!results$missing_analysis$has_missing) {
        "<p><strong>✓ No missing values detected in the dataset.</strong></p>"
      } else {
        paste0(
          "<p><strong>⚠ Warning: Missing values detected!</strong><br>",
          "Total missing values: ", results$missing_analysis$total_missing, "<br>",
          "Variables with missing values:<br>",
          paste(mapply(function(var, count) {
            paste0("• ", var, ": ", count, " missing values")
          }, results$missing_analysis$vars_with_missing, 
          results$missing_analysis$missing_counts), 
          collapse = "<br>"),
          "</p>",
          "<p><em>Recommendation: Please address missing values before proceeding with EFA. 
          Common approaches include:<br>
          - Listwise deletion (if missing data is minimal)<br>
          - Mean substitution (not recommended for EFA)<br>
          - Multiple imputation (preferred for EFA)</em></p>"
        )
      }
      
      # Return the complete HTML content
      HTML(paste0(
        "<h3>Dataset Overview</h3>",
        "<p><strong>Basic Information:</strong><br>",
        "Number of rows: ", results$basic_info$n_rows, "<br>",
        "Number of columns: ", results$basic_info$n_cols, "<br>",
        "Variables: ", paste(results$basic_info$variable_names, collapse = ", "), "</p>",
        
        "<h3>Missing Data Analysis</h3>",
        missing_section,
        
        "<h3>Normality Assessment</h3>",
        "<h4>Univariate Normality</h4>",
        create_univariate_table(results$univariate_stats)$table,
        create_univariate_table(results$univariate_stats)$summary,
        
        "<h4>Multivariate Normality Test (Mardia's Test)</h4>",
        "<p><strong>Results:</strong><br>",
        "Mardia's Multivariate Skewness: ", round(results$mvn_results$skewness, 5), "<br>",
        "Mardia's Multivariate Kurtosis: ", round(results$mvn_results$kurtosis, 5), "<br>",
        "Skewness p-value: ", format(results$mvn_results$skew_p, digits = 5, scientific = FALSE), "<br>",
        "Kurtosis p-value: ", format(results$mvn_results$kurt_p, digits = 5, scientific = FALSE), "</p>",
        
        "<p><strong>Interpretation:</strong><br>",
        if(results$mvn_results$skew_p < 0.05 || results$mvn_results$kurt_p < 0.05) {
          paste0(
            "• Your data significantly deviates from multivariate normality (p < 0.05).<br>",
            "• This is common in real-world data and has implications for your factor analysis:<br>",
            "&nbsp;&nbsp;1. You should use the Spearman correlation matrix<br>",
            "&nbsp;&nbsp;2. Consider robust estimation methods like Principal Axis Factoring<br>",
            "&nbsp;&nbsp;3. Be cautious with Maximum Likelihood estimation as it assumes multivariate normality"
          )
        } else {
          paste0(
            "• Your data does not significantly deviate from multivariate normality (p ≥ 0.05).<br>",
            "• This means you can confidently use either Pearson or Spearman correlations<br>",
            "• All factor extraction methods, including Maximum Likelihood, are appropriate"
          )
        },
        "</p>",
        
        "<h3>Correlation Analysis</h3>",
        "<h4>Correlation Matrix Heatmap</h4>",
        plot_correlation_heatmap(results$corr_matrix),
        
        "<h4>Matrix Determinant</h4>",
        "<p><strong>Matrix Health Indicators:</strong><br>",
        "• Matrix determinant: ", format_small_value(results$corr_det), 
        "<br><em>Interpretation:</em> ", 
        if(results$corr_det < 0.00001) {
          "Very low determinant indicates potential multicollinearity issues. This suggests some variables may be too highly correlated, which could affect factor stability."
        } else if(results$corr_det < 0.0001) {
          "Low determinant suggests some correlation redundancy. While not critical, this indicates moderate levels of correlation between variables."
        } else if(results$corr_det < 0.001) {
          "Acceptable determinant value, though some moderate correlations exist. This is common and generally not problematic."
        } else {
          "Good determinant value indicating healthy levels of correlation without excessive multicollinearity."
        },
        "</p>",
        
        "<h3>Sampling Adequacy</h3>",
        "<h4>KMO (Kaiser-Meyer-Olkin) Test</h4>",
        "<p><strong>What is KMO?</strong><br>",
        "The KMO test measures the proportion of variance among your variables that might be common variance. It indicates the suitability of your data for factor analysis by examining patterns of correlations.</p>",
        
        "<p><strong>Overall Results:</strong><br>",
        "Overall MSA: ", round(results$kmo_result$MSA, 3),
        "<br><strong>Interpretation:</strong> ", 
        if(results$kmo_result$MSA >= 0.90) {
          "Marvelous - Your data is exceptionally suited for factor analysis. The variables share a very high proportion of common variance, suggesting a strong underlying factor structure."
        } else if(results$kmo_result$MSA >= 0.80) {
          "Meritorious - Your data is very well suited for factor analysis. The variables share a substantial amount of common variance, indicating clear underlying factors."
        } else if(results$kmo_result$MSA >= 0.70) {
          "Middling - Factor analysis should yield reliable factors. While not optimal, there is sufficient common variance to proceed with the analysis."
        } else if(results$kmo_result$MSA >= 0.60) {
          "Mediocre - Factor analysis results should be interpreted with caution. The common variance between variables is relatively low, which may affect factor stability."
        } else if(results$kmo_result$MSA >= 0.50) {
          "Miserable - Factor analysis may not yield reliable results. The variables share minimal common variance, suggesting weak underlying factors."
        } else {
          "Unacceptable - Your data is not suitable for factor analysis. There is insufficient common variance between variables to identify meaningful factors."
        },
        "</p>",
        
        "<h4>Individual MSA Values</h4>",
        kable(data.frame(
          Variable = names(results$kmo_result$MSAi),
          MSA = round(results$kmo_result$MSAi, 3),
          Interpretation = sapply(results$kmo_result$MSAi, function(x) {
            case_when(
              x >= 0.9 ~ "Marvelous",
              x >= 0.8 ~ "Meritorious",
              x >= 0.7 ~ "Middling",
              x >= 0.6 ~ "Mediocre",
              x >= 0.5 ~ "Miserable",
              TRUE ~ "Unacceptable"
            )
          })
        ), format = "html", row.names = FALSE) %>%
          kable_styling(full_width = FALSE, 
                        position = "left",
                        bootstrap_options = "bordered"),
        
        "<h4>Bartlett's Test of Sphericity</h4>",
        "<p><strong>What does this test tell us?</strong><br>",
        "Bartlett's test examines whether your correlation matrix is significantly different from an identity matrix (a matrix where variables correlate perfectly with themselves but have zero correlation with other variables).</p>",
        
        "<p><strong>Results:</strong><br>",
        "Chi-Square: ", format(results$bartlett_test$chisq, digits = 10), "<br>",
        "Degrees of Freedom: ", results$bartlett_test$df, "<br>",
        "p-value: ", format_small_value(results$bartlett_test$p.value), "</p>",
        
        "<p><strong>Interpretation:</strong><br>",
        if(results$bartlett_test$p.value < 0.05) {
          paste0(
            "• The test is significant (p < 0.05)<br>",
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
        "</p>",
        
        "<h3>Recommendations for EFA</h3>",
        "<p>Based on the diagnostic results, the following settings are recommended for your EFA:</p>",
        
        "<h4>Suggested Thresholds:</h4>",
        "<p>General guidelines for interpretation:</p>",
        "<ul>",
        "<li>Factor Loading threshold: <strong>0.3 to 0.4</strong> (minimum)<br>",
        "<em>Note: Higher thresholds (e.g., 0.5 or 0.6) may be more appropriate for smaller sample sizes or exploratory work</em></li>",
        "<li>Communality threshold: <strong>0.2</strong> (minimum)<br>",
        "<em>Note: Variables with communalities below this threshold may not be well-represented in the factor solution</em></li>",
        "</ul>",
        "<p><em>Note: These are general guidelines. Your final choice of thresholds should consider:<br>",
        "• Sample size (larger samples can support lower thresholds)<br>",
        "• Research phase (confirmatory work may require stricter thresholds)<br>",
        "• Theoretical importance of variables<br>",
        "• Domain-specific standards in your field</em></p>"
      ))
    }
    
    # Handle download of the diagnostics report
    output$export_diagnostics <- downloadHandler(
      filename = function() {
        "Diagnostic Report.html"
      },
      content = function(file) {
        results <- diagnostics()
        if (is.null(results)) return(NULL)
        
        # Write the HTML content to the file
        html_content <- create_report_template(results)
        writeLines(as.character(html_content), file)
      }
    )
  })
}
