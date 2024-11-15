library(shiny)
library(psych)  # For factor analysis
library(nFactors)  # For parallel analysis and scree plot
library(DT)  # For displaying data tables
library(ggplot2)  # For enhanced plotting
library(rmarkdown)
library(htmltools)
library(kableExtra)

# Define UI for Module 2: Factor Estimation
mod2_factors_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Module 2: Factor Estimation Using Multiple Methods"),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("datafile"), "Upload CSV File", accept = ".csv"),
        checkboxInput(ns("header"), "Dataset has a header row", TRUE),
        numericInput(ns("maxFactors"), "Maximum Number of Factors to Consider", value = 10, min = 1, step = 1),
        actionButton(ns("runFactorEstimation"), "Estimate Number of Factors", 
                     class = "btn-primary btn-block"),
        br(),  # Add some spacing
        downloadButton(ns("exportHTML"), "Download Results", 
                       class = "btn-primary btn-block")
      ),
      mainPanel(
        h3("Uploaded Dataset"),
        DTOutput(ns("dataTable")),
        hr(),
        h3("Variance Explained"),
        plotOutput(ns("variancePlot")),
        DTOutput(ns("varianceTable")),
        downloadButton(ns("downloadResults"), "Download Results"),
        hr(),
        h3("Factor Estimation Results"),
        verbatimTextOutput(ns("factorResults")),
        plotOutput(ns("screePlot")),
        plotOutput(ns("vssPlot")),
        hr(),
        h3("Summary and Interpretation"),
        uiOutput(ns("factorInterpretation"))
      )
    )
  )
}

# Define server logic for Module 2 using moduleServer
mod2_factors_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    dataset <- reactiveVal()
    
    # Add the reactive values object to store results
    results <- reactiveValues(
      kaiser_factors = NULL,
      pa_factors = NULL,
      map_factors = NULL,
      reckase_factors = NULL,
      var_table = NULL,
      eigenvalues = NULL,
      variance_explained = NULL,
      cum_variance = NULL
    )
    
    # Load dataset when file is uploaded
    observeEvent(input$datafile, {
      req(input$datafile)
      data <- read.csv(input$datafile$datapath, header = input$header)
      dataset(data)
      output$dataTable <- renderDT({
        datatable(data)
      })
    })
    
    # Perform factor estimation
    observeEvent(input$runFactorEstimation, {
      req(dataset())
      data <- dataset()
      
      # Ensure all columns are numeric
      numeric_data <- data %>% select_if(is.numeric)
      if (ncol(numeric_data) < 2) {
        output$factorResults <- renderPrint({
          "The dataset needs at least two numeric variables for factor analysis."
        })
        return()
      }
      
      # Kaiser Criterion (Eigenvalue > 1)
      fa_eigen <- eigen(cor(numeric_data))$values
      kaiser_factors <- sum(fa_eigen > 1)
      
      # Parallel Analysis
      pa <- parallel(subject = nrow(numeric_data), var = ncol(numeric_data), rep = 100, cent = .05)
      pa_factors <- sum(fa_eigen > pa$eigen$qevpea)
      
      # Scree Plot
      output$screePlot <- renderPlot({
        scree(numeric_data, factors = TRUE, pc = TRUE, main = "Scree Plot")
        legend("topright", legend = c("PC (Principal Components)", "FA (Factor Analysis)"), lty = 1:2, col = 1:2)
      })
      
      # Velicer's MAP Test using VSS function from psych package
      map_result <- VSS(numeric_data, n = input$maxFactors, rotate = "none", fm = "minres")
      map_factors <- which.min(map_result$map)
      
      # Calculate eigenvalues and variance explained
      cor_matrix <- cor(numeric_data)
      eigen_results <- eigen(cor_matrix)
      eigenvalues <- eigen_results$values
      
      # Calculate variance explained
      var_explained <- eigenvalues / sum(eigenvalues) * 100
      cum_var <- cumsum(var_explained)
      
      # Create variance explained table
      var_table <- data.frame(
        Factor = 1:length(eigenvalues),
        Eigenvalue = eigenvalues,
        `Variance Explained (%)` = var_explained,
        `Cumulative Variance (%)` = cum_var
      )
      
      # Additional retention criteria
      # Reckase criterion (variance explained > 5%)
      reckase_factors <- sum(var_explained > 5)
      
      # Create variance plot
      output$variancePlot <- renderPlot({
        var_df <- data.frame(
          Factor = 1:length(eigenvalues),
          Individual = var_explained,
          Cumulative = cum_var
        )
        
        ggplot(var_df, aes(x = Factor)) +
          geom_col(aes(y = Individual), fill = "steelblue", alpha = 0.7) +
          geom_line(aes(y = Cumulative, color = "Cumulative"), linewidth = 1) +
          geom_point(aes(y = Cumulative), color = "red", size = 3) +
          scale_y_continuous(
            name = "Variance Explained (%)",
            sec.axis = sec_axis(~., name = "Cumulative Variance (%)")
          ) +
          labs(title = "Scree Plot with Cumulative Variance",
               x = "Factor Number") +
          theme_minimal() +
          scale_color_manual(values = c("Cumulative" = "red")) +
          theme(legend.title = element_blank())
      })
      
      # Display variance table
      output$varianceTable <- renderDT({
        datatable(var_table, 
                  options = list(pageLength = 5),
                  rownames = FALSE)
      })
      
      # Create downloadable results
      output$downloadResults <- downloadHandler(
        filename = function() {
          paste("factor_analysis_results_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          write.csv(var_table, file, row.names = FALSE)
        }
      )
      
      # Update factor results output
      output$factorResults <- renderPrint({
        cat("Factor Estimation Results:\n")
        cat("---------------------------\n")
        cat("Kaiser Criterion (eigenvalue > 1): ", kaiser_factors, " factors\n")
        cat("Parallel Analysis: ", pa_factors, " factors\n")
        cat("Velicer's MAP Test: ", map_factors, " factors\n")
        cat("Reckase Criterion (variance > 5%): ", reckase_factors, " factors\n")
      })
      
      # Add VSS analysis
      vss_result <- VSS(numeric_data, n = input$maxFactors, rotate = "varimax", fm = "minres")
      
      # Create VSS plot - remove the title argument completely
      output$vssPlot <- renderPlot({
        # Set the title using base R plotting parameters
        par(mar = c(5, 4, 4, 2) + 0.1)  # Adjust margins if needed
        VSS.plot(vss_result)        
      })
      
      # Enhanced interpretation
      output$factorInterpretation <- renderUI({
        tagList(
          h4("Factor Retention Criteria Results:"),
          tags$ul(
            tags$li(strong("Kaiser Criterion"), " (eigenvalue > 1) suggests ", kaiser_factors, " factors"),
            tags$li(strong("Parallel Analysis"), " suggests ", pa_factors, " factors"),
            tags$li(strong("Velicer's MAP Test"), " suggests ", map_factors, " factors"),
            tags$li(strong("Reckase Criterion"), " (variance > 5%) suggests ", reckase_factors, " factors")
        ),
        h4("Very Simple Structure (VSS) Analysis:"),
        tags$ul(
          tags$li("The VSS plot shows how well different factor solutions fit the data at different levels of complexity"),
          tags$li("Complexity 1: Items load primarily on one factor (cluster-like solution)"),
          tags$li("Complexity 2: Items can load on up to two factors"),
          tags$li("Higher complexities (3-4): Items can load on multiple factors"),
          tags$li("Look for where the lines peak or level off - this suggests the optimal number of factors"),
          tags$li("Solutions with complexity 1-2 are typically most interpretable in psychological research")
        ),
        h4("Interpretation Guidelines:"),
        tags$ul(
          tags$li("Kaiser Criterion tends to overestimate the number of factors"),
          tags$li("Parallel Analysis is generally considered more accurate"),
          tags$li("Consider both statistical criteria and theoretical interpretability"),
          tags$li("Look for the 'elbow' in the scree plot where the curve levels off"),
          tags$li("Consider retaining factors that explain at least 5% of variance"),
          tags$li(HTML("For cumulative variance explained in psychological/social science research:<br>",
                       "- 50-60% is considered adequate<br>",
                       "- 60-70% is considered good<br>",
                       "- >70% is considered excellent<br>",
                       "Note: These thresholds are lower than in physical sciences where 80-90% might be expected"))
        ),
        tags$p("The variance explained table and plots provide additional context for your decision. 
        The bar plot shows individual variance explained by each factor, while the line shows 
           cumulative variance explained.")        
        )
      })
      
      # Store results in reactive values
      results$kaiser_factors <- kaiser_factors
      results$pa_factors <- pa_factors
      results$map_factors <- map_factors
      results$reckase_factors <- reckase_factors
      results$var_table <- var_table
      results$eigenvalues <- eigenvalues
      results$variance_explained <- var_explained
      results$cum_variance <- cum_var
    })
    
    # Create downloadable HTML report
    output$exportHTML <- downloadHandler(
      filename = function() {
        "Estimated_Number_of_Factors.html"
      },
      content = function(file) {
        # Generate Scree Plot for static HTML output
        temp_plot <- tempfile(fileext = ".png")
        png(temp_plot)
        scree(dataset() %>% select_if(is.numeric), factors = TRUE, pc = TRUE, main = "Scree Plot")
        legend("topright", legend = c("PC (Principal Components)", "FA (Factor Analysis)"), lty = 1:2, col = 1:2)
        dev.off()
        
        # Manually recreate the summary and interpretation text
        interpretation_content <- paste0(
          "<h4>Factor Retention Criteria Results:</h4>",
          "<ul>",
          "<li><strong>Kaiser Criterion</strong> (eigenvalue > 1) suggests ", results$kaiser_factors, " factors</li>",
          "<li><strong>Parallel Analysis</strong> suggests ", results$pa_factors, " factors</li>",
          "<li><strong>Velicer's MAP Test</strong> suggests ", results$map_factors, " factors</li>",
          "<li><strong>Reckase Criterion</strong> (variance > 5%) suggests ", results$reckase_factors, " factors</li>",
          "</ul>",
          "<h4>Very Simple Structure (VSS) Analysis:</h4>",
          "<ul>",
          "<li>The VSS plot shows how well different factor solutions fit the data at different levels of complexity</li>",
          "<li>Complexity 1: Items load primarily on one factor (cluster-like solution)</li>",
          "<li>Complexity 2: Items can load on up to two factors</li>",
          "<li>Higher complexities (3-4): Items can load on multiple factors</li>",
          "<li>Look for where the lines peak or level off - this suggests the optimal number of factors</li>",
          "<li>Solutions with complexity 1-2 are typically most interpretable in psychological research</li>",
          "</ul>",
          "<h4>Interpretation Guidelines:</h4>",
          "<ul>",
          "<li>Kaiser Criterion tends to overestimate the number of factors</li>",
          "<li>Parallel Analysis is generally considered more accurate</li>",
          "<li>Consider both statistical criteria and theoretical interpretability</li>",
          "<li>Look for the 'elbow' in the scree plot where the curve levels off</li>",
          "<li>Consider retaining factors that explain at least 5% of variance</li>",
          "<li>For cumulative variance explained in psychological/social science research:<br>",
          "- 50-60% is considered adequate<br>",
          "- 60-70% is considered good<br>",
          "- >70% is considered excellent<br>",
          "Note: These thresholds are lower than in physical sciences where 80-90% might be expected</li>",
          "</ul>",
          "<p>The variance explained table and plots provide additional context for your decision.</p>"
        )
        
        # Create HTML content for the report
        html_content <- paste0(
          "<html><body>",
          "<h3>Factor Estimation Results</h3>",
          "<pre>",
          "Kaiser Criterion (eigenvalue > 1): ", results$kaiser_factors, " factors<br>",
          "Parallel Analysis: ", results$pa_factors, " factors<br>",
          "Velicer's MAP Test: ", results$map_factors, " factors<br>",
          "Reckase Criterion (variance > 5%): ", results$reckase_factors, " factors",
          "</pre>",
          "<h3>Variance Explained Table</h3>",
          kable(results$var_table, format = "html") %>% kable_styling(full_width = FALSE),
          "<h3>Scree Plot</h3>",
          "<img src='", temp_plot, "' width='600'>",
          "<h3>VSS Plot</h3>",
          "<img src='", temp_plot, "' width='600'>",
          "<h3>Summary and Interpretation</h3>",
          interpretation_content,
          "</body></html>"
        )
        
        writeLines(html_content, file)
      }
    )
  })
}
