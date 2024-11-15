library(shiny)

# Define UI for Module 0: Introduction
mod0_introduction_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Welcome to gbEFA: Guided Best-practice Exploratory Factor Analysis"),
    
    # Main content using HTML for formatting
    HTML('
      <div class="container" style="max-width: 900px; margin: 20px auto; line-height: 1.6;">
        <h3>Overview</h3>
        <p>gbEFA is a comprehensive tool designed to guide you through the process of conducting 
        Exploratory Factor Analysis (EFA) in a systematic and rigorous manner. The application 
        follows best practices in factor analysis and provides detailed diagnostics at each step.</p>
        
        <h3>Analysis Workflow</h3>
        <p>The analysis is structured into four sequential modules, each focusing on a crucial 
        aspect of factor analysis:</p>
        
        <div class="module-box" style="background: #f8f9fa; padding: 15px; margin: 10px 0; border-left: 4px solid #007bff;">
          <h4>1. Multicollinearity Check</h4>
          <p><strong>Purpose:</strong> Assess and address multicollinearity in your data</p>
          <p><strong>Key Features:</strong></p>
          <ul>
            <li>Variance Inflation Factor (VIF) analysis</li>
            <li>Correlation matrix determinant evaluation</li>
            <li>Automated identification of problematic variables</li>
            <li>Option to download cleaned dataset</li>
          </ul>
        </div>
        
        <div class="module-box" style="background: #f8f9fa; padding: 15px; margin: 10px 0; border-left: 4px solid #28a745;">
          <h4>2. Factor Estimation</h4>
          <p><strong>Purpose:</strong> Determine the optimal number of factors to extract</p>
          <p><strong>Key Features:</strong></p>
          <ul>
            <li>Multiple factor retention criteria:
              <ul>
                <li>Kaiser criterion (eigenvalue > 1)</li>
                <li>Parallel analysis</li>
                <li>Velicer\'s MAP test</li>
                <li>Variance explained criteria</li>
              </ul>
            </li>
            <li>Interactive scree plot</li>
            <li>Very Simple Structure (VSS) analysis</li>
            <li>Comprehensive interpretation guidelines</li>
          </ul>
        </div>
        
        <div class="module-box" style="background: #f8f9fa; padding: 15px; margin: 10px 0; border-left: 4px solid #ffc107;">
          <h4>3. Diagnostics</h4>
          <p><strong>Purpose:</strong> Evaluate data suitability for factor analysis</p>
          <p><strong>Key Features:</strong></p>
          <ul>
            <li>Sampling adequacy tests:
              <ul>
                <li>Kaiser-Meyer-Olkin (KMO) test</li>
                <li>Bartlett\'s test of sphericity</li>
              </ul>
            </li>
            <li>Normality assessment:
              <ul>
                <li>Univariate normality tests</li>
                <li>Mardia\'s multivariate normality test</li>
              </ul>
            </li>
            <li>Correlation matrix analysis</li>
            <li>Interactive correlation heatmap</li>
          </ul>
        </div>
        
        <div class="module-box" style="background: #f8f9fa; padding: 15px; margin: 10px 0; border-left: 4px solid #dc3545;">
          <h4>4. Exploratory Factor Analysis</h4>
          <p><strong>Purpose:</strong> Perform the final EFA with optimal settings</p>
          <p><strong>Key Features:</strong></p>
          <ul>
            <li>Multiple extraction methods:
              <ul>
                <li>Principal Axis Factoring</li>
                <li>Minimum Residual</li>
              </ul>
            </li>
            <li>Various rotation options:
              <ul>
                <li>Promax</li>
                <li>Varimax</li>
                <li>Oblimin</li>
                <li>Quartimax</li>
              </ul>
            </li>
            <li>Correlation matrix options:
              <ul>
                <li>Pearson</li>
                <li>Spearman</li>
                <li>Polychoric</li>
              </ul>
            </li>
            <li>Customizable thresholds for:
              <ul>
                <li>Factor loadings</li>
                <li>Communalities</li>
              </ul>
            </li>
            <li>Iterative refinement process</li>
            <li>Comprehensive reliability analysis:
              <ul>
                <li>Cronbach\'s alpha</li>
                <li>McDonald\'s omega</li>
              </ul>
            </li>
            <li>Detailed results export options</li>
          </ul>
        </div>
        
        <h3>Getting Started</h3>
        <p>To begin your analysis:</p>
        <ol>
          <li>Start with the Multicollinearity Check tab to prepare your data</li>
          <li>Use the Factor Estimation tab to determine the optimal number of factors</li>
          <li>Review your data\'s suitability in the Diagnostics tab</li>
          <li>Finally, perform the EFA in the Exploratory Factor Analysis tab</li>
        </ol>
        
        <div class="alert alert-info" style="margin-top: 20px;">
          <strong>Note:</strong> Each module provides detailed interpretations and recommendations 
          to help you make informed decisions throughout the analysis process.
        </div>
      </div>
    ')
  )
}

# Define server logic for Module 0
mod0_introduction_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No server-side logic needed for the introduction module
  })
} 