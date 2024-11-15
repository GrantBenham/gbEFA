library(shiny)

# Load the separate module files
source("data/mod0_introduction.R")
source("data/mod1_multicollinearity.R")
source("data/mod2_factors.R")
source("data/mod3_diagnostics.R")
source("data/mod4_efa.R")

# Define UI for the integrated Shiny app
ui <- fluidPage(
  navbarPage("Integrated gbEFA Application",
             tabPanel("Introduction", mod0_introduction_ui("mod0")),
             tabPanel("Multicollinearity Check", mod1_multicollinearity_ui("mod1")),
             tabPanel("Factor Estimation", mod2_factors_ui("mod2")),
             tabPanel("Diagnostics", mod3_diagnostics_ui("mod3")),
             tabPanel("Exploratory Factor Analysis", mod4_efa_ui("mod4"))
  )
)

# Define server logic for the integrated Shiny app
server <- function(input, output, session) {
  # Call Module 0's server function
  mod0_introduction_server("mod0")
  
  # Call Module 1's server function
  mod1_multicollinearity_server("mod1")
  
  # Call Module 2's server function
  mod2_factors_server("mod2")
  
  # Call Module 3's server function
  mod3_diagnostics_server("mod3")
  
  # Call Module 4's server function
  mod4_efa_server("mod4")
}

# Run the integrated Shiny app
shinyApp(ui = ui, server = server)
