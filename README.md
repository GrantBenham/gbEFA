# Integrated gbEFA Application

## Overview

The Integrated gbEFA Application is a Shiny web application designed for Exploratory Factor Analysis (EFA) and related diagnostics. This application allows users to upload datasets, check for multicollinearity, estimate factors, and perform various diagnostics to ensure the suitability of data for factor analysis.

## Features

- **Introduction Module**: Provides an overview of the application and its functionalities.
- **Multicollinearity Check Module**: Allows users to upload a dataset and check for multicollinearity using Variance Inflation Factor (VIF) analysis.
- **Factor Estimation Module**: Estimates the number of factors to retain based on various criteria.
- **Diagnostics Module**: Conducts data diagnostics, including normality tests and correlation analysis.
- **Exploratory Factor Analysis Module**: Performs EFA with options for different extraction and rotation methods, and generates detailed reports.

## Requirements

To run this application, you need the following R packages:

- `shiny`
- `rmarkdown`
- `knitr`
- `kableExtra`
- `plotly`
- `htmlwidgets`
- `ggplot2`
- `reshape2`
- `psych`
- `dplyr`
- `htmltools`
- `base64enc`


You can install the required packages using the following command:



install.packages(c("shiny", "rmarkdown", "knitr", "kableExtra", "plotly", "htmlwidgets", "ggplot2", "reshape2", "psych", "dplyr", "htmltools", "base64enc"))


## Usage

1. Clone the repository to your local machine:
   ```bash
   git clone https://github.com/yourusername/yourrepository.git
   cd yourrepository
   ```

2. Open R or RStudio and set the working directory to the cloned repository.

3. Run the application:
   ```r
   shiny::runApp("integrated_gbEFA.R")
   ```

4. The application will open in your default web browser.

## How to Contribute

Contributions are welcome! If you have suggestions for improvements or new features, please open an issue or submit a pull request.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- Thanks to the contributors and the R community for their support and resources.
- This application is built upon the principles of Exploratory Factor Analysis and aims to provide a user-friendly interface for researchers and practitioners.

## Contact

For any inquiries, please contact [gbenham@live.com](mailto:gbenham@live.com).

