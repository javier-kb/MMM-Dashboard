# Marketing Mix Model Dashboard

A simple R Shiny app to visualize the outputs of three different MMM models developed by Measure.Monks. This application allows users to interact with the results of the models, view plots, and perform analysis based on specified date ranges and categories.

## Running the App Locally

This app is designed to be run locally from R. It is not yet at production level and requires an R environment set up on your local machine.

### Prerequisites

Before running the app, ensure that you have the following libraries installed in your R environment. You can install them using the `install.packages()` function:

```{r, eval = FALSE}
install.packages(c(
  "shiny", 
  "readxl", 
  "ggplot2", 
  "dplyr", 
  "RColorBrewer", 
  "plotly", 
  "tidyr", 
  "zoo", 
  "lubridate", 
  "tools", 
  "shinyWidgets", 
  "shinyjs"
), repos = "https://cloud.r-project.org/")
```

## Running the App

1.  Ensure you have R and RStudio installed on your local machine.
2.  Open RStudio and set your working directory to the folder containing the app files.
3.  Run the following code in your R console to start the Shiny app:

```{r, eval = FALSE}
library(shiny)
runApp()
```

Once the app is running, you should be able to access it locally through the RStudio Viewer or in your web browser at the specified local address.

## Interface Overview

The Dashboard is split into three tabs, which reflect the outputs of the three different models: "Sales and ROI", "Brand" and "Generic". A date selector at the top of the main panel is included to modify the date range of the displayed plots.

### Sales and ROI tab

Contains four plots:

1.  Sales Marketing Mix: can be filtered by a specific marketing category or all together. When a specific category is selected, it can also be optionally shown as a percentage of total sales
2.  Total Contribution by Category Across Time Period
3.  Monthly Spend, Profit and ROI
4.  Spend vs Sales (for a specific selected marketing category)

### Brand tab

Contains two plots:

1.  Brand Marketing Mix: can be filtered by a specific marketing category or all together. When a specific category is selected, it can also be optionally shown as a percentage of the total value for the Brand KPI
2.  Total Contribution by Category Across Time Period

### Generic tab

Contains two plots:

1.  Generic Marketing Mix: can be filtered by a specific marketing category or all together. When a specific category is selected, it can also be optionally shown as a percentage of the total value for the Generic KPI
2.  Total Contribution by Category Across Time Period
