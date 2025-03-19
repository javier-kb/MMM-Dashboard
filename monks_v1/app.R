library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(plotly)
library(tidyr)
library(zoo)
library(lubridate)
library(tools)
library(shinyWidgets)
library(shinyjs)

source("./prep_data.R")
source("./plots.R")

ui <- fluidPage(
    # shinyjs
    useShinyjs(),
    
    # Poppins font
    tags$head(
        tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Poppins&display=swap")
    ),
    
    # CSS
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "app_css.css")
    ),
    
    # Sticky header with logo on the left and title on the right
    tags$header(
        class = "sticky-header",
        tags$img(src = "measure_monks_logo.jpeg", height = "50px", alt = "Measure Monks Logo")
    ),
    
    # Main content area (after the sticky header)
    div(class = "main-content",
        
        # Main panel
        div(
            class = "main-panel",
            div(
                style = "display: flex; justify-content: space-between; align-items: center; padding:0px 20px;",
                h4("Model outputs"),
                div(
                    style = "display:inline-block;",
                    dateRangeInput("date_range", 
                                   "", 
                                   start = NULL, 
                                   end = NULL,
                                   min = NULL, 
                                   max = NULL,
                                   format = "yyyy-mm-dd")
                )
            ),
            
            hr(style = "margin-top:5px;margin-bottom:5px;"),
            
            # Tabset Panel for different models
            tabsetPanel(
                id = "model_tabs",
                
                # Sales and ROI tab
                tabPanel("Sales and ROI",
                         value = "Sales",
                         div(class = "section-container",
                             div(class = "section-left",
                                 p("Sales Marketing Mix", class = "section-title"),
                                 p("Partial contributions of each marketing category towards total sales by date according to the developed model. The black line displays the actual values and the red line displays the fitted values. Note that negative values suggest an inverse relationship between the category and sales.", class = "section-description")
                             ),
                             selectInput(inputId = "category_sales_ts", label = "", choices = NULL), # Empty choices to be updated dynamically
                             div(class = "switch-label",
                                 materialSwitch(inputId = "percentage_sales_switch", label = "As % of total (Fitted)")
                             )
                         ),
                         plotlyOutput("decomp_sales"),
                         div(class = "content-box",
                             p("Total Contribution by Category Across Time Period", class = "section-title"),
                             p("Aggregated contribution by category across the whole time period", class = "section-description") 
                         ),
                         plotlyOutput("waterfall_sales"),
                         div(class = "content-box",
                             p("Monthly Spend, Profit and ROI", class = "section-title"),
                             p("Total marketing spend against the profit generated each month. Return on Investment expressed as a percentage below", class = "section-description") 
                         ),
                         plotlyOutput("roi_plot"),
                         div(class = "section-container",
                             div(class = "section-left",
                                 p("Spend vs Sales", class = "section-title"),
                                 p("Total spend for the selected category by month compared to the total number of units sold attributed to the same category as per the Sales model.
                                   ", class = "section-description")
                             ),
                             selectInput(inputId = "category_sales_svs", label = "", choices = NULL), # Empty choices to be updated dynamically
                         ),
                         plotlyOutput("spend_vs_sales_plot")
                ),
                
                # Brand tab
                tabPanel("Brand",
                         value = "Brand",
                         div(class = "section-container",
                             div(class = "section-left",
                                 p("Brand Marketing Mix", class = "section-title"),
                                 p("Partial contributions of each marketing category towards total brand KPI value by date according to the developed model. The black line displays the actual values and the red line displays the fitted values. Note that negative values suggest an inverse relationship between the category and the KPI value", class = "section-description")
                             ),
                             selectInput(inputId = "category_brand_ts", label = "", choices = NULL), # Empty choices to be updated dynamically
                             div(class = "switch-label",
                                 materialSwitch(inputId = "percentage_brand_switch", label = "As % of total (Fitted)")
                             )
                         ),
                         plotlyOutput("decomp_brand"),
                         div(class = "content-box",
                             p("Total Contribution by Category Across Time Period", class = "section-title"),
                             p("Aggregated contribution by category across the whole time period", class = "section-description")
                         ),
                         plotlyOutput("waterfall_brand")
                ),
                
                # Generic tab
                tabPanel("Generic",
                         value = "Generic",
                         div(class = "section-container",
                             div(class = "section-left",
                                 p("Generic Marketing Mix", class = "section-title"),
                                 p("Partial contributions of each marketing category towards total generic KPI value by date according to the developed model. The black line displays the actual values and the red line displays the fitted values. Note that negative values suggest an inverse relationship between the category and the KPI value", class = "section-description")
                             ),
                             selectInput(inputId = "category_generic_ts", label = "", choices = NULL), # Empty choices to be updated dynamically
                             div(class = "switch-label",
                                 materialSwitch(inputId = "percentage_generic_switch", label = "As % of total (Fitted)")
                             )
                         ),
                         plotlyOutput("decomp_generic"),
                         div(class = "content-box",
                             p("Total Contribution by Category Across Time Period", class = "section-title"),
                             p("Aggregated contribution by category across the whole time period", class = "section-description")
                         ),
                         plotlyOutput("waterfall_generic")
                )
            )
            
        )
    )
)


server <- function(input, output, session) {
    
    # Ensure prep_data() is run first to prepare data before any other operations
    prep_data()  # This creates Decomp
    
    # Define the palette for distinct colors
    palette_set3 <- brewer.pal(12, "Set3")
    extra_colors <- colorRampPalette(brewer.pal(9, "Set1"))(10)
    final_palette <- c(palette_set3, extra_colors)
    
    # Observe and dynamically update the select inputs when Decomp data is ready
    observe({
        req(Decomp)  # Ensure that Decomp data is available
        
        # Update selectInput for Sales tab
        updateSelectInput(session, "category_sales_ts", 
                          choices = c("All categories", unique(Decomp$Category)))
        
        # Update selectInput for Brand tab
        updateSelectInput(session, "category_brand_ts", 
                          choices = c("All categories", unique(Decomp$Category)))
        
        # Update selectInput for Generic tab
        updateSelectInput(session, "category_generic_ts", 
                          choices = c("All categories", unique(Decomp$Category)))
    })
    
    observe({
        req(category_mapping)
        updateSelectInput(session, "category_sales_svs", 
                          choices = c(unique(category_mapping$Category)))
    })
    
    # Observe the Date Range and update UI accordingly
    observe({
        req(Decomp)  # Ensure Decomp data exists before accessing it
        
        # Get min and max dates from the data (ensure Date column is of Date class)
        min_date <- min(Decomp$Date, na.rm = TRUE)
        max_date <- max(Decomp$Date, na.rm = TRUE)
        
        # Update the date range input to reflect the range of the data
        updateDateRangeInput(session, "date_range",
                             min = min_date,
                             max = max_date,
                             start = min_date,
                             end = max_date)
    })
    
    # Disabling percentage switch based on category selection
    observe({
        if(input$category_sales_ts == "All categories"){
            shinyjs::runjs('$("#percentage_sales_switch").prop("checked", false);$("#percentage_sales_switch").siblings().css("color", "#cecece");')
            shinyjs::disable("percentage_sales_switch")
        } else {
            shinyjs::enable("percentage_sales_switch")
            shinyjs::runjs('$("#percentage_sales_switch").siblings().css("color", "black");')
        }
        
        if(input$category_brand_ts == "All categories"){
            shinyjs::runjs('$("#percentage_brand_switch").prop("checked", false);$("#percentage_brand_switch").siblings().css("color", "#cecece");')
            shinyjs::disable("percentage_brand_switch")
        } else {
            shinyjs::enable("percentage_brand_switch")
            shinyjs::runjs('$("#percentage_brand_switch").siblings().css("color", "black");')
        }
        
        if(input$category_generic_ts == "All categories"){
            shinyjs::runjs('$("#percentage_generic_switch").prop("checked", false);$("#percentage_generic_switch").siblings().css("color", "#cecece");')
            shinyjs::disable("percentage_generic_switch")
        } else {
            shinyjs::enable("percentage_generic_switch")
            shinyjs::runjs('$("#percentage_generic_switch").siblings().css("color", "black");')
        }
    })
    
    # Ensure the date range input is available before rendering plots
    observe({
        req(input$date_range)  # Ensure date_range is available
        
        # Filter and plot data for "Sales" model
        output$decomp_sales <- renderPlotly({
            req(Decomp)  # Ensure Decomp data is available
            plot_time_series("Sales",
                             input$category_sales_ts,
                             input$date_range[1],
                             input$date_range[2],
                             final_palette,
                             input$percentage_sales_switch
            )
        })
        
        output$decomp_brand <- renderPlotly({
            req(Decomp)  # Ensure Decomp data is available
            plot_time_series("Brand",
                             input$category_brand_ts,
                             input$date_range[1],
                             input$date_range[2],
                             final_palette,
                             input$percentage_brand_switch
            )
        })
        
        output$decomp_generic <- renderPlotly({
            req(Decomp)  # Ensure Decomp data is available
            plot_time_series("Generic",
                             input$category_generic_ts,
                             input$date_range[1],
                             input$date_range[2],
                             final_palette,
                             input$percentage_generic_switch)
        })
        
        output$roi_plot <- renderPlotly({
            req(ProfitSpendROI)  # Ensure ProfitSpendROI is available
            plot_roi(
                input$date_range[1],
                input$date_range[2]
            )
        })
        
        output$spend_vs_sales_plot <- renderPlotly({
            plot_spend_vs_sales(
                input$category_sales_svs,
                input$date_range[1],
                input$date_range[2]
                
            )
        })
        
        output$waterfall_sales <- renderPlotly({
            req(Decomp)
            plot_waterfall(
                "Sales",
                input$date_range[1],
                input$date_range[2]
            )
        })
        
        output$waterfall_brand <- renderPlotly({
            req(Decomp)
            plot_waterfall(
                "Brand",
                input$date_range[1],
                input$date_range[2]
            )
        })
        
        output$waterfall_generic <- renderPlotly({
            req(Decomp)
            plot_waterfall(
                "Generic",
                input$date_range[1],
                input$date_range[2]
            )
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

