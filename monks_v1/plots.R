plot_roi <- function(start_date, end_date){
    
    # Filter the data based on the selected date range
    filtered_ProfitSpend <- ProfitSpend %>%
        filter(DateGroup >= as.Date(start_date) & DateGroup <= as.Date(end_date))
    
    filtered_ProfitSpendROI <- ProfitSpendROI %>%
        filter(DateGroup >= as.Date(start_date) & DateGroup <= as.Date(end_date))
    
    # Create the bar plot for Profit and Spend
    bar_plot <- plot_ly(data = filtered_ProfitSpend, 
                        x = ~DateGroup, 
                        y = ~Total, 
                        type = 'bar', 
                        color = ~Category, 
                        colors = c("darkolivegreen3", "brown2")) %>%
        layout(
            xaxis = list(title = "Date", tickangle = 45, titlefont = list(family = "Poppins", size = 14)),
            yaxis = list(title = "Total", titlefont = list(family = "Poppins", size = 14)),
            barmode = 'dodge',
            showlegend = TRUE,
            font = list(family = "Poppins")  # Apply Poppins font globally
        )
    
    # Create the line plot for ROI using plot_ly
    roi_plot <- plot_ly(data = filtered_ProfitSpendROI, 
                        x = ~DateGroup, 
                        y = ~ROI, 
                        type = 'scatter', 
                        mode = 'lines', 
                        line = list(color = 'darkblue', width = 2),
                        name = "ROI (%)") %>%
        layout(
            xaxis = list(title = "Date", tickangle = 45, titlefont = list(family = "Poppins", size = 14)),
            yaxis2 = list(title = "ROI (%)", overlaying = "y", side = "right", titlefont = list(family = "Poppins", size = 14)),
            showlegend = TRUE,
            font = list(family = "Poppins")  # Apply Poppins font globally
        )
    
    # Combine the two plots into one with shared x-axes
    subplot(bar_plot, roi_plot, nrows = 2, shareX = TRUE) %>%
        layout(
            height = 600,  # Adjust the overall height to prevent cutting off
            showlegend = TRUE,
            margin = list(t = 50, b = 50, l = 60, r = 60)  # Adjust margins to create space around the plot
        )
}

plot_time_series <- function(model, category, date_start, date_end, palette, as_percentage = F){

    # Filter the data based on model, date range, and category if provided
    filtered_data <- Decomp %>%
        filter(Model_name == model,
               Date >= as.Date(date_start),
               Date <= as.Date(date_end))

    # If category is provided and is not "All categories", filter by category
    if (category != "All categories") {
        filtered_data <- filtered_data %>%
            filter(Category == category)
    }

    # Check if the filtered data is empty
    if (nrow(filtered_data) == 0) {
        # print("No data available for the given filter")
        return(NULL)  # Return NULL if no data available
    }

    # Create the plot using ggplot2
    if(as_percentage){
        p <- ggplot(data = filtered_data) +
            geom_bar(aes(x = Date, y = Percentage, fill = Category), stat = "identity") +
            theme_minimal() +
            scale_fill_manual(values = palette) +
            theme(
                text = element_text(family = "Poppins"),
                axis.text = element_text(family = "Poppins"),
                legend.text = element_text(family = "Poppins")
            )
    }else{
        p <- ggplot(data = filtered_data) +
            geom_bar(aes(x = Date, y = Value, fill = Category), stat = "identity") +
            theme_minimal() +
            scale_fill_manual(values = palette) +
            theme(
                text = element_text(family = "Poppins"),
                axis.text = element_text(family = "Poppins"),
                legend.text = element_text(family = "Poppins")
            )
    }


    # If category is "All categories", include the line plot and legend
    if (category == "All categories") {
        p <- p + geom_line(aes(x = Date, y = Actual), colour = "#001D37") + geom_line(aes(x = Date, y = Fitted), colour = "red")
    }

    # If category is not "All categories", remove the legend
    if (category != "All categories") {
        p <- p + theme(legend.position = "none")
    }

    # Return the plot as a plotly object
    ggplotly(p) %>%
        layout(font = list(family = "Poppins"))
}

plot_waterfall <- function(model, date_start, date_end){
    
    df <-
        Decomp %>% 
        filter(Model_name == model)%>%
        mutate(DateGroup = dmy(paste0("01-", DateGroup))) %>%
        arrange(DateGroup) %>% 
        filter(DateGroup>date_start, DateGroup<date_end) %>% 
        group_by(Category)%>%
        summarise(Total = sum(Value, na.rm = TRUE))
    
    plot_ly(data = df, x = ~Category, y = ~Total, type = 'waterfall', 
            measure = 'relative', # Set the measure to "relative" for waterfall plot
            decreasing = list(marker = list(color = 'red')), # Customize color for negative values
            increasing = list(marker = list(color = 'green')), # Customize color for positive values
            totals = list(marker = list(color = 'blue'))) %>% # Customize color for the total bar
        layout(
            xaxis = list(title = "Category"),
            yaxis = list(title = "Total"),
            showlegend = FALSE
        )
    
}

plot_spend_vs_sales <- function(category, date_start, date_end) {
    
    # Filter and prepare the data
    filtered_data <- SpendAgainstSales %>%
        filter(Category == category) %>%
        mutate(DateGroup = dmy(paste0("01-", DateGroup))) %>%
        arrange(DateGroup) %>%
        filter(DateGroup >= date_start, DateGroup <= date_end)
    
    print(filtered_data)
    
    # Plot for Spend
    spend_plot <- plot_ly(filtered_data, x = ~DateGroup, y = ~Spend, type = 'bar', name = 'Spend (£)', marker = list(color = 'red')) %>%
        layout(
            # title = paste("Spend vs Sales for", category),
            yaxis = list(title = "Spend (£)", zeroline = FALSE),
            xaxis = list(title = "Date Group")
        )
    
    # Plot for TotalValue (Sales)
    sales_plot <- plot_ly(filtered_data, x = ~DateGroup, y = ~TotalValue, type = 'bar', name = 'No of Sales', marker = list(color = 'green')) %>%
        layout(
            yaxis = list(title = "Sales (Units)", zeroline = FALSE),
            xaxis = list(title = "Date Group")
        )
    
    # Combine both plots using subplot and share the x-axis
    subplot(spend_plot, sales_plot, nrows = 2, shareX = TRUE) %>%
        layout(
            # title = paste("Spend vs Sales for", category),
            showlegend = T
        )
}







