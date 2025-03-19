prep_data <- function(){
    
    # Read all sheets in the Excel file
    sheet_names <- excel_sheets("Senior R Developer Test.xlsx")
    
    for (sheet in sheet_names) {
        temp_df <- read_excel("Senior R Developer Test.xlsx", sheet = sheet)
        colnames(temp_df) <- toTitleCase(colnames(temp_df))
        assign(sheet, temp_df, envir = .GlobalEnv)  # Assign to global environment
    }
    
    # Join actual fitted and residuals data to decomp
    Decomp <<- Decomp %>% 
        left_join(DateList) %>% 
        left_join(ActualFittedResiduals, by = c("Date", "Model_name")) %>% 
        mutate(Date = as.Date(Date),
               Percentage = (Value/Fitted)*100
               )
    # mutate(Value = Value/1000) %>% 
    # rename("Value (000s)" = "Value")
    
    # Calculate total spend by date groups
    SpendDataGroup <<- SpendData %>% 
        group_by(DateGroup) %>%
        summarise(Total = sum(Spend, na.rm = TRUE)) %>% 
        mutate(Category = "Spend (£)")
    
    # Calculate total profit by date groups
    ProfitData <<- ROI %>% 
        group_by(Date_group) %>%
        summarise(Total = sum(Profit, na.rm = TRUE)) %>% 
        mutate(Category = "Profit (£)")
    
    # Combine profit and spend data
    ProfitSpend <<- ProfitData %>% 
        rename("DateGroup" = "Date_group") %>% 
        rbind(SpendDataGroup) %>%
        mutate(DateGroup = dmy(paste0("01-", DateGroup))) %>%
        arrange(DateGroup)
    
    # Calculate ROI in wide format
    ProfitSpendROI <<- ProfitSpend %>%
        pivot_wider(names_from = Category, values_from = Total) %>%
        mutate(ROI = (`Profit (£)` / `Spend (£)`)*100)
    
    # Mapping spend variable to model categories
    category_mapping <<- data.frame(
        SpendVariable = c(
            "Media - Brand-Total-Spend", 
            "Media - BRTV-Total-Spend", 
            "Media - Direct Mail-Total-Spend", 
            "Media - Display Online-Total-Spend", 
            "Media - Generic-Total-Spend", 
            "Media - Inserts-Total-Spend", 
            "Media - Press Radio & Doordrops-Total-Spend", 
            "Media - Retargeting-Criteo-Spend", 
            "Media - Social-Facebook-Spend", 
            "Media - TV-Total-Spend"
        ),
        Category = c(
            "Brand", 
            "BRTV", 
            "Direct Mail", 
            "Display Online", 
            "Generic", 
            "Inserts", 
            "Press Radio & Doordrops", 
            "Retargeting", 
            "Social", 
            "TV"
        )
    )
    
    DecompGroupSales <<- 
        Decomp %>%
        filter(Model_name == "Sales") %>%
        group_by(DateGroup, Category) %>%
        summarise(TotalValue = sum(Value, na.rm = TRUE)) %>%
        ungroup()
    
    SpendAgainstSales <<- 
        SpendData %>% 
        left_join(category_mapping) %>% 
        left_join(DecompGroupSales, by = c("Category", "DateGroup")) %>% 
        filter(SpendVariable %in% category_mapping$SpendVariable)

    
}












