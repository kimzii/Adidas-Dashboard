library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(lubridate)
library(caret)
library(scales)
library(datasets)

# Data loading & preprocessing
adidas_data_raw <- read.csv("adidas.csv", stringsAsFactors = FALSE)

adidas_data <- adidas_data_raw %>%
  mutate(
    Invoice.Date = as.Date(Invoice.Date, format = "%d/%m/%Y"),
    Price.per.Unit = as.numeric(gsub("[$,]", "", Price.per.Unit)),
    Units.Sold = as.numeric(gsub("[,]", "", Units.Sold)),
    Total.Sales = as.numeric(gsub("[$,]", "", Total.Sales)),
    Operating.Profit = as.numeric(gsub("[$,]", "", Operating.Profit)),
    Operating.Margin = as.numeric(gsub("[%]", "", Operating.Margin)) / 100
  ) %>%
  drop_na(Invoice.Date, Price.per.Unit, Units.Sold, Total.Sales)

# State lookup for map
state_lookup <- tibble(
  state_name = tolower(state.name),
  state_abb = state.abb
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = tags$div(
    style = "display: flex; align-items: center; justify-content: center; width: 100%; padding-top: 5px;",
    tags$img(src = "logo.png", height = "40px", style = "margin-right: 10px;"),
  )),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard Home", tabName = "home", icon = icon("tachometer-alt")),
      menuItem("Diagnostic Analysis", tabName = "diagnostic", icon = icon("search")),
      menuItem("Predictive Analysis", tabName = "predictive", icon = icon("chart-line")),
      menuItem("Prescriptive Analysis", tabName = "prescriptive", icon = icon("lightbulb")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
    /* Keep sidebar fixed on desktop */
    .main-sidebar {
      position: fixed;
      top: 50px;
      height: calc(100% - 50px);
      overflow: hidden;
    }

    .main-header {
      position: fixed;
      width: 100%;
      z-index: 1030;
    }

    .wrapper {
      padding-top: 50px;
    }

    .content-wrapper, .right-side {
      margin-left: 230px;
      height: calc(100vh - 50px);
      overflow-y: auto;
    }

    /* Responsive: hide sidebar on small screens and use toggle */
    @media (max-width: 768px) {
      .main-sidebar {
        position: absolute;
        transform: translateX(-230px);
        transition: transform 0.3s ease;
        z-index: 999;
      }

      .sidebar-open .main-sidebar {
        transform: translateX(0);
      }

      .content-wrapper, .right-side {
        margin-left: 0 !important;
      }

      .main-header {
        position: fixed;
      }

      .wrapper {
        padding-top: 50px;
      }

      /* Optional: dark overlay on open sidebar */
      .sidebar-open .content-wrapper::before {
        content: '';
        position: fixed;
        top: 50px;
        left: 0;
        width: 100%;
        height: calc(100% - 50px);
        background: rgba(0, 0, 0, 0.3);
        z-index: 998;
      }
    }
  "))
    ),
    
    
    tabItems(
      # Dashboard Home Tab
      tabItem(tabName = "home",
              fluidRow(
                box(
                  title = "Filters",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  dateRangeInput("date_filter", "Select Invoice Date Range:",
                                 start = min(adidas_data$Invoice.Date),
                                 end = max(adidas_data$Invoice.Date),
                                 min = min(adidas_data$Invoice.Date),
                                 max = max(adidas_data$Invoice.Date)),
                  selectInput("retailer_filter", "Select Retailers:",
                              choices = sort(unique(adidas_data$Retailer)),
                              multiple = TRUE,
                              selected = unique(adidas_data$Retailer))
                )
              ),
              fluidRow(
                infoBoxOutput("sales_period1", width = 3),
                infoBoxOutput("sales_period2", width = 3),
                infoBoxOutput("sales_pct_change", width = 3),
                infoBoxOutput("total_units_sold", width = 3)
              ),
              fluidRow(
                box(title = "Sales by State (USA)", status = "info", solidHeader = TRUE, width = 12,
                    plotlyOutput("sales_map"))
              ),
              fluidRow(
                box(title = "Top 10 Products by Sales", status = "primary", solidHeader = TRUE, width = 6,
                    plotlyOutput("top_products")),
                box(title = "Sales by Sales Method", status = "primary", solidHeader = TRUE, width = 6,
                    plotlyOutput("sales_by_method"))
              )
      ),
      
      # Diagnostic Analysis Tab (unchanged for brevity)
      tabItem(tabName = "diagnostic",
              fluidRow(
                box(title = "Monthly Sales Trend", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("monthly_sales"))
              ),
              fluidRow(
                box(title = "Price vs Total Sales", status = "warning", solidHeader = TRUE, width = 6,
                    plotlyOutput("price_vs_sales")),
                box(title = "Units Sold vs Total Sales", status = "warning", solidHeader = TRUE, width = 6,
                    plotlyOutput("units_vs_sales"))
              ),
              fluidRow(
                box(title = "Sales Distribution by Region", status = "info", solidHeader = TRUE, width = 12,
                    plotlyOutput("sales_by_region"))
              )
      ),
      
      # Predictive Analysis Tab
      tabItem(tabName = "predictive",
              fluidRow(
                box(title = "Predicted vs Actual Sales", status = "success", solidHeader = TRUE, width = 12,
                    plotlyOutput("predicted_vs_actual"))
              ),
              fluidRow(
                box(title = "Model Performance Metrics", status = "primary", solidHeader = TRUE, width = 12,
                    verbatimTextOutput("model_metrics"))
              )
      ),
      
      # Prescriptive Analysis Tab
      tabItem(tabName = "prescriptive",
              fluidRow(
                box(title = "Recommendations", status = "danger", solidHeader = TRUE, width = 12,
                    verbatimTextOutput("recommendations"))
              ),
              fluidRow(
                box(title = "Profit by Region and Sales Method", status = "info", solidHeader = TRUE, width = 12,
                    plotlyOutput("profit_region_method"))
              )
      ),
      
      # About Tab
      tabItem(tabName = "about",
              h2("About the Adidas Sales Dashboard"),
              br(),
              p("This dashboard provides a comprehensive view of Adidas sales performance across regions, products, and sales methods."),
              p("It is designed to offer diagnostic, predictive, and prescriptive insights to support sales and marketing decisions."),
              h4("🔍 Key Features:"),
              tags$ul(
                tags$li(strong("Dashboard Home:"), "High-level KPIs and overview charts including sales by state, top products, and sales method."),
                tags$li(strong("Diagnostic Analysis:"), "Deeper exploration of monthly sales trends, price and sales relationships, and regional sales distribution."),
                tags$li(strong("Predictive Analysis:"), "Sales prediction model performance and comparison of predicted vs actual sales."),
                tags$li(strong("Prescriptive Analysis:"), "Actionable recommendations and profit analysis by region and sales channel."),
                tags$li(strong("About:"), "Information about the dashboard's purpose and dataset.")
              ),
              h4("🎯 Purpose:"),
              p("To empower Adidas sales strategy by providing data-driven insights that help optimize marketing, inventory, and sales operations."),
              h4("📊 Dataset:"),
              p("Contains sales transactions across multiple US regions and sales methods with detailed product-level data."),
              h4("📁 Data Source:"),
              tags$p(
                "Dataset sourced internally or from market research, preprocessed for this dashboard. You can also find the dataset on ",
                a(href = "https://www.kaggle.com/datasets/heemalichaudhari/adidas-sales-dataset", "Kaggle", target="_blank"),
                "."
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive filtered data for dashboard
  filtered_data <- reactive({
    data <- adidas_data
    # Filter by date
    date_range <- input$date_filter
    if (!is.null(date_range)) {
      data <- data %>% filter(Invoice.Date >= date_range[1], Invoice.Date <= date_range[2])
    }
    # Filter by retailer
    if (!is.null(input$retailer_filter) && length(input$retailer_filter) > 0) {
      data <- data %>% filter(Retailer %in% input$retailer_filter)
    }
    data
  })
  
  # Prepare comparison periods for infoBoxes:
  # We split the date range into two equal halves for comparison
  comparison_data <- reactive({
    data <- filtered_data()
    date_range <- input$date_filter
    if (is.null(date_range) || length(date_range) < 2) return(NULL)
    mid_date <- date_range[1] + as.numeric(difftime(date_range[2], date_range[1], units = "days"))/2
    
    period1 <- data %>% filter(Invoice.Date >= date_range[1], Invoice.Date <= mid_date)
    period2 <- data %>% filter(Invoice.Date > mid_date, Invoice.Date <= date_range[2])
    
    list(
      period1_sales = sum(period1$Total.Sales, na.rm = TRUE),
      period2_sales = sum(period2$Total.Sales, na.rm = TRUE)
    )
  })
  
  # InfoBoxes for comparison
  
  output$sales_period1 <- renderInfoBox({
    comp <- comparison_data()
    if (is.null(comp)) return(NULL)
    infoBox(
      "Sales Period 1",
      dollar(comp$period1_sales),
      icon = icon("calendar-alt"),
      color = "blue"
    )
  })
  
  output$sales_period2 <- renderInfoBox({
    comp <- comparison_data()
    if (is.null(comp)) return(NULL)
    infoBox(
      "Sales Period 2",
      dollar(comp$period2_sales),
      icon = icon("calendar-alt"),
      color = "navy"
    )
  })
  
  output$sales_pct_change <- renderInfoBox({
    comp <- comparison_data()
    if (is.null(comp)) return(NULL)
    pct_change <- ifelse(comp$period1_sales == 0, NA, (comp$period2_sales - comp$period1_sales)/comp$period1_sales * 100)
    
    # Choose color and icon based on increase or decrease
    color <- ifelse(pct_change >= 0, "green", "red")
    icon_used <- ifelse(pct_change >= 0, "arrow-up", "arrow-down")
    
    infoBox(
      "Sales % Change",
      ifelse(is.na(pct_change), "N/A", paste0(round(pct_change, 2), "%")),
      icon = icon(icon_used),
      color = color
    )
  })
  
  output$total_units_sold <- renderInfoBox({
    data <- filtered_data()
    infoBox(
      "Total Units Sold",
      comma(sum(data$Units.Sold, na.rm = TRUE)),
      icon = icon("boxes"),
      color = "orange"
    )
  })
  
  # Update state_sales reactive based on filtered data
  state_sales <- reactive({
    data <- filtered_data() %>%
      mutate(State_lower = tolower(State)) %>%
      left_join(state_lookup, by = c("State_lower" = "state_name")) %>%
      filter(!is.na(state_abb)) %>%
      group_by(state_abb) %>%
      summarise(Total_Sales = sum(Total.Sales, na.rm = TRUE),
                Total_Units = sum(Units.Sold, na.rm = TRUE))
    data
  })
  
  # Update product_sales reactive filtered
  product_sales <- reactive({
    filtered_data() %>%
      group_by(Product) %>%
      summarise(Total_Sales = sum(Total.Sales, na.rm = TRUE),
                Total_Units = sum(Units.Sold, na.rm = TRUE)) %>%
      arrange(desc(Total_Sales))
  })
  
  # Update method_sales reactive filtered
  method_sales <- reactive({
    filtered_data() %>%
      group_by(Sales.Method) %>%
      summarise(Total_Sales = sum(Total.Sales, na.rm = TRUE),
                Total_Units = sum(Units.Sold, na.rm = TRUE))
  })
  
  # Reactive for predictive model with filtered data
  model_data_filtered <- reactive({
    df <- filtered_data() %>%
      select(Total.Sales, Price.per.Unit, Units.Sold, Sales.Method) %>%
      mutate(Sales.Method = as.factor(Sales.Method))
    df
  })
  
  model_fit <- reactive({
    train(Total.Sales ~ ., data = model_data_filtered(), method = "lm")
  })
  
  output$sales_map <- renderPlotly({
    df <- state_sales()
    plot_ly(
      type = "choropleth",
      locations = df$state_abb,
      locationmode = "USA-states",
      z = df$Total_Sales,
      text = paste(df$state_abb, "<br>Total Sales:", scales::dollar(df$Total_Sales)),
      colorscale = list(
        c(0, "lightgray"),
        c(1, "blue")
      ),
      colorbar = list(title = "Total Sales"),
      marker = list(line = list(color = 'rgb(255,255,255)', width = 2))
    ) %>% layout(
      geo = list(scope = 'usa'),
      margin = list(t = 0, b = 0, l = 0, r = 0)
    )
  })
  
  output$top_products <- renderPlotly({
    df <- product_sales() %>% slice_max(Total_Sales, n = 10)
    p <- ggplot(df, aes(x = reorder(Product, Total_Sales), y = Total_Sales)) +
      geom_col(fill = "darkorange") +
      coord_flip() +
      labs(x = "Product", y = "Total Sales") +
      scale_y_continuous(labels = scales::dollar)
    ggplotly(p)
  })
  
  output$sales_by_method <- renderPlotly({
    df <- method_sales()
    p <- ggplot(df, aes(x = Sales.Method, y = Total_Sales, fill = Sales.Method)) +
      geom_col(show.legend = FALSE) +
      labs(x = "Sales Method", y = "Total Sales") +
      scale_y_continuous(labels = scales::dollar) +
      theme_minimal()
    ggplotly(p)
  })
  
  output$monthly_sales <- renderPlotly({
    monthly <- filtered_data() %>%
      mutate(Month = floor_date(Invoice.Date, "month")) %>%
      group_by(Month) %>%
      summarise(Total_Sales = sum(Total.Sales, na.rm = TRUE))
    p <- ggplot(monthly, aes(x = Month, y = Total_Sales)) +
      geom_line(color = "steelblue") +
      geom_point(color = "steelblue") +
      scale_y_continuous(labels = scales::dollar) +
      labs(x = "Month", y = "Total Sales") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$price_vs_sales <- renderPlotly({
    df <- filtered_data()
    p <- ggplot(df, aes(x = Price.per.Unit, y = Total.Sales)) +
      geom_point(alpha = 0.5, color = "forestgreen") +
      scale_y_continuous(labels = scales::dollar) +
      labs(x = "Price per Unit", y = "Total Sales") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$units_vs_sales <- renderPlotly({
    df <- filtered_data()
    p <- ggplot(df, aes(x = Units.Sold, y = Total.Sales)) +
      geom_point(alpha = 0.5, color = "darkred") +
      scale_y_continuous(labels = scales::dollar) +
      labs(x = "Units Sold", y = "Total Sales") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$sales_by_region <- renderPlotly({
    region_sales <- filtered_data() %>%
      group_by(Region) %>%
      summarise(Total_Sales = sum(Total.Sales, na.rm = TRUE))
    p <- ggplot(region_sales, aes(x = Region, y = Total_Sales, fill = Region)) +
      geom_col(show.legend = FALSE) +
      scale_y_continuous(labels = scales::dollar) +
      labs(x = "Region", y = "Total Sales") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$predicted_vs_actual <- renderPlotly({
    model_fit_obj <- model_fit()
    df <- model_data_filtered()
    preds <- predict(model_fit_obj, newdata = df)
    
    p <- ggplot(df, aes(x = Total.Sales, y = preds)) +
      geom_point(color = "darkorange") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      labs(x = "Actual Total Sales", y = "Predicted Total Sales") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$model_metrics <- renderPrint({
    model_fit_obj <- model_fit()
    df <- model_data_filtered()
    preds <- predict(model_fit_obj, newdata = df)
    r2_val <- caret::R2(preds, df$Total.Sales)
    rmse_val <- caret::RMSE(preds, df$Total.Sales)
    cat(sprintf("Model Performance Metrics:\nR-squared: %.3f\nRMSE: %s", r2_val, scales::dollar(rmse_val)))
  })
  
  output$recommendations <- renderPrint({
    cat("Recommendations based on current sales data and model insights:\n")
    cat("- Focus marketing efforts on high performing products like Men's Street Footwear and Women's Apparel.\n")
    cat("- Enhance promotions in regions with lower sales to boost revenue.\n")
    cat("- Consider optimizing inventory distribution by sales method; e.g., increase stock in-store if sales are higher.\n")
    cat("- Monitor products with low operating margins for cost optimization.\n")
    cat("- Use predictive insights to adjust pricing strategies for maximizing profit.\n")
  })
  
  output$profit_region_method <- renderPlotly({
    profit_data <- filtered_data() %>%
      group_by(Region, Sales.Method) %>%
      summarise(Total_Profit = sum(Operating.Profit, na.rm = TRUE))
    p <- ggplot(profit_data, aes(x = Region, y = Total_Profit, fill = Sales.Method)) +
      geom_col(position = "dodge") +
      scale_y_continuous(labels = scales::dollar) +
      labs(x = "Region", y = "Operating Profit", fill = "Sales Method") +
      theme_minimal()
    ggplotly(p)
  })
  
}

# Run app
shinyApp(ui, server)
