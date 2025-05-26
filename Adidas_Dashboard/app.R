# Load libraries
library(shinythemes)
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
ui <- navbarPage(
  title = div(
    tags$img(src = "logo.png", height = "20px", style = "margin-right:10px;"),
    "Adidas Dashboard"
  ),
  theme = shinytheme("cyborg"),
  
  tags$head(
    tags$style(HTML("
        /* Reset margin and padding on body/html */
        html, body {
          margin: 0; 
          padding: 0;
          height: 100%;
        }
        
        h2 {
          font-size: 36px !important;
          font-weight: bold;
        }
        h3 {
          font-size: 28px !important;
          font-weight: bold;
        }
        h4 {
          font-size: 22px !important;
          font-weight: bold;
          }

        /* Recommendations styling */
        .rec-card {
          border-left: 5px solid;
          padding: 15px;
          margin-bottom: 15px;
          border-radius: 4px;
        }
        .rec-marketing {
          background:#002b47;
          border-color: #0099f9;
        }
        .rec-operations {
          background: #16390e;
          border-color: #4dc731;
        }
        .rec-pricing {
          background: #410505;
          border-color: #e61313;
        }
        .rec-title {
          margin-bottom: 10px;
        }
        .rec-suggested {
          background: #d1ecf1;
          color: #0c5460;
          padding: 10px;
          border-radius: 3px;
          font-weight: bold;
        }
      "))
  ),
  
  tabPanel("Home",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               dateRangeInput("date_filter", "Select Invoice Date Range:",
                              start = min(adidas_data$Invoice.Date),
                              end = max(adidas_data$Invoice.Date)),
               selectInput("retailer_filter", "Select Retailers:",
                           choices = sort(unique(adidas_data$Retailer)),
                           multiple = TRUE,
                           selected = unique(adidas_data$Retailer)),
               selectInput("sales_method_filter", "Select Sales Method(s):",
                           choices = sort(unique(adidas_data$Sales.Method)),
                           multiple = TRUE,
                           selected = unique(adidas_data$Sales.Method)),
               selectInput("region_filter", "Select Region(s):",
                           choices = sort(unique(adidas_data$Region)),
                           multiple = TRUE,
                           selected = unique(adidas_data$Region)),
               selectInput("product_filter", "Select Product(s):",
                           choices = sort(unique(adidas_data$Product)),
                           multiple = TRUE,
                           selected = unique(adidas_data$Product))
             ),
             mainPanel(
               fluidPage(
                 h2("Welcome to the Adidas Sales Dashboard"),
                 p("This dashboard provides insights into sales data, trends, and recommendations."),
                 fluidRow(
                   column(width = 6, md = 3, infoBoxOutput("sales_period1", width = NULL)),
                   column(width = 6, md = 3, infoBoxOutput("sales_period2", width = NULL)),
                   column(width = 6, md = 3, infoBoxOutput("sales_pct_change", width = NULL)),
                   column(width = 6, md = 3, infoBoxOutput("total_units_sold", width = NULL))
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
               )
             )
           )
  ),
  
  tabPanel("Diagnostic",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               dateRangeInput("date_filter", "Select Invoice Date Range:",
                              start = min(adidas_data$Invoice.Date),
                              end = max(adidas_data$Invoice.Date)),
               selectInput("retailer_filter", "Select Retailers:",
                           choices = sort(unique(adidas_data$Retailer)),
                           multiple = TRUE,
                           selected = unique(adidas_data$Retailer)),
               selectInput("sales_method_filter", "Select Sales Method(s):",
                           choices = sort(unique(adidas_data$Sales.Method)),
                           multiple = TRUE,
                           selected = unique(adidas_data$Sales.Method)),
               selectInput("region_filter", "Select Region(s):",
                           choices = sort(unique(adidas_data$Region)),
                           multiple = TRUE,
                           selected = unique(adidas_data$Region)),
               selectInput("product_filter", "Select Product(s):",
                           choices = sort(unique(adidas_data$Product)),
                           multiple = TRUE,
                           selected = unique(adidas_data$Product))
             ),
             mainPanel(
               fluidPage(
                 fluidRow(
                   box(title = "Monthly Sales Trend", width = 12, plotlyOutput("monthly_sales"))
                 ),
                 fluidRow(
                   box(title = "Price vs Total Sales", width = 6, plotlyOutput("price_vs_sales")),
                   box(title = "Units Sold vs Total Sales", width = 6, plotlyOutput("units_vs_sales"))
                 ),
                 fluidRow(
                   box(title = "Sales Distribution by Region", width = 12, plotlyOutput("sales_by_region"))
                 )
               )
             )
           )
  ),
  
  tabPanel("Predictive",
           fluidPage(
             fluidRow(
               box(title = "Predicted vs Actual Sales", width = 12, plotlyOutput("predicted_vs_actual"))
             ),
             fluidRow(
               box(title = "Model Performance Metrics", width = 12, htmlOutput("model_metrics"))
             )
           )
  ),
  
  tabPanel("Prescriptive",
           fluidPage(
             fluidRow(
               box(title = "Strategic Recommendations", width = 12, uiOutput("styled_recommendations"))
             ),
             fluidRow(
               box(title = "Profit by Region and Sales Method", width = 12, plotlyOutput("profit_region_method"))
             )
           )
  ),
  
  tabPanel("About",
           fluidPage(
             div(style = "text-align: center; max-width: 900px; margin: auto;",
                 h2("About the Adidas Sales Dashboard"),
                 br(),
                 p("This dashboard provides a comprehensive view of Adidas sales performance across regions, products, and sales methods."),
                 p("It is designed to offer diagnostic, predictive, and prescriptive insights to support sales and marketing decisions."),
                 h4("Key Features"),
                 tags$ul(
                   tags$li(strong("Dashboard Home:"), " High-level KPIs and overview charts including sales by state, top products, and sales method."),
                   tags$li(strong("Diagnostic Analysis:"), " Deeper exploration of monthly sales trends, price and sales relationships, and regional sales distribution."),
                   tags$li(strong("Predictive Analysis:"), " Sales prediction model performance and comparison of predicted vs actual sales."),
                   tags$li(strong("Prescriptive Analysis:"), " Actionable recommendations and profit analysis by region and sales channel."),
                   tags$li(strong("About:"), " Information about the dashboard's purpose and dataset.")
                 ),
                 h4("Purpose"),
                 p("To empower Adidas sales strategy by providing data-driven insights that help optimize marketing, inventory, and sales operations."),
                 h4("Dataset"),
                 p("Contains sales transactions across multiple US regions and sales methods with detailed product-level data."),
                 h4("Data Source"),
                 tags$p(
                   "Dataset sourced internally or from market research, preprocessed for this dashboard. You can also find the dataset on ",
                   a(href = "https://www.kaggle.com/datasets/heemalichaudhari/adidas-sales-dataset", "Kaggle", target = "_blank"),
                   "."
                 )
             )
           )
  )
)
  

# Server
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    data <- adidas_data
    date_range <- input$date_filter
    if (!is.null(date_range)) {
      data <- data %>% filter(Invoice.Date >= date_range[1], Invoice.Date <= date_range[2])
    }
    if (!is.null(input$retailer_filter) && length(input$retailer_filter) > 0) {
      data <- data %>% filter(Retailer %in% input$retailer_filter)
    }
    if (!is.null(input$sales_method_filter) && length(input$sales_method_filter) > 0) {
      data <- data %>% filter(Sales.Method %in% input$sales_method_filter)
    }
    if (!is.null(input$region_filter) && length(input$region_filter) > 0) {
      data <- data %>% filter(Region %in% input$region_filter)
    }
    if (!is.null(input$product_filter) && length(input$product_filter) > 0) {
      data <- data %>% filter(Product %in% input$product_filter)
    }
    data
  })
  
  
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
  
  output$sales_period1 <- renderInfoBox({
    comp <- comparison_data()
    if (is.null(comp)) return(NULL)
    infoBox("Sales Period 1", dollar(comp$period1_sales), icon = icon("calendar-alt"), color = "blue")
  })
  
  output$sales_period2 <- renderInfoBox({
    comp <- comparison_data()
    if (is.null(comp)) return(NULL)
    infoBox("Sales Period 2", dollar(comp$period2_sales), icon = icon("calendar-alt"), color = "navy")
  })
  
  output$sales_pct_change <- renderInfoBox({
    comp <- comparison_data()
    if (is.null(comp)) return(NULL)
    pct_change <- ifelse(comp$period1_sales == 0, NA, (comp$period2_sales - comp$period1_sales)/comp$period1_sales * 100)
    color <- ifelse(pct_change >= 0, "green", "red")
    icon_used <- ifelse(pct_change >= 0, "arrow-up", "arrow-down")
    infoBox("Sales % Change", ifelse(is.na(pct_change), "N/A", paste0(round(pct_change, 2), "%")), icon = icon(icon_used), color = color)
  })
  
  output$total_units_sold <- renderInfoBox({
    data <- filtered_data()
    infoBox("Total Units Sold", comma(sum(data$Units.Sold, na.rm = TRUE)), icon = icon("boxes"), color = "orange")
  })
  
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
  
  product_sales <- reactive({
    filtered_data() %>%
      group_by(Product) %>%
      summarise(Total_Sales = sum(Total.Sales, na.rm = TRUE),
                Total_Units = sum(Units.Sold, na.rm = TRUE)) %>%
      arrange(desc(Total_Sales))
  })
  
  method_sales <- reactive({
    filtered_data() %>%
      group_by(Sales.Method) %>%
      summarise(Total_Sales = sum(Total.Sales, na.rm = TRUE),
                Total_Units = sum(Units.Sold, na.rm = TRUE))
  })
  
  model_data_filtered <- reactive({
    df <- filtered_data() %>%
      select(Total.Sales, Price.per.Unit, Units.Sold, Sales.Method) %>%
      filter(
        !is.na(Total.Sales),
        !is.na(Price.per.Unit),
        !is.na(Units.Sold),
        !is.na(Sales.Method)
      ) %>%
      mutate(Sales.Method = as.factor(Sales.Method))
    
    # Return only if enough data
    if (nrow(df) < 2) return(NULL)
    df
  })
  
  model_fit <- reactive({
    df <- model_data_filtered()
    if (is.null(df)) return(NULL)
    train(Total.Sales ~ ., data = df, method = "lm")
  })
  
  
  output$sales_map <- renderPlotly({
    df <- state_sales()
    plot_ly(
      data = df,
      type = "choropleth",
      locations = ~state_abb,
      locationmode = "USA-states",
      z = ~Total_Sales,
      text = ~paste(state_abb, "<br>Total Sales:", scales::dollar(Total_Sales)),
      colorscale = "Viridis",      # Use the built-in Viridis colorscale here
      colorbar = list(title = "Total Sales"),
      marker = list(line = list(color = 'rgb(255,255,255)', width = 2))
    ) %>% layout(
      geo = list(scope = 'usa'),
      margin = list(t = 0, b = 0, l = 0, r = 0)
    )
  })
  
  
  output$top_products <- renderPlotly({
    df <- product_sales() %>% slice_max(Total_Sales, n = 10)
    
    # Create the ggplot with rotated x-axis labels
    p <- ggplot(df, aes(x = reorder(Product, Total_Sales), y = Total_Sales)) +
      geom_col(fill = "#003c73") +
      coord_flip() +  # Flip the axis to make it easier to read
      labs(x = "Product", y = "Total Sales") +
      scale_y_continuous(labels = scales::dollar) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
        axis.text.y = element_text(size = 10)  # Adjust font size if needed
      )
    
    ggplotly(p)
  })
  
  output$sales_by_method <- renderPlotly({
    df <- method_sales()
    p <- ggplot(df, aes(x = Sales.Method, y = Total_Sales, fill = Sales.Method)) +
      geom_col(show.legend = FALSE) +
      labs(x = "Sales Method", y = "Total Sales") +
      scale_y_continuous(labels = scales::dollar) +
      scale_fill_manual(values = c("#003c73", "#d6b300", "#5cd623")) +  # Apply custom colors here
      theme_minimal()
    ggplotly(p)
  })
  
  
  output$monthly_sales <- renderPlotly({
    monthly <- filtered_data() %>%
      mutate(Month = floor_date(Invoice.Date, "month")) %>%
      group_by(Month) %>%
      summarise(Total_Sales = sum(Total.Sales, na.rm = TRUE))
    p <- ggplot(monthly, aes(x = Month, y = Total_Sales)) +
      geom_line(color = "#003c73") +
      geom_point(color = "#5cd623") +
      scale_y_continuous(labels = scales::dollar) +
      labs(x = "Month", y = "Total Sales") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$price_vs_sales <- renderPlotly({
    df <- filtered_data()
    p <- ggplot(df, aes(x = Price.per.Unit, y = Total.Sales)) +
      geom_point(alpha = 0.5, color = "#5cd623") +
      scale_y_continuous(labels = scales::dollar) +
      labs(x = "Price per Unit", y = "Total Sales") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$units_vs_sales <- renderPlotly({
    df <- filtered_data()
    p <- ggplot(df, aes(x = Units.Sold, y = Total.Sales)) +
      geom_point(alpha = 0.5, color = "#003c73") +
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
      scale_fill_manual(values = c("#003c73", "#be1720", "#d65b00", "#d6b300", "#4eb81e")) +
      labs(x = "Region", y = "Total Sales") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$predicted_vs_actual <- renderPlotly({
    model_fit_obj <- model_fit()
    df <- model_data_filtered()
    preds <- predict(model_fit_obj, newdata = df)
    
    p <- ggplot(df, aes(x = Total.Sales, y = preds)) +
      geom_point(color = "#003c73") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      labs(x = "Actual Total Sales", y = "Predicted Total Sales") +
      scale_x_continuous(labels = scales::dollar) +
      scale_y_continuous(labels = scales::dollar) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$model_metrics <- renderUI({
    model_fit_obj <- model_fit()
    df <- model_data_filtered()
    preds <- predict(model_fit_obj, newdata = df)
    
    r2_val <- caret::R2(preds, df$Total.Sales)
    rmse_val <- caret::RMSE(preds, df$Total.Sales)
    
    HTML(paste0(
      "<div style='font-size:16px;'>",
      "<p><strong>Model Performance Summary:</strong></p>",
      "<ul style='line-height:1.6;'>",
      "<li><span style='color:#0073e6;'>R-squared:</span> <strong>", round(r2_val, 3), "</strong> — indicates how well the model explains variance in sales.</li>",
      "<li><span style='color:#d9534f;'>RMSE:</span> <strong>", scales::dollar(rmse_val), "</strong> — the average prediction error in dollars.</li>",
      "</ul>",
      "<p>These metrics suggest the accuracy of predictions made using product price, units sold, and sales method.</p>",
      "</div>"
    ))
  })
  
  # Styled Recommendations UI
  output$styled_recommendations <- renderUI({
    tagList(
      div(class = "rec-card rec-marketing",
          tags$h4(icon("bullhorn"), " Targeted Marketing Campaign in North America", class = "rec-title"),
          tags$p("Focus marketing campaigns on the Northeast and West Coast regions, targeting high performing products like Men's Street Footwear and Women's Apparel to sustain sales momentum."),
          div(class = "rec-suggested", "Suggested Action: Launch targeted regional promotions and digital campaigns.")
      ),
      div(class = "rec-card rec-operations",
          tags$h4(icon("cogs"), " Optimize Inventory Distribution", class = "rec-title"),
          tags$p("Analyze regional sales data to adjust stock levels and reduce stockouts or overstock."),
          div(class = "rec-suggested", "Suggested Action: Implement dynamic inventory management based on sales velocity.")
      ),
      div(class = "rec-card rec-pricing",
          tags$h4(icon("tags"), " Strategic Price Adjustment", class = "rec-title"),
          tags$p("Adjust pricing for products with declining sales and low operating margins to improve profitability."),
          div(class = "rec-suggested", "Suggested Action: Use predictive insights to optimize price points regionally.")
      )
    )
  })
  
  output$profit_region_method <- renderPlotly({
    profit_data <- filtered_data() %>%
      group_by(Region, Sales.Method) %>%
      summarise(Total_Profit = sum(Operating.Profit, na.rm = TRUE), .groups = "drop")
    
    p <- ggplot(profit_data, aes(x = Region, y = Total_Profit, fill = Sales.Method)) +
      geom_col(position = "dodge") +
      scale_y_continuous(labels = scales::dollar) +
      labs(x = "Region", y = "Operating Profit", fill = "Sales Method") +
      scale_fill_manual(values = c("#003c73", "#d6b300", "#5cd623")) +
      theme_minimal()
    
    ggplotly(p)
  })
  
}

# Run app
shinyApp(ui, server)