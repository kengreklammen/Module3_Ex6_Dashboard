#
# This is a Shiny web application for a business-oriented dashboard.
#

if(!require("shiny")) install.packages("shiny")
if(!require("rio")) install.packages("rio")
if(!require("dplyr")) install.packages("dplyr")
#if(!require("ggplot2")) install.packages("ggplot2")
if(!require("bslib")) install.packages("bslib")
#if(!require("shinythemes")) install.packages("shinythemes")
if (!require("shinydashboard")) install.packages("shinydashboard")
if(!require("highcharter")) install.packages("highcharter")
if(!require("plotly")) install.packages("plotly")
if(!require("reactable")) install.packages("reactable")
if(!require("waiter")) install.packages("waiter")
if(!require("leaflet")) install.packages("leaflet")
if(!require("DT")) install.packages("DT")
if(!require("shinycssloaders")) install.packages("shinycssloaders")

library(shiny)
library(rio)
library(dplyr)
#library(ggplot2)
library(bslib)
#library(shinythemes)
library(shinydashboard)
library(highcharter)
library(plotly)
library(reactable)
library(waiter)
library(leaflet)
library(DT)
library(shinycssloaders)

# load data
retail_data1 <- import("datasets/online_retail_II_1.csv", stringsAsFactors = FALSE)
retail_data2 <- import("datasets/online_retail_II_2.csv", stringsAsFactors = FALSE)
country_data <- import("datasets/countries.csv", stringsAsFactors = FALSE)

retail_data <- retail_data1 %>%
  bind_rows(retail_data2)

# We need to remove the SPACE character from the column names
colnames(retail_data) <- gsub(" ", "", colnames(retail_data))

# Data cleaning: removing rows where the CustomerID is missing, and adding a Revenue column
retail_data <- retail_data %>%
  filter(!is.na(CustomerID))

# We need to convert the ugly date format into usable one, and convert the Quantity and Price to numeric format
retail_data <- retail_data %>%
  mutate(
    InvoiceDate = as.POSIXct(InvoiceDate, format="%m/%d/%Y %H:%M", tz="UTC"),
    Quantity = as.numeric(Quantity),
    Price = as.numeric(Price)
  )

# We insert a new Revenue column
retail_data <- retail_data %>%
  mutate(Revenue = Quantity * Price)

#View(retail_data)

# This is for the first selectInput field
countryList <- retail_data %>% select(Country) %>% unique()
#View(countryList)

stockCodeList <- retail_data %>% select(StockCode) %>% unique()
#View(stockCodeList)


# top 3 products per country
dh <- retail_data %>%
  group_by(Country) %>%
  summarise(Freq = sum(Quantity))
#View(dh)

df_total_sales <- retail_data %>%
  group_by(Country) %>%
  summarise(Total_sales = sum(Revenue)) %>% left_join(country_data, by = "Country")
#View(df_total_sales)

df <- slice_max(retail_data, Price, n=3)
#View(df)



# *****************************************************  UI  ****************************************************
ui <- dashboardPage(
  dashboardHeader(title = "Sales dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Sales over time", tabName = "all_sales_diagram"),
      menuItem("Top products", tabName = "top_sales_diagram"),
      menuItem("Sales map", tabName = "sales_map"),
      menuItem("All sales", tabName = "search_table")
    )
  ),
  dashboardBody(
    useWaiter(),
    useHostess(),
	  #waiterPreloader(),
    
    waiterShowOnLoad(
      html = tagList(
        hostess_loader(
          "loader",
          preset = "fan",
          text_color = "#f2f2f2",
          class = "label-center",
          center_page = TRUE
        ),
        br(),
        tagAppendAttributes(style = "margin-left:-50px",
                            p(
                              sample(
                                c(
                                  "We're loading the app. Fetching stardust...",
                                  "The app is almost ready. Summoning unicorns...",
                                  "Hold on, the app is being loaded! Chasing rainbows...",
                                  "Loading the app: sending telepathic messages...",
                                  "We're loading the app: teaching squirrels to water ski...",
                                  "App is loading! Counting clouds...",
                                  "We're preparing the app for you, that means: taming wild pixels..."
                                ),
                                1)
                            ))
      )
    ),
    tabItems(
    
      # ********************** First page **********************
      tabItem(
        tabName = "all_sales_diagram", fluidPage(
          h1("Sales over time"),
          withSpinner(plotlyOutput("plotlyplot")),
          selectInput(
            inputId = "country",
            label = "Choose a country",
            choices = sort(unique(countryList$Country)),
            multiple = FALSE,
            selectize = TRUE,
            selected = "USA"
          )
        )
      ),  
      
      # ********************* Second page **********************
      tabItem(
        tabName = "top_sales_diagram", fluidPage(
          h1("Top products"),
          withSpinner(highchartOutput("highchartPlot")),
          selectInput(
            inputId = "country",
            label = "Choose the number of top products",
            choices = c(1, 5, 10),
            multiple = FALSE,
            selectize = TRUE,
            selected = 1
          )
        )
      ),
      
      # ********************** Third page **********************
      tabItem(
        tabName = "sales_map", fluidPage(
          h1("Sales map"),
          withSpinner(leafletOutput("mymap", height = 500))
        )
      ),
      
      # ********************** Fourth page **********************
      tabItem(
        tabName = "search_table", fluidPage(
          h1("All sales"),
          withSpinner(DTOutput("dttable")),
          selectInput("filter_country", "Please choose a country:", choices = NULL, multiple = FALSE)
        )
      )
    )
   
     
    
  )
)

print(class(ui))


# ************************************************* SERVER LOGIC ************************************************
# Define server logic required to draw a histogram
server <- function(input, output) {
  Sys.sleep(5)
  hostess <- Hostess$new("loader")
  


  hostess$set(20) 
  # Reactive filtered data
  filtered_by_country <- eventReactive(input$country, {
    retail_data %>% filter(Country == input$country)
  })
  
  hostess$set(30)
  output$plotlyplot <- renderPlotly({
    plot_ly(
      filtered_by_country(),
      x = ~InvoiceDate,
      y = ~Revenue
    ) %>%
      add_lines()
  })
  
  hostess$set(40)
  # series_list <- sales_data %>%
  #   select(-Quarter) %>%
  #   pivot_longer(cols = everything(), names_to = "Product", values_to = "Sales") %>%
  #   group_by(Product) %>%
  #   summarise(data = list(Sales), .groups = "drop") %>%
  #   mutate(series = purrr::map2(Product, data, ~ list(name = .x, data = .y))) %>%
  #   pull(series)
  # 
  
  hostess$set(50)
  output$highchartPlot <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Top products by country") %>%
      hc_xAxis(categories = c("Country")) %>%
      hc_yAxis(title = list(text = "Volume")) %>%
      hc_plotOptions(column = list(stacking = "normal")) %>%
      hc_series(#series_list
        list(name = "Product A", data = c(5000, 7000, 8000, 6000)),
        list(name = "Product B", data = c(3000, 5000, 6000, 4000)),
        list(name = "Product C", data = c(2000, 4000, 5000, 3000))
      )
  })
  
  hostess$set(70)
  output$mymap <- renderLeaflet({
    leaflet(df_total_sales) %>%
      addTiles() %>%
      addMarkers(~Lon, ~Lat, popup =  ~paste0(Country, "<br/> Total sales: $", round(Total_sales, 0)), ) %>%
      setView(lng = 10.451526, lat = 51.165691, zoom = 3)
  })
  
  
  hostess$set(80)
  output$dttable <- renderDT({
    datatable(retail_data)
  })
  
  
  waiter_hide()
}

# Run the application 
shinyApp(ui = ui, server = server)