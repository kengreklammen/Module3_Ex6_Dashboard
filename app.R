#
# This is a Shiny web application for a business-oriented dashboard.
#

if(!require("shiny")) install.packages("shiny")
if(!require("rio")) install.packages("rio")
if(!require("dplyr")) install.packages("dplyr")
#if(!require("ggplot2")) install.packages("ggplot2")
if(!require("bslib")) install.packages("bslib")
#if(!require("shinythemes")) install.packages("shinythemes")
if(!require("highcharter")) install.packages("highcharter")
if(!require("plotly")) install.packages("plotly")
#if(!require("reactable")) install.packages("reactable")
if(!require("waiter")) install.packages("waiter")
if(!require("leaflet")) install.packages("leaflet")
if(!require("DT")) install.packages("DT")

library(shiny)
library(rio)
library(dplyr)
#library(ggplot2)
library(bslib)
#library(shinythemes)
library(highcharter)
library(plotly)
#library(reactable)
library(waiter)
library(leaflet)
library(DT)

# load data
retail_data1 <- import("datasets/online_retail_II_1.csv")
retail_data2 <- import("datasets/online_retail_II_2.csv")

retail_data <- retail_data1 %>%
  bind_rows(retail_data2)

# We need to remove the SPACE character from the column names
colnames(retail_data) <- gsub(" ", "", colnames(retail_data))

# Data cleaning: removing rows where the CustomerID is missing, and adding a Revenue column
retail_data <- retail_data %>%
  filter(!is.na(CustomerID)) %>%
  mutate(Revenue = Quantity * Price)
#View(retail_data)

# This is for the first selectInput field
countryList <- retail_data %>% select(Country) %>% unique()
#View(countryList)

stockCodeList <- retail_data %>% select(StockCode) %>% unique()
#View(stockCodeList)


# *****************************************************  UI  ****************************************************
ui <- page_fillable(
  useWaiter(),
  useHostess(),
  
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
  
  titlePanel("Business-oriented Shiny dashboard application"),
  layout_columns(
    # ********************** First card **********************
    card(
      height = 550,
      card_header(""),
      full_screen = T,
      plotlyOutput("plotlyplot"),
      selectInput(
        inputId = "country",
        label = "Choose a country",
        choices = countryList$Country,
        multiple = FALSE,
        selectize = TRUE,
        selected = "Australia"
      )
    ),
    
    # ********************* Second card **********************
    
    card(
      height = 550,
      card_header("Highcharter diagram"),
      full_screen = T,
      card_body(highchartOutput("highchartPlot")),
      selectInput(
        inputId = "country",
        label = "Choose a country",
        choices = c(1, 5, 10),
        multiple = FALSE,
        selectize = TRUE,
        selected = 1
      )
    ),
    
    
    # ********************** Third card **********************
    card(
      card_header("Leaflet map"),
      full_screen = T,
      leafletOutput("mymap", height = 500)
      
    ),
    
    
    # ********************** Third card **********************
    card(
      card_header("Data table"),
      full_screen = T,
      DTOutput("dttable")
      
    ),
    
    col_widths = c(6, 6, 6, 6)
    
  )
)



# ************************************************* SERVER LOGIC ************************************************
# Define server logic required to draw a histogram
server <- function(input, output) {
  hostess <- Hostess$new("loader")
  
  
  hostess$set(20) 
  # Reactive filtered data
  filtered_by_country <- eventReactive(input$country, {
    retail_data %>% filter(Country == input$country)
    #browser()
  })
  
  hostess$set(30)
  output$plotlyplot <- renderPlotly({
    #browser()
    plot_ly(filtered_by_country(), x = ~InvoiceDate, y = ~Revenue) %>%
      add_lines()
  })
  
  hostess$set(40)
  # placeholder for soe server-side code
  
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
    leaflet() %>%
      addTiles() %>%
      addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R") %>% # A temporary default map and location
      setView(lng = 174.768, lat = -36.852, zoom = 12)
  })
  
  
  hostess$set(80)
  output$dttable <- renderDT({
    datatable(iris)  # Just a sample dataset
  })
  
  
  waiter_hide()
}

# Run the application 
shinyApp(ui = ui, server = server)