#
# This is a Shiny web application.
#
#
#
#
#
#

if(!require("shiny")) install.packages("shiny")
if(!require("rio")) install.packages("rio")
if(!require("dplyr")) install.packages("dplyr")
#if(!require("ggplot2")) install.packages("ggplot2")
if(!require("bslib")) install.packages("bslib")
#if(!require("shinythemes")) install.packages("shinythemes")
#if(!require("highcharter")) install.packages("highcharter")
if(!require("plotly")) install.packages("plotly")
#if(!require("leaflet")) install.packages("leaflet")
#if(!require("DT")) install.packages("DT")
#if(!require("reactable")) install.packages("reactable")

library(shiny)
library(rio)
library(dplyr)
#library(ggplot2)
library(bslib)
#library(shinythemes)
#library(highcharter)
library(plotly)
#library(leaflet)
#library(DT)
#library(reactable)

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

View(retail_data)

# This is for the first selectInput field
countryList <- retail_data %>% select(Country) %>% unique()

# *****************************************************  UI  ****************************************************
ui <- fluidPage(
  
  titlePanel("Business-oriented Shiny dashboard application"),
  
  sidebarLayout(
    # ********************************************* SIDE PANEL **********************************************
    sidebarPanel(
      selectInput(
        inputId = "country",
        label = "Choose a country",
        choices = countryList$Country,
        multiple = FALSE,
        selectize = TRUE,
        selected = "Australia"
      )
      
    ),
    
    # ********************************************* MAIN PANEL **********************************************
    mainPanel(
      card(
        card_header("Card header"),
        #full_screen = T,
        plotlyOutput("plotlyplot")
      )
      
      
      
      
      
      
      
    )
  )
)

# ************************************************* SERVER LOGIC ************************************************
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  

  
 # countryList <- retail_data %>% select(Country) %>% unique()

    
  #browser()
  
  
  # x <- c(1:100)
  # random_y <- rnorm(100, mean = 0)
  # data <- data.frame(x, random_y)
  # 
  # fig <- plot_ly(data, x = ~x, y = ~random_y, type = 'scatter', mode = 'lines')
  # 
  # fig


   
  # Reactive filtered data
  filtered_by_country <- eventReactive(input$country, {
    retail_data %>% filter(Country == input$country)
    #browser()
  })
  
  
  output$plotlyplot <- renderPlotly({
    #browser()
    plot_ly(filtered_by_country(), x = ~InvoiceDate, y = ~Revenue) %>%
      #filter(city %in% input$cities) %>%
      #group_by(Country) %>%
      add_lines()
  })
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)