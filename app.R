library(shiny)
library(tidyverse)
library(dplyr)
library(DT)

co2_data <- read.csv("owid-co2-data-3.csv")

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("CO2 Emissions Data Explorer"),
  
  # Sidebar with a slider input for selecting range of years and a select input for choosing countries
  sidebarLayout(
    sidebarPanel(
      p("Pick countries and the years you would like to analyze certain emissions! 
        All averages are measured in Million Tonnes!"),
      sliderInput("year_range", "Select Year Range:",
                  min = min(co2_data$year), max = max(co2_data$year),
                  value = c(1950, 2021)),
      selectizeInput("countries", "Select Countries:", 
                     choices = unique(co2_data$country), multiple = TRUE),
      checkboxInput("all_countries", "All Countries", value = FALSE),
      
      
      
    ),
    
    # Show a table with the average co2 emissions by country and year
    mainPanel(
      tabsetPanel(
        # Opening page
        tabPanel("Welcome",
                 p("Welcome to the", em("CO2 Emissions Data Explorer!"),
                   "Use the sidebar to select a range of 
                   years and countries to analyze the average CO2, nitrous oxide, 
                   methane, oil CO2, and trade CO2 emissions. 
                   You can view the results by navigating to the respective tabs."),
                 p("This app uses data from the", strong("Our World in Data website.") ),
                 p("Click on the tabs above to see the data analysis. Also, 
                   check out the filters on the left here. Note: They will not
                   change anything on this page! Checkout the plot and table page
                   for more!"),
                 p("There are:", strong(nrow(co2_data)), "rows and", strong(ncol(co2_data)), "columns!"),
                 p("Below exist just some of these rows and columns!"),
                 h2("Random Sample of Data: "), 
                 DT::dataTableOutput("random_sample_table")
        ),
        
        # Plot page
        tabPanel("Plot",
                 p("This page will show a plot of CO2 emissions over time for 
                   selected countries."),
                 p("Note: Not all countries have data and those countries will 
                   have no visual lines!"),
                 plotOutput("co2_plot"),
                 h4("Overall Average CO2 Emissions: ", textOutput("overall_avg_co2"))
        ),
        
        # Table page
        tabPanel("Table",
                 h4("Selected subset contains", textOutput("num_obs"), 
                    "observations."),
                 DT::dataTableOutput("co2_table"),
                 DT::dataTableOutput("nox_table"),
                 DT::dataTableOutput("ch4_table"),
                 DT::dataTableOutput("oil_co2_table"),
                 DT::dataTableOutput("trade_co2_table"), 
                 
        )
      )
    )
  )
)


# Define server logic
server <- function(input, output) {
  filtered_data <- reactive({
    if (input$all_countries) {
      co2_data %>% filter(year >= input$year_range[1], year <= input$year_range[2])
    } else {
      co2_data %>% filter(year >= input$year_range[1], year <= input$year_range[2], country %in% input$countries)
    }
  })
  
  # Calculate overall average CO2 emissions
  overall_avg_co2 <- reactive({
    filtered_data() %>% summarise(avg_co2 = mean(co2, na.rm = TRUE))
  })
  
  # Create a plot of CO2 emissions over time for selected countries
  output$co2_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = year, y = .data[[input$variable]], color = country)) +
      geom_line() +
      xlab("Year") +
      ylab("CO2 Emissions (Million Tonnes)") +
      ggtitle(paste("CO2 Emissions by Year and Country:", input$variable)) +
      theme_bw()
  })
  
  # Render the overall average CO2 emissions
  output$overall_avg_co2 <- renderText({
    paste0(round(overall_avg_co2()$avg_co2, 2), " Million Tonnes")
  })
  cols_to_show <- c("country", "year", "population", "gdp", 
                    "co2", "co2_per_capita", "co2_per_gdp",
                    "methane", "methane_per_capita", "nitrous_oxide", "nitrous_oxide_per_capita",
                    "oil_co2", "oil_co2_per_capita", "total_ghg",
                    "trade_co2")
  
  
  
  # Create a random sample of the data
  sample_data <- co2_data %>% select(cols_to_show) %>% sample_n(10)
  
  # Render the table of the random sample
  output$random_sample_table <- DT::renderDataTable({
    DT::datatable(sample_data, options = list(pageLength = 10))
  })
  
  # Get the filtered data based on selected year range and country selection
  filtered_data <- reactive({
    if (input$all_countries) {
      co2_data %>% filter(year >= input$year_range[1], year <= input$year_range[2])
    } else {
      co2_data %>% filter(year >= input$year_range[1], year <= input$year_range[2], country %in% input$countries)
    }
  })
  
  # Calculate average co2 emissions by country and year
  co2_table <- reactive({
    filtered_data() %>% group_by(country) %>% summarise(avg_co2 = mean(co2, na.rm = TRUE))
  })
  
  # Calculate average nitrous oxide emissions by country and year
  nox_table <- reactive({
    filtered_data() %>% group_by(country) %>% summarise(avg_nox = mean(nitrous_oxide, na.rm = TRUE))
  })
  
  # Calculate average methane emissions by country and year
  ch4_table <- reactive({
    filtered_data() %>% group_by(country) %>% summarise(avg_ch4 = mean(methane, na.rm = TRUE))
  })
  
  #Calculate average oil co2 emissions by country and year
  
  oil_co2_table <- reactive({
    filtered_data() %>% group_by(country) %>% summarise(avg_oil_co2 = mean(oil_co2, na.rm = TRUE))
  })
  
  #Calculate average trade co2 emissions by country and year
  
  trade_co2_table <- reactive({
    filtered_data() %>% group_by(country) %>% summarise(avg_trade_co2 = mean(trade_co2, na.rm = TRUE))
  })
  
  #Render the number of observations in the selected subset
  
  output$num_obs <- renderText({
    paste(nrow(filtered_data()), "rows of")
  })
  
  #Render the overall average co2 emissions for the selected subset
  
  output$overall_avg_co2 <- renderText({
    paste(round(mean(filtered_data()$co2, na.rm = TRUE), 2), "Million Tonnes")
  })
  
  #Render the CO2 plot for selected countries
  
  output$co2_plot <- renderPlot({
    if (input$all_countries) {
      co2_data %>%
        filter(year >= input$year_range[1], year <= input$year_range[2]) %>%
        ggplot(aes(x = year, y = co2, color = country)) +
        geom_line() +
        labs(title = "CO2 Emissions Over Time", x = "Year", y = "CO2 Emissions (Million Tonnes)")
    } else {
      co2_data %>%
        filter(year >= input$year_range[1], year <= input$year_range[2], country %in% input$countries) %>%
        ggplot(aes(x = year, y = co2, color = country)) +
        geom_line() +
        labs(title = "CO2 Emissions Over Time", x = "Year", y = "CO2 Emissions (Million Tonnes)")
    }
  })
  
  
  #Render the CO2 table for selected countries
  output$co2_table <- DT::renderDataTable({
    co2_table() %>%
      arrange(desc(avg_co2)) %>%
      datatable(rownames = FALSE, options = list(lengthMenu = c(5, 10, 15), pageLength = 10))
  })
  
  #Render the nitrous oxide table for selected countries
  output$nox_table <- DT::renderDataTable({
    nox_table() %>%
      arrange(desc(avg_nox)) %>%
      datatable(rownames = FALSE, options = list(lengthMenu = c(5, 10, 15), pageLength = 10))
  })
  
  #Render the methane table for selected countries
  output$ch4_table <- DT::renderDataTable({
    ch4_table() %>%
      arrange(desc(avg_ch4)) %>%
      datatable(rownames = FALSE, options = list(lengthMenu = c(5, 10, 15), pageLength = 10))
  })
  
  #Render the oil CO2 table for selected countries
  output$oil_co2_table <- DT::renderDataTable({
    oil_co2_table() %>%
      arrange(desc(avg_oil_co2)) %>%
      datatable(rownames = FALSE, options = list(lengthMenu = c(5, 10, 15), pageLength = 10))
  })
  
  #Render the trade CO2 table for selected countries
  output$trade_co2_table <- DT::renderDataTable({
    trade_co2_table() %>%
      arrange(desc(avg_trade_co2)) %>%
      DT::datatable(rownames = FALSE, options = list(pageLength = 10))
  })
}
#Run the application
shinyApp(ui = ui, server = server)