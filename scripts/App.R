# Load necessary libraries
library(shiny)
library(shinythemes)
library(leaflet)
library(ggplot2)
library(dplyr)
library(DT)
library(readr)
library(tidyr)
library(rmarkdown)
library(readxl)
# Load the cholera datasets
cholera_cases <- read.csv("cholera_cases.csv")
cholera_deaths <- read.csv("cholera_deaths.csv")
cholera_cfr <- read.csv("cholera_cfr.csv")

read.csv("cholera_cases.csv")


# Convert reported_deaths and case_fatality_rate to numeric
cholera_deaths$Number.of.reported.deaths.from.cholera <- as.numeric(cholera_deaths$Number.of.reported.deaths.from.cholera)
cholera_cfr$Cholera.case.fatality.rate <- as.numeric(cholera_cfr$Cholera.case.fatality.rate)

# Check for any NA generated during conversion
sum(is.na(cholera_deaths$reported_deaths))
sum(is.na(cholera_cfr$case_fatality_rate))


# Merge datasets by country and year using the correct column names
cholera_data <- cholera_cases %>%
  left_join(cholera_deaths, by = c("Countries..territories.and.areas" = "Countries..territories.and.areas", 
                                   "Year" = "Year")) %>%
  left_join(cholera_cfr, by = c("Countries..territories.and.areas" = "Countries..territories.and.areas", 
                                "Year" = "Year"))

# Inspect merged data
head(cholera_data)

# Data frame of country coordinates
Country.name = c('Afghanistan', 'Australia', 'Bahrain', 'Bangladesh', 'Benin', 'Kenya', 'United States', 'India', 'Nigeria', 'Brazil')
Latitude = c(33.93911, -25.2744, 26.0667, 23.685, 9.3077, -1.286389, 37.0902, 20.5937, 9.082, -14.235)
Longitude = c(67.709953, 133.7751, 50.5577, 90.3563, 2.3158, 36.817223, -95.7129, 78.9629, 8.6753, -51.9253)
country_coords <- data.frame(Country.name, Latitude, Longitude)

# Checking column names
names(cholera_data)
names(country_coords)

# Merging cholera data with the full country coordinates using the 'Country' column
cholera_data_with_full_coords <- merge(cholera_data, country_coords, 
                                       by.x = "Countries..territories.and.areas", 
                                       by.y = "Country.name", all.x = TRUE)

# UI function
ui <- fluidPage(
  theme = shinytheme("cerulean"), # Apply cerulean theme for modern look
  titlePanel("Cholera Outbreak Data Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearRange", 
                  "Select Year Range:", 
                  min = min(cholera_data_with_full_coords$Year, na.rm = TRUE), 
                  max = max(cholera_data_with_full_coords$Year, na.rm = TRUE), 
                  value = c(min(cholera_data_with_full_coords$Year, na.rm = TRUE), 
                            max(cholera_data_with_full_coords$Year, na.rm = TRUE)),
                  sep = ""), # Year range slider
      
      selectInput("selectedCountry", 
                  "Select Country:", 
                  choices = c("All", unique(cholera_data_with_full_coords$Countries..territories.and.areas)),
                  selected = "All") # Country selection dropdown
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Cholera Map", leafletOutput("choleraMap")), # Cholera map
        tabPanel("Data Table", DTOutput("dataTable")),        # Data table
        tabPanel("Cholera Summary", plotOutput("choleraPlot")) # Cholera summary plot
      )
    )
  )
)

# Server function
server <- function(input, output, session) {
  
  # Reactive data based on user inputs for year range and selected country
  filtered_data <- reactive({
    data <- cholera_data_with_full_coords
    
    # Filter by year range
    data <- data %>% filter(Year >= input$yearRange[1] & Year <= input$yearRange[2])
    
    # Filter by selected country if not "All"
    if (input$selectedCountry != "All") {
      data <- data %>% filter(Countries..territories.and.areas == input$selectedCountry)
    }
    
    return(data)
  })
  
  # Render Leaflet Map with uniform circle markers
  output$choleraMap <- renderLeaflet({
    data <- filtered_data()
    
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        ~Longitude, ~Latitude,
        radius = 4, # Small and uniform circle size for all markers
        color = "#0073B7", # Blue color for the circle markers
        stroke = TRUE,
        fillOpacity = 0.8, # Adjusted opacity for visibility
        popup = ~paste(
          "<b>Country:</b>", Countries..territories.and.areas, "<br>",
          "<b>Year:</b>", Year, "<br>",
          "<b>Cholera Cases:</b>", Number.of.reported.cases.of.cholera, "<br>",
          "<b>Deaths:</b>", Number.of.reported.deaths.from.cholera, "<br>",
          "<b>CFR (%):</b>", Cholera.case.fatality.rate
        )
      ) %>%
      setView(lng = mean(data$Longitude, na.rm = TRUE), 
              lat = mean(data$Latitude, na.rm = TRUE), zoom = 2)
  })
  
  # Render Data Table
  output$dataTable <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  # Render Cholera Summary Plot
  output$choleraPlot <- renderPlot({
    data <- filtered_data()
    
    ggplot(data, aes(x = Year, y = Number.of.reported.cases.of.cholera, color = Countries..territories.and.areas)) +
      geom_line() +
      labs(title = "Cholera Cases Over Time", x = "Year", y = "Number of Cases") +
      theme_minimal()
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)