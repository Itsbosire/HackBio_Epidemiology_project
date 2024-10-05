#setwd
#setwd("C:/Users/polla/OneDrive/Desktop/HACKBIOINTERN")
#.libPaths("C:/Users/polla/OneDrive/Desktop/HACKBIOINTERN/TASK3")

#install packages
#install.packages("shinydashboard")
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("leaflet")
#install.packages("shiny")
#install.packages("readxl")
#install.packages("shinythemes")
#install.packages("rsconnect")
#install.packages("packrat")
#install.packages("devtools")
#devtools::find_rtools()
#Load necessary libraries
library(tidyverse)
library(ggplot2)
library(leaflet)
library(shiny)
library(readxl)
library(shinydashboard)
library(shinythemes)
library(rsconnect)
library(packrat)
library(devtools)

#load my csv file
world_cholera <- readxl::read_xlsx("world_cholera_cases.xlsx")

#preview file
head(world_cholera)

#check column names
colnames(world_cholera)

#check for missing values in file
colSums(is.na(world_cholera))

#View the data type of each column, if they are numerals or characters
str(world_cholera)
#View(world_cholera)
#Section for Plots;

#dataframe for country coordinates:
country_name <- c('Afghanistan', 'Australia', 'Bahrain', 'Bangladesh', 'Benin', 'Burundi', 'Cameroon', 'China', 'Congo', 'D.R.Congo',
                  'Dominican Republic', 'Eswatini', 'Ethiopia', 'France', 'Germany', 'Haiti', 'India', 'Iran', 'Iraq', 'Japan',
                  'Kenya', 'Lebanon', 'Liberia', 'Malawi', 'Malaysia', 'Mozambique', 'Nepal', 'New Zealand', 'Nigeria', 'Pakistan',
                  'Philippines', 'Qatar', 'Rwanda', 'Saudi Arabia', 'Somalia', 'South Africa', 'South Sudan', 'Sudan', 'Sweden',
                  'Tanzania', 'Thailand', 'Togo', 'Turks and Caicos Islands', 'Zambia', 'Zimbabwe')

latitude <- c(33.93911, -25.2744, 26.0667, 23.685, 9.3077, -3.3731, 3.848, 35.8617, -0.228, -4.0383, 
              18.7357, -26.5225, 9.145, 46.6034, 51.1657, 18.9712, 20.5937, 32.4279, 33.2232, 36.2048,
              -1.286389, 33.8547, 6.4281, -13.2543, 4.2105, -18.6657, 28.3949, -40.9006, 9.082, 30.3753,
              12.8797, 25.3548, -1.9403, 23.8859, 5.1521, -30.5595, 6.877, 12.8628, 60.1282, -6.369, 
              15.8700, 8.6195, 21.694, -13.1339, -19.0154)

longitude <- c(67.709953, 133.7751, 50.5577, 90.3563, 2.3158, 29.9189, 11.5021, 104.1954, 15.8277, 21.7587,
               -70.1627, 31.4659, 40.4897, 2.2137, 10.4515, -72.2852, 78.9629, 53.688, 43.6793, 138.2529,
               36.817223, 35.8623, -9.4295, 34.3015, 101.9758, 35.5296, 84.124, 174.8859, 8.6753, 69.3451,
               121.774, 51.1839, 29.8739, 45.0792, 46.1996, 22.9375, 30.2176, 18.6435, 34.8888, 100.9925, 
               0.8248, -71.7979, 27.8493, 29.1549, 30.8024)
#create dataframe:
country_coords <- data.frame(country_name, latitude, longitude)
#View(country_coords)
# merge into world_cholera and country coords into one data set:
cholera_data <- merge(world_cholera, country_coords, by.x = "country", by.y = "country_name", all.x = TRUE)
#View(cholera_data)
#check data types
str(cholera_data)



#create a line plot for reported cases overtime
ggplot(cholera_data, aes(x = year, y = total_cases, colour = country)) +
  geom_line(linewidth = 1) + 
  labs(title = "Reported Cases of Cholera over Time", x = "Year", y = "Number of Reported Cases") + theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_color_viridis_d()#gives a vibrant colour scale

#Barplot for deaths by country
ggplot(cholera_data, aes(x = country, y = deaths, fill = domestic_cases)) + geom_bar(stat = "identity", position = "dodge") +
labs(title = "Cholera Death Rates by Country", x = "Country", y = "Number of Deaths") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_gradient(low = "blue", high = "red") # colour gradient based on domestic cases

# Summary of total cases by continent and calculate percentage
cholera_by_continent <- cholera_data %>%
  group_by(continent) %>%
  summarise(total_cases = sum(total_cases, na.rm = TRUE)) %>%
  mutate(Percentage = round(100 * total_cases / sum(total_cases), 2))  # Calculate percentage

#Create the pie chart
ggplot(cholera_by_continent, aes(x = "", y = total_cases, fill = continent)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_stack(vjust = 0.5)) +  # Add percentage labels inside pie
  labs(title = "Distribution of Cholera Cases by Continent", 
       x = NULL, y = NULL) +
  theme_void() +  # Clean chart, removing axis and grids
  scale_fill_brewer(palette = "Set3")  # Choose a nice color palette

# Check for missing longitude and latitude data
cholera_data_clean <- cholera_data %>%
  filter(!is.na(longitude) & !is.na(latitude))  # Remove rows with missing latitude/longitude
#View(cholera_data_clean)

#Create interactive map:
leaflet(data = cholera_data_clean) %>% 
  addTiles() %>%
  addCircleMarkers(~longitude, ~latitude,
                   radius = ~sqrt(total_cases)/100, #scale the marker size by total cases
                   color = ~ifelse(deaths > 0, "red", "green"),
                   label = ~paste(country, "Reported Cases:", total_cases, "<br>", "Deaths:", deaths)) %>%
  addLegend(position = "bottomright",
            title = "Reported Cases",
            colors = c("red", "green"),
            labels = c("No Deaths", "Deaths"))

  
#Shiny app build:

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  titlePanel("Cholera Outbreak Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select a Country", choices = unique(cholera_data_clean$country), selected = "Kenya"),
      sliderInput("year", "Select Year Range", 
                  min = min(cholera_data_clean$year), 
                  max = max(cholera_data_clean$year),
                  value = c(min(cholera_data_clean$year), max(cholera_data_clean$year)),
                  step = 1)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Reported Cases Over Time", plotOutput("linePlot")),
        tabPanel("Cholera Deaths by Country", plotOutput("barPlot")),
        tabPanel("Interactive Map", leafletOutput("choleraMap"))
      )
    )
  )
)

#Server logic for app

server <- function(input, output) {
  #section for filtering data based on my input(country, year)
  filtered_data <-reactive({
    cholera_data_clean %>% filter(country == input$country & year >= input$year[1] & year <= input$year[2])
  })
  #line plot for total cases over time:
  output$linePlot <- renderPlot({
    ggplot(filtered_data(), aes(x = year, y = total_cases, colour = country)) + 
      geom_line(linewidth = 1) +
      labs(title = "Cholera Reported Cases over Time", x = "Year", y = "Number of Reported Cases") + 
      theme_minimal()
  })
  #Barplot for deaths by country
  output$barPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = country, y = deaths, fill = domestic_cases)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Cholera Deaths by Country", x = "Country", y = "Number of Deaths") +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_gradient(low = "blue", high = "red")
  })
  #interactive map continent
  output$choleraMap <- renderLeaflet({
    leaflet(data = filtered_data()) %>%
      addTiles() %>%
      addCircleMarkers(~longitude, ~latitude,
                       radius = ~sqrt(total_cases)/100,
                       color = ~ifelse(deaths > 0 , "red", "green"),
                       label = ~paste(country, "Reported Cases:", total_cases, "<br>", "Deaths:", deaths))
  })

}
#Run the shinyapp
shinyApp(ui = ui, server = server)

