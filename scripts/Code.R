# Set the working directory
setwd("C:/Users/Administrator/Desktop/Hackbio Cancer Internship/Cholera Outbreak Dataset")
getwd()
# Install necessary library for Excel files 
install.packages("readxl")
#Load the Package
library(readxl)
# Load the cholera datasets
cholera_cases <- read.csv("cholera_cases.csv")
cholera_deaths <- read.csv("cholera_deaths.csv")
cholera_cfr <- read.csv("cholera_cfr.csv")
# View the column names of each dataset to confirm they loaded correctly
colnames(cholera_cases)
colnames(cholera_deaths)
colnames(cholera_cfr)

# Convert reported_deaths and case_fatality_rate to numeric
cholera_deaths$Number.of.reported.deaths.from.cholera <- as.numeric(cholera_deaths$Number.of.reported.deaths.from.cholera)
cholera_cfr$Cholera.case.fatality.rate <- as.numeric(cholera_cfr$Cholera.case.fatality.rate)

# Check for any NA generated during conversion
sum(is.na(cholera_deaths$reported_deaths))
sum(is.na(cholera_cfr$case_fatality_rate))
# Install and load necessary packages
install.packages("countrycode")
install.packages("ggplot2")
library(countrycode)
library(ggplot2)
library(dplyr)

# Load the datasets (you already did this step)
cholera_cases <- read.csv("cholera_cases.csv")
cholera_deaths <- read.csv("cholera_deaths.csv")
cholera_cfr <- read.csv("cholera_cfr.csv")


# Manually replace "Serbia and Montenegro" with "Serbia" before using countrycode
cholera_cfr <- cholera_cfr %>%
  mutate(Countries..territories.and.areas = gsub("Serbia and Montenegro", "Serbia", Countries..territories.and.areas)) %>%
  mutate(Continent = countrycode(Countries..territories.and.areas, "country.name", "continent"))

# Check if the replacement was successful
unique(cholera_cfr$Countries..territories.and.areas)



# Now map countries to continents
cholera_cases$Continent <- countrycode(sourcevar = cholera_cases$Countries..territories.and.areas,
                                       origin = "country.name", destination = "continent")

cholera_deaths$Continent <- countrycode(sourcevar = cholera_deaths$Countries..territories.and.areas,
                                        origin = "country.name", destination = "continent")


cholera_deaths <- cholera_deaths %>%
  mutate(Continent = countrycode(Countries..territories.and.areas, "country.name", "continent"))

cholera_cfr <- cholera_cfr %>%
  mutate(Continent = countrycode(Countries..territories.and.areas, "country.name", "continent"))


# Summarize the data by continent for Cholera Cases
cholera_cases_by_continent <- cholera_cases %>%
  group_by(Continent) %>%
  summarise(Total_Cases = sum(Number.of.reported.cases.of.cholera, na.rm = TRUE)) %>%
  mutate(Percentage = round(100 * Total_Cases / sum(Total_Cases), 1)) # Calculate percentage

# Create the pie chart for Cholera Cases
ggplot(cholera_cases_by_continent, aes(x = "", y = Total_Cases, fill = Continent)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_stack(vjust = 0.5)) +  # Add percentage labels inside pie
  labs(title = "Distribution of Cholera Cases by Continent", 
       x = NULL, y = NULL) +
  theme_void() +  # Clean chart, removing axis and grids
  scale_fill_brewer(palette = "Set3")  # Choose a nice color palette


#2. Pie Chart of Cholera Deaths by Continent
# Convert the 'Number.of.reported.deaths.from.cholera' column to numeric
cholera_deaths$Number.of.reported.deaths.from.cholera <- as.numeric(cholera_deaths$Number.of.reported.deaths.from.cholera)

# Summarize the data by continent for Cholera Deaths
cholera_deaths_by_continent <- cholera_deaths %>%
  group_by(Continent) %>%
  summarise(Total_Deaths = sum(Number.of.reported.deaths.from.cholera, na.rm = TRUE)) %>%
  mutate(Percentage = round(100 * Total_Deaths / sum(Total_Deaths), 1))  # Calculate percentage

# Create the pie chart for Cholera Deaths
ggplot(cholera_deaths_by_continent, aes(x = "", y = Total_Deaths, fill = Continent)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_stack(vjust = 0.5)) +  # Add percentage labels inside pie
  labs(title = "Distribution of Cholera Deaths by Continent", 
       x = NULL, y = NULL) +
  theme_void() +  # Clean chart, removing axis and grids
  scale_fill_brewer(palette = "Set3")  
# Creating a bar plot with cholera cases and deaths
ggplot(top_10_countries, aes(x = reorder(Country, -Total_Cases))) +
  geom_bar(aes(y = Total_Cases, fill = "Cholera Cases"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Total_Deaths, fill = "Cholera Deaths"), stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Cholera Cases" = "blue", "Cholera Deaths" = "red")) +
  theme_minimal() +
  coord_flip() +  # Flip coordinates for better readability
  labs(
    title = "Top 10 Countries with Highest Cholera Cases and Deaths",
    x = "Country",
    y = "Counts",
    fill = "Legend"
  ) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Adding fatality rate as text labels
ggplot(top_10_countries, aes(x = reorder(Country, -Total_Cases), y = Total_Cases)) +
  geom_bar(aes(fill = "Cholera Cases"), stat = "identity") +
  geom_bar(aes(y = Total_Deaths, fill = "Cholera Deaths"), stat = "identity") +
  geom_text(aes(y = Total_Deaths, label = sprintf("%.2f", Fatality_Rate)), vjust = -0.5) +
  scale_fill_manual(values = c("Cholera Cases" = "blue", "Cholera Deaths" = "red")) +
  theme_minimal() +
  coord_flip() +  # Flip coordinates for better readability
  labs(
    title = "Top 10 Countries with Cholera Cases, Deaths, and Fatality Rate",
    x = "Country",
    y = "Counts",
    fill = "Legend"
  )






#regional pie chart for east africa (cholera cases and deaths)
# Define the list of East African countries
east_africa <- c("Kenya", "Tanzania", "Uganda", "Rwanda", "Burundi", 
                 "South Sudan", "Ethiopia", "Somalia", "Djibouti", "Eritrea")

# Filter the cases and deaths data for East African countries
cholera_cases_east_africa <- cholera_cases %>%
  filter(Countries..territories.and.areas %in% east_africa)

cholera_deaths_east_africa <- cholera_deaths %>%
  filter(Countries..territories.and.areas %in% east_africa)

# Summarize Cholera Cases by Country
cholera_cases_summary <- cholera_cases_east_africa %>%
  group_by(Countries..territories.and.areas) %>%
  summarise(Total_Cases = sum(Number.of.reported.cases.of.cholera, na.rm = TRUE)) %>%
  mutate(Percentage = round(100 * Total_Cases / sum(Total_Cases), 1))  # Calculate percentage

# Summarize Cholera Deaths by Country
cholera_deaths_summary <- cholera_deaths_east_africa %>%
  group_by(Countries..territories.and.areas) %>%
  summarise(Total_Deaths = sum(Number.of.reported.deaths.from.cholera, na.rm = TRUE)) %>%
  mutate(Percentage = round(100 * Total_Deaths / sum(Total_Deaths), 1))  # Calculate percentage


#For proper spacing of %labels
install.packages("plotly")
library(plotly)

# Pie chart for Cholera Cases in East Africa
plot_ly(data = cholera_cases_summary, 
        labels = ~Countries..territories.and.areas, 
        values = ~Total_Cases, 
        type = 'pie', 
        textinfo = 'label+percent',  # Display label and percentage
        insidetextorientation = 'radial',  # Text orientation
        marker = list(colors = RColorBrewer::brewer.pal(length(unique(cholera_cases_summary$Countries..territories.and.areas)), "Set3"))) %>%
  layout(title = "Distribution of Cholera Cases in East Africa")

# Pie chart for Cholera Deaths in East Africa
plot_ly(data = cholera_deaths_summary, 
        labels = ~Countries..territories.and.areas, 
        values = ~Total_Deaths, 
        type = 'pie', 
        textinfo = 'label+percent',  # Display label and percentage
        insidetextorientation = 'radial',  # Text orientation
        marker = list(colors = RColorBrewer::brewer.pal(length(unique(cholera_deaths_summary$Countries..territories.and.areas)), "Set3"))) %>%
  layout(title = "Distribution of Cholera Deaths in East Africa")

# Merge datasets by country and year using the correct column names
cholera_data <- cholera_cases %>%
  left_join(cholera_deaths, by = c("Countries..territories.and.areas" = "Countries..territories.and.areas", 
                                   "Year" = "Year")) %>%
  left_join(cholera_cfr, by = c("Countries..territories.and.areas" = "Countries..territories.and.areas", 
                                "Year" = "Year"))
# Inspect merged data
head(cholera_data)

# Filter the merged data for Kenya only
cholera_data_kenya <- cholera_data %>%
  filter(Countries..territories.and.areas == "Kenya")

# Inspect the filtered data
head(cholera_data_kenya)

# Calculate basic statistics for Cholera case fatality rate in Kenya
summary_stats <- cholera_data_kenya %>%
  summarise(
    mean_cfr = mean(Cholera.case.fatality.rate, na.rm = TRUE),
    median_cfr = median(Cholera.case.fatality.rate, na.rm = TRUE),
    max_cfr = max(Cholera.case.fatality.rate, na.rm = TRUE),
    min_cfr = min(Cholera.case.fatality.rate, na.rm = TRUE)
  )


#1.Time Series Line Graph

# Load the required libraries for visualization
library(ggplot2)
library(tidyr)

# Create a line plot of Cholera case fatality rate over the years
ggplot(cholera_data_kenya, aes(x = Year, y = Cholera.case.fatality.rate)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(
    title = "Cholera Case Fatality Rate in Kenya Over Time",
    x = "Year",
    y = "Cholera Case Fatality Rate (%)"
  ) +
  theme_minimal()