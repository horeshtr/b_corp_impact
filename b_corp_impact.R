#------------------------------------------------------------------#
#   Project:  B Corp Impact Data
#   Author:   Travis Horesh
#   Source:   Data World
#               https://data.world/blab/b-corp-impact-data
#   Date:     December 2022
#------------------------------------------------------------------#

if(!require(shiny)) install.packages("shiny", repos = "https://cran.us.r-project.org")
if(!require(bslib)) install.packages("bslib", repos = "https://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "https://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "https://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "https://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "https://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "https://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "https://cran.us.r-project.org")
if(!require(tidygeocoder)) install.packages("tidygeocoder", repos = "https://cran.us.r-project.org")

library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(leaflet)
library(dplyr)
library(lubridate)
library(stringr)
library(tidygeocoder)
library(tidyverse)

data <- read.csv("https://query.data.world/s/qtnt2oj6vfmv43w5kcn4vlc5uf43fa", 
                 header=TRUE, 
                 stringsAsFactors=FALSE,
                 na.strings = c("", " ")
                )
glimpse(data)
head(data)

# select only companies that are currently certified
data <- data %>%
  filter(current_status == "certified")

# select only the most current certification for each company
data <- data %>%
  group_by(company_id) %>%
  filter(date_certified == max(date_certified)) %>%
  ungroup()

# add lat/long data
data <- data %>%
  geocode(city = city,
          country = country,
          method = "osm", 
          lat = latitude,
          long = longitude)

# function to check whether lat/long call is needed?


# create indices for columns to retain for summary and detailed datasets
summary_col_index <- c(1, 2, 6:14, 18:23, 135, 136)
detailed_col_index <- c(1, 3:5, 15:17, 24:134)

# subset into summary and detailed datasets
data_summary <- data[,summary_col_index]
data_detailed <- data[,detailed_col_index]

# --------------------------------------------------------------------- #
#   Plots and Exploratory Analyses 
# ----------------------------------------------------------------------#

# Determine number of distinct values in key categorical fields
col_index <- c(7:14)
n_dist <- function(column){
  n_distinct(data_summary[,column])
}
num_dist_values <- sapply(col_index, n_dist)
num_dist_values <- matrix(num_dist_values, ncol = ncol(data_summary[,col_index]))
colnames(num_dist_values) <- colnames(data_summary[,col_index])
num_dist_values <- as_tibble(num_dist_values)
num_dist_values

# Distribution and Scores by company size
data_summary %>%
  ggplot(aes(x = size)) + 
  geom_histogram(stat = "count")

data_summary %>%
  ggplot(aes(x = size, y = overall_score)) +
  geom_boxplot()

# Distribution and Scores by company sector
data_summary %>%
  ggplot(aes(x = sector)) + 
  geom_histogram(stat = "count") + 
  guides(x = guide_axis(angle = 45, n.dodge = 1))

data_summary %>%
  ggplot(aes(x = sector, y = overall_score)) +
  geom_boxplot() + 
  guides(x = guide_axis(angle = 45, n.dodge = 1))

# Distribution and Scores by industry_category
data_summary %>%
  ggplot(aes(x = industry_category)) + 
  geom_histogram(stat = "count") + 
  guides(x = guide_axis(angle = 45, n.dodge = 1))

data_summary %>%
  ggplot(aes(x = industry_category, y = overall_score)) +
  geom_boxplot() + 
  guides(x = guide_axis(angle = 45, n.dodge = 1))


# --------------------------------------------------------------------- #
#   TEST
# ----------------------------------------------------------------------#

# products_search <- data_summary[str_detect(data_summary$description, input$product),]
# products_search %>% paste(products_search$description)

#subset(data_summary, input$product %in% description)

# for re-ordering size values, check this source: 
#   https://r-graph-gallery.com/267-reorder-a-variable-in-ggplot2.html
  
data_summary %>% 
  filter(is.na(latitude) == TRUE | is.na(longitude) == TRUE) %>% 
  select(company_id, latitude, longitude)

# Title formatting
#, style = "text-align: center; background: #1f9e89; color: white;"

# Top N and Description formatting
# cellArgs = list(style = "padding: 5px; border: 1px darkgray;"),

# Location formatting
# style = "border: 1px darkgray;",
# cellArgs = list(style = "padding: 5px"),

# Plot fill code
#, fill = "#31688e"


# --------------------------------------------------------------------- #
#   Shiny App Code
# ----------------------------------------------------------------------#

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Find a B Corp", windowTitle = "Shiny App to Find a B Corp"),
  
  #Subtitle
  h4(em("based on industry and impact")),
  
  # Sidebar with industry, product, and top N selectors 
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "industry", 
        label = "Select an industry:",
        choices = unique(data_summary$industry)
        ),
      textInput(
        inputId = "product",
        label = "Search for a product:",
        placeholder = "Ex: coffee"
        ),
      sliderInput(
        inputId = "top_n",
        label = "Select the number of top companies", 
        min = 1, max = 10, value = 5, step = 1
        ),
      width = 3
    ),
    
    # Show a plot of top N companies by score, text descriptions of companies, and their locations
    mainPanel(
      fluidRow(
        splitLayout(
          cellWidths = c("50%", "50%"),
          
          # Section Titles
          h3("Company Score"),
          h3("Company Description"),
        )
      ),
      
      fluidRow(
        splitLayout(
          cellWidths = c("50%", "50%"),
          
          
          # Top_N Plot
          plotOutput(outputId = "score"),
        
          # Description
          tableOutput(outputId = "description")
        )
      ),
      
      fluidRow(
        # Section Title
        h3("Company Location")
      ),
      
      fluidRow(
        splitLayout(
          
          cellWidths = c("50%", "50%"),
          
        
          # Location Info
          leafletOutput(outputId = "map"),
          tableOutput(outputId = "location")
        )
      ),
      
      # Set overall Main Panel dimensions
      width = 9
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  data_filtered <- reactive({
    data_summary %>% 
      filter(industry == input$industry) %>%
      top_n(input$top_n, overall_score) %>%
      arrange(desc(overall_score))
  })
  
  # Plot the top N companies by overall score
  output$score <- renderPlot({
    data_filtered() %>% 
      select(company_id, company_name, industry, products_and_services, overall_score) %>%
      
      ggplot() + 
      geom_col(aes(x = overall_score, y = reorder(company_name, overall_score))) +
      labs(
        x = "Overall Score",
        y = "Company"
      )
  })
  
  # Output company description
  output$description <- renderTable({
    description_data <- data_filtered() %>% 
      select(company_name, description)
  })
  
  # Output company location map
  output$map <- renderLeaflet({
    points <- data_filtered() %>% 
      select(company_name, latitude, longitude)
    
    leaflet() %>%
      addTiles() %>%  
      addMarkers(
        lng = points$longitude, 
        lat = points$latitude, 
        popup = points$company_name
      )
  })
  
  # Output company location table
  output$location <- renderTable({
    location_data <- data_filtered() %>% 
      mutate(location = paste(city, state, country, sep = ", ")) %>%
      select(company_name, location)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
