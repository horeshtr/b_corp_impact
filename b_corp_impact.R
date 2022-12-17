#------------------------------------------------------------------#
#   Project:  B Corp Impact Data
#   Author:   Travis Horesh
#   Source:   Data World
#               https://data.world/blab/b-corp-impact-data
#   Date:     December 2022
#------------------------------------------------------------------#

if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")

library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)

data <- read.csv("https://query.data.world/s/qtnt2oj6vfmv43w5kcn4vlc5uf43fa", 
                 header=TRUE, 
                 stringsAsFactors=FALSE,
                 na.strings = c("", " ")
                )
glimpse(data)
head(data)

# select only companies that are currently certified
data_cert <- data %>%
  filter(current_status == "certified")
glimpse(data_cert)

# select only the most current certification for each company
data_cert <- data_cert %>%
  group_by(company_id) %>%
  filter(date_certified == max(date_certified)) %>%
  ungroup()

# create indices for columns to retain for summary and detailed datasets
summary_col_index <- c(1, 2, 6:14, 18:23)
detailed_col_index <- c(1, 3:5, 15:17, 24:134)

# subset into summary and detailed datasets
data_summary <- data_cert[,summary_col_index]
data_detailed <- data_cert[,detailed_col_index]

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
products_search <- data_summary[str_detect(data_summary$description, input$product),]
products_search %>% paste(products_search$description)


# --------------------------------------------------------------------- #
#   Shiny App Code
# ----------------------------------------------------------------------#

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("B Corp Impact Data"),
  
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
        )
    ),
    
    # Show a plot of top N companies by score, text descriptions of companies, and their locations
    mainPanel(
      # Top_N Plot
      h2("Company Score"),
      plotOutput(outputId = "score"),
      
      # Location Info
      h2("Company Location"),
      tableOutput(outputId = "location"),
      
      # Description
      h2("Company Description"),
      tableOutput(outputId = "description")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Plot the top N companies by overall score
  output$score <- renderPlot({
    data_summary %>% 
      select(company_id, company_name, industry, products_and_services, overall_score) %>%
      filter(industry == input$industry) %>%
      top_n(input$top_n, overall_score) %>%
      arrange(desc(overall_score)) %>%
      ggplot() + 
      geom_col(aes(x = overall_score, y = reorder(company_name, overall_score))) +
      labs(
        x = "Overall Score",
        y = "Company"
      )
  })
  
  # Output company location
  output$location <- renderTable({
    location_data <- data_summary %>% 
      filter(industry == input$industry) %>%
      top_n(input$top_n, overall_score) %>%
      arrange(desc(overall_score)) %>%
      mutate(location = paste(city, state, country, sep = ", ")) %>%
      select(company_name, location)
  })
  
  # Output company description
  output$description <- renderTable({
    description_data <- data_summary %>% 
      filter(industry == input$industry) %>%
      top_n(input$top_n, overall_score) %>%
      arrange(desc(overall_score)) %>%
      select(company_name, description)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
