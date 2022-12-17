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


# --------------------------------------------------------------------- #
#   Plots and Exploratory Analyses 
# ----------------------------------------------------------------------#

# Determine number of distinct values in key categorical fields
col_index <- c(7:14)
n_dist <- function(column){
  n_distinct(data[,column])
}
num_dist_values <- sapply(col_index, n_dist)
num_dist_values <- matrix(num_dist_values, ncol = ncol(data[,col_index]))
colnames(num_dist_values) <- colnames(data[,col_index])
num_dist_values <- as_tibble(num_dist_values)
num_dist_values

# Distribution and Scores by company size
data %>%
  ggplot(aes(x = size)) + 
  geom_histogram(stat = "count")

data %>%
  ggplot(aes(x = size, y = overall_score)) +
  geom_boxplot()

# Distribution and Scores by company sector
data %>%
  ggplot(aes(x = sector)) + 
  geom_histogram(stat = "count") + 
  guides(x = guide_axis(angle = 45, n.dodge = 1))

data %>%
  ggplot(aes(x = sector, y = overall_score)) +
  geom_boxplot() + 
  guides(x = guide_axis(angle = 45, n.dodge = 1))

# Distribution and Scores by industry_category
data %>%
  ggplot(aes(x = industry_category)) + 
  geom_histogram(stat = "count") + 
  guides(x = guide_axis(angle = 45, n.dodge = 1))

data %>%
  ggplot(aes(x = industry_category, y = overall_score)) +
  geom_boxplot() + 
  guides(x = guide_axis(angle = 45, n.dodge = 1))


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
      selectInput("industry", "Select an industry:", choices = unique(data$industry)),
      textInput("product", "Search for a product:", value = "Ex: coffee"),
      sliderInput("top_n", "Select the number of top companies", min = 1, max = 10, value = 5, step = 1)
    ),
    
    # Show a plot and table
    mainPanel(
      #
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Plot the top N companies by overall score
  output$score <- renderPlot({
    data %>% 
      select(company_id, company_name, industry, products_and_services, overall_score) %>%
      filter(industry == input$industry,
             input$product %in% description
      ) %>%
      top_n(overall_score, input$top_n)
      ggplot() + 
      geom_col(aes(overall_score, company_name)) +
      labs(
        x = "Company",
        y = "Overall Score"
      )
  })
  
  # Output company description
  output$description <- renderText({
    data %>%
      filter(state == input$state,
             date >= input$dates[1], 
             date <= input$dates[2]
      ) %>%
      group_by(shape) %>%
      summarize(num_sightings = n(),
                #min_duration = min(duration),
                #max_duration = max(duration),
                #avg_duration = mean(duration),
                #median_duration = median(duration)
      )
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
