#------------------------------------------------------------------#
#   Project:  B Corp Impact Data
#   Author:   Travis Horesh
#   Source:   Data World
#               https://data.world/blab/b-corp-impact-data
#   Date:     December 2022
#------------------------------------------------------------------#

if(!require(shiny)) install.packages("shiny", repos = "https://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shiny", repos = "https://cran.us.r-project.org")
if(!require("DT")) install.packages("DT", repos = "https://cran.us.r-project.org")
if(!require(bslib)) install.packages("bslib", repos = "https://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "https://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "https://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "https://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "https://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "https://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "https://cran.us.r-project.org")
if(!require(tidygeocoder)) install.packages("tidygeocoder", repos = "https://cran.us.r-project.org")

library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(dplyr)
library(lubridate)
library(stringr)
library(tidygeocoder)
library(tidyverse)

setwd("/Users/GreenTea/projects/b_corp_impact")

data <- read.csv("https://query.data.world/s/qtnt2oj6vfmv43w5kcn4vlc5uf43fa", 
                header=TRUE,
                stringsAsFactors=FALSE,
                na.strings = c("", " ")
               )

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
           long = longitude,
           timeout = 40)


#######################################################################################
# data <- 
#   if (file.exists("b_corp_impact_data_with_lat_long.csv")) {
#     read.csv("b_corp_impact_data_with_lat_long.csv", 
#              header=TRUE, 
#              stringsAsFactors=FALSE,
#              na.strings = c("", " ")
#     )
#   } else {
#       read.csv("https://query.data.world/s/qtnt2oj6vfmv43w5kcn4vlc5uf43fa", 
#                      header=TRUE, 
#                      stringsAsFactors=FALSE,
#                      na.strings = c("", " ")
#                     )
#     
#       # select only companies that are currently certified
#       data <- data %>%
#         filter(current_status == "certified")
#       
#       # select only the most current certification for each company
#       data <- data %>%
#         group_by(company_id) %>%
#         filter(date_certified == max(date_certified)) %>%
#         ungroup()
#       
#       # add lat/long data
#       data <- head(data, n = 10) %>%
#         geocode(city = city,
#                 country = country,
#                 method = "osm", 
#                 lat = latitude,
#                 long = longitude,
#                 timeout = 40)
#       
#       # save dataset with lat/long details
#       write_csv(data, "b_corp_impact_data_with_lat_long.csv", quote = "needed")
#   }
 #######################################################################################


# take an initial look at the data
glimpse(data)
head(data)

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


##########################################################################
# Possible data table fix - don't understand the JS code and options
# source 1 = https://stackoverflow.com/questions/49247508/how-to-truncate-text-in-datatable-in-r-shiny
# source 2 = https://rstudio.github.io/DT/options.html#fn2

# Output company description
# output$description <- renderDT({
#   description_data <- data_filtered() %>% 
#     select(company_name, description)
#   
#   datatable(
#     data = description_data, 
#     options = list(columnDefs = list(list(
#       targets = 2,
#       render = JS(
#         "function(data, type, row, meta) {",
#         "return type === 'display' && data.length > 200 ?",
#         "'<span title=\"' + data + '\">' + data.substr(0, 200) + '...</span>' : data;",
#         "}")
#     ))), callback = JS('table.page(3).draw(false);'))
# })

# Other possible table fixes:
# tags$head(
#   tags$style(HTML("
#       .box {
#         overflow: scroll;
#       }"))
# ),
# 
# wrap the table in a div(..., style = "overflow-y: auto; height: 300px")
# DT::datatable(..., options = list(scrollY = '300px')) 


##########################################################################


# --------------------------------------------------------------------- #
#   Shiny App Code
# ----------------------------------------------------------------------#

# Define UI for application that draws a histogram
ui <- dashboardPage(

  # Change overall appearance
  skin = "purple",
  
  # Application title
  dashboardHeader(title = "Find a B Corp"),
  
  # Sidebar with industry, product, and top N selectors 
  dashboardSidebar(disable = TRUE),
    
  # Show a plot of top N companies by score, text descriptions of companies, and their locations
  dashboardBody(
    column(
      width = 3,
      box(
        width = NULL,
        status = "warning",
        selectInput(
          inputId = "industry", 
          label = "Select an industry:",
          choices = unique(data_summary$industry)
        )
      ),
      # box(
      #   width = NULL,
      #   status = "warning",
      #   textInput(
      #     inputId = "product",
      #     label = "Search for a product:",
      #     placeholder = "Ex: coffee"
      #   )
      # ),
      box(
        width = NULL,
        status = "warning",
        sliderInput(
          inputId = "top_n",
          label = "Select the number of top companies", 
          min = 1, max = 10, value = 5, step = 1
        )
      )
    ),
    
    column(
      width = 9,
      fluidRow(
        infoBoxOutput(outputId = "company_count"),
        infoBoxOutput(outputId = "tot_company_count")
      ),
      
      fluidRow(
        # Top_N Plot
        box(
          title = "Company Score",
          plotOutput(outputId = "score")
        ),
      
        # Description
        box(
          style= "width: 480px; overflow-x: scroll; height:420px; overflow-y: scroll;",
          title = "Company Description",
          tableOutput(outputId = "description")
        )
      ),
      
      fluidRow(
        # Location Info
        box(leafletOutput(outputId = "map")),
        box(
          title = "Location Details",
          tableOutput(outputId = "location")
        )
      )
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
  
  # Value output for number of companies in selected industry
  output$company_count <- renderValueBox({
    company_count <- data_summary %>%
      filter(industry == input$industry) %>%
      count()
    
    valueBox(
      value = company_count,
      subtitle = "Companies in Selected Industry",
      color = "light-blue",
      width = 4
    )
  })
  
  # Value output for total number of companies in dataset
  output$tot_company_count <- renderValueBox({
    total_comp_count <- nrow(data_summary)
    
    valueBox(
      value = total_comp_count,
      subtitle = "Companies in Dataset",
      color = "light-blue",
      width = 4
    )
  })
  
  # Plot the top N companies by overall score
  output$score <- renderPlot({
    data_filtered() %>% 
      select(company_id, company_name, industry, products_and_services, overall_score) %>%
      
      ggplot() + 
      geom_col(aes(x = overall_score, y = reorder(company_name, overall_score)), fill = "#31688e") +
      labs(x = NULL, y = NULL) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank()
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
