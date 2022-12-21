#------------------------------------------------------------------#
#   Project:  Find a B Corp
#   Author:   Travis Horesh
#   Source:   Data World
#               https://data.world/blab/b-corp-impact-data
#   Date:     December 2022
#------------------------------------------------------------------#


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
    
    #load data
    
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
