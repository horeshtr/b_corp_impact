#------------------------------------------------------------------#
#   Project:  Find a B Corp
#   Author:   Travis Horesh
#   Source:   Data World
#               https://data.world/blab/b-corp-impact-data
#   Date:     December 2022
#------------------------------------------------------------------#

#### Code removed from original app file ####

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
