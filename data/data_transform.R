#------------------------------------------------------------------#
#   Project:  Find a B Corp
#   Function: Data preparation
#   Author:   Travis Horesh
#   Source:   Data World
#               https://data.world/blab/b-corp-impact-data
#   Date:     December 2022
#------------------------------------------------------------------#

# install packages if required
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

# load libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(dplyr)
library(lubridate)
library(stringr)
library(tidygeocoder)
library(tidyverse)

# set working directory
setwd("/Users/GreenTea/projects/find_b_corp_app/data")


# determine which data source to read in; load and transform it, if needed
if (file.exists("b_corp_impact_data_with_lat_long.rds")) {
    data <- readRDS(file = "b_corp_impact_data_with_lat_long.rds")
  } else {
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

      # save dataset with lat/long details
      saveRDS(data, file = "b_corp_impact_data_with_lat_long.rds")
  }

# create indices for columns to retain for summary and detailed datasets
summary_col_index <- c(1, 2, 6:14, 18:23, 135, 136)
detailed_col_index <- c(1, 3:5, 15:17, 24:134)

# subset into summary and detailed datasets
data_summary <- data[,summary_col_index]
data_detailed <- data[,detailed_col_index]

#save subsetted datasets
saveRDS(data_summary, "b_corp_impact_data_summary.rds")