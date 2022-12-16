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

length(unique(data$industry))
length(unique(data$country))
length(unique(data$state))
length(unique(data$size))

# industry_sum
data %>%
  ggplot(aes(x = size, y = overall_score)) +
  geom_boxplot()