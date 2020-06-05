# Loads libraries, data, and make summary tables of the data

# Load functions and libraries
library(tidyverse)
library(lubridate)
library(cowplot)
library(ggforce)
library(MASS)

# Option to preview all columns in a data frame
options(dplyr.width = Inf)

# Load custom functions
source(here::here('code', '0functions.R'))

# Load data

# Use the RDS files instead of csv so that rating levels are
# correctly ordered
df <- readRDS(here::here('data', 'data_processed.Rds'))
df_complete <- readRDS(here::here('data', 'data_processed_complete.Rds'))

# Prep data for models with rating as dependent variable
df_ratings <- prepRatingsData(df_complete)
df_ratings_bev <- df_ratings$df_bev
df_ratings_phev <- df_ratings$df_phev

# Prep data for models with change in rating as dependent variable
df_ratings_change <- prepRatingsChangeData(df_complete)
df_ratings_change_bev <- df_ratings_change$df_bev
df_ratings_change_phev <- df_ratings_change$df_phev

# Load EV sales data for Figure 1
evSales <- read_csv(here::here('data', 'evSalesData.csv')) %>%
    filter(date < ymd('2019-09-01')) # Latest date of scraped data

# Create summary data frames of the data
source(here::here('code', 'make_summary_tables.R'))
