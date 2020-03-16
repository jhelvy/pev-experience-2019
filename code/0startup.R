# Load libraries
library(tidyverse)
library(lubridate)
library(rvest)
library(cowplot)
library(ggforce)
library(MASS)

# Option to preview all columns in a data frame
options(dplyr.width = Inf)

# Load functions & data, and create summary data frames of the data
source(file.path('code', '0functions.R'))
source(file.path('code', '0loadData.R'))
