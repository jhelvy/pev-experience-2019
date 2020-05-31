# This file is used to fit models. The main object created after running any
# model is called "fit". This is over-written every time you run any of the
# following models. Use code at the bottom of this file to view the results of
# any model.

# Setup -----------------------------------------------------------------------

# Load functions and libraries
library(tidyverse)
library(lubridate)
library(rvest)
library(cowplot)
library(ggforce)
library(MASS)

# Option to preview all columns in a data frame
options(dplyr.width = Inf)

# Load functions & data, and create summary data frames of the data
source(here::here('code', '0functions.R'))
source(here::here('code', '0loadData.R'))

# Load data
df <- readRDS(here::here('data', 'autoshow_complete.Rds'))

# Prep data for models with rating as dependent variable
df_ratings <- prepRatingsData(df)
df_ratings_bev <- df_ratings$df_bev
df_ratings_phev <- df_ratings$df_phev

# Prep data for models with change in rating as dependent variable
df_ratings_change <- prepRatingsChangeData(df)
df_ratings_change_bev <- df_ratings_change$df_bev
df_ratings_change_phev <- df_ratings_change$df_phev

# Models in Table 5 in main text ----------------------------------------------

# Model 1: Main effect for BEV consideration rating
fit <- addFitStats(polr(
    rating ~ periodAfter,
    data = df_ratings_bev, Hess = TRUE))
printPolrSummaryTable(fit)

# Model 2a: Effect for BEV ratings depending on if respondent had greater
#           knowledge about PEV refueling
fit <- addFitStats(polr(
    rating ~  periodAfter + fuelElec_only + fuelGas_only + fuel_bothanswers +
    periodAfter*fuelElec_only + periodAfter*fuelGas_only + 
    periodAfter*fuel_bothanswers,
    data = df_ratings_bev, Hess = TRUE))
printPolrSummaryTable(fit)

# Model 2b: Effect for BEV ratings depending on if respondent had greater
#           knowledge about PEV subsidies
fit <- addFitStats(polr(
    rating ~  periodAfter + subsidy_correct + periodAfter*subsidy_correct,
    data = df_ratings_bev, Hess = TRUE))
printPolrSummaryTable(fit)

# Model 3: Effect for BEV ratings depending on if neighbor has PEV
fit <- addFitStats(polr(
    rating ~ periodAfter + neighborhasEV + periodAfter*neighborhasEV,
    data = df_ratings_bev, Hess = TRUE))
printPolrSummaryTable(fit)

# Model 4: Effect for BEV ratings depending on which vehicle model rode in
fit <- addFitStats(polr(
    rating ~ periodAfter + car_etron +car_kona + car_leaf + car_nexo +
    car_priusprime + periodAfter*car_etron + periodAfter*car_kona + 
    periodAfter*car_leaf + periodAfter*car_nexo + periodAfter*car_priusprime,
    data = df_ratings_bev, Hess = TRUE))
printPolrSummaryTable(fit)

# Model 5: Effect for BEV ratings w/all main coefficients
fit <- addFitStats(polr(
    rating ~  periodAfter + fuelElec_only + fuelGas_only + fuel_bothanswers +
    subsidy_correct + neighborhasEV + car_etron +car_kona + car_leaf + 
    car_nexo + car_priusprime + periodAfter*fuelElec_only + 
    periodAfter*fuelGas_only + periodAfter*fuel_bothanswers + 
    periodAfter*subsidy_correct +  periodAfter*neighborhasEV + 
    periodAfter*car_etron + periodAfter*car_kona +
    periodAfter*car_leaf + periodAfter*car_nexo + periodAfter*car_priusprime,
    data = df_ratings_bev, Hess = TRUE))
printPolrSummaryTable(fit)

# Models in Table A3 in SI ----------------------------------------------------

# Model A1: Main effect for PHEV consideration rating
fit <- addFitStats(polr(
    rating ~ periodAfter,
    data = df_ratings_phev, Hess = TRUE))
printPolrSummaryTable(fit)

# Model A2a: Effect for PHEV ratings depending on if respondent had greater
#            knowledge about PEV refueling
fit <- addFitStats(polr(
    rating ~  periodAfter + fuelElec_only + fuelGas_only + fuel_bothanswers +
    periodAfter*fuelElec_only + periodAfter*fuelGas_only + 
    periodAfter*fuel_bothanswers,
    data = df_ratings_phev, Hess = TRUE))
printPolrSummaryTable(fit)

# Model A2b: Effect for PHEV ratings depending on if respondent had greater
#            knowledge about PEV subsidies
fit <- addFitStats(polr(
    rating ~  periodAfter + subsidy_correct + periodAfter*subsidy_correct,
    data = df_ratings_phev, Hess = TRUE))
printPolrSummaryTable(fit)

# Model A3: Effect for PEV ratings depending on if neighbor has PEV
fit <- addFitStats(polr(
    rating ~ periodAfter + neighborhasEV + periodAfter*neighborhasEV,
    data = df_ratings_phev, Hess = TRUE))
printPolrSummaryTable(fit)

# Model A4: Effect for PHEV ratings depending on which vehicle model rode in
fit <- addFitStats(polr(
    rating ~ periodAfter + car_etron +car_kona + car_leaf + car_nexo +
    car_priusprime + periodAfter*car_etron + periodAfter*car_kona + 
    periodAfter*car_leaf + periodAfter*car_nexo + periodAfter*car_priusprime,
    data = df_ratings_phev, Hess = TRUE))
printPolrSummaryTable(fit)

# Model A5: Effect for BEV ratings w/all main coefficients
fit <- addFitStats(polr(
    rating ~  periodAfter + fuelElec_only + fuelGas_only + fuel_bothanswers +
    subsidy_correct + neighborhasEV + car_kona + car_leaf + car_etron +
    car_nexo + car_priusprime + periodAfter*fuelElec_only + 
    periodAfter*fuelGas_only + periodAfter*fuel_bothanswers + 
    periodAfter*subsidy_correct +  periodAfter*neighborhasEV + 
    periodAfter*car_etron + periodAfter*car_kona +
    periodAfter*car_leaf + periodAfter*car_nexo + periodAfter*car_priusprime,
    data = df_ratings_phev, Hess = TRUE))
printPolrSummaryTable(fit)

# Models in Table A4 in SI ----------------------------------------------------

# Model A6a: Effect for BEV ratings depending on if multicar household
fit <- addFitStats(polr(
    rating ~ periodAfter + multicar + periodAfter*multicar,
    data = df_ratings_bev, Hess = TRUE))
printPolrSummaryTable(fit)

# Model A6b: Effect for PHEV ratings depending on if multicar household
fit <- addFitStats(polr(
    rating ~ periodAfter + multicar + periodAfter*multicar,
    data = df_ratings_phev, Hess = TRUE))
printPolrSummaryTable(fit)

# Models in Table A5 in SI ----------------------------------------------------

# Model A7a: Effect for BEV ratings depending on whether have at-home parking
fit <- addFitStats(polr(
    rating ~ periodAfter + BEV_parking + periodAfter*BEV_parking,
    data = df_ratings_bev, Hess = TRUE))
printPolrSummaryTable(fit)

# Model A7b: Effect for PHEV ratings depending on whether have at-home parking
fit <- addFitStats(polr(
    rating ~ periodAfter + BEV_parking + periodAfter*BEV_parking,
    data = df_ratings_phev, Hess = TRUE))
printPolrSummaryTable(fit)

# Models in Table A6 in SI ----------------------------------------------------

# Model A8a: Effect for BEV ratings depending on if rode in BEV, PHEV, or FCEV
fit <- addFitStats(polr(
    rating ~ periodAfter + drove_in_BEV + drove_in_PHEV + drove_in_FCEV + 
    periodAfter*drove_in_BEV + periodAfter*drove_in_PHEV + 
    periodAfter*drove_in_FCEV,
    data = df_ratings_bev, Hess = TRUE))
printPolrSummaryTable(fit)

# Model A8b: Effect for PHEV ratings depending on if rode in BEV, PHEV, or FCEV
fit <- addFitStats(polr(
    rating ~ periodAfter + drove_in_BEV + drove_in_PHEV + drove_in_FCEV + 
    periodAfter*drove_in_BEV + periodAfter*drove_in_PHEV + 
    periodAfter*drove_in_FCEV,
    data = df_ratings_phev, Hess = TRUE))
printPolrSummaryTable(fit)

# Models in Table A7 in SI ----------------------------------------------------

# Model A9a: Effect for BEV ratings depending on how many cars participant rode in
fit <- addFitStats(polr(
    rating ~ periodAfter + count_cars_driven + periodAfter*count_cars_driven,
    data = df_ratings_bev, Hess = TRUE))
printPolrSummaryTable(fit)

# Model A9b: Effect for BEV ratings depending on how many cars participant rode in
fit <- addFitStats(polr(
    rating ~ periodAfter + count_cars_driven + periodAfter*count_cars_driven,
    data = df_ratings_phev, Hess = TRUE))
printPolrSummaryTable(fit)

# Models in Table A8 in SI ----------------------------------------------------

# Models where the dependent variable is the "change" in the rating
# rather than the rating level

# Model A10a: Effect for BEV rating change
fit <- polr(
    ratingChange ~ fuelElec_only + fuelGas_only + fuel_bothanswers +
    subsidy_correct + neighborhasEV + car_etron + car_kona + car_leaf +
    car_nexo + car_priusprime,
    data = df_ratings_change_bev, Hess = TRUE)
printPolrSummaryTable(fit)

# Model A10b: Effect for PHEV rating change
fit <- polr(
    ratingChange ~ fuelElec_only + fuelGas_only + fuel_bothanswers +
    subsidy_correct + neighborhasEV + car_etron + car_kona + car_leaf +
    car_nexo + car_priusprime,
    data = df_ratings_change_phev, Hess = TRUE)
printPolrSummaryTable(fit)

# View results of any model ---------------------------------------------------

# View the stored summary table
fit$coef_summary

# Print a more detailed summary table
printPolrSummaryTable(fit)

# Preview predicted probabilities:
fit$probs %>%
    filter(stat == 'mean') %>%
    spread(period, p)

# Show percent change in before/after ratings:
percentChangeSummary(fit)

# Plot the predicted probabilities
probsPlotSingle(fit)
probsPlotMulti(fit)

