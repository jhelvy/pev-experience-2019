# This file is used to fit the models in SI Section A8. This uses a different 
# model specification from the rest of the models in the main text and SI. 
# In this specification, the outcome states are the *change* in the ratings 
# before and after the PEV experience (as opposed to the ratings themselves). 

# Load libraries, data, and make summary tables of the data
source(here::here('code', '0setup.R'))

# Models in Table A8 in SI ----------------------------------------------------

# Models where the dependent variable is the "change" in the rating
# rather than the rating level

# Model A10a: Effect for BEV rating change
fit <- addFitStats(polr(
    ratingChange ~ fuelElec_only + fuelGas_only + fuel_bothanswers +
    subsidy_correct + neighborhasEV + car_etron + car_kona + car_leaf +
    car_nexo + car_priusprime,
    data = df_ratings_change_bev, Hess = TRUE),
    numDraws = 10^4, changeModel = T)
formattedSummaryTable(fit)

# Model A10b: Effect for PHEV rating change
fit <- addFitStats(polr(
    ratingChange ~ fuelElec_only + fuelGas_only + fuel_bothanswers +
    subsidy_correct + neighborhasEV + car_etron + car_kona + car_leaf +
    car_nexo + car_priusprime,
    data = df_ratings_change_phev, Hess = TRUE),
    numDraws = 10^4, changeModel = T)
formattedSummaryTable(fit)

# View results of any model ---------------------------------------------------

# View the stored summary table of the coefficients
coefSummaryTable(fit)

# Print a more detailed summary table with better formatting
formattedSummaryTable(fit)

# Preview predicted probabilities:
fit$probs %>%
    filter(stat == 'mean') %>%
    spread(rating, p) 
