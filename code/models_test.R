# --------------------------------------------------------------------------
# Test to see how well each fit model predicts the actual choices made
# using the bev_test and phev_test data frames

# For each test, run multiple iterations of loading the data, fitting the
# model, and computing prediction accuracy. You should compare the results
# after running multiple iterations to make sure that the choice of the
# training and test data doesn't affect the results

# The main object created after running any model is called "test". This is
# over-written every time you run any of the following model tests.
# Use code at the bottom of this file to view the results of any "test".

# Setup ---------------------------------------------------------------------

source(file.path('code', '0startup.R'))

# Models in Table 5 in main text ----------------------------------------------

# Model 1: Main effect for BEV consideration rating
test <- predictionAccuracy(
    modelFunc = fit_bev,
    frac      = 0.7,
    niter     = 100)

# Model 2a: Effect for BEV ratings depending on if respondent had greater
#           knowledge about PEV refueling
test <- predictionAccuracy(
    modelFunc = fit_bev_knowledge_fuels,
    frac      = 0.7,
    niter     = 100)

# Model 2b: Effect for BEV ratings depending on if respondent had greater
#           knowledge about PEV subsidies
test <- predictionAccuracy(
    modelFunc = fit_bev_knowledge_subsidies,
    frac      = 0.7,
    niter     = 100)

# Model 3: Effect for BEV ratings depending on if neighbor has PEV
test <- predictionAccuracy(
    modelFunc = fit_bev_neighborEV,
    frac      = 0.7,
    niter     = 100)

# Model 4: Effect for BEV ratings depending on which vehicle model rode in
test <- predictionAccuracy(
    modelFunc = fit_bev_carModels,
    frac      = 0.7,
    niter     = 100)

# Models in Table A3 in SI ----------------------------------------------------

# Model A1: Main effect for PHEV consideration rating
test <- predictionAccuracy(
    modelFunc = fit_phev,
    frac      = 0.7,
    niter     = 100)

# Model A2a: Effect for PHEV ratings depending on if respondent had greater
#            knowledge about PEV refueling
test <- predictionAccuracy(
    modelFunc = fit_phev_knowledge_fuels,
    frac      = 0.7,
    niter     = 100)

# Model A2b: Effect for PHEV ratings depending on if respondent had greater
#            knowledge about PEV subsidies
test <- predictionAccuracy(
    modelFunc = fit_phev_knowledge_subsidies,
    frac      = 0.7,
    niter     = 100)

# Model A3: Effect for PEV ratings depending on if neighbor has PEV
test <- predictionAccuracy(
    modelFunc = fit_phev_neighborEV,
    frac      = 0.7,
    niter     = 100)

# Model A4: Effect for PHEV ratings depending on which vehicle model rode in
test <- predictionAccuracy(
    modelFunc = fit_phev_carModels,
    frac      = 0.7,
    niter     = 100)

# Models in Table A4 in SI ----------------------------------------------------

# Model A5a: Effect for BEV ratings depending on if multicar household
test <- predictionAccuracy(
    modelFunc = fit_bev_multicar,
    frac      = 0.7,
    niter     = 100)

# Model A5b: Effect for PHEV ratings depending on if multicar household
test <- predictionAccuracy(
    modelFunc = fit_phev_multicar,
    frac      = 0.7,
    niter     = 100)

# Models in Table A5 in SI ----------------------------------------------------

# Model A6a: Effect for BEV ratings depending on whether have at-home parking
test <- predictionAccuracy(
    modelFunc = fit_bev_parking,
    frac      = 0.7,
    niter     = 100)

# Model A6b: Effect for PHEV ratings depending on whether have at-home parking
test <- predictionAccuracy(
    modelFunc = fit_phev_parking,
    frac      = 0.7,
    niter     = 100)

# Models in Table A6 in SI ----------------------------------------------------

# Model A7a: Effect for BEV ratings depending on if rode in BEV or PHEV
test <- predictionAccuracy(
    modelFunc = fit_bev_carType,
    frac      = 0.7,
    niter     = 100)

# Model A7b: Effect for PHEV ratings depending on if rode in BEV or PHEV
test <- predictionAccuracy(
    modelFunc = fit_phev_carType,
    frac      = 0.7,
    niter     = 100)

# Print results ---------------------------------------------------------------

test %>%
    group_by(period) %>%
    summarise(mean = mean(p), sd = sd(p))
