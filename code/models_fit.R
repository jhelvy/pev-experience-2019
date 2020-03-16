# This file is used to fit models. All models are defined in the "0functions.R"
# file in this directory. They are defined as functions so that you can
# choose whether to fit the model using the full set of observations
# (split = FALSE) or a randomly assigned "training" set with a "testing" set
# (split = TRUE) held out to check predictive accuracy.

# The first thing that is done in each of these functions is read-in
# the "df_complete" data set, format it for modeling, and then create the
# testing and training data frames. If split = FALSE, then the data is NOT
# split into training and testing sets - the full dataset is used in estimation.
# If split = TRUE, you will get slightly different results every time you run a
# model because the exact data used to train and test will vary.
# This is done intentionally so that models can be fit and compared using
# different subsets of the data. The default setting is to use 70% of the
# data for training and 30% for testing, but this can be modified by setting
# "frac" equal to a value between 0 and 1.
#
# The main object created after running any model is called "fit". This is
# over-written every time you run any of the following models.
# Use code at the bottom of this file to view the results of any "fit" model.

# Setup -----------------------------------------------------------------------

source(file.path('code', '0startup.R'))

# Models in Table 5 in main text ----------------------------------------------

# Model 1: Main effect for BEV consideration rating
fit <- fit_bev(split = FALSE, frac = 0.7)

# Model 2a: Effect for BEV ratings depending on if respondent had greater
#           knowledge about PEV refueling
fit <- fit_bev_knowledge_fuels(split = FALSE, frac = 0.7)

# Model 2b: Effect for BEV ratings depending on if respondent had greater
#           knowledge about PEV subsidies
fit <- fit_bev_knowledge_subsidies(split = FALSE, frac = 0.7)

# Model 3: Effect for BEV ratings depending on if neighbor has PEV
fit <- fit_bev_neighborEV(split = FALSE, frac = 0.7)

# Model 4: Effect for BEV ratings depending on which vehicle model rode in
fit <- fit_bev_carModels(split = FALSE, frac = 0.7)

# Models in Table A3 in SI ----------------------------------------------------

# Model A1: Main effect for PHEV consideration rating
fit <- fit_phev(split = FALSE, frac = 0.7)

# Model A2a: Effect for PHEV ratings depending on if respondent had greater
#            knowledge about PEV refueling
fit <- fit_phev_knowledge_fuels(split = FALSE, frac = 0.7)

# Model A2b: Effect for PHEV ratings depending on if respondent had greater
#            knowledge about PEV subsidies
fit <- fit_phev_knowledge_subsidies(split = FALSE, frac = 0.7)

# Model A3: Effect for PEV ratings depending on if neighbor has PEV
fit <- fit_phev_neighborEV(split = FALSE, frac = 0.7)

# Model A4: Effect for PHEV ratings depending on which vehicle model rode in
fit <- fit_phev_carModels(split = FALSE, frac = 0.7)

# Models in Table A4 in SI ----------------------------------------------------

# Model A5a: Effect for BEV ratings depending on if multicar household
fit <- fit_bev_multicar(split = FALSE, frac = 0.7)

# Model A5b: Effect for PHEV ratings depending on if multicar household
fit <- fit_phev_multicar(split = FALSE, frac = 0.7)

# Models in Table A5 in SI ----------------------------------------------------

# Model A6a: Effect for BEV ratings depending on whether have at-home parking
fit <- fit_bev_parking(split = FALSE, frac = 0.7)

# Model A6b: Effect for PHEV ratings depending on whether have at-home parking
fit <- fit_phev_parking(split = FALSE, frac = 0.7)

# Models in Table A6 in SI ----------------------------------------------------

# Model A7a: Effect for BEV ratings depending on if rode in BEV or PHEV
fit <- fit_bev_carType(split = FALSE, frac = 0.7)

# Model A7b: Effect for PHEV ratings depending on if rode in BEV or PHEV
fit <- fit_phev_carType(split = FALSE, frac = 0.7)

# View results ----------------------------------------------------------------

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

