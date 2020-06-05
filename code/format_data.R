# --------------------------------------------------------------------------
# Setup
library(here)
library(tidyverse)
options(dplyr.width = Inf) # Option to preview all columns in a data frame
source(here('code', '0functions.R'))

# --------------------------------------------------------------------------
# Read in and format the raw DC data

df = read_csv(here('data', 'data_raw.csv'), na = c('', 'NA', '#N/A'))

# Rename variable names
namesOrig = names(df)
names(df) = c(
    'numCarsOwned', 'currentCarYear', 'currentCarMake',
    'timeToNextPurchase', 'considerPhev.before', 'considerBev.before',
    'recommendPhev.before', 'recommendBev.before', 'subsidy', 'fuelElec',
    'fuelGas', 'fuelNA', 'postalCode', 'currentCarModel', 'parking',
    'neighborsOwnEv', 'minorsIncluded', 'numMinors',  'dateCreated', 'time',
    'considerPhev.after', 'considerBev.after', 'recommendPhev.after',
    'recommendBev.after', 'vehiclesRodeIn', 'nextPurchaseBrand'
    )

# General formatting
df = df %>%
    mutate(
        currentCarAge = 2019 - currentCarYear,
        ownsCar =
            ifelse(is.na(numCarsOwned), NA,
            ifelse(numCarsOwned > 0, 1, 0))
    ) %>%
    dplyr::select(
        dateCreated, time, postalCode, minorsIncluded, numMinors, parking,
        neighborsOwnEv, timeToNextPurchase, nextPurchaseBrand, numCarsOwned,
        ownsCar,currentCarYear, currentCarMake, currentCarModel, currentCarAge,
        subsidy, fuelElec, fuelGas, vehiclesRodeIn,  considerPhev.before,
        considerBev.before, recommendPhev.before, recommendBev.before,
        considerPhev.after, considerBev.after, recommendPhev.after,
        recommendBev.after
    ) %>%

    # ----------------------------------------------------------------------
    # Fuel knowledge question formatting

    # Replace long strings with abbreviations
    mutate(
        fuelElec = str_replace(fuelElec,
                               'Plug-in Hybrid Electric Vehicle', 'PHEV'),
        fuelElec = str_replace(fuelElec,
                               'Pure Electric Vehicle', 'BEV'),
        fuelElec = str_replace(fuelElec,
                               'Hybrid Electric Vehicle', 'Hybrid'),
        fuelGas = str_replace(fuelGas,
                              'Plug-in Hybrid Electric Vehicle', 'PHEV'),
        fuelGas = str_replace(fuelGas,
                              'Pure Electric Vehicle', 'BEV'),
        fuelGas = str_replace(fuelGas,
                              'Hybrid Electric Vehicle', 'Hybrid')
    ) %>%
    # Create variables for all unique fueling question responses
    mutate(
        fuelElec_BEV  = ifelse(str_detect(fuelElec, 'BEV'), 1, 0),
        fuelElec_PHEV = ifelse(str_detect(fuelElec, 'PHEV'), 1, 0),
        fuelElec_HEV  = ifelse(str_detect(fuelElec, 'Hybrid'), 1, 0),
        fuelGas_BEV   = ifelse(str_detect(fuelGas,  'BEV'), 1, 0),
        fuelGas_PHEV  = ifelse(str_detect(fuelGas,  'PHEV'), 1, 0),
        fuelGas_HEV   = ifelse(str_detect(fuelGas,  'Hybrid'), 1, 0)
    ) %>%
    # Replace original fuelGas and fuelElec variables
    left_join(data.frame(
        fuelElec2     = c('HEV Only', 'PHEV Only', 'BEV Only',
                          'HEV & PHEV', 'HEV & BEV', 'PHEV & BEV',
                          'HEV, PHEV, & BEV'),
        fuelElec_BEV  = c(0, 0, 1, 0, 1, 1, 1),
        fuelElec_PHEV = c(0, 1, 0, 1, 0, 1, 1),
        fuelElec_HEV  = c(1, 0, 0, 1, 1, 0, 1)
    )) %>%
    left_join(data.frame(
        fuelGas2     = c('HEV Only', 'PHEV Only', 'BEV Only',
                          'HEV & PHEV', 'HEV & BEV', 'PHEV & BEV',
                          'HEV, PHEV, & BEV'),
        fuelGas_BEV  = c(0, 0, 1, 0, 1, 1, 1),
        fuelGas_PHEV = c(0, 1, 0, 1, 0, 1, 1),
        fuelGas_HEV  = c(1, 0, 0, 1, 1, 0, 1)
    )) %>%
    mutate(
        fuelElec = fuelElec2,
        fuelGas  = fuelGas2
    ) %>%
    select(
        -fuelElec2, -fuelGas2, -fuelElec_BEV, -fuelElec_PHEV, -fuelElec_HEV,
        -fuelGas_BEV, -fuelGas_PHEV, -fuelGas_HEV
    ) %>%
    # Create variables for whether they got the fueling questions correct
    mutate(
        fuelElec_correct = ifelse(fuelElec == 'PHEV & BEV', 1, 0),
        fuelGas_correct = ifelse(fuelGas == 'HEV & PHEV', 1, 0),
        fuel_bothanswers = fuelElec_correct + fuelGas_correct,
        fuel_bothanswers = ifelse(fuel_bothanswers == 2, 1, 0)
    ) %>%

    # ----------------------------------------------------------------------
    # Subsidy knowledge question formatting

    # Create variables for whether they got the subsidy question correct
    mutate(
        subsidy_correct = ifelse(subsidy == '7500', 1, 0),
        subsidy_unsure  = ifelse(subsidy == "I'm not sure", 1, 0)
    ) %>%

    # ----------------------------------------------------------------------
    # Before / after, consideration / recommend questions

    # Fix capitalization error in the 'considerPhev.after' variable
    mutate(
    considerPhev.after = str_replace(
        considerPhev.after, 'Maybe / Not Sure', 'Maybe / Not sure')
    ) %>%

    # ----------------------------------------------------------------------
    # Format parking

    mutate(
        p_driveway  = ifelse(str_detect(parking, 'Driveway / carport'), 1, 0),
        p_sharedgar = ifelse(str_detect(parking, 'Shared parking garage'), 1, 0),
        p_street = ifelse(str_detect(parking, 'Street parking'), 1, 0),
        p_SFHgar  = ifelse(str_detect(parking, 'Single-family garage'), 1, 0),
        p_sharedlot  = ifelse(str_detect(parking, 'Shared parking lot'), 1, 0),
        p_other  = ifelse(str_detect(parking, 'Other'), 1, 0),
        home_parking = ifelse( p_driveway == 1 | p_SFHgar == 1, 1, 0)
    ) %>%

    # ----------------------------------------------------------------------
    # Format vehicles rode in

    mutate(
        car_kona = ifelse(str_detect(vehiclesRodeIn, 'Hyundai Kona Electric'), 1, 0),
        car_leaf = ifelse(str_detect(vehiclesRodeIn, 'Nissan Leaf'), 1, 0),
        car_etron = ifelse(str_detect(vehiclesRodeIn, 'Audi e-tron'), 1, 0),
        car_nexo = ifelse(str_detect(vehiclesRodeIn, 'Hyundai Nexo Fuel Cell'), 1, 0),
        car_priusprime = ifelse(str_detect(vehiclesRodeIn, 'Toyota Prius Prime'), 1, 0),
        car_unknown = ifelse(str_detect(vehiclesRodeIn, 'unknown'), 1, 0),
        drove_in_BEV = ifelse( car_kona == 1 | car_leaf == 1 | car_etron ==1, 1, 0),
        drove_in_PHEV = ifelse(car_priusprime == 1, 1, 0),
        drove_in_FCEV = ifelse(car_nexo == 1, 1, 0),
        kinds_cars_driven = drove_in_BEV + drove_in_PHEV + drove_in_FCEV,
        count_cars_driven = car_kona + car_leaf + car_etron + car_nexo + car_priusprime,
        rodeMoreThanOne = ifelse(count_cars_driven > 1, 1, 0)
    ) %>%
    # ----------------------------------------------------------------------
    # Format neighbor owns EV

    mutate(
        neighborhasEV = ifelse(str_detect(neighborsOwnEv, 'Yes'), 1, 0)
    ) %>%

# ----------------------------------------------------------------------
# Format mutlicar as binary yes or no if number of cars owned >1

    mutate(
        multicar = ifelse(numCarsOwned > 1, 1, 0)
    )

    # ----------------------------------------------------------------------
# Format answers to knowledge questions as only
 df =  df %>%
    mutate(
        fuelElec_only = ifelse(fuel_bothanswers == 1, 0,
                               ifelse(fuelElec_correct == 1,1,0)),
        fuelGas_only = ifelse(fuel_bothanswers == 1, 0,
                               ifelse(fuelGas_correct == 1,1,0)))

# Order rating factors
ratingLevels <- c(
    "Definitely not", "Probably not", "Maybe / Not sure", "Probably yes",
    "Definitely yes")
df$considerPhev.before <- factor(df$considerPhev.before,
    levels = ratingLevels, ordered = TRUE)
df$considerBev.before <- factor(df$considerBev.before,
    levels = ratingLevels, ordered = TRUE)
df$recommendPhev.before <- factor(df$recommendPhev.before,
    levels = ratingLevels, ordered = TRUE)
df$recommendBev.before <- factor(df$recommendBev.before,
    levels = ratingLevels, ordered = TRUE)
df$considerPhev.after <- factor(df$considerPhev.after,
    levels = ratingLevels, ordered = TRUE)
df$considerBev.after <- factor(df$considerBev.after,
    levels = ratingLevels, ordered = TRUE)
df$recommendPhev.after <- factor(df$recommendPhev.after,
    levels = ratingLevels, ordered = TRUE)
df$recommendBev.after <- factor(df$recommendBev.after,
    levels = ratingLevels, ordered = TRUE)

# ----------------------------------------------------------------------
# Format change in response
df = df %>%
    mutate(
        considerBev.change = ifelse(
            considerBev.after > considerBev.before, 'Positive', ifelse(
            considerBev.after < considerBev.before, 'Negative', 'No change')),
        considerPhev.change = ifelse(
            considerPhev.after > considerPhev.before, 'Positive', ifelse(
            considerPhev.after < considerPhev.before, 'Negative', 'No change')),
        considerBev.change = as.factor(considerBev.change),
        considerPhev.change = as.factor(considerPhev.change))

# ----------------------------------------------------------------------

# Save as both csv and Rds to keep ordered levels
saveRDS(df, here('data', 'data_processed.Rds'))

# ----------------------------------------------------------------------
# Save another version with NAs removed for consideration questions

df_complete = df %>%
    filter(
        !is.na(considerBev.after),
        !is.na(considerPhev.before),
        !is.na(considerPhev.after),
        !is.na(considerBev.before),
        !is.na(considerBev.after))
saveRDS(df_complete, here('data', 'data_processed_complete.Rds'))

