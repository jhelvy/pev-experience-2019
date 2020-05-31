# --------------------------------------------------------------------------
# Load the data

# Use the RDS files instead of csv so that rating levels are
# correctly ordered
df <- readRDS(here::here('data', 'autoshow.Rds'))
df_complete <- readRDS(here::here('data', 'autoshow_complete.Rds'))

# --------------------------------------------------------------------------
# Summary of BEV & PHEV consideration ratings

bevSummary <- df_complete %>%
    count(considerBev.before, considerBev.after) %>%
    dplyr::select(
        before = considerBev.before, after = considerBev.after, n) %>%
    group_by(before) %>%
    mutate(percent = round(100*(n / sum(n)), 2)) %>%
    # Add color label and title factors
    mutate(ratingColor =
        ifelse(after > before, 'Positive',
        ifelse(after < before, 'Negative', 'No change'))) %>%
    ungroup()

phevSummary <- df_complete %>%
    count(considerPhev.before, considerPhev.after) %>%
    dplyr::select(
        before = considerPhev.before, after = considerPhev.after, n) %>%
    group_by(before) %>%
    mutate(percent = round(100*(n / sum(n)), 2)) %>%
    # Add color label and title factors
    mutate(ratingColor =
        ifelse(after > before, 'Positive',
        ifelse(after < before, 'Negative', 'No change'))) %>%
    ungroup()

bevSummary$type <- 'BEV'
phevSummary$type <- 'PHEV'
bothSummary <- bind_rows(bevSummary, phevSummary)

# --------------------------------------------------------------------------
# Summary of consideration ratings

# Main summary
main <- df_complete %>%
    count(considerBev.before, considerBev.after,
          considerPhev.before, considerPhev.after) %>%
    gather(key = 'type', value = 'rating', -n) %>%
    separate(type, into = c('type', 'period'), sep = '\\.') %>%
    separate(type, into = c('drop', 'type'), sep = 'consider') %>%
    mutate(
        type   = str_to_upper(type),
        period = str_to_title(period),
        rating = factor(rating, ordered = TRUE, levels = ratings)) %>%
    dplyr::select(type, period, rating, n) %>%
    # Count rating by type and period
    group_by(type, period, rating) %>%
    summarise(count = sum(n)) %>%
    # Compute percentage breakdown of counts by type and period
    group_by(type, period) %>%
    mutate(
        percent     = round(100*(count / sum(count)), 2),
        cum_percent = cumsum(percent)) %>%
    ungroup() %>%
    # Relevel factors for plotting
    mutate(
        period = fct_relevel(period, c('Before', 'After')),
        rating = fct_relevel(rating, rev(levels(ratings)))) %>%
    arrange(type, period)

# Summaries based on consumer responses
charging <- countSummary(df_complete, BEV_parking) %>%
    mutate(BEV_parking = ifelse(BEV_parking == 1, 'Yes', 'No'))
ownsCar <- countSummary(df_complete, ownsCar) %>%
    mutate(ownsCar = ifelse(ownsCar == 1, 'Yes', 'No'))
neighborEV <- countSummary(df_complete, neighborhasEV) %>%
    mutate(neighborhasEV = ifelse(neighborhasEV == 1, 'Yes', 'No'))

# Summaries based on type of car rode in
typeBEV <- countSummarySub(df_complete, drove_in_BEV, 'BEV')
typePHEV <- countSummarySub(df_complete, drove_in_PHEV, 'PHEV')
typeFCEV <- countSummarySub(df_complete, drove_in_FCEV, 'FCEV')
carType <- bind_rows(typeBEV, typePHEV, typeFCEV)

# Summaries based on number of car rode in
carMulti <- countSummary(df_complete, rodeMoreThanOne) %>%
    mutate(rodeMoreThanOne = ifelse(rodeMoreThanOne == 1, 'Yes', 'No'))

# Summaries based on car model rode in
kona <- countSummarySub(df_complete, car_kona, 'Kona')
leaf <- countSummarySub(df_complete, car_leaf, 'Leaf')
etron <- countSummarySub(df_complete, car_etron, 'eTron')
nexo <- countSummarySub(df_complete, car_nexo, 'Nexo')
carModel <- bind_rows(kona, leaf, etron, nexo)
# priusprime <- countSummary(df_complete, car_priusprime)

# Summaries based on knowledge
subsidy <- countSummary(df_complete, subsidy_correct) %>%
    mutate(subsidy_correct = ifelse(subsidy_correct == 1, 'Yes', 'No'))
fuelGas <- countSummary(df_complete, fuelGas_correct) %>%
    mutate(fuelGas_correct = ifelse(fuelGas_correct == 1, 'Yes', 'No'))
fuelElec <- countSummary(df_complete, fuelElec_correct) %>%
    mutate(fuelElec_correct = ifelse(fuelElec_correct == 1, 'Yes', 'No'))
fuelBoth <- countSummary(df_complete, fuel_bothanswers) %>%
    mutate(fuel_bothanswers = ifelse(fuel_bothanswers == 1, 'Yes', 'No'))

