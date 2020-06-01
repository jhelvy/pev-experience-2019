# DATA PREP -------------------------------------------------------------------

# Global vars -----------------------------------------------------------------

ratingLevels <- c(
    "Definitely not", "Probably not", "Maybe / Not sure", "Probably yes",
    "Definitely yes")
ratingLevels <- factor(ratingLevels, ordered = TRUE, levels = ratingLevels)

ratingChangeLevels <- c(
    "Negative", "No change", "Positive")
ratingChangeLevels <- factor(ratingChangeLevels, ordered = TRUE,
                       levels = ratingChangeLevels)

# Summarize ratings data ------------------------------------------------------

countSummary <- function(df, var) {
    counts <- df_complete %>%
        count(considerBev.before, considerBev.after,
              considerPhev.before, considerPhev.after, {{var}}) %>%
        gather(key = 'type', value = 'rating', -n, -{{var}}) %>%
        separate(type, into = c('type', 'period'), sep = '\\.') %>%
        separate(type, into = c('drop', 'type'), sep = 'consider') %>%
        mutate(
            type   = str_to_upper(type),
            period = str_to_title(period),
            rating = factor(rating, ordered = TRUE, levels = ratingLevels)) %>%
        dplyr::select(type, period, rating, {{var}}, n) %>%
        # Count rating by type, period, and var
        group_by(type, period, rating, {{var}}) %>%
        summarise(count = sum(n)) %>%
        # Compute percentage breakdown of counts by type, period, and var
        group_by(type, period, {{var}}) %>%
        mutate(
            percent     = round(100*(count / sum(count)), 2),
            cum_percent = cumsum(percent)) %>%
        ungroup() %>%
        # Re-level factors for plotting
        mutate(
            period = fct_relevel(period, c('Before', 'After')),
            rating = fct_relevel(rating, rev(levels(ratingLevels)))) %>%
        arrange(type, period, {{var}})
    return(counts)
}

# Summarizes only a subset of the results based on the var provided
countSummarySub <- function(df, var, name) {
    result <- countSummary(df, {{var}}) %>%
        filter({{var}} == 1) %>%
        dplyr::select(-{{var}}) %>%
        mutate(var = name)
    return(result)
}

# Data prep for model fitting -------------------------------------------------

prepRatingsData <- function(df) {
    # Reshape the data for the consideration score before and after
    df <- df %>%
        gather(key = 'type', value = 'rating',
               considerPhev.before:considerBev.after) %>%
        filter(str_detect(type, 'recommend') == FALSE) %>%
        separate(type, into = c('type', 'period'), sep = '\\.') %>%
        separate(type, into = c('drop', 'type'), sep = 'consider') %>%
        dplyr::select(
            fuelElec_only, fuelGas_only, fuel_bothanswers,
            subsidy_correct, neighborhasEV, BEV_parking, drove_in_BEV,
            drove_in_PHEV, drove_in_FCEV, car_kona, car_leaf, car_etron,
            car_nexo, car_priusprime, multicar, count_cars_driven,
            type, period, rating) %>%
        mutate(
            type   = str_to_upper(type),
            period = str_to_title(period),
            period = factor(period, levels = c('Before', 'After')),
            periodAfter = ifelse(period == 'After', 1, 0),
            rating = factor(rating, ordered = TRUE, levels = ratingLevels))
    df_bev <- df %>%
        filter(type == 'BEV') %>%
        mutate(id = row_number())
    df_phev <- df %>%
        filter(type == 'PHEV') %>%
        mutate(id = row_number())
    return(list(df_bev = df_bev, df_phev = df_phev))
}

prepRatingsChangeData <- function(df) {
    df <- df %>%
        gather(key = 'type', value = 'ratingChange',
               considerBev.change:considerPhev.change) %>%
        mutate(type = ifelse(type == 'considerBev.change', 'BEV', 'PHEV')) %>%
        dplyr::select(
            fuelElec_only, fuelGas_only, fuel_bothanswers,
            subsidy_correct, neighborhasEV, BEV_parking, drove_in_BEV,
            drove_in_PHEV, drove_in_FCEV, car_kona, car_leaf, car_etron,
            car_nexo, car_priusprime, multicar, count_cars_driven,
            type, ratingChange) %>%
            mutate(
                type   = str_to_upper(type),
                ratingChange = factor(ratingChange, ordered = TRUE,
                                      levels = ratingChangeLevels))
    df_bev <- df %>%
        filter(type == 'BEV') %>%
        mutate(id = row_number())
    df_phev <- df %>%
        filter(type == 'PHEV') %>%
        mutate(id = row_number())
    return(list(df_bev = df_bev, df_phev = df_phev))
}




# SUMMARIZE RESULTS -----------------------------------------------------------

# Get summary tables from fit models ------------------------------------------

coefSummaryTable <- function(fit) {
    summary_table <- as.data.frame(coef(summary(fit)))
    pval <- pnorm(abs(summary_table[, "t value"]), lower.tail = FALSE)*2
    signifCodes <- getSignifCodes(pval)
    summary_table <- cbind(
        round(summary_table, 5), "p value" = round(pval, 4), ' ' = signifCodes)
    return(summary_table)
}

formattedSummaryTable <- function(fit) {
    # Get numbers to print
    printTable <- coefSummaryTable(fit) %>%
        mutate(
            Coef = round(Value, 3),
            `Std. Error` = paste('(',
                                 round(`Std. Error`, 3), ')', sep = '')) %>%
        dplyr::select(Coef, `Std. Error`, ` `)
    row.names(printTable) <- getCoefNames(fit)
    # Print the numbers
    print(printTable)
    cat('---', '\n', sep='')
    cat("Signif. codes:  '***'=0.001, '**'=0.01, '*'=0.05, '.'=0.1, ' '=1", '\n',
        sep='')
    cat('---', '\n', sep='')
    cat('N obs:' , fit$n, '\n')
    cat('Log-likelihood:' , logLik(fit)[1], '\n')
}

getSignifCodes = function(pVal) {
    signif = rep('', length(pVal))
    signif[which(pVal <= 0.001)] <- '***'
    signif[which(pVal >  0.001 & pVal <= 0.01)] <- '**'
    signif[which(pVal >  0.01  & pVal <= 0.05)] <- '*'
    signif[which(pVal >  0.05  & pVal <= 0.1)] <- '.'
    return(signif)
}

# Compute rating probabilities ------------------------------------------------
#
# Notation:
# 'alpha' refers to all the "after" coefficient
# 'beta' refers to all the intercept coefficients
# 'delta' refers to all the other covariates
#
# The model used is:
#
# Before rating: logit(P < 1) = beta1
# After rating: logit(P < 1) = beta1 - alpha1
#
# Other covariates are denoted by delta, e.g. delta1 represents the effect
# for those who got the knowledge questions correct. In this case, the model
# would be:
#
# Before rating w/out knowledge: logit(P < 1) = beta1
# Before rating w/knowledge: logit(P < 1) = beta1 - delta1
# After rating w/out knowledge: logit(P < 1) = beta1 - alpha1
# After rating w/knowledge: logit(P < 1) = beta1 - alpha1 - delta1
#
# For the second rating category, everything would be the same, except
# you would add beta2 to the model. For the third rating category, you
# would add beta3, and so on.

addFitStats <- function(fit, numDraws = 10^4) {
    # Add draws of the coefficients to the 'fit' object
    fit$draws <- getUncertaintyDraws(fit, numDraws)
    names(fit$draws) <- getCoefNames(fit)
    # Use the draws to add computed probabilities to the object
    fit$probs <- computeProbsSummary(fit, numDraws)
    # Add coefficient summary table
    fit$coef_summary <- coefSummaryTable(fit)
    return(fit)
}

getCoefNames <- function(fit) {
    return(names(c(fit$coefficients, fit$zeta)))
}

# The stratey to compute the probabilities is to take a lot of draws
# of each model parameter, and then compute the probability of each possible
# rating outcome using those draws. The draws allow us to pass through
# parameter uncertainy
computeProbsSummary <- function(fit, numDraws = 10^4) {
    nBreaks <- length(fit$zeta)
    draws_alpha <- repDraws(fit$draws[names(fit$coefficients)[1]], nBreaks)
    draws_beta <- fit$draws[names(fit$zeta)]
    # Compute the probabilities for the baseline before case
    draws_before <- draws_beta
    probs_before <- computeProbsCI(draws_before)
    probs_before$case <- 'Before-baseline'
    # Compute the probabilities for the baseline after case
    draws_after <- draws_beta - draws_alpha
    probs_after <- computeProbsCI(draws_after)
    probs_after$case <- 'After-baseline'
    # For each remaining delta parameter, compute the probabilities by
    # subtracting the additional alpha parameter draws
    result <- bind_rows(probs_before, probs_after)
    if (length(fit$coefficients) > 1) {
        for (i in 2:length(fit$coefficients)) {
            delta <- fit$coefficients[i]
            draws_delta <- repDraws(fit$draws[names(delta)], nBreaks)
            temp_before <- draws_before - draws_delta
            temp_after <- draws_after - draws_delta
            probs_temp_before <- computeProbsCI(temp_before)
            probs_temp_after <- computeProbsCI(temp_after)
            probs_temp_before$case <- paste('Before', names(delta), sep = '-')
            probs_temp_after$case <- paste('After', names(delta), sep = '-')
            result <- bind_rows(result, probs_temp_before, probs_temp_after)
        }
    }
    result <- result %>%
        separate(case, into = c('period', 'case'), sep = '-')
    return(result)
}

getUncertaintyDraws = function(fit, numDraws) {
    varcov <- abs(solve(fit$Hessian))
    coefs  <- c(fit$coefficients, fit$zeta)
    draws  <- data.frame(mvrnorm(numDraws, coefs, varcov))
    return(draws)
}

repDraws <- function(draws, n) {
    return(matrix(rep(as.matrix(draws), n), ncol = n, byrow = FALSE))
}

# Computes the probabilities of choosing each rating with uncertainty
computeProbsCI <- function(draws) {
    prob_draws <- getProbDraws(draws)
    probsCI <- as_tibble(t(apply(prob_draws, 2, ci)))
    result <- probsCI %>%
        dplyr::mutate(rating = ratingLevels) %>%
        dplyr::select(rating, mean, lower, upper) %>%
        gather(key = 'stat', value = 'p', mean:upper)
    return(result)
}

getProbDraws <- function(draws) {
    prob_draws <- exp(draws) / (1 + exp(draws))
    prob_draws_shifted <- cbind(
        prob_draws[,2:ncol(draws)], unit = matrix(1, nrow = nrow(draws)))
    prob_draws <- cbind(prob_draws[,1], prob_draws_shifted - prob_draws)
    names(prob_draws) <- ratingLevels
    return(prob_draws)
}

# This function returns the probabilities without uncertainty
getProbs <- function(coefs) {
    probs <- exp(coefs) / (1 + exp(coefs))
    probs_shifted <- c(probs[2:length(coefs)], 1)
    probs <- c(probs[1], probs_shifted - probs)
    names(probs) <- ratingLevels
    return(probs)
}

# Returns a confidence interval from a vector of data
ci = function(data, alpha = 0.025) {
    B <- mean(data, na.rm = T)
    L <- quantile(data, alpha, na.rm = T)
    U <- quantile(data, 1-alpha, na.rm = T)
    ests <- c(B,L,U)
    names(ests) <- c('mean', 'lower', 'upper')
    return(ests)
}

percentChangeSummary <- function(fit) {
    result <- fit$probs %>%
        filter(stat == 'mean') %>%
        spread(period, p) %>%
        mutate(percent_change = round(100*(After - Before) / Before))
    return(result)
}



# PLOT RESULTS ----------------------------------------------------------------

theme_bars <- function() {
    theme_bw(base_family = 'Fira Sans Condensed',
             base_size = 12) +
        theme(
            strip.text.x = element_text(hjust = 0),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            legend.position = 'none')
}

theme_barplot <- function() {
    theme_minimal(base_family = 'Fira Sans Condensed') +
        theme(
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
            legend.position = c(0.3, .95),
            legend.background = element_rect(
                fill = 'white', color = 'black'),
            legend.justification = c("right", "top"))
}

probsPlotSingle <- function(fit) {
    plotColors <- c('grey80', 'sienna')
    plotTitle <- paste0(
        'Predicted probability of choosing rating\nbefore & after ',
        'experience (N = ', fit$nresp, ')')
    plot <- fit$probs %>%
    spread(stat, p) %>%
    mutate(
        period = fct_relevel(period, c('Before', 'After')),
        rating = fct_recode(rating,
                            'Definitely\nyes' = 'Definitely yes',
                            'Probably\nyes' = 'Probably yes',
                            'Maybe /\nNot sure' = 'Maybe / Not sure',
                            'Definitely\nnot' = 'Definitely not',
                            'Probably\nnot' = 'Probably not')) %>%
        ggplot(aes(x = rating, y = mean, ymin = lower, ymax = upper,
                   fill = period)) +
        geom_bar(stat = 'identity', width = 0.7,
                 position = position_dodge2(preserve = "single")) +
        geom_errorbar(width = 0.2, position = position_dodge(width = 0.7)) +
        scale_fill_manual(values = plotColors) +
        scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
        theme_barplot() +
        geom_hline(yintercept = 0) +
        labs(
            x = 'Rating',
            y = 'Probability of choosing rating',
            fill = 'Period',
            title = plotTitle)
    return(plot)
}

probsPlotMulti <- function(
    fit,
    factorNames = levels(as.factor(fit$probs$case)),
    xlab = 'Case',
    l_position = c(0.8, 1.15)) {
    plotColors <- c('grey80', 'sienna')
    plotTitle <- paste0(
        'Predicted probability of choosing rating\nbefore & after ',
        'experience (N = ', fit$nresp, ')')
    plot <- fit$probs %>%
        spread(stat, p) %>%
        mutate(period = fct_relevel(period, c('Before', 'After'))) %>%
        left_join(data.frame(
            case = levels(as.factor(fit$probs$case)),
            case_name = factor(factorNames, ordered = T,
                               levels = factorNames))) %>%
        ggplot(aes(x = case_name, y = mean, ymin = lower, ymax = upper,
                   fill = period)) +
        geom_bar(stat = 'identity', width = 0.7,
                 position = position_dodge2(preserve = "single")) +
        geom_errorbar(width = 0.2, position = position_dodge(width = 0.7)) +
        facet_wrap(vars(rating), nrow = 1) +
        scale_fill_manual(values = plotColors,
                          guide = guide_legend(direction = "horizontal")) +
        scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
        theme_barplot() +
        geom_hline(yintercept = 0) +
        panel_border() +
        theme(legend.position = l_position) +
        labs(
            x = xlab,
            y = 'Probability of choosing rating',
            title = plotTitle,
            fill = 'Period: ')
    return(plot)
}
