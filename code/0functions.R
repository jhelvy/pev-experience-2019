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
            subsidy_correct, neighborhasEV, home_parking, drove_in_BEV,
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
            subsidy_correct, neighborhasEV, home_parking, drove_in_BEV,
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
# alpha = The intercept coefficients
# beta  = The "period after" coefficient
# gamma = Coefficients for all other covariates
# delta = Coefficients for the interaction of the "period after" effect and
#         all other covariates
#
# The baseline model used is:
#
# Before rating: logit(P < j) = alpha
# After rating:  logit(P < j) = alpha - beta
#
# Other covariates are denoted by gamma, e.g. gamma1 represents the effect
# for those who got the knowledge questions correct. In this case, the model
# would be:
#
# Before rating w/out knowledge: logit(P < j) = alpha1
# Before rating w/knowledge:     logit(P < j) = alpha1 - gamma1
# After rating w/out knowledge:  logit(P < j) = alpha1 - beta1
# After rating w/knowledge:      logit(P < j) = alpha1 - beta1 - gamma1 - delta1
#
# For the second rating category, everything would be the same, except
# you would use alpha2 instead of alpha1. For the third rating category, you
# would add alpha3, and so on.

addFitStats <- function(fit, numDraws = 10^4, changeModel = FALSE) {
    # Add draws of the coefficients to the 'fit' object
    fit$draws <- getUncertaintyDraws(fit, numDraws)
    names(fit$draws) <- getCoefNames(fit)
    # Use the draws to add computed probabilities to the object
    if (changeModel) {
        fit$probs <- computeChangeProbsSummary(fit, numDraws)
    } else {
        fit$probs <- computeProbsSummary(fit, numDraws)
    }
    # Add coefficient summary table
    fit$coef_summary <- coefSummaryTable(fit)
    return(fit)
}

getUncertaintyDraws = function(fit, numDraws) {
    varcov <- abs(solve(fit$Hessian))
    coefs  <- c(fit$coefficients, fit$zeta)
    draws  <- data.frame(mvrnorm(numDraws, coefs, varcov))
    return(draws)
}

# The stratey to compute the probabilities with uncertainty is to take a lot of
# draws of each model parameter, and then compute the probability of each
# possible rating outcome using those draws. The draws allow us to pass through
# parameter uncertainy

computeChangeProbsSummary <- function(fit, numDraws = 10^4) {
    nBreaks <- length(fit$zeta)
    draws_alpha <- fit$draws[names(fit$zeta)]
    # For each gamma parameter, compute the probabilities by
    # subtracting the additional gamma and delta parameter draws
    gammaNames <- getGammaCoefNames(fit)
    result <- list()
    if (length(gammaNames) > 0) {
        for (i in 1:length(gammaNames)) {
            gammaName <- gammaNames[i]
            gamma <- fit$coefficients[gammaName]
            draws_gamma <- repDraws(fit$draws[gammaName], nBreaks)
            temp <- draws_alpha - draws_gamma
            probs_temp <- computeProbsCI(temp, ratingChangeLevels)
            probs_temp$case <- gammaName
            result[[i]] <- probs_temp
        }
    }
    return(do.call(bind_rows, result))
}

computeProbsSummary <- function(fit, numDraws = 10^4) {
    nBreaks <- length(fit$zeta)
    draws_alpha <- fit$draws[names(fit$zeta)]
    draws_beta <- repDraws(fit$draws['periodAfter'], nBreaks)
    # Compute the probabilities for the baseline before case
    draws_before <- draws_alpha
    probs_before <- computeProbsCI(draws_before, ratingLevels)
    probs_before$case <- 'Before-baseline'
    # Compute the probabilities for the baseline after case
    draws_after <- draws_alpha - draws_beta
    probs_after <- computeProbsCI(draws_after, ratingLevels)
    probs_after$case <- 'After-baseline'
    # For each remaining gamma parameter, compute the probabilities by
    # subtracting the additional gamma and delta parameter draws
    result <- bind_rows(probs_before, probs_after)
    gammaNames <- getGammaCoefNames(fit)
    deltaNames <- getDeltaCoefNames(fit)
    if (length(gammaNames) > 0) {
        for (gammaName in gammaNames) {
            deltaName <- deltaNames[str_detect(deltaNames, gammaName)]
            gamma <- fit$coefficients[gammaName]
            delta <- fit$coefficients[deltaName]
            draws_gamma <- repDraws(fit$draws[gammaName], nBreaks)
            draws_delta <- repDraws(fit$draws[deltaName], nBreaks)
            temp_before <- draws_before - draws_gamma
            temp_after <- draws_after - draws_gamma - draws_delta
            probs_temp_before <- computeProbsCI(temp_before, ratingLevels)
            probs_temp_after <- computeProbsCI(temp_after, ratingLevels)
            probs_temp_before$case <- paste('Before', gammaName, sep = '-')
            probs_temp_after$case <- paste('After', gammaName, sep = '-')
            result <- bind_rows(result, probs_temp_before, probs_temp_after)
        }
    }
    result <- result %>%
        separate(case, into = c('period', 'case'), sep = '-')
    return(result)
}

repDraws <- function(draws, n) {
    return(matrix(rep(as.matrix(draws), n), ncol = n, byrow = FALSE))
}

getCoefNames <- function(fit) {
    return(names(c(fit$coefficients, fit$zeta)))
}

getGammaCoefNames <- function(fit) {
    gammaNames <- data.frame(v = getCoefNames(fit)) %>%
        filter(str_detect(v, '\\|') == FALSE) %>%
        filter(str_detect(v, ':') == FALSE) %>%
        filter(str_detect(v, 'periodAfter') == FALSE)
    return(as.character(gammaNames$v))
}

getDeltaCoefNames <- function(fit) {
    deltaNames <- data.frame(v = getCoefNames(fit)) %>%
        filter(str_detect(v, ':'))
    return(as.character(deltaNames$v))
}

# Computes the probabilities of choosing each rating with uncertainty
computeProbsCI <- function(draws, levels) {
    prob_draws <- getProbDraws(draws, levels)
    probsCI <- as_tibble(t(apply(prob_draws, 2, ci)))
    result <- probsCI %>%
        dplyr::mutate(rating = levels) %>%
        dplyr::select(rating, mean, lower, upper) %>%
        gather(key = 'stat', value = 'p', mean:upper)
    return(result)
}

getProbDraws <- function(draws, levels) {
    prob_draws <- exp(draws) / (1 + exp(draws))
    prob_draws_shifted <- cbind(
        prob_draws[,2:ncol(draws)], unit = matrix(1, nrow = nrow(draws)))
    prob_draws <- cbind(prob_draws[,1], prob_draws_shifted - prob_draws)
    names(prob_draws) <- levels
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
    title <- paste0(
        'Predicted probability of choosing rating\n',
        'before & after experience')
    subtitle <- paste0(
        'Number of observations = ', scales::comma(fit$n))
    plot <- fit$probs %>%
    spread(stat, p) %>%
    mutate(
        period = fct_relevel(period, c('Before', 'After')),
        rating = fct_recode(rating,
            'Definitely\nyes'   = 'Definitely yes',
            'Probably\nyes'     = 'Probably yes',
            'Maybe /\nNot sure' = 'Maybe / Not sure',
            'Definitely\nnot'   = 'Definitely not',
            'Probably\nnot'     = 'Probably not')) %>%
        ggplot(aes(x = rating, y = mean, ymin = lower, ymax = upper,
                   fill = period)) +
        geom_col(width = 0.7, position = position_dodge2(preserve = "single")) +
        geom_errorbar(width = 0.2, position = position_dodge(width = 0.7)) +
        scale_fill_manual(values = plotColors) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
        theme_barplot() +
        geom_hline(yintercept = 0) +
        labs(x = 'Rating',
             y = 'Probability of choosing rating',
             fill = 'Period',
             title = title,
             subtitle = subtitle)
    return(plot)
}

probsPlotMulti <- function(fit,
        factorNames = levels(as.factor(fit$probs$case)),
        xlab = 'Case',
        l_position = c(0.8, 1.15)) {
    plotColors <- c('grey80', 'sienna')
    title <- 'Predicted probability of choosing rating before & after experience'
    subtitle <- paste0(
        'Number of observations = ', scales::comma(fit$n))
    plot <- fit$probs %>%
        spread(stat, p) %>%
        mutate(period = fct_relevel(period, c('Before', 'After'))) %>%
        left_join(data.frame(
            case = levels(as.factor(fit$probs$case)),
            case_name = factor(factorNames, ordered = T,
                               levels = factorNames))) %>%
        ggplot(aes(x = case_name, y = mean, ymin = lower, ymax = upper,
                   fill = period)) +
        geom_col(width = 0.7, position = position_dodge2(preserve = "single")) +
        geom_errorbar(width = 0.2, position = position_dodge(width = 0.7)) +
        facet_wrap(vars(rating), nrow = 1) +
        scale_fill_manual(values = plotColors,
                          guide = guide_legend(direction = "horizontal")) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
        theme_barplot() +
        geom_hline(yintercept = 0) +
        panel_border() +
        theme(legend.position = l_position) +
        labs(
            x = xlab,
            y = 'Probability of choosing rating',
            title = title,
            subtitle = subtitle,
            fill = 'Period: ')
    return(plot)
}
