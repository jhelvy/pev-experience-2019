# Setup ---------------------------------------------------------------------

source(file.path('code', '0startup.R'))
plotColors <- c('grey80', 'sienna')
sankeyColors <- c('steelblue', 'sienna', 'grey80')

# Compute 2018 fraction of PEV sales (BEV + PHEV)
pevSales2018 <- evSales  %>% 
    group_by(year) %>% 
    summarise(sales = sum(sales)) %>% 
    filter(year == 2018)
allSales2018 <- 17274250 # source: https://www.marklines.com/en/statistics/flash_sales/salesfig_usa_2018
# Market share:
100*(pevSales2018$sales / allSales2018) 


# Figure 1 --------------------------------------------------------------------

# Import data
evSales <- read_csv(file.path('data', 'evSalesData.csv')) %>%
    filter(date < ymd('2019-09-01')) # Latest date of scraped data

# Make the figure
figure1 <- evSales %>%
    filter(category == 'bev') %>%
    mutate(
        sales = sales / 10^3,
        category =
            ifelse(
                vehicle == 'Tesla Model 3', 'Tesla Model 3',
            ifelse(
                (brand == 'Tesla') & (vehicle != 'Model 3'),
                'Tesla Model S & X', 'Non-Tesla')),
        category = fct_relevel(
            category, 'Tesla Model 3', 'Tesla Model S & X', 'Non-Tesla')) %>%
    group_by(category, date) %>%
    summarise(sales = sum(sales)) %>%
    ggplot(aes(x = date, y = sales)) +
    geom_col(aes(fill = category)) +
    scale_x_date(
        limits = ymd(c('2011-01-01', '2019-09-01')),
        date_breaks = '1 year',
        date_labels = "%Y") +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
    scale_fill_manual(values = c('#80B1D3', '#175279', '#FF3B3F')) +
    theme_minimal_hgrid(font_family = 'Roboto Condensed') +
    theme(
        legend.position = c(0.8, .95),
        legend.background = element_rect(
            fill = 'white', color = 'white', size = 3),
        legend.justification = c("right", "top")) +
    labs(x       = NULL,
         y       = 'Sales (Thousands)',
         title   = 'U.S. Monthly Sales of Battery Electric Vehicles',
         fill    = 'Vehicle Model',
         caption = 'Data sources: hybridcars.com & insideEVs.com')

ggsave(file.path('figures', 'figure1.png'),
       figure1, width = 10, heigh = 5)

# Figure 2 --------------------------------------------------------------------

# Create the plot data frame
bevSankeyDf <- bevSummary %>%
    dplyr::select(before, after, n, ratingColor) %>%
    gather_set_data(1:2) %>%
    mutate(
        x = str_to_title(x),
        x = fct_relevel(x, levels = c('Before', 'After')),
        y = fct_recode(y,
                       'Def.\nyes' = 'Definitely yes',
                       'Prob.\nyes' = 'Probably yes',
                       'Maybe /\nNot sure' = 'Maybe / Not sure',
                       'Def.\nnot' = 'Definitely not',
                       'Prob.\nnot' = 'Probably not'),
        y = fct_relevel(y, levels = rev(levels(y))),
        ratingColor = fct_relevel(ratingColor, levels = c(
            'Positive', 'Negative', 'No change')))

# Make the plot
spacing <- 320 # Approximate spacing between y rating categories
bevSankeyLabelsBefore <-
    bevSankeyDf %>%
    group_by(x, before) %>%
    summarise(n = sum(n)) %>%
    mutate(
        spacer = c(0, cumsum(rep(spacing, 4))),
        cumn = cumsum(n),
        diff = cumn - lag(cumn),
        diff = ifelse(is.na(diff), 0, diff),
        y = lag(cumn) + (diff / 2) + spacer,
        y = ifelse(is.na(y), cumn / 2, y)) %>%
    filter(x == 'Before') %>%
    mutate(
        percent = round(n / sum(n), 2),
        percent = paste0(100*percent, '%'))
bevSankeyLabelsAfter <- bevSankeyDf %>%
    group_by(x, after) %>%
    summarise(n = sum(n)) %>%
    mutate(
        spacer = c(0, cumsum(rep(spacing, 4))),
        cumn = cumsum(n),
        diff = cumn - lag(cumn),
        diff = ifelse(is.na(diff), 0, diff),
        y = lag(cumn) + (diff / 2) + spacer,
        y = ifelse(is.na(y), cumn / 2, y)) %>%
    filter(x == 'After') %>%
    mutate(
        percent = round(n / sum(n), 2),
        # manually correct rounding of 52% to 51% for maybe category
        percent = ifelse(percent == 0.52, 0.51, percent), #
        percent = paste0(100*percent, '%'))
figure2 <- bevSankeyDf %>%
    ggplot(aes(x = x, id = id, split = y, value = n)) +
    geom_parallel_sets(aes(fill = ratingColor), axis.width = 0.13, alpha=0.7) +
    geom_parallel_sets_axes(axis.width = 0.1, fill='grey80', color='grey80') +
    geom_parallel_sets_labels(
        color = 'black',
        size  = 10/.pt,
        angle = 90) +
    geom_text(data = bevSankeyLabelsBefore,
              aes(label = percent, x = x, y = y),
              inherit.aes = FALSE,
              nudge_x     = -0.08,
              hjust       = 1,
              family      = 'Fira Sans Condensed') +
    geom_text(data = bevSankeyLabelsAfter,
              aes(label = percent, x = x, y = y),
              inherit.aes = FALSE,
              nudge_x     = 0.08,
              hjust       = 0,
              family      = 'Fira Sans Condensed') +
    scale_fill_manual(values = sankeyColors) +
    scale_y_continuous(breaks = NULL) +
    scale_x_discrete(
        name = NULL,
        expand = c(0, 0.18)) +
    theme_half_open(font_family = 'Fira Sans Condensed') +
    theme(legend.position = c(0.985, 0.87),
          plot.margin = margin(0.1, 4.0, 0.1, 0.1, "cm"),
          axis.title.y = element_text(vjust = -0.5),
          axis.line = element_blank(),
          axis.ticks = element_blank()) +
    labs(
        fill  = 'Change in\nbefore / after rating',
        y = paste0('Percentage of respondents (n = ',
                   sum(bevSankeyLabelsBefore$n), ')'))

ggsave(file.path('figures', 'figure2.png'),
       figure2, width = 9, height = 6)

# Figure 3 --------------------------------------------------------------------

figure3 <- probsPlotSingle(fit_bev(split = FALSE)) +
    theme(legend.position = c(0.9, 0.85),
          legend.background = element_blank(),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          axis.title.x = element_text(vjust = -0.5))

ggsave(file.path('figures', 'figure3.png'),
       figure3, width = 5, height = 4)

# Figure 4 --------------------------------------------------------------------

# Barplot of subsidy knowledge question responses
knowledge_barplot_subsidy <- df_complete %>%
    mutate(
        facetName = paste0(
            'Do you know the current maximum\n',
            'subsidy available from the federal\n',
            'government for purchasing a PEV?'),
        subsidy = as.factor(subsidy),
        subsidy = fct_recode(subsidy,
            '$1,000'  = '1000',
            '$2,500'  = '2500',
            '$5,000'  = '5000',
            '$7,500'  = '7500',
            '$10,000' = '10000'),
        subsidy = fct_relevel(subsidy, c(
            '$1,000', '$2,500', '$5,000', '$7,500', '$10,000',
            "I'm not sure"))) %>%
    ggplot(aes(x = subsidy)) +
    geom_bar(aes(fill = as.factor(subsidy_correct)), width = 0.7) +
    coord_flip() +
    facet_wrap(vars(facetName)) +
    scale_fill_manual(values=c('gray', 'forestgreen')) +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
    theme_bars() +
    labs(x    = 'Response options',
         y    = 'Number of Respondents',
         fill = 'Correct\nResponse')

# Barplot of fuel knowledge question responses
knowledge_barplot_fuels <- df_complete %>%
    dplyr::select(fuelElec, fuelGas) %>%
    gather(fuel, response, fuelElec:fuelGas) %>%
    separate(fuel, into=c('drop', 'fuel'), sep='fuel') %>%
    dplyr::select(-drop) %>%
    mutate(
        correct = ifelse(
            fuel=='Elec' & response=='PHEV & BEV', 1, ifelse(
                fuel=='Gas' & response=='HEV & PHEV', 1, 0))) %>%
    left_join(data.frame(
        fuel = c('Gas', 'Elec'),
        fuelQuestion = c(
            'Please select which vehicle(s) can\nrun on gasoline:',
            'Please select which vehicle(s) can\nbe plugged-in:'))) %>%
    ggplot(aes(x=response)) +
    geom_bar(aes(fill=as.factor(correct)), width = 0.7) +
    facet_wrap(~fuelQuestion) +
    coord_flip() +
    scale_fill_manual(values=c('gray', 'forestgreen')) +
    scale_x_discrete(limits=rev(c(
        'HEV Only', 'PHEV Only', 'BEV Only', 'HEV & PHEV', 'HEV & BEV',
        'PHEV & BEV', 'HEV, PHEV, & BEV'))) +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
    theme_bars() +
    labs(x='Response options',
         y='Number of Respondents',
         fill='Correct\nResponse')

# Create multi panel plot of knowedge question results
figure4 <- plot_grid(
    knowledge_barplot_fuels,
    knowledge_barplot_subsidy,
    labels = c('A', 'B'), nrow = 1, rel_widths = c(1, 0.52))

ggsave(file.path('figures', 'figure4.png'),
       figure4, width = 10, height = 3.5)

# Figure 5 --------------------------------------------------------------------

# Effect for BEV ratings depending on if respondent had greater knowledge
# about BEV refueling
bev_knowledge_fuels_plot <- probsPlotMulti(
    fit_bev_knowledge_fuels(split = FALSE),
    factorNames = c(
        'Neither', 'Both', 'Plug-in only', 'Gas only'),
    xlab = 'Fuel knowledge questions correctly answered',
    l_position = c(0.8, 1.3))

# Effect for BEV ratings depending on if respondent had greater knowledge
# about PEV subsidies
bev_knowledge_subsidy_plot <- probsPlotMulti(
    fit_bev_knowledge_subsidies(split = FALSE),
    factorNames = c('No', 'Yes'),
    xlab = 'Subsidy knowledge question correctly answered',
    l_position = c(0.8, 1.25)) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
          axis.title.x = element_text(vjust = -0.5))

# Create multi panel plot of knowedge question results
figure5 <- plot_grid(
    bev_knowledge_fuels_plot + theme(legend.position = 'none'),
    bev_knowledge_subsidy_plot + theme(legend.position = 'none'),
    labels = c('A', 'B'), ncol = 1, rel_heights = c(1, 0.9))
legend <- get_legend(bev_knowledge_fuels_plot +
                         theme(legend.position = c(0.99, 1)))
figure5 <- plot_grid(figure5, legend,
                     ncol = 1, rel_heights = c(1, 0.06))

ggsave(file.path('figures', 'figure5.png'),
       figure5, width = 8, height = 8)

# Figure 6 --------------------------------------------------------------------

figure6 <- probsPlotMulti(
    fit_bev_neighborEV(split = FALSE),
    factorNames = c('No', 'Yes'),
    xlab        = 'Neighbor with EV',
    l_position  = c(0.99, 1.23)) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
          axis.title.x = element_text(vjust = -0.5))

ggsave(file.path('figures', 'figure6.png'),
       figure6, width = 8, height = 4)

# Figure 7 --------------------------------------------------------------------

figure7 <- probsPlotMulti(
    fit_bev_carModels(split = FALSE),
    factorNames = c('Baseline', 'e-tron', 'Kona', 'Leaf', 'Nexo', 'Prius Prime'),
    xlab        = 'Car Model',
    l_position  = c(0.99, 1.3))

ggsave(file.path('figures', 'figure7.png'),
       figure7, width = 8, height = 4)

# Figure A1 -------------------------------------------------------------------

# Create the plot data frame
phevSankeyDf <- phevSummary %>%
    dplyr::select(before, after, n, ratingColor) %>%
    gather_set_data(1:2) %>%
    mutate(
        x = str_to_title(x),
        x = fct_relevel(x, levels = c('Before', 'After')),
        y = fct_recode(y,
                       'Def.\nyes' = 'Definitely yes',
                       'Prob.\nyes' = 'Probably yes',
                       'Maybe /\nNot sure' = 'Maybe / Not sure',
                       'Def.\nnot' = 'Definitely not',
                       'Prob.\nnot' = 'Probably not'),
        y = fct_relevel(y, levels = rev(levels(y))),
        ratingColor = fct_relevel(ratingColor, levels = c(
            'Positive', 'Negative', 'No change')))

# Make the plot
spacing <- 320 # Approximate spacing between y rating categories
phevSankeyLabelsBefore <-
    phevSankeyDf %>%
    group_by(x, before) %>%
    summarise(n = sum(n)) %>%
    mutate(
        spacer = c(0, cumsum(rep(spacing, 4))),
        cumn = cumsum(n),
        diff = cumn - lag(cumn),
        diff = ifelse(is.na(diff), 0, diff),
        y = lag(cumn) + (diff / 2) + spacer,
        y = ifelse(is.na(y), cumn / 2, y)) %>%
    filter(x == 'Before') %>%
    mutate(
        percent = round(n / sum(n), 2),
        percent = paste0(100*percent, '%'))
phevSankeyLabelsAfter <- phevSankeyDf %>%
    group_by(x, after) %>%
    summarise(n = sum(n)) %>%
    mutate(
        spacer = c(0, cumsum(rep(spacing, 4))),
        cumn = cumsum(n),
        diff = cumn - lag(cumn),
        diff = ifelse(is.na(diff), 0, diff),
        y = lag(cumn) + (diff / 2) + spacer,
        y = ifelse(is.na(y), cumn / 2, y)) %>%
    filter(x == 'After') %>%
    mutate(
        percent = round(n / sum(n), 2),
        # manually correct rounding of 52% to 51% for maybe category
        percent = ifelse(percent == 0.52, 0.51, percent), #
        percent = paste0(100*percent, '%'))
figureA1 <- phevSankeyDf %>%
    ggplot(aes(x = x, id = id, split = y, value = n)) +
    geom_parallel_sets(aes(fill = ratingColor), axis.width = 0.13, alpha=0.7) +
    geom_parallel_sets_axes(axis.width = 0.1, fill='grey80', color='grey80') +
    geom_parallel_sets_labels(
        color = 'black',
        size  = 10/.pt,
        angle = 90) +
    geom_text(data = phevSankeyLabelsBefore,
              aes(label = percent, x = x, y = y),
              inherit.aes = FALSE,
              nudge_x     = -0.08,
              hjust       = 1,
              family      = 'Fira Sans Condensed') +
    geom_text(data = phevSankeyLabelsAfter,
              aes(label = percent, x = x, y = y),
              inherit.aes = FALSE,
              nudge_x     = 0.08,
              hjust       = 0,
              family      = 'Fira Sans Condensed') +
    scale_fill_manual(values = sankeyColors) +
    scale_y_continuous(breaks = NULL) +
    scale_x_discrete(
        name = NULL,
        expand = c(0, 0.18)) +
    theme_half_open(font_family = 'Fira Sans Condensed') +
    theme(legend.position = c(0.985, 0.87),
          plot.margin = margin(0.1, 4.0, 0.1, 0.1, "cm"),
          axis.title.y = element_text(vjust = -0.5),
          axis.line = element_blank(),
          axis.ticks = element_blank()) +
    labs(
        fill  = 'Change in\nbefore / after rating',
        y = paste0('Percentage of respondents (n = ',
                   sum(phevSankeyLabelsBefore$n), ')'))

ggsave(file.path('figures', 'figureA1.png'),
       figureA1, width = 9, height = 6)
