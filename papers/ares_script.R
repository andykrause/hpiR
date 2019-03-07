#***************************************************************************************************
#
#   Analytical Script for ARES paper
#
#***************************************************************************************************

### Preliminary Commands ---------------------------------------------------------------------------

 ## Load Libraries

  library(hpiR)
  library(tidyverse)

 ## Load Data

  data(seattle_sales)
  data(ex_sales)

 ## Load Custom Functions

  source(file.path(getwd(), 'papers', 'ares_script_functions.r'))

### Create Data ------------------------------------------------------------------------------------

  # Hedonic Data
  hed_df <- hedCreateTrans(trans_df = seattle_sales,
                           prop_id = 'pinx',
                           trans_id = 'sale_id',
                           price = 'sale_price',
                           date = 'sale_date',
                           periodicity = 'monthly')

  # Repeat Transaction Data
  rt_df <- rtCreateTrans(trans_df = seattle_sales,
                         prop_id = 'pinx',
                         trans_id = 'sale_id',
                         price = 'sale_price',
                         date = 'sale_date',
                         periodicity = 'monthly')

### Random Forest Example --------------------------------------------------------------------------

  # Estimate Model
  rf_model <- ranger::ranger(price ~ use_type + beds + baths + bldg_grade + tot_sf + latitude +
                               longitude + trans_period,
                             data = hed_df,
                             num.trees = 200,
                             seed = 1)

  # Example 1
  rf_1 <- rfSimulate(rf_obj = rf_model,
                     rf_df = hed_df,
                     sim_count = 1,
                     seed = 1)$coefficients %>%
    dplyr::mutate(index = 100 * (1+coefficient)) %>%
    dplyr::select(period = time,
                  value = index)

  # Example 2
  rf_2 <- rfSimulate(rf_obj = rf_model,
                     rf_df = hed_df,
                     sim_count = 1,
                     seed = 2)$coefficients %>%
    dplyr::mutate(index = 100 * (1+coefficient)) %>%
    dplyr::select(period = time,
                  value = index)

  # Example 3
  rf_3 <- rfSimulate(rf_obj = rf_model,
                     rf_df = hed_df,
                     sim_count = 1,
                     seed = 3)$coefficients %>%
    dplyr::mutate(index = 100 * (1+coefficient)) %>%
    dplyr::select(period = time,
                  value = index)

  # Example 4
  rf_4 <- rfSimulate(rf_obj = rf_model,
                     rf_df = hed_df,
                     sim_count = 1,
                     seed = 4)$coefficients %>%
    dplyr::mutate(index = 100 * (1+coefficient)) %>%
    dplyr::select(period = time,
                  value = index)

  # Example 5
  rf_5 <- rfSimulate(rf_obj = rf_model,
                     rf_df = hed_df,
                     sim_count = 1,
                     seed = 5)$coefficients %>%
    dplyr::mutate(index = 100 * (1+coefficient)) %>%
    dplyr::select(period = time,
                  value = index)


  #### Convert this to Partial....

  # Examples 1 to 5
  rf_15 <- dplyr::bind_rows(list(rf_1, rf_2, rf_3, rf_4, rf_5)) %>%
    dplyr::group_by(period) %>%
    dplyr::summarize(value = mean(value))

  rf_1to5 <- dplyr::bind_rows(list(rf_1 %>% dplyr::mutate(example = 'Ex. 1'),
                                   rf_2 %>% dplyr::mutate(example = 'Ex. 2'),
                                   rf_3 %>% dplyr::mutate(example = 'Ex. 3'),
                                   rf_4 %>% dplyr::mutate(example = 'Ex. 4'),
                                   rf_5 %>% dplyr::mutate(example = 'Ex. 5')))

  gg_1 <- ggplot() +
    geom_line(data = rf_1,
              aes(x = period, y = value)) +
    scale_color_manual(name = 'Model', values = 'gray10') +
    theme(legend.position = 'bottom') +
    ylab('Index Value\n') +
    xlab('\nTime') +
    scale_x_continuous(breaks = c(seq(1,85,12)), labels = 2010:2017) +
    ggtitle('Example Random Forest-Derived HPI')

  saveRDS(gg_1, file.path(getwd(), 'papers', 'ex1plot.RDS'))

  gg_1to5 <- ggplot() +
    geom_line(data = rf_1to5,
              aes(x = period, y = value, group = example, color = example)) +
    scale_color_manual(name = 'Model', values = rep('gray10', 5)) +
    theme(legend.position = 'bottom') +
    ylab('Index Value\n') +
    xlab('\nTime') +
    scale_x_continuous(breaks = c(seq(1,85,12)), labels = 2010:2017) +
    ggtitle('Example Random Forest-Derived HPI')

  saveRDS(gg_1to5, file.path(getwd(), 'papers', 'ex15plot.RDS'))

  gg_1to5a <- ggplot() +
    geom_line(data = rf_1to5,
              aes(x = period, y = value, group = example, color = example)) +
    scale_color_manual(name = 'Model', values = rep('gray70', 5)) +
    theme(legend.position = 'bottom') +
    ylab('Index Value\n') +
    xlab('\nTime') +
    scale_x_continuous(breaks = c(seq(1,85,12)), labels = 2010:2017) +
    geom_line(data = rf_15, aes(x = period, y = value), lwd = 1.5, color = 2) +
    ggtitle('Example Random Forest-Derived HPI')

 saveRDS(gg_1to5a, file.path(getwd(), 'papers', 'ex15aplot.RDS'))

## 100 version

 rf_100 <- rfSimulate(rf_obj = rf_model,
                      rf_df = hed_df,
                      sim_count = 100,
                      seed = 100)$coefficients %>%
   dplyr::mutate(index = 100 * (1+coefficient)) %>%
   dplyr::select(period = time,
                 value = index)

### Compare to RT/HED ------------------------------------------------------------------------------

 rt_hpi <- rtIndex(trans_df = rt_df,
                   estimator = 'robust',
                   log_dep = TRUE,
                   trim_model = FALSE,
                   max_period = 84,
                   smooth = TRUE)
 he_hpi <- hedIndex(trans_df = hed_df,
                    estimator = 'robust',
                    log_dep = TRUE,
                    dep_var = 'price',
                    ind_var = c('use_type', 'lot_sf', 'tot_sf', 'beds', 'baths', 'eff_age', 'area'),
                    trim_model = FALSE,
                    max_period = 84,
                    smooth = TRUE)

 rti_df <- data.frame(period = rt_hpi$index$period,
                      value = as.numeric(rt_hpi$index$value),
                      model = 'Repeat Sales')
 hei_df <- data.frame(period = he_hpi$index$period,
                      value = as.numeric(he_hpi$index$value),
                      model = 'Hedonic')
 rhi_df <- rbind(rti_df, hei_df, rf_100 %>% dplyr::mutate(model = 'Random Forest (100)'))

 gg_rhr <- ggplot() +
   geom_line(data = rhi_df,
             aes(x = period, y = value, group = model, color = model),
             lwd = 1.5) +
   scale_color_manual(name = 'Model', values = c('blue', 'purple', 'red')) +
   theme(legend.position = 'bottom') +
   ylab('Index Value\n') +
   xlab('\nTime') +
   scale_x_continuous(breaks = c(seq(1,85,12)), labels = 2010:2017) +
   ggtitle('Comparison of HPIs')

 saveRDS(gg_rhr, file.path(getwd(), 'papers', 'rhrplot.RDS'))

### Simple Comparisons -----------------------------------------------------------------------------

 ## Repeat Transaction Example
 rt_hpi <- rtIndex(trans_df = rt_df,
                   estimator = 'robust',
                   log_dep = TRUE,
                   trim_model = FALSE,
                   max_period = 84,
                   smooth = TRUE)

 ## Hedonic sales Example
 he_hpi <- hedIndex(trans_df = hed_df,
                    estimator = 'robust',
                    log_dep = TRUE,
                    dep_var = 'price',
                    ind_var = c('use_type', 'lot_sf', 'tot_sf', 'beds', 'baths', 'eff_age', 'area'),
                    trim_model = FALSE,
                    max_period = 84,
                    smooth = TRUE)

 # Random Forest
 rfs_hpi <- rfIndex(trans_df = hed_df,
                   estimator = 'sim',
                   dep_var = 'price',
                   ind_var = c('use_type', 'lot_sf', 'tot_sf', 'beds', 'baths', 'eff_age', 'area',
                               'latitude', 'longitude'),
                   max_period = 84,
                   smooth = FALSE,
                   ntrees = 100,
                   sim_count = 100)

 rfp_hpi <- rfIndex(trans_df = hed_df,
                    estimator = 'pdp',
                    dep_var = 'price',
                    ind_var = c('use_type', 'lot_sf', 'tot_sf', 'beds', 'baths', 'eff_age', 'area',
                                'latitude', 'longitude'),
                    max_period = 84,
                    smooth = FALSE,
                    ntrees = 100,
                    sim_count = 100)

### Comparisons ------------------------------------------------------------------------------------

 full_comp <- threeWayComparison(data_obj = seattle_sales,
                                 ntrees = 100,
                                 sim_count = 100)


 ff <- summarizeComp(list(full_comp))

 saveRDS(ff$summ, file.path(getwd(), 'papers', 'full_summ.RDS'))
 saveRDS(ff, file.path(getwd(), 'papers', 'full_comp.RDS'))

## Do these same results hold (Over a small geo area period)

  geo_df <- seattle_sales %>% dplyr::filter(!area %in% 23)
  #geo_df <- seattle_sales %>% dplyr::filter(area %in% 7)

  geo_ <- suppressWarnings(
    purrr::map(.x = split(geo_df, geo_df$area),
                          .f = threeWayComparison,
                          hed_var = c('use_type', 'lot_sf', 'tot_sf', 'beds', 'baths', 'eff_age'),
                          lm_recover = TRUE))

  geo_comp <- summarizeComp(geo_)
  saveRDS(geo_comp$summ, file.path(getwd(), 'papers', 'geo_summ.RDS'))
  saveRDS(geo_comp, file.path(getwd(), 'papers', 'geo_comp.RDS'))


## Do these same results hold (Over a tiny geo area period)
  time_data <- list(seattle_sales %>% dplyr::filter(sale_date <= '2011-12-31'),
                    seattle_sales %>% dplyr::filter(sale_date <= '2012-12-31' &
                                                      sale_date >= '2011-01-01'),
                    seattle_sales %>% dplyr::filter(sale_date <= '2013-12-31' &
                                                      sale_date >= '2012-01-01'),
                    seattle_sales %>% dplyr::filter(sale_date <= '2014-12-31' &
                                                      sale_date >= '2013-01-01'),
                    seattle_sales %>% dplyr::filter(sale_date <= '2015-12-31' &
                                                      sale_date >= '2014-01-01'),
                    seattle_sales %>% dplyr::filter(sale_date <= '2016-12-31' &
                                                      sale_date >= '2015-01-01'))

  time_ <- purrr::map(.x = time_data,
                      .f = threeWayComparison,
                      train_period = 12,
                      max_period = 24,
                      lm_recover = TRUE)

  time_summ <- summarizeComp(time_)
  saveRDS(time_summ$summ, file.path(getwd(), 'papers', 'time_summ.RDS'))
  saveRDS(time_summ, file.path(getwd(), 'papers', 'time_comp.RDS'))

  ## Errors by time (check oversmoothing....are errors bigger in periods with larger increases)

### Random Forest Hyperparmater tests --------------------------------------------------------------
#
#   ## Small Area
# exgrid <- purrr::map2(.x = rep(c(10, 50, 100, 200), 4),
#                       .y = c(rep(10, 4), rep(50, 4), rep(100, 4), rep(200, 4)),
#                       .f = testRfHpi,
#                       ex_sales = ex_sales)
#
#   save.image("C:/Users/andyx/Desktop/ares.RData")
#
#  # All Seattle
#   allgrid <- purrr::map2(.x = rep(c(10, 50, 100, 200), 4),
#                         .y = c(rep(10, 4), rep(50, 4), rep(100, 4), rep(200, 4)),
#                         .f = testRfHpi,
#                         ex_sales = seattle_sales)
#
#   # Area X
#   save.image("C:/Users/andyx/Desktop/ares.RData")
#
#   x <- 43
#   xgrid <- purrr::map2(.x = rep(c(10, 50, 100, 200), 4),
#                          .y = c(rep(10, 4), rep(50, 4), rep(100, 4), rep(200, 4)),
#                          .f = testRfHpi,
#                          ex_sales = seattle_sales  %>% dplyr::filter(area == x))
#   # Area Y
#   y <- 77
#   ygrid <- purrr::map2(.x = rep(c(10, 50, 100, 200), 4),
#                          .y = c(rep(10, 4), rep(50, 4), rep(100, 4), rep(200, 4)),
#                          .f = testRfHpi,
#                          ex_sales = seattle_sales  %>% dplyr::filter(area == y))
#
#   # Area Z
#   z <- 22
#   zgrid <- purrr::map2(.x = rep(c(10, 50, 100, 200), 4),
#                          .y = c(rep(10, 4), rep(50, 4), rep(100, 4), rep(200, 4)),
#                          .f = testRfHpi,
#                          ex_sales = seattle_sales  %>% dplyr::filter(area == z))
#
#   save.image("C:/Users/andyx/Desktop/ares.RData")
#
# ###############
#
#
#
