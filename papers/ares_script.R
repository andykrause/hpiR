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
                         data = 'sale_date',
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

## 500 version

 rf_500 <- rfSimulate(rf_obj = rf_model,
                      rf_df = hed_df,
                      sim_count = 500,
                      seed = 500)$coefficients %>%
   dplyr::mutate(index = 100 * (1+coefficient)) %>%
   dplyr::select(period = time,
                 value = index)

### Compare to RT/HED ------------------------------------------------------------------------------

 rt_hpi <- rtIndex(trans_df = sales_rtdf,
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
 rhi_df <- rbind(rti_df, hei_df, rf_500 %>% dplyr::mutate(model = 'Random Forest (500)'))

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






### Comparisons ------------------------------------------------------------------------------------

  ## Small Area
exgrid <- purrr::map2(.x = rep(c(10, 50, 100, 200), 4),
                      .y = c(rep(10, 4), rep(50, 4), rep(100, 4), rep(200, 4)),
                      .f = testRfHpi,
                      ex_sales = ex_sales)

  save.image("C:/Users/andyx/Desktop/ares.RData")

 # All Seattle
  allgrid <- purrr::map2(.x = rep(c(10, 50, 100, 200), 4),
                        .y = c(rep(10, 4), rep(50, 4), rep(100, 4), rep(200, 4)),
                        .f = testRfHpi,
                        ex_sales = seattle_sales)

  # Area X
  save.image("C:/Users/andyx/Desktop/ares.RData")

  x <- 43
  xgrid <- purrr::map2(.x = rep(c(10, 50, 100, 200), 4),
                         .y = c(rep(10, 4), rep(50, 4), rep(100, 4), rep(200, 4)),
                         .f = testRfHpi,
                         ex_sales = seattle_sales  %>% dplyr::filter(area == x))
  # Area Y
  y <- 77
  ygrid <- purrr::map2(.x = rep(c(10, 50, 100, 200), 4),
                         .y = c(rep(10, 4), rep(50, 4), rep(100, 4), rep(200, 4)),
                         .f = testRfHpi,
                         ex_sales = seattle_sales  %>% dplyr::filter(area == y))

  # Area Z
  z <- 22
  zgrid <- purrr::map2(.x = rep(c(10, 50, 100, 200), 4),
                         .y = c(rep(10, 4), rep(50, 4), rep(100, 4), rep(200, 4)),
                         .f = testRfHpi,
                         ex_sales = seattle_sales  %>% dplyr::filter(area == z))

  save.image("C:/Users/andyx/Desktop/ares.RData")

###############


rt_hpi <- rtIndex(trans_df = sales_rtdf,
                  estimator = 'robust',
                  log_dep = TRUE,
                  trim_model = FALSE,
                  max_period = 84,
                  smooth = TRUE)
he_hpi <- hedIndex(trans_df = sales_hhdf,
                   estimator = 'robust',
                   log_dep = TRUE,
                   dep_var = 'price',
                   ind_var = c('use_type', 'lot_sf', 'tot_sf', 'beds', 'baths', 'eff_age', 'area'),
                   trim_model = FALSE,
                   max_period = 84,
                   smooth = TRUE)


rt_vol <- calcVolatility(index = rt_hpi$index$value,
                            window = 3)
he_vol <- calcVolatility(index = he_hpi$index$value,
                         window = 3)


rt_is_accr <- calcAccuracy(hpi_obj = rt_hpi,
                           test_type = 'rt',
                           test_method = 'insample',
                           smooth = FALSE)
summary(abs(rt_is_accr$pred_error))

he_is_accr <- calcAccuracy(hpi_obj = he_hpi,
                           test_type = 'rt',
                           pred_df = sales_rtdf,
                           test_method = 'insample',
                           smooth = FALSE)
summary(abs(he_is_accr$pred_error))




rt_kf_accr <- calcAccuracy(hpi_obj = rt_hpi,
                           test_type = 'rt',
                           test_method = 'kfold',
                           k = 10,
                           seed = 123)
summary(abs(rt_kf_accr$pred_error))

he_kf_accr <- calcAccuracy(hpi_obj = he_hpi,
                           test_type = 'rt',
                           test_method = 'kfold',
                           k = 10,
                           seed = 123,
                           pred_df = rt_hpi$data)
summary(abs(he_kf_accr$pred_error))




rt_series <- createSeries(hpi_obj = rt_hpi,
                          train_period = 24,
                          max_period = 84)
he_series <- createSeries(hpi_obj = he_hpi,
                          train_period = 24,
                          max_period = 84)

plot(rt_series)
plot(he_series)

rt_series <- calcSeriesVolatility(series_obj = rt_series,
                                  window = 3,
                                  smooth = FALSE)
he_series <- calcSeriesVolatility(series_obj = he_series,
                                  window = 3,
                                  smooth = FALSE)

rt_rev <- calcRevision(series_obj = rt_series)
he_rev <- calcRevision(series_obj = he_series)

plot(rt_rev, measure='median')
plot(he_rev, measure='median')

rt_series <- calcSeriesAccuracy(series_obj = rt_series,
                                test_method = 'forecast',
                                test_type = 'rt',
                                smooth = FALSE,
                                in_place = TRUE)
summary(abs(rt_series$accuracy$pred_error))

he_series <- calcSeriesAccuracy(series_obj = he_series,
                                test_method = 'forecast',
                                test_type = 'rt',
                                smooth = FALSE,
                                pred_df = rt_hpi$data,
                                in_place = TRUE)
summary(abs(he_series$accuracy$pred_error))

