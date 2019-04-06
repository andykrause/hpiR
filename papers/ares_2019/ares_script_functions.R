
### Create Sales -----------------------------------------------------------------------------------

createSales <- function(sales_df,
                        periodicity = 'monthly'){

  sales_hdf <- dateToPeriod(trans_df = sales_df,
                            date = 'sale_date',
                            periodicity = periodicity)

  sales_rtdf <- rtCreateTrans(trans_df = sales_hdf,
                              prop_id = 'pinx',
                              trans_id = 'sale_id',
                              price = 'sale_price')

  sales_hhdf <- hedCreateTrans(trans_df = sales_df,
                               prop_id = 'pinx',
                               trans_id = 'sale_id',
                               price = 'sale_price',
                               date= 'sale_date',
                               periodicity = periodicity)

  list(raw = sales_hdf,
       rt = sales_rtdf,
       hed = sales_hhdf)
}

### Three Way Comparison ---------------------------------------------------------------------------

threeWayComparison <- function(data_obj,
                               periodicity = 'monthly',
                               ntrees = 200,
                               sim_count = 200,
                               train_period = 24,
                               max_period = 84,
                               hed_var = c('use_type', 'lot_sf', 'tot_sf', 'beds',
                                           'baths', 'eff_age', 'area'),
                               rf_var = c('use_type', 'lot_sf', 'tot_sf', 'beds', 'baths', 'eff_age',
                                          'latitude', 'longitude'),
                               ...){

  message('****************Building Data')

  # Hedonic Data
  hed_df <- hedCreateTrans(trans_df = data_obj,
                           prop_id = 'pinx',
                           trans_id = 'sale_id',
                           price = 'sale_price',
                           date = 'sale_date',
                           periodicity = periodicity)

  # Repeat Transaction Data
  rt_df <- rtCreateTrans(trans_df = data_obj,
                         prop_id = 'pinx',
                         trans_id = 'sale_id',
                         price = 'sale_price',
                         date = 'sale_date',
                         periodicity = periodicity)


  message('Building Indexes')

  ## Repeat Transaction Example
  rt_hpi <- rtIndex(trans_df = rt_df,
                    estimator = 'robust',
                    log_dep = TRUE,
                    trim_model = FALSE,
                    max_period = max_period,
                    smooth = TRUE,
                    ...)

  ## Hedonic sales Example
  he_hpi <- hedIndex(trans_df = hed_df,
                     estimator = 'robust',
                     log_dep = TRUE,
                     dep_var = 'price',
                     ind_var = hed_var,
                     trim_model = FALSE,
                     max_period = max_period,
                     smooth = TRUE)

  # Random Forest
  rfs_hpi <- rfIndex(trans_df = hed_df,
                     estimator = 'shap',
                     dep_var = 'price',
                     ind_var = rf_var[rf_var != 'use_type'],
                     max_period = max_period,
                     smooth = FALSE,
                     shap_k = 50,
                     ntrees = ntrees,
                     sim_count = sim_count)

  # Random Forest
  rfp_hpi <- rfIndex(trans_df = hed_df,
                     estimator = 'pdp',
                     dep_var = 'price',
                     ind_var = rf_var,
                     max_period = max_period,
                     smooth = FALSE,
                     ntrees = ntrees,
                     sim_count = sim_count)

  message('Comparing Index Volatilities')
  ## Volatility

  rt_hpi <- calcVolatility(index = rt_hpi,
                           window = 3,
                           in_place = TRUE)
  rt_hpi <- calcVolatility(index = rt_hpi,
                           window = 3,
                           in_place = TRUE,
                           smooth = TRUE,
                           in_place_name = 'volatility_smooth')
  he_hpi <- calcVolatility(index = he_hpi,
                           window = 3,
                           in_place = TRUE)
  he_hpi <- calcVolatility(index = he_hpi,
                           window = 3,
                           in_place = TRUE,
                           smooth = TRUE,
                           in_place_name = 'volatility_smooth')
  rfs_hpi <- calcVolatility(index = rfs_hpi,
                            window = 3,
                            in_place = TRUE)
  rfp_hpi <- calcVolatility(index = rfp_hpi,
                            window = 3,
                            in_place = TRUE)


  message('Comparing In-Sample Accuracy')
  ## In sample accuracy
  rt_hpi <- calcAccuracy(hpi_obj = rt_hpi,
                         test_method = 'insample',
                         test_type = 'rt',
                         in_place = TRUE,
                         in_place_name = 'is_accuracy')
  rt_hpi <- calcAccuracy(hpi_obj = rt_hpi,
                         test_method = 'insample',
                         test_type = 'rt',
                         smooth = TRUE,
                         in_place = TRUE,
                         in_place_name = 'is_accuracy_smooth')
  he_hpi <- calcAccuracy(hpi_obj = he_hpi,
                         test_method = 'insample',
                         test_type = 'rt',
                         pred_df = rt_df,
                         in_place = TRUE,
                         in_place_name = 'is_accuracy')
  he_hpi <- calcAccuracy(hpi_obj = he_hpi,
                         test_method = 'insample',
                         test_type = 'rt',
                         pred_df = rt_df,
                         smooth = TRUE,
                         in_place = TRUE,
                         in_place_name = 'is_accuracy_smooth')
  rfs_hpi <- calcAccuracy(hpi_obj = rfs_hpi,
                          test_method = 'insample',
                          test_type = 'rt',
                          pred_df = rt_df,
                          in_place = TRUE,
                          in_place_name = 'is_accuracy')
  rfp_hpi <- calcAccuracy(hpi_obj = rfp_hpi,
                          test_method = 'insample',
                          test_type = 'rt',
                          pred_df = rt_df,
                          in_place = TRUE,
                          in_place_name = 'is_accuracy')


  message('Comparing Out-of-Sample (KFold) Accuracy')

  ## Out of sample K Fold accuracy
  rt_hpi <- calcAccuracy(hpi_obj = rt_hpi,
                         test_method = 'kfold',
                         test_type = 'rt',
                         in_place = TRUE,
                         in_place_name = 'kf_accuracy',
                         ...)
  rt_hpi <- calcAccuracy(hpi_obj = rt_hpi,
                         test_method = 'kfold',
                         test_type = 'rt',
                         smooth = TRUE,
                         in_place = TRUE,
                         in_place_name = 'kf_accuracy_smooth',
                         ...)
  he_hpi <- calcAccuracy(hpi_obj = he_hpi,
                         test_method = 'kfold',
                         test_type = 'rt',
                         pred_df = rt_df,
                         in_place = TRUE,
                         in_place_name = 'kf_accuracy')
  he_hpi <- calcAccuracy(hpi_obj = he_hpi,
                         test_method = 'kfold',
                         test_type = 'rt',
                         pred_df = rt_df,
                         smooth = TRUE,
                         in_place = TRUE,
                         in_place_name = 'kf_accuracy_smooth')

  rfs_hpi <- calcAccuracy(hpi_obj = rfs_hpi,
                          test_method = 'kfold',
                          test_type = 'rt',
                          pred_df = rt_df,
                          in_place = TRUE,
                          in_place_name = 'kf_accuracy',
                          ntrees = ntrees,
                          shap_k = 50,
                          ind_var = rf_var[rf_var != 'use_type'],
                          sim_count = sim_count)

  rfp_hpi <- calcAccuracy(hpi_obj = rfp_hpi,
                          test_method = 'kfold',
                          test_type = 'rt',
                          pred_df = rt_df,
                          in_place = TRUE,
                          in_place_name = 'kf_accuracy',
                          ntrees = ntrees,
                          sim_count = sim_count)


  message('Creating Series')

  ## Series
  suppressWarnings(
    rt_series <- createSeries(hpi_obj = rt_hpi,
                              train_period = train_period,
                              max_period = max_period,
                              smooth = TRUE,
                              ...) %>% smoothSeries())

  suppressWarnings(he_series <- createSeries(hpi_obj = he_hpi,
                                             train_period = train_period,
                                             max_period = max_period,
                                             smooth = TRUE)  %>% smoothSeries())

  suppressWarnings(rfs_series <- createSeries(hpi_obj = rfs_hpi,
                                              train_period = train_period,
                                              max_period = max_period,
                                              ntrees = ntrees,
                                              shap_k = 50,
                                              ind_var = rf_var[rf_var != 'use_type'],
                                              sim_count = sim_count))

  suppressWarnings(rfp_series <- createSeries(hpi_obj = rfp_hpi,
                                              train_period = train_period,
                                              max_period = max_period,
                                              ntrees = ntrees,
                                              sim_count = sim_count))

  message('Comparing Series Volatilities')

  # Series Volatility
  rt_series <- calcSeriesVolatility(series_obj = rt_series,
                                    window = 3,
                                    smooth = FALSE,
                                    in_place = TRUE,
                                    in_place_name = 'volatility')
  rt_series <- calcSeriesVolatility(series_obj = rt_series,
                                    window = 3,
                                    smooth = TRUE,
                                    in_place = TRUE,
                                    in_place_name = 'volatility_smooth')

  he_series <- calcSeriesVolatility(series_obj = he_series,
                                    window = 3,
                                    smooth = FALSE,
                                    in_place = TRUE,
                                    in_place_name = 'volatility')
  he_series <- calcSeriesVolatility(series_obj = he_series,
                                    window = 3,
                                    smooth = TRUE,
                                    in_place = TRUE,
                                    in_place_name = 'volatility_smooth')

  rfs_series <- calcSeriesVolatility(series_obj = rfs_series,
                                     window = 3,
                                     smooth = FALSE,
                                     in_place = TRUE,
                                     in_place_name = 'volatility')
  rfp_series <- calcSeriesVolatility(series_obj = rfp_series,
                                     window = 3,
                                     smooth = FALSE,
                                     in_place = TRUE,
                                     in_place_name = 'volatility')

  message('Calculating Revisions')

  ## Revision
  rt_series <- calcRevision(series_obj = rt_series,
                            in_place = TRUE,
                            in_place_name = 'revision')
  rt_series <- calcRevision(series_obj = rt_series,
                            smooth = TRUE,
                            in_place = TRUE,
                            in_place_name = 'revision_smooth')
  he_series <- calcRevision(series_obj = he_series,
                            in_place = TRUE,
                            in_place_name = 'revision')
  he_series <- calcRevision(series_obj = he_series,
                            smooth = TRUE,
                            in_place = TRUE,
                            in_place_name = 'revision_smooth')
  rfs_series <- calcRevision(series_obj = rfs_series,
                             in_place = TRUE,
                             in_place_name = 'revision')
  rfp_series <- calcRevision(series_obj = rfp_series,
                             in_place = TRUE,
                             in_place_name = 'revision')

  message('Comparing Out-of-Sample (Prediction) Accuracy')

  ## Prediction accuracy
  rt_series <- calcSeriesAccuracy(series_obj = rt_series,
                                  test_method = 'forecast',
                                  test_type = 'rt',
                                  smooth = FALSE,
                                  in_place = TRUE,
                                  in_place_name = 'pr_accuracy')
  rt_series <- calcSeriesAccuracy(series_obj = rt_series,
                                  test_method = 'forecast',
                                  test_type = 'rt',
                                  smooth = TRUE,
                                  in_place = TRUE,
                                  in_place_name = 'pr_accuracy_smooth')
  he_series <- calcSeriesAccuracy(series_obj = he_series,
                                  test_method = 'forecast',
                                  test_type = 'rt',
                                  pred_df = rt_df,
                                  smooth = FALSE,
                                  in_place = TRUE,
                                  in_place_name = 'pr_accuracy')
  he_series <- calcSeriesAccuracy(series_obj = he_series,
                                  test_method = 'forecast',
                                  test_type = 'rt',
                                  pred_df = rt_df,
                                  smooth = TRUE,
                                  in_place = TRUE,
                                  in_place_name = 'pr_accuracy_smooth')
  rfs_series <- calcSeriesAccuracy(series_obj = rfs_series,
                                   test_method = 'forecast',
                                   test_type = 'rt',
                                   pred_df = rt_df,
                                   smooth = FALSE,
                                   in_place = TRUE,
                                   in_place_name = 'pr_accuracy')
  rfp_series <- calcSeriesAccuracy(series_obj = rfp_series,
                                   test_method = 'forecast',
                                   test_type = 'rt',
                                   pred_df = rt_df,
                                   smooth = FALSE,
                                   in_place = TRUE,
                                   in_place_name = 'pr_accuracy')

  ## Combine into results
  vol <- c(rt_hpi$index$volatility$mean,
           rt_hpi$index$volatility_smooth$mean,
           he_hpi$index$volatility$mean,
           he_hpi$index$volatility_smooth$mean,
           rfs_hpi$index$volatility$mean,
           rfp_hpi$index$volatility$mean)
  is_accr <- c(median(abs(rt_hpi$index$is_accuracy$pred_error), na.rm=T),
               median(abs(rt_hpi$index$is_accuracy_smooth$pred_error), na.rm=T),
               median(abs(he_hpi$index$is_accuracy$pred_error), na.rm=T),
               median(abs(he_hpi$index$is_accuracy_smooth$pred_error), na.rm=T),
               median(abs(rfs_hpi$index$is_accuracy$pred_error), na.rm=T),
               median(abs(rfp_hpi$index$is_accuracy$pred_error), na.rm=T))
  kf_accr <- c(median(abs(rt_hpi$index$kf_accuracy$pred_error), na.rm=T),
               median(abs(rt_hpi$index$kf_accuracy_smooth$pred_error), na.rm=T),
               median(abs(he_hpi$index$kf_accuracy$pred_error), na.rm=T),
               median(abs(he_hpi$index$kf_accuracy_smooth$pred_error), na.rm=T),
               median(abs(rfs_hpi$index$kf_accuracy$pred_error), na.rm=T),
               median(abs(rfp_hpi$index$kf_accuracy$pred_error), na.rm=T))
  rev <- c(rt_series$revision$mean,
           rt_series$revision_smooth$mean,
           he_series$revision$mean,
           he_series$revision_smooth$mean,
           rfs_series$revision$mean,
           rfp_series$revision$mean)
  pr_accr <- c(median(abs(rt_series$pr_accuracy$pred_error), na.rm=T),
               median(abs(rt_series$pr_accuracy_smooth$pred_error), na.rm=T),
               median(abs(he_series$pr_accuracy$pred_error), na.rm=T),
               median(abs(he_series$pr_accuracy_smooth$pred_error), na.rm=T),
               median(abs(rfs_series$pr_accuracy$pred_error), na.rm=T),
               median(abs(rfp_series$pr_accuracy$pred_error), na.rm=T))

  summ_df <- data.frame(type = c('RT', 'RT_smooth', 'Hed', 'Hed_smooth', 'RFs', 'RFp'),
                        vol = vol,
                        rev = rev,
                        is_accr = is_accr,
                        kf_accr = kf_accr,
                        pr_accr = pr_accr)

  list(hpi = list(rt=rt_hpi,
                  he=he_hpi,
                  rfs=rfs_hpi,
                  rfp=rfp_hpi),
       series = list(rt=rt_series,
                     he =he_series,
                     rfs=rfs_series,
                     rfp=rfp_series),
       summary = summ_df)
}










testRfHpi <- function(ex_sales,
                      ntrees = 200,
                      sim_count = 250,
                      sim_per = NULL,
                      ind_var = c('use_type', 'lot_sf', 'tot_sf', 'beds', 'baths',
                                  'eff_age', 'latitude', 'longitude'),
                      max_period = max_period,
                      periodicity = 'monthly',
                      verbose = TRUE,
                      ...){

  cat('**** Trees: ', ntrees, "    Sim Count: ", sim_count, "********\n\n")

  if (verbose) message('Creating sales object')
  sales_ <- createSales(ex_sales, 'monthly')


  if (verbose) message('Creating hpi object')
  hpi_time <- system.time(
    hpi <- rfIndex(trans_df = sales_$hed,
                 estimator = 'base',
                 dep_var = 'price',
                 ind_var = ind_var,
                 trim_model = FALSE,
                 max_period = max_period,
                 smooth = FALSE,
                 ntrees = ntrees,
                 sim_count = sim_count,
                 sim_per = sim_per,
                 ...))
  if (verbose) message('....', round(hpi_time[3], 1))


  if (verbose) message('Calculating volatility')
  vol <- calcVolatility(index = hpi$index$value,
                        window = 3)
  if (verbose) message('....', round(vol$median, 3))


  if (verbose) message('Calculating in sample accuracy')
  is_accr <- calcAccuracy(hpi_obj = hpi,
                          test_type = 'rt',
                          pred_df = sales_$rt,
                          test_method = 'insample',
                          smooth = FALSE)
  if (verbose) message('....', round(median(abs(is_accr$pred_error)), 3))


  if (verbose) message('Creating series object')
  series_time <- system.time(
    series <- createSeries(hpi_obj = hpi,
                         train_period = train_period,
                         max_period = max_period,
                         sim_count = sim_count,
                         ntrees = ntrees))
  if (verbose) message('....', round(series_time[3], 1))

  if (verbose) message('Calculating series volatility')
  series <- calcSeriesVolatility(series_obj = series,
                                 window = 3,
                                 smooth = FALSE)


  if (verbose) message('Calculating revision')
  rev <- calcRevision(series_obj = series)
  if (verbose) message('....', round(rev$median, 3))

  if (verbose) message('Calculating series accuracy')
  series <- calcSeriesAccuracy(series_obj = series,
                               test_method = 'forecast',
                               test_type = 'rt',
                               smooth = FALSE,
                               pred_df = sales_$rt,
                               in_place = TRUE)
  if (verbose) message('....', round(median(abs(series$accuracy$pred_error)), 3))

  list(hpi = hpi,
       vol = vol,
       series = series,
       rev = rev,
       is_accr = is_accr,
       os_accr = series$accuracy,
       accr_summ = c(is = median(abs(is_accr$pred_error)),
                     os = median(abs(series$accuracy$pred_error))),
       runtime = c(hpi = hpi_time[3],
                   series = series_time[3]))

}


summarizeComp <- function(comp_){

  ## Combine into results
  vol <- c(median(lapply(comp_, function(x) x$hpi$rt$index$volatility$median) %>% unlist()),
           median(lapply(comp_, function(x) x$hpi$rt$index$volatility_smooth$median) %>% unlist()),
           median(lapply(comp_, function(x) x$hpi$he$index$volatility$median) %>% unlist()),
           median(lapply(comp_, function(x) x$hpi$he$index$volatility_smooth$median) %>% unlist()),
           median(lapply(comp_, function(x) x$hpi$rfs$index$volatility$median) %>% unlist()),
           median(lapply(comp_, function(x) x$hpi$rfp$index$volatility$median) %>% unlist()))


  is_accr <- c(median(abs(lapply(comp_, function(x) x$hpi$rt$index$is_accuracy) %>%
                            dplyr::bind_rows() %>% dplyr::select(pred_error) %>% unlist())),
               median(abs(lapply(comp_, function(x) x$hpi$rt$index$is_accuracy_smooth) %>%
                            dplyr::bind_rows() %>% dplyr::select(pred_error) %>% unlist())),
               median(abs(lapply(comp_, function(x) x$hpi$he$index$is_accuracy) %>%
                            dplyr::bind_rows() %>% dplyr::select(pred_error) %>% unlist())),
               median(abs(lapply(comp_, function(x) x$hpi$he$index$is_accuracy_smooth) %>%
                            dplyr::bind_rows() %>% dplyr::select(pred_error) %>% unlist())),
               median(abs(lapply(comp_, function(x) x$hpi$rfs$index$is_accuracy) %>%
                            dplyr::bind_rows() %>% dplyr::select(pred_error) %>% unlist())),
               median(abs(lapply(comp_, function(x) x$hpi$rfp$index$is_accuracy) %>%
                            dplyr::bind_rows() %>% dplyr::select(pred_error) %>% unlist())))

  is_accr_data <- list(lapply(comp_, function(x) x$hpi$rt$index$is_accuracy) %>% dplyr::bind_rows() %>%
                         dplyr::mutate(model = 'rt'),
                       lapply(comp_, function(x) x$hpi$rt$index$is_accuracy_smooth) %>% dplyr::bind_rows()%>%
                         dplyr::mutate(model = 'rts'),
                       lapply(comp_, function(x) x$hpi$he$index$is_accuracy) %>% dplyr::bind_rows()%>%
                         dplyr::mutate(model = 'he'),
                       lapply(comp_, function(x) x$hpi$he$index$is_accuracy_smooth) %>% dplyr::bind_rows()%>%
                         dplyr::mutate(model = 'hes'),
                       lapply(comp_, function(x) x$hpi$rfs$index$is_accuracy) %>% dplyr::bind_rows()%>%
                         dplyr::mutate(model = 'rfs'),
                       lapply(comp_, function(x) x$hpi$rfp$index$is_accuracy) %>% dplyr::bind_rows()%>%
                         dplyr::mutate(model = 'rfp')) %>%
    dplyr::bind_rows()

  kf_accr <- c(median(abs(lapply(comp_, function(x) x$hpi$rt$index$kf_accuracy) %>%
                            dplyr::bind_rows() %>% dplyr::select(pred_error) %>% unlist()), na.rm=T),
               median(abs(lapply(comp_, function(x) x$hpi$rt$index$kf_accuracy_smooth) %>%
                            dplyr::bind_rows() %>% dplyr::select(pred_error) %>% unlist()), na.rm=T),
               median(abs(lapply(comp_, function(x) x$hpi$he$index$kf_accuracy) %>%
                            dplyr::bind_rows() %>% dplyr::select(pred_error) %>% unlist())),
               median(abs(lapply(comp_, function(x) x$hpi$he$index$kf_accuracy_smooth) %>%
                            dplyr::bind_rows() %>% dplyr::select(pred_error) %>% unlist())),
               median(abs(lapply(comp_, function(x) x$hpi$rfs$index$kf_accuracy) %>%
                            dplyr::bind_rows() %>% dplyr::select(pred_error) %>% unlist())),
               median(abs(lapply(comp_, function(x) x$hpi$rfp$index$kf_accuracy) %>%
                            dplyr::bind_rows() %>% dplyr::select(pred_error) %>% unlist())))

  kf_accr_data <- list(lapply(comp_, function(x) x$hpi$rt$index$kf_accuracy) %>% dplyr::bind_rows()%>%
                         dplyr::mutate(model = 'rt'),
                       lapply(comp_, function(x) x$hpi$rt$index$kf_accuracy_smooth) %>% dplyr::bind_rows()%>%
                         dplyr::mutate(model = 'rts'),
                       lapply(comp_, function(x) x$hpi$he$index$kf_accuracy) %>% dplyr::bind_rows()%>%
                         dplyr::mutate(model = 'he'),
                       lapply(comp_, function(x) x$hpi$he$index$kf_accuracy_smooth) %>% dplyr::bind_rows()%>%
                         dplyr::mutate(model = 'hes'),
                       lapply(comp_, function(x) x$hpi$rfs$index$kf_accuracy) %>% dplyr::bind_rows()%>%
                         dplyr::mutate(model = 'rfs'),
                       lapply(comp_, function(x) x$hpi$rfp$index$kf_accuracy) %>% dplyr::bind_rows()%>%
                         dplyr::mutate(model = 'rfp')) %>%
    dplyr::bind_rows()

  rev <- c(median(abs(lapply(comp_, function(x) x$series$rt$revision$median) %>% unlist())),
           median(abs(lapply(comp_, function(x) x$series$rt$revision_smooth$median) %>% unlist())),
           median(abs(lapply(comp_, function(x) x$series$he$revision$median) %>% unlist())),
           median(abs(lapply(comp_, function(x) x$series$he$revision_smooth$median) %>% unlist())),
           median(abs(lapply(comp_, function(x) x$series$rfs$revision$median) %>% unlist())),
           median(abs(lapply(comp_, function(x) x$series$rfp$revision$median) %>% unlist())))

  pr_accr <- c(median(abs(lapply(comp_, function(x) x$series$rt$pr_accuracy) %>%
                            dplyr::bind_rows()%>% dplyr::select(pred_error) %>% unlist())),
               median(abs(lapply(comp_, function(x) x$series$rt$pr_accuracy_smooth) %>%
                            dplyr::bind_rows()%>% dplyr::select(pred_error) %>% unlist())),
               median(abs(lapply(comp_, function(x) x$series$he$pr_accuracy) %>%
                            dplyr::bind_rows()%>% dplyr::select(pred_error) %>% unlist())),
               median(abs(lapply(comp_, function(x) x$series$he$pr_accuracy_smooth) %>%
                            dplyr::bind_rows()%>% dplyr::select(pred_error) %>% unlist())),
               median(abs(lapply(comp_, function(x) x$series$rfs$pr_accuracy) %>%
                            dplyr::bind_rows()%>% dplyr::select(pred_error) %>% unlist())),
               median(abs(lapply(comp_, function(x) x$series$rfp$pr_accuracy) %>%
                            dplyr::bind_rows()%>% dplyr::select(pred_error) %>% unlist())))

  pr_accr_data <- list(lapply(comp_, function(x) x$series$rt$pr_accuracy) %>% dplyr::bind_rows()%>%
                         dplyr::mutate(model = 'rt'),
                       lapply(comp_, function(x) x$series$rt$pr_accuracy_smooth) %>% dplyr::bind_rows()%>%
                         dplyr::mutate(model = 'rts'),
                       lapply(comp_, function(x) x$series$he$pr_accuracy) %>% dplyr::bind_rows()%>%
                         dplyr::mutate(model = 'he'),
                       lapply(comp_, function(x) x$series$he$pr_accuracy_smooth) %>% dplyr::bind_rows()%>%
                         dplyr::mutate(model = 'hes'),
                       lapply(comp_, function(x) x$series$rfs$pr_accuracy) %>% dplyr::bind_rows()%>%
                         dplyr::mutate(model = 'rfs'),
                       lapply(comp_, function(x) x$series$rfp$pr_accuracy) %>% dplyr::bind_rows()%>%
                         dplyr::mutate(model = 'rfp')) %>%
    dplyr::bind_rows()


  summ_df <- data.frame(type = c('RT', 'RT_smooth', 'Hed', 'Hed_smooth', 'RFs', 'RFp'),
                        vol = vol,
                        rev = rev,
                        is_accr = is_accr,
                        kf_accr = kf_accr,
                        pr_accr = pr_accr)
  list(pr=pr_accr_data,
       kf=kf_accr_data,
       is=is_accr_data,
       summ=summ_df)

}
