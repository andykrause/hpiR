

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


testRfHpi <- function(ex_sales,
                      ntrees = 200,
                      sim_count = 250,
                      sim_per = NULL,
                      ind_var = c('use_type', 'lot_sf', 'tot_sf', 'beds', 'baths',
                                  'eff_age', 'latitude', 'longitude'),
                      max_period = 84,
                      periodicity = 'monthly',
                      verbose = TRUE,
                      ...){

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


  if (verbose) message('Calculating volatility')
  vol <- calcVolatility(index = hpi$index$value,
                        window = 3)


  if (verbose) message('Calculating in sample accuracy')
  is_accr <- calcAccuracy(hpi_obj = hpi,
                          test_type = 'rt',
                          pred_df = sales_$rt,
                          test_method = 'insample',
                          smooth = FALSE)


  if (verbose) message('Creating series object')
  series_time <- system.time(
    series <- createSeries(hpi_obj = hpi,
                         train_period = 24,
                         max_period = max_period,
                         sim_count = sim_count,
                         ntrees = ntrees))


  if (verbose) message('Calculating series volatility')
  series <- calcSeriesVolatility(series_obj = series,
                                 window = 3,
                                 smooth = FALSE)


  if (verbose) message('Calculating revision')
  rev <- calcRevision(series_obj = series)


  if (verbose) message('Calculating series accuracy')
  series <- calcSeriesAccuracy(series_obj = series,
                               test_method = 'forecast',
                               test_type = 'rt',
                               smooth = FALSE,
                               pred_df = sales_$rt,
                               in_place = TRUE)

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
