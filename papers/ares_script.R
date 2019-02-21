
library(hpiR)
library(tidyverse)

data(seattle_sales)

sales_hdf <- dateToPeriod(trans_df = seattle_sales,
                          date = 'sale_date',
                          periodicity = 'monthly')

sales_rtdf <- rtCreateTrans(trans_df = sales_hdf,
                            prop_id = 'pinx',
                            trans_id = 'sale_id',
                            price = 'sale_price')

sales_hhdf <- hedCreateTrans(trans_df = seattle_sales,
                             prop_id = 'pinx',
                             trans_id = 'sale_id',
                             price = 'sale_price',
                             date= 'sale_date',
                             periodicity = 'monthly')

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
rf_hpi <- rfIndex(trans_df = sales_hhdf,
                  estimator = 'base',
                  dep_var = 'price',
                  ind_var = c('use_type', 'lot_sf', 'tot_sf', 'beds', 'baths', 'eff_age',
                              'latitude', 'longitude'),
                  trim_model = FALSE,
                  max_period = 84,
                  smooth = FALSE,
                  sim_count = 250)


rt_vol <- calcVolatility(index = rt_hpi$index$value,
                            window = 3)
he_vol <- calcVolatility(index = he_hpi$index$value,
                         window = 3)
rf_vol <- calcVolatility(index = rf_hpi$index$value,
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

rf_is_accr <- calcAccuracy(hpi_obj = rf_hpi,
                           test_type = 'rt',
                           pred_df = sales_rtdf,
                           test_method = 'insample',
                           smooth = FALSE)
summary(abs(rf_is_accr$pred_error))



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

rf_kf_accr <- calcAccuracy(hpi_obj = rf_hpi,
                           test_type = 'rt',
                           test_method = 'kfold',
                           k = 10,
                           seed = 123,
                           pred_df = rt_hpi$data,
                           sim_count = 250,
                           ntrees = 100)
summary(abs(rf_kf_accr$pred_error))



rt_series <- createSeries(hpi_obj = rt_hpi,
                          train_period = 24,
                          max_period = 84)
he_series <- createSeries(hpi_obj = he_hpi,
                          train_period = 24,
                          max_period = 84)
rf_series <- createSeries(hpi_obj = rf_hpi,
                          train_period = 24,
                          max_period = 84,
                          sim_count = 250,
                          ntrees = 100)

plot(rt_series)
plot(he_series)
plot(rf_series)

rt_series <- calcSeriesVolatility(series_obj = rt_series,
                                  window = 3,
                                  smooth = FALSE)
he_series <- calcSeriesVolatility(series_obj = he_series,
                                  window = 3,
                                  smooth = FALSE)
rf_series <- calcSeriesVolatility(series_obj = rf_series,
                                  window = 3,
                                  smooth = FALSE)

rt_rev <- calcRevision(series_obj = rt_series)
he_rev <- calcRevision(series_obj = he_series)
rf_rev <- calcRevision(series_obj = rf_series)

plot(rt_rev, measure='median')
plot(he_rev, measure='median')
plot(rf_rev, measure='median')

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

rf_series <- calcSeriesAccuracy(series_obj = rf_series,
                                test_method = 'forecast',
                                test_type = 'rt',
                                smooth = FALSE,
                                pred_df = rt_hpi$data,
                                in_place = TRUE)
summary(abs(rf_series$accuracy$pred_error))

### Small area -------------------------------------------------------------------------------------

data(seattle_sales)
data(ex_sales)

sales_hdf <- dateToPeriod(trans_df = ex_sales,
                          date = 'sale_date',
                          periodicity = 'monthly')

sales_rtdf <- rtCreateTrans(trans_df = sales_hdf,
                            prop_id = 'pinx',
                            trans_id = 'sale_id',
                            price = 'sale_price')

sales_hhdf <- hedCreateTrans(trans_df = ex_sales,
                             prop_id = 'pinx',
                             trans_id = 'sale_id',
                             price = 'sale_price',
                             date= 'sale_date',
                             periodicity = 'monthly')

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
rf_hpi <- rfIndex(trans_df = sales_hhdf,
                  estimator = 'base',
                  dep_var = 'price',
                  ind_var = c('use_type', 'lot_sf', 'tot_sf', 'beds', 'baths', 'eff_age',
                              'latitude', 'longitude'),
                  trim_model = FALSE,
                  max_period = 84,
                  smooth = FALSE,
                  sim_count = 250)


rt_vol <- calcVolatility(index = rt_hpi$index$value,
                         window = 3)
he_vol <- calcVolatility(index = he_hpi$index$value,
                         window = 3)
rf_vol <- calcVolatility(index = rf_hpi$index$value,
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

rf_is_accr <- calcAccuracy(hpi_obj = rf_hpi,
                           test_type = 'rt',
                           pred_df = sales_rtdf,
                           test_method = 'insample',
                           smooth = FALSE)
summary(abs(rf_is_accr$pred_error))



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

rf_kf_accr <- calcAccuracy(hpi_obj = rf_hpi,
                           test_type = 'rt',
                           test_method = 'kfold',
                           k = 10,
                           seed = 123,
                           pred_df = rt_hpi$data,
                           sim_count = 250,
                           ntrees = 100)
summary(abs(rf_kf_accr$pred_error))



rt_series <- createSeries(hpi_obj = rt_hpi,
                          train_period = 24,
                          max_period = 84)
he_series <- createSeries(hpi_obj = he_hpi,
                          train_period = 24,
                          max_period = 84)
rf_series <- createSeries(hpi_obj = rf_hpi,
                          train_period = 24,
                          max_period = 84,
                          sim_count = 250,
                          ntrees = 100)

plot(rt_series)
plot(he_series)
plot(rf_series)

rt_series <- calcSeriesVolatility(series_obj = rt_series,
                                  window = 3,
                                  smooth = FALSE)
he_series <- calcSeriesVolatility(series_obj = he_series,
                                  window = 3,
                                  smooth = FALSE)
rf_series <- calcSeriesVolatility(series_obj = rf_series,
                                  window = 3,
                                  smooth = FALSE)

rt_rev <- calcRevision(series_obj = rt_series)
he_rev <- calcRevision(series_obj = he_series)
rf_rev <- calcRevision(series_obj = rf_series)

plot(rt_rev, measure='median')
plot(he_rev, measure='median')
plot(rf_rev, measure='median')

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

rf_series <- calcSeriesAccuracy(series_obj = rf_series,
                                test_method = 'forecast',
                                test_type = 'rt',
                                smooth = FALSE,
                                pred_df = rt_hpi$data,
                                in_place = TRUE)
summary(abs(rf_series$accuracy$pred_error))


### Small area -------------------------------------------------------------------------------------

data(ex_sales)
ex_sales <- seattle_sales %>% dplyr::filter(beds <= 2)

sales_hdf <- dateToPeriod(trans_df = ex_sales,
                          date = 'sale_date',
                          periodicity = 'monthly')

sales_rtdf <- rtCreateTrans(trans_df = sales_hdf,
                            prop_id = 'pinx',
                            trans_id = 'sale_id',
                            price = 'sale_price')

sales_hhdf <- hedCreateTrans(trans_df = ex_sales,
                             prop_id = 'pinx',
                             trans_id = 'sale_id',
                             price = 'sale_price',
                             date= 'sale_date',
                             periodicity = 'monthly')

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
rf_hpi <- rfIndex(trans_df = sales_hhdf,
                  estimator = 'base',
                  dep_var = 'price',
                  ind_var = c('use_type', 'lot_sf', 'tot_sf', 'beds', 'baths', 'eff_age',
                              'latitude', 'longitude'),
                  trim_model = FALSE,
                  max_period = 84,
                  smooth = FALSE,
                  sim_count = 250)


rt_vol <- calcVolatility(index = rt_hpi$index$value,
                         window = 3)
he_vol <- calcVolatility(index = he_hpi$index$value,
                         window = 3)
rf_vol <- calcVolatility(index = rf_hpi$index$value,
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

rf_is_accr <- calcAccuracy(hpi_obj = rf_hpi,
                           test_type = 'rt',
                           pred_df = sales_rtdf,
                           test_method = 'insample',
                           smooth = FALSE)
summary(abs(rf_is_accr$pred_error))



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

rf_kf_accr <- calcAccuracy(hpi_obj = rf_hpi,
                           test_type = 'rt',
                           test_method = 'kfold',
                           k = 10,
                           seed = 123,
                           pred_df = rt_hpi$data,
                           sim_count = 250,
                           ntrees = 100)
summary(abs(rf_kf_accr$pred_error))



rt_series <- createSeries(hpi_obj = rt_hpi,
                          train_period = 24,
                          max_period = 84)
he_series <- createSeries(hpi_obj = he_hpi,
                          train_period = 24,
                          max_period = 84)
rf_series <- createSeries(hpi_obj = rf_hpi,
                          train_period = 24,
                          max_period = 84,
                          sim_count = 250,
                          ntrees = 100)

plot(rt_series)
plot(he_series)
plot(rf_series)

rt_series <- calcSeriesVolatility(series_obj = rt_series,
                                  window = 3,
                                  smooth = FALSE)
he_series <- calcSeriesVolatility(series_obj = he_series,
                                  window = 3,
                                  smooth = FALSE)
rf_series <- calcSeriesVolatility(series_obj = rf_series,
                                  window = 3,
                                  smooth = FALSE)

rt_rev <- calcRevision(series_obj = rt_series)
he_rev <- calcRevision(series_obj = he_series)
rf_rev <- calcRevision(series_obj = rf_series)

plot(rt_rev, measure='median')
plot(he_rev, measure='median')
plot(rf_rev, measure='median')

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

rf_series <- calcSeriesAccuracy(series_obj = rf_series,
                                test_method = 'forecast',
                                test_type = 'rt',
                                smooth = FALSE,
                                pred_df = rt_hpi$data,
                                in_place = TRUE)
summary(abs(rf_series$accuracy$pred_error))

### Small area -------------------------------------------------------------------------------------

data(ex_sales)
ex_sales <- seattle_sales %>% dplyr::filter(beds > 2)

sales_hdf <- dateToPeriod(trans_df = ex_sales,
                          date = 'sale_date',
                          periodicity = 'monthly')

sales_rtdf <- rtCreateTrans(trans_df = sales_hdf,
                            prop_id = 'pinx',
                            trans_id = 'sale_id',
                            price = 'sale_price')

sales_hhdf <- hedCreateTrans(trans_df = ex_sales,
                             prop_id = 'pinx',
                             trans_id = 'sale_id',
                             price = 'sale_price',
                             date= 'sale_date',
                             periodicity = 'monthly')

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
rf_hpi <- rfIndex(trans_df = sales_hhdf,
                  estimator = 'base',
                  dep_var = 'price',
                  ind_var = c('use_type', 'lot_sf', 'tot_sf', 'beds', 'baths', 'eff_age',
                              'latitude', 'longitude'),
                  trim_model = FALSE,
                  max_period = 84,
                  smooth = FALSE,
                  sim_count = 250)


rt_vol <- calcVolatility(index = rt_hpi$index$value,
                         window = 3)
he_vol <- calcVolatility(index = he_hpi$index$value,
                         window = 3)
rf_vol <- calcVolatility(index = rf_hpi$index$value,
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

rf_is_accr <- calcAccuracy(hpi_obj = rf_hpi,
                           test_type = 'rt',
                           pred_df = sales_rtdf,
                           test_method = 'insample',
                           smooth = FALSE)
summary(abs(rf_is_accr$pred_error))



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

rf_kf_accr <- calcAccuracy(hpi_obj = rf_hpi,
                           test_type = 'rt',
                           test_method = 'kfold',
                           k = 10,
                           seed = 123,
                           pred_df = rt_hpi$data,
                           sim_count = 250,
                           ntrees = 100)
summary(abs(rf_kf_accr$pred_error))



rt_series <- createSeries(hpi_obj = rt_hpi,
                          train_period = 24,
                          max_period = 84)
he_series <- createSeries(hpi_obj = he_hpi,
                          train_period = 24,
                          max_period = 84)
rf_series <- createSeries(hpi_obj = rf_hpi,
                          train_period = 24,
                          max_period = 84,
                          sim_count = 250,
                          ntrees = 100)

plot(rt_series)
plot(he_series)
plot(rf_series)

rt_series <- calcSeriesVolatility(series_obj = rt_series,
                                  window = 3,
                                  smooth = FALSE)
he_series <- calcSeriesVolatility(series_obj = he_series,
                                  window = 3,
                                  smooth = FALSE)
rf_series <- calcSeriesVolatility(series_obj = rf_series,
                                  window = 3,
                                  smooth = FALSE)

rt_rev <- calcRevision(series_obj = rt_series)
he_rev <- calcRevision(series_obj = he_series)
rf_rev <- calcRevision(series_obj = rf_series)

plot(rt_rev, measure='median')
plot(he_rev, measure='median')
plot(rf_rev, measure='median')

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

rf_series <- calcSeriesAccuracy(series_obj = rf_series,
                                test_method = 'forecast',
                                test_type = 'rt',
                                smooth = FALSE,
                                pred_df = rt_hpi$data,
                                in_place = TRUE)
summary(abs(rf_series$accuracy$pred_error))



