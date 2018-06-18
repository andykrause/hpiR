#*****************************************************************************************
#                                                                                        *
#   Unit tests for hpiR package - HPI Analysis Functions                                 *
#                                                                                        *
#*****************************************************************************************

  library(hpiR)
  library(testthat)

  ## Load Data

  sales <- get(data(seattle_sales))

  ## Prep data for tests

  # Basic sales DF
  sales_df <- dateToPeriod(trans_df = sales,
                           date = 'sale_date',
                           periodicity = 'monthly')

  # Hedonic Data
  hed_df <- hedCreateTrans(trans_df = sales,
                           prop_id = 'pinx',
                           trans_id = 'sale_id',
                           price = 'sale_price',
                           date = 'sale_date',
                           periodicity = 'monthly')

  # Repeat Sales Data
  rt_df <- rtCreateTrans(trans_df = sales_df,
                         prop_id = 'pinx',
                         trans_id = 'sale_id',
                         price = 'sale_price')

  # Full hedonic Index
  hed_index <- hedIndex(trans_df = hed_df,
                        estimator = 'weighted',
                        log_dep = FALSE,
                        dep_var = 'price',
                        ind_var = c('tot_sf', 'beds', 'baths'),
                        weights = runif(nrow(hed_df), 0, 1))

  # Full repeat sales index
  rt_index <- rtIndex(trans_df = rt_df,
                      estimator = 'base',
                      log_dep = TRUE,
                      periodicity = 'monthly')

### Volatility Function -------------------------------------------------------------

context('calcVolatility()')

  # Sample 'hed' hpi object for further testing

  test_that('Volatility Function works with a variety of inputs',{

    # Standard Input (ts object)
    expect_is(index_vol <- calcVolatility(index = hed_index$index$index,
                                          window = 3),
              'indexvolatility')

    # Hpi Index object
    expect_is(index_vol <- calcVolatility(index = hed_index$index,
                                          window = 3),
              'indexvolatility')

    # Full HPI Object
    expect_is(index_vol <- calcVolatility(index = hed_index,
                                               window = 3),
              'indexvolatility')

  })

  test_that('Errors are given when index is bad',{

    # Non-sensical index
    expect_error(index_vol <- calcVolatility(index = 'abc',
                                                  window = 3))

    # Negative Window
    expect_error(index_vol <- calcVolatility(index = hed_index$index,
                                                  window = -1))

    # Char Window
    expect_error(index_vol <- calcVolatility(index = hed_index$index,
                                                  window = 'x'))

    # NA Window
    expect_error(index_vol <- calcVolatility(index = hed_index$index,
                                                  window = NA_integer_))

  })

  test_that('Returning in place works',{

    # Standard Input (ts object)
    expect_is(index_vol <- calcVolatility(index = hed_index$index$index,
                                               window = 3,
                                               in_place = TRUE),
              'indexvolatility')

    # Add it to the Hpi Index object
    expect_is(hed_index$index <- calcVolatility(index = hed_index$index,
                                                     window = 3,
                                                     in_place = TRUE),
              'hpiindex')

    # Add it to the Full HPI Object (to the hpiindex object)
    expect_is(hed_index <- calcVolatility(index = hed_index,
                                               window = 3,
                                               in_place = TRUE),
              'hpi')

    # Add it to the Full HPI Object (to the hpiindex object) with new name
    expect_is(hed_index <- calcVolatility(index = hed_index,
                                               window = 3,
                                               in_place = TRUE,
                                               in_place_name = 'xxx'),
              'hpi')
    expect_is(hed_index$index$xxx, 'indexvolatility')


  })

### Test Smoothing Functions -------------------------------------------------------------

context('smoothIndex()')

   test_that('smoothing Function works with a variety of inputs',{

     # Standard Input (ts object)
     expect_is(index_smooth <- smoothIndex(index = hed_index$index$index,
                                           order = 3),
               'smoothindex')

     # Hpi Index object
     expect_is(index_smooth <- smoothIndex(index = hed_index$index,
                                           order = 4),
               'smoothindex')

     # Full HPI Object
     expect_is(index_smooth <- smoothIndex(index = hed_index,
                                           order = 6),
               'smoothindex')

   })

  test_that('Errors are given when index is bad',{

    # Non-sensical index
    expect_error(index_smooth <- smoothIndex(index = 'abc',
                                             order = 3))

    # Negative Order
    expect_error(index_smooth <- smoothIndex(index = hed_index,
                                             order = -3))

    # Char Window
    expect_error(index_smooth <- smoothIndex(index = hed_index,
                                             order = 'x'))

    # NA Window
    expect_error(index_smooth <- smoothIndex(index = hed_index,
                                             order = NA_integer_))

  })

  test_that('Returning in place works',{

    # Standard Input (ts object)
    expect_is(index_smooth <- smoothIndex(index = hed_index$index$index,
                                          order = 3,
                                          in_place = TRUE),
              'smoothindex')

    # Add it to the Hpi Index object
    expect_is(hed_index$index <- smoothIndex(index = hed_index$index,
                                             order = 3,
                                             in_place = TRUE),
              'hpiindex')

    # Add it to the Full HPI Object (to the hpiindex object)
    expect_is(hed_index <- smoothIndex(index = hed_index,
                                       order = 3,
                                       in_place = TRUE),
              'hpi')

    # Add it to the Full HPI Object (to the hpiindex object) with new name
    expect_is(hed_index <- smoothIndex(index = hed_index,
                                       order = 3,
                                       in_place = TRUE,
                                       in_place_name = 'xxx'),
              'hpi')
    expect_is(hed_index$index$xxx, 'smoothindex')

  })


### Test Series Functions ----------------------------------------------------------------

context('calcIndexSeries()')

  test_that('Index Series works', {

   expect_is(hed_series <- calcIndexSeries(hpi_obj = hed_index,
                                           train_period = 24),
             'hpiseries')

   expect_is(rt_series <- calcIndexSeries(hpi_obj = rt_index,
                                          train_period = 24),
             'hpiseries')

  })

  test_that('Parameter arguments work',{

    # Train Range and max period
    expect_true(length(hed_series <- calcIndexSeries(hpi_obj = hed_index,
                                                     train_period = 12,
                                                     max_period = 50)) == 39)

    # Max period is limited to lenght of 'hpi' object index
    expect_true(length(hed_series <- calcIndexSeries(hpi_obj = hed_index,
                                                     train_period = 12,
                                                     max_period = 150)) == 73)

    # Name Prefix
    expect_true(names(rt_series <- calcIndexSeries(hpi_obj = rt_index,
                                                   train_period = 24,
                                                   max_period = 70,
                                                   name_prefix = 'xxx'))[1] == 'xxx24')

  })

  test_that('Creating in place (adding to hpi object) works', {

    # Basic in_place
    expect_is(hed_index <- calcIndexSeries(hpi_obj = hed_index,
                                           train_period = 24,
                                           max_period = 50,
                                           in_place = TRUE),
              'hpi')
    expect_is(hed_index$series, 'hpiseries')

    # With name
    expect_is(hed_index <- calcIndexSeries(hpi_obj = hed_index,
                                           train_period = 24,
                                           max_period = 50,
                                           in_place = TRUE,
                                           in_place_name = 'xxx'),
              'hpi')
    expect_is(hed_index$xxx, 'hpiseries')

  })

  test_that('Bad argument create errors',{

    # Bad hpi_obj
    expect_error(hed_series <- calcIndexSeries(hpi_obj = hed_index$index,
                                               train_period = 24,
                                               max_period = 50))

    # Bad train_period
    expect_error(hed_series <- calcIndexSeries(hpi_obj = hed_index,
                                               train_period = 'x',
                                               max_period = 50))

    # Bad train_period
    expect_error(hed_series <- calcIndexSeries(hpi_obj = hed_index,
                                               train_period = 99,
                                               max_period = 50))

  })

context('buildForecastIDs()')

  test_that('buildForecastIDs works', {

    expect_true(length(is_data <- buildForecastIDs(time_cut = 33,
                                             hpi_df = hed_index$data,
                                             train = TRUE)) == 11863)

    expect_true(length(is_data <- buildForecastIDs(time_cut = 33,
                                             hpi_df = rt_index$data,
                                             train = TRUE)) == 287)

    expect_true(length(is_data <- buildForecastIDs(time_cut = 33,
                                             hpi_df = hed_index$data,
                                             train = FALSE)) == 437)

    expect_true(length(is_data <- buildForecastIDs(time_cut = 33,
                                             hpi_df = rt_index$data,
                                             train = FALSE)) == 21)

  })

  test_that('buildForecastIDs() does not work with bad arguments',{

    # Bad Data
    expect_error(is_data <- buildForecastIDs(time_cut = 33,
                                             hpi_df = hed_index,
                                             train = TRUE))

    # Bad time cut
    expect_error(is_data <- buildForecastIDs(time_cut = -1,
                                             hpi_df = hed_index$data,
                                             train = TRUE))

  })

### Revision Functions --------------------------------------------------------------

context('calcRevision()')

  # Add Series to the hpi object for further testing
  hed_index <- calcIndexSeries(hpi_obj = hed_index,
                               train_period = 24,
                               max_period = 84,
                               in_place = TRUE)

  # Add Series to the hpi object for further testing
  rt_index <- calcIndexSeries(hpi_obj = rt_index,
                              train_period = 24,
                              max_period = 84,
                              in_place = TRUE,
                              in_place_name = 's84')

  test_that('calcRevision() works',{

    # Standard series object
    expect_is(calcRevision(series_obj = hed_index$series), 'indexrevision')

    # Extract from an hpi object
    expect_is(calcRevision(series_obj = hed_index), 'indexrevision')

    # Extract from an hpi object with a different name
    expect_is(calcRevision(series_obj = rt_index,
                           series_name = 's84'), 'indexrevision')

  })

  test_that('calcRevision() with in_place additions works',{

    # Standard in_place
    expect_is(calcRevision(series_obj = hed_index,
                           in_place = TRUE),
              'hpi')

    # In place with new name
    expect_is(rt_index <- calcRevision(series_obj = rt_index,
                                       series_name = 's84',
                                       in_place = TRUE,
                                       in_place_name ='r84'),
              'hpi')
    expect_is(rt_index$r84, 'indexrevision')

  })

  test_that('Bad arguments create errors',{

    # Bad series_obj
    expect_error(hed_rev <- calcRevision(series_obj = hed_index$data))

    # Bad series_name
    expect_error(hed_rev <- calcRevision(series_obj = hed_index,
                                         series_name = 'xxx'))

  })

### Test Accuracy Functions --------------------------------------------------------------

context('calcErrors() before error functions')

  test_that('bad arguments fail',{

    # Bad HPI objs
    expect_error(calcAccuracy(hpi_obj = 'xxx'))
    expect_error(calcAccuracy(hpi_obj = hed_index$data))

    # Disagreement between hpi_obj and index_data
    expect_error(calcAccuracy(hpi_obj = hed_index,
                              test_type = 'rt'))
    expect_error(calcAccuracy(hpi_obj = rt_index,
                              test_type = 'hed'))
    expect_error(calcAccuracy(hpi_obj = hed_index,
                              test_type = 'rt',
                              index_data = hed_index$data))
    expect_error(calcAccuracy(hpi_obj = rt_index,
                              test_type = 'hed',
                              index_data = rt_index$data))

    # Bad test_method
    expect_error(calcAccuracy(hpi_obj = rt_index,
                              test_type = 'rt',
                              test_method = 'x'))

    # Bad test_type
    expect_error(calcAccuracy(hpi_obj = rt_index,
                              test_type = 'x',
                              test_method = 'insample'))

  ## More to come

})

  test_that('calcAccuracy can estimate a series object', {

    # Simple format
    expect_is(rts <- calcAccuracy(hpi_obj = rt_index,
                                  test_type = 'rt',
                                  index_data = rt_index$data),
              'indexerrors')

    # with limited start and end and new name
    expect_is(hes <- calcAccuracy(hpi_obj = hed_index,
                                  test_type = 'rt',
                                  index_data = rt_index$data,
                                  train_period = 24,
                                  max_period = 36,
                                  series_name = 's36'),
              'indexerrors')

  })

context('calcInSampleError()')

  test_that('in sample error fails with bad arguments',{

    # Bad Data
    expect_error(rt_error <- calcInSampleError(pred_data = hed_index,
                                               index = hed_index$index$index))

    # Bad Index
    expect_error(rt_error <- calcInSampleError(pred_data = rt_index$data,
                                               index = hed_index$index))

  })

  test_that('in sample error works',{

    # All data
    expect_is(rt_error <- calcInSampleError(pred_data = rt_index$data,
                                            index = hed_index$index$index),
              'indexerrors')

    # Sparse data
    expect_is(rt_error <- calcInSampleError(pred_data = rt_index$data[1:4, ],
                                            index = hed_index$index$index),
              'indexerrors')

    # No data
    expect_is(rt_error <- calcInSampleError(pred_data = rt_index$data[0, ],
                                            index = hed_index$index$index),
              'indexerrors')

  })

context('calcKFoldError()')

  test_that('in sample error fails with bad arguments',{

    # Bad hpi_obj
    expect_error(rt_error <- calcKFoldError(hpi_obj = hed_index$index,
                                            pred_data = rt_index$data))

    # Bad pred_data
    expect_error(rt_error <- calcKFoldError(hpi_obj = hed_index,
                                            pred_data = rt_index))

    # Bad k
    expect_error(rt_error <- calcKFoldError(hpi_obj = hed_index,
                                            pred_data = rt_index$data,
                                            k = 'a'))

    # Bad seed
    expect_error(rt_error <- calcKFoldError(hpi_obj = hed_index,
                                            pred_data = rt_index$data,
                                            seed = 'x'))
  })

  test_that('kfold works',{

    # All data
    expect_is(rt_error <- calcKFoldError(hpi_obj = hed_index,
                                         pred_data = rt_index$data),
              'indexerrors')


    # Sparse data
    expect_is(rt_error <- calcKFoldError(hpi_obj = hed_index,
                                         pred_data = rt_index$data[1:40, ]),
              'indexerrors')

    # No data
    expect_is(rt_error <- calcKFoldError(hpi_obj = hed_index,
                                            pred_data = rt_index$data[0, ]),
              'indexerrors')

  })

context('calcForecastError()')

  test_that('forecast fails with bad arguments',{

    # Bad is_obj
    expect_error(rt_error <- calcForecastError(is_obj = hed_index$index,
                                               pred_data = rt_index$data))

    # Bad pred_data
    expect_error(rt_error <- calcForecastError(is_obj = hed_index$series,
                                               pred_data = rt_index))

  })

  test_that('forecast works',{

    # All data
    expect_is(rt_error <- calcForecastError(is_obj = hed_index$series,
                                            pred_data = rt_index$data),
              'indexerrors')

    # Sparse data
    expect_is(rt_error <- calcForecastError(is_obj = hed_index$series,
                                            pred_data = rt_index$data[1:40, ]),
              'indexerrors')

    # No data
    expect_is(rt_error <- calcForecastError(is_obj = hed_index$series,
                                            pred_data = rt_index$data[0, ]),
              'indexerrors')

  })

context('calcAccuracy() after error functions')

  test_that('calcAccuracy works with insample errors',{

    # Returns an error object
    expect_is(rt_error <- calcAccuracy(hpi_obj = rt_index,
                                       test_type = 'rt',
                                       test_method = 'insample',
                                       index_data = rt_index$data),
              'indexerrors')

    # Returns an error object in place
    expect_is(hed_index <- calcAccuracy(hpi_obj = hed_index,
                                        test_type = 'rt',
                                        test_method = 'insample',
                                        index_data = rt_index$data,
                                        in_place = TRUE,
                                        in_place_name = 'errors'),
              'hpi')
    expect_is(hed_index$errors, 'indexerrors')
    expect_true(attr(hed_index$errors, 'test_method') == 'insample')

  })

  test_that('calcAccuracy works with kfold errors',{

    # Returns an error object
    expect_is(rt_error <- calcAccuracy(hpi_obj = rt_index,
                                       test_type = 'rt',
                                       test_method = 'kfold',
                                       index_data = rt_index$data),
              'indexerrors')

    # Returns an error object in place
    expect_is(hed_index <- calcAccuracy(hpi_obj = hed_index,
                                        test_type = 'rt',
                                        test_method = 'kfold',
                                        index_data = rt_index$data,
                                        in_place = TRUE,
                                        in_place_name = 'errors'),
              'hpi')
    expect_is(hed_index$errors, 'indexerrors')
    expect_true(attr(hed_index$errors, 'test_method') == 'kfold')

  })

  test_that('calcAccuracy works with forecast errors',{

    # Returns an error object
    expect_is(rt_error <- calcAccuracy(hpi_obj = rt_index,
                                       test_type = 'rt',
                                       test_method = 'forecast',
                                       index_data = rt_index$data),
              'indexerrors')

    # Returns an error object in place
    expect_is(hed_index <- calcAccuracy(hpi_obj = hed_index,
                                        test_type = 'rt',
                                        test_method = 'forecast',
                                        index_data = rt_index$data,
                                        in_place = TRUE,
                                        in_place_name = 'errors'),
              'hpi')
    expect_is(hed_index$errors, 'indexerrors')
    expect_true(attr(hed_index$errors, 'test_method') == 'forecast')

  })


### Test Blending Functions --------------------------------------------------------------

context('blendIndexes()')

  test_that('blendIndexes() works',{

    # Basic Blend of two
    expect_is(blend_index <- blendIndexes(index_list = list(rt_index$index,
                                                            hed_index$index)),
              'indexblend')

    # With weights
    expect_is(blend_index <- blendIndexes(index_list = list(rt_index$index,
                                                             hed_index$index),
                                          weights=c(.25, .75)),
              'indexblend')

    # More than two
    expect_is(blend_index <- blendIndexes(index_list = list(rt_index$index,
                                                            hed_index$index,
                                                            hed_index$index)),
              'indexblend')
  })

  test_that('blendIndexes() fails with bad arguments', {

    # Bad index
    expect_error(blend_index <- blendIndexes(index_list = list(rt_index,
                                                               hed_index$index)))

    # Bad length
    bad_index <- hed_index$index
    bad_index$index <- bad_index$index[1:80]
    expect_error(blend_index <- blendIndexes(index_list = list(rt_index$index,
                                                               bad_index)))

    # Bad Weights
    expect_error(blend_index <- blendIndexes(index_list = list(rt_index$index,
                                                               hed_index$index),
                                             weights = c(.5, .4)))

    # Bad Weights
    expect_error(blend_index <- blendIndexes(index_list = list(rt_index$index,
                                                               hed_index$index),
                                             weights = c(.5, .4, .1)))

  })
