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
                        weights = runif(nrow(hed_df), 0, 1),
                        smooth = TRUE)

  # Full repeat sales index
  rt_index <- rtIndex(trans_df = rt_df,
                      estimator = 'base',
                      log_dep = TRUE,
                      periodicity = 'monthly',
                      smooth = TRUE)



### Series Functions ----------------------------------------------------------------

  context('createSeries()')

  test_that('Index Series works', {

    expect_is(hed_series <- createSeries(hpi_obj = hed_index,
                                         train_period = 24),
              'serieshpi')

    expect_is(rt_series <- createSeries(hpi_obj = rt_index,
                                        train_period = 24,
                                        max_period = 50),
              'serieshpi')
    expect_true(length(rt_series$hpis) == 27)

  })

    test_that('Parameter arguments work',{

      # Train Range and max period

      # Max period is limited to lenght of 'hpi' object index
      expect_true(length(hed_series <- createSeries(hpi_obj = hed_index,
                                                    train_period = 12,
                                                    max_period = 150)$hpis) == 73)
    })


    test_that('Bad arguments create errors',{

      # Bad hpi_obj
      expect_error(hed_series <- createSeries(hpi_obj = hed_index$index,
                                              train_period = 24,
                                              max_period = 50))

      # Bad train_period
      expect_error(hed_series <- createSeries(hpi_obj = hed_index,
                                              train_period = 'x',
                                              max_period = 50))

      # Bad train_period
      expect_error(hed_series <- createSeries(hpi_obj = hed_index,
                                              train_period = 99,
                                              max_period = 50))

    })

    context('smoothSeries()')

    # Create Series
    rt_series <- createSeries(hpi_obj = rt_index, train_period = 24)

    test_that('smoothSeries() works as intended', {

      # Standard Return
      expect_is(rt_series <- smoothSeries(series_obj = rt_series,
                                          order = 5),
                'serieshpi')
      expect_is(rt_series$hpis[[1]]$index$smooth, 'indexsmooth')
      expect_is(rt_series$hpis[[1]]$index, 'hpiindex')

    })

    test_that('smoothSeries() breaks with bad arguments.',{

      # Bad series obj
      expect_error(rt_sseries <- smoothSeries(series_obj = rt_series[[1]],
                                              order = 5))

      # Bad order
      expect_error(rt_sseries <- smoothSeries(series_obj = rt_series,
                                              order = -1))

    })

    ## Create Series for remaining analyses
    hed_series <- createSeries(hpi_obj = hed_index,
                               train_period = 24,
                               max_period = 30)

    rt_series <- createSeries(hpi_obj = rt_index,
                              train_period = 24)
    rt_series <- smoothSeries(rt_series)

### Volatility Function -------------------------------------------------------------

context('calcVolatility()')

  # Sample 'hed' hpi object for further testing

  test_that('Volatility Function works with a variety of inputs',{

    # Standard Input (ts object)
    expect_is(index_vol <- calcVolatility(index = hed_index$index$value,
                                          window = 3),
              'indexvolatility')

    # HPI Index object
    expect_is(index_vol <- calcVolatility(index = hed_index$index,
                                          window = 3),
              'indexvolatility')

    # Full HPI Object
    expect_is(index_vol <- calcVolatility(index = hed_index,
                                               window = 3),
              'indexvolatility')

  })

  test_that('Volatility Function works for smoothed indexes',{

    # Standard Input (ts object)
    expect_is(index_vol <- calcVolatility(index = hed_index$index$smooth,
                                          window = 3),
              'indexvolatility')

    # HPI Index object
    expect_is(index_vol <- calcVolatility(index = hed_index$index,
                                          window = 3,
                                          smooth = TRUE),
              'indexvolatility')

    # Throws error if smooth is gone
    ex_index <- hed_index
    ex_index$index$smooth <- NULL
    expect_error(calcVolatility(index = ex_index,
                                window = 3,
                                smooth = TRUE), 'No smoothed')
    expect_error(calcVolatility(index = ex_index$index,
                                window = 3,
                                smooth = TRUE), 'No smoothed')

    # Full HPI Object
    expect_is(index_vol <- calcVolatility(index = hed_index,
                                          window = 3,
                                          smooth = TRUE),
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
                                             window = NA_integer_,
                                             smooth = TRUE))

  })

  test_that('Returning in place works',{

    # Standard Input (ts object)
    expect_is(index_vol <- calcVolatility(index = hed_index$index$value,
                                               window = 3,
                                               in_place = TRUE),
              'indexvolatility')

    # Add it to the HPI Index object
    expect_is(hed_index$index <- calcVolatility(index = hed_index$index,
                                                window = 3,
                                                in_place = TRUE),
              'hpiindex')

    # Add it to the HPI Index object Smooth
    expect_is(hed_index$index <- calcVolatility(index = hed_index$index,
                                                window = 3,
                                                in_place = TRUE,
                                                smooth = TRUE),
              'hpiindex')
    expect_is(hed_index$index$volatility_smooth, 'indexvolatility')

    # Add it to the Full HPI Object (to the hpiindex object)
    expect_is(hed_index <- calcVolatility(index = hed_index,
                                          window = 3,
                                          in_place = TRUE),
              'hpi')

    # Add it to the Full HPI Object (to the hpiindex object) smooth
    expect_is(hed_index <- calcVolatility(index = hed_index,
                                          window = 3,
                                          in_place = TRUE,
                                          smooth = TRUE),
              'hpi')
    expect_is(hed_index$index$volatility_smooth, 'indexvolatility')

    # Add it to the Full HPI Object (to the hpiindex object) with new name
    expect_is(hed_index <- calcVolatility(index = hed_index,
                                               window = 3,
                                               in_place = TRUE,
                                               in_place_name = 'xxx'),
              'hpi')
    expect_is(hed_index$index$xxx, 'indexvolatility')

  })

context('calcSeriesVolatility()')

test_that('Volatility Function works with a variety of inputs',{

  # Standard Input (ts object)
  expect_is(series_vol <- calcSeriesVolatility(series_obj = rt_series,
                                               window = 3),
            'serieshpi')

  expect_is(series_vol <- calcSeriesVolatility(series_obj = rt_series,
                                               window = 3,
                                               smooth = TRUE),
            'serieshpi')
  expect_true('volatility' %in% names(series_vol))

})

test_that('Fails if bad arguments', {

  expect_error(series_vol <- calcSeriesVolatility(series_obj = index,
                                                  window = 3))


})

### Accuracy Functions --------------------------------------------------------------

context('calcAccuracy() before error functions')

  test_that('bad arguments fail',{

    # Bad HPI objs
    expect_error(calcAccuracy(hpi_obj = 'xxx'))
    expect_error(calcAccuracy(hpi_obj = hed_index$data))

    # Disagreement between hpi_obj and pred_df
    expect_error(calcAccuracy(hpi_obj = hed_index,
                              test_type = 'rt'))
    expect_error(calcAccuracy(hpi_obj = rt_index,
                              test_type = 'hed'))
    expect_error(calcAccuracy(hpi_obj = hed_index,
                              test_type = 'rt',
                              pred_df = hed_index$data))
    expect_error(calcAccuracy(hpi_obj = rt_index,
                              test_type = 'hed',
                              pred_df = rt_index$data))

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

context('calcInSampleError()')

  test_that('in sample error fails with bad arguments',{

    # Bad Data
    expect_error(rt_error <- calcInSampleError(pred_df = hed_index,
                                               index = hed_index$index$value))

    # Bad Index
    expect_error(rt_error <- calcInSampleError(pred_df = rt_index$data,
                                               index = hed_index$index))

  })

  test_that('in sample error works',{

    # All data
    expect_is(rt_error <- calcInSampleError(pred_df = rt_index$data,
                                            index = hed_index$index$value),
              'hpiaccuracy')

    # All data smooth
    expect_is(rt_error <- calcInSampleError(pred_df = rt_index$data,
                                            index = hed_index$index$smooth),
              'hpiaccuracy')

    # Sparse data
    expect_is(rt_error <- calcInSampleError(pred_df = rt_index$data[1:4, ],
                                            index = hed_index$index$value),
              'hpiaccuracy')

    # No data
    expect_is(rt_error <- calcInSampleError(pred_df = rt_index$data[0, ],
                                            index = hed_index$index$value),
              'hpiaccuracy')

  })

context('calcKFoldError()')

  test_that('kFold error fails with bad arguments',{

    # Bad hpi_obj
    expect_error(rt_error <- calcKFoldError(hpi_obj = hed_index$index,
                                            pred_df = rt_index$data))

    # Bad pred_df
    expect_error(rt_error <- calcKFoldError(hpi_obj = hed_index,
                                            pred_df = rt_index))

    # Bad k
    expect_error(rt_error <- calcKFoldError(hpi_obj = hed_index,
                                            pred_df = rt_index$data,
                                            k = 'a'))

    # Bad seed
    expect_error(rt_error <- calcKFoldError(hpi_obj = hed_index,
                                            pred_df = rt_index$data,
                                            seed = 'x'))

  })

  test_that('kfold works',{

    # All data
    expect_is(rt_error <- calcKFoldError(hpi_obj = hed_index,
                                         pred_df = rt_index$data),
              'hpiaccuracy')
    expect_true(ncol(rt_error) == 6)

    # All data - smooth
    expect_is(rt_error <- calcKFoldError(hpi_obj = hed_index,
                                         pred_df = rt_index$data,
                                         smooth = TRUE),
              'hpiaccuracy')

    # Sparse data
    expect_is(rt_error <- calcKFoldError(hpi_obj = hed_index,
                                         pred_df = rt_index$data[1:40, ]),
              'hpiaccuracy')

    # No data
    expect_is(rt_error <- calcKFoldError(hpi_obj = hed_index,
                                            pred_df = rt_index$data[0, ]),
              'hpiaccuracy')

  })

context('calcAccuracy() after error functions')

  test_that('calcAccuracy works with insample errors',{

    # Returns an error object
    expect_is(rt_error <- calcAccuracy(hpi_obj = rt_index,
                                       test_type = 'rt',
                                       test_method = 'insample',
                                       pred_df = rt_index$data),
              'hpiaccuracy')
    expect_true(ncol(rt_error) == 6)


    # Returns an error object in place
    expect_is(hed_index <- calcAccuracy(hpi_obj = hed_index,
                                        test_type = 'rt',
                                        test_method = 'insample',
                                        pred_df = rt_index$data,
                                        in_place = TRUE,
                                        in_place_name ='acc'),
              'hpi')
    expect_is(hed_index$index$acc, 'hpiaccuracy')
    expect_true(attr(hed_index$index$acc, 'test_method') == 'insample')

  })

  test_that('calcAccuracy works with kfold errors',{

    # Returns an error object
    expect_is(rt_error <- calcAccuracy(hpi_obj = rt_index,
                                       test_type = 'rt',
                                       test_method = 'kfold',
                                       pred_df = rt_index$data),
              'hpiaccuracy')
    expect_true(ncol(rt_error) == 6)


    # Returns an error object in place
    expect_is(hed_index <- calcAccuracy(hpi_obj = hed_index,
                                        test_type = 'rt',
                                        test_method = 'kfold',
                                        pred_df = rt_index$data,
                                        in_place = TRUE,
                                        in_place_name = 'errors'),
              'hpi')
    expect_is(hed_index$index$errors, 'hpiaccuracy')
    expect_true(attr(hed_index$index$errors, 'test_method') == 'kfold')

  })

#### Series Accuracy --------------------------------------------------------------------

context('calcSeriesAccuracy()')

  test_that('calcSeriesAccuracy() fails with bad arguments',{

    # Bad series
    expect_error(rt_series <- calcSeriesAccuracy(series_obj = rt_series$data,
                                                 test_method = 'insample',
                                                 test_type = 'rt'))

    # Bad test_method
    expect_error(rt_series <- calcSeriesAccuracy(series_obj = rt_series,
                                                 test_method = 'xxx',
                                                 test_type = 'rt',
                                                 smooth = TRUE))

    # Bad test_type
    expect_error(rt_series <- calcSeriesAccuracy(series_obj = rt_series,
                                                 test_method = 'kfold',
                                                 test_type = 'rtx',
                                                 smooth = TRUE))

    # Bad pred_df
    expect_error(hed_series <- calcSeriesAccuracy(series_obj = hed_series,
                                                 test_method = 'insample',
                                                 test_type = 'rt'))

  })

  test_that('calcSeriesAccuracy() insample works',{

    # Regular
    # expect_is(rt_acc <- calcSeriesAccuracy(series_obj = rt_series,
    #                                           test_method = 'insample',
    #                                           test_type = 'rt'),
    #           'seriesaccuracy')

    # Smooth and in place
    expect_is(rt_series <- calcSeriesAccuracy(series_obj = rt_series,
                                              test_method = 'insample',
                                              test_type = 'rt',
                                              smooth = TRUE,
                                              in_place = TRUE),
              'serieshpi')
    expect_true('accuracy_smooth' %in% names(rt_series))
    expect_is(rt_series$accuracy_smooth, 'seriesaccuracy')

    # Smooth when no smooth existing (ERROR)
    expect_error(hed_series <- calcSeriesAccuracy(series_obj = hed_series,
                                                  test_method = 'insample',
                                                  test_type = 'rt',
                                                  smooth = TRUE,
                                                 pred_df = rt_series$data))
  })

  test_that('calcSeriesAccuracy() kfold works',{

    # Regular
    # expect_is(hed_acc <- calcSeriesAccuracy(series_obj = hed_series,
    #                                         test_method = 'kfold',
    #                                         test_type = 'rt',
    #                                         pred_df = rt_index$data),
    #           'seriesaccuracy')

    # Smooth and in place
    # expect_is(rt_series <- calcSeriesAccuracy(series_obj = rt_series,
    #                                           test_method = 'kfold',
    #                                           test_type = 'rt',
    #                                           pred_df = rt_index$data,
    #                                           smooth = TRUE,
    #                                           in_place = TRUE),
    #           'serieshpi')
    # expect_true('accuracy_smooth' %in% names(rt_series))
    # expect_is(rt_series$accuracy_smooth, 'seriesaccuracy')

    # Smooth when no smooth existing (ERROR)
    expect_error(hed_series <- calcSeriesAccuracy(series_obj = hed_series,
                                               test_method = 'kfold',
                                               test_type = 'rt',
                                               smooth = TRUE,
                                               pred_df = rt_series$data))

  })


  test_that('calcSeriesAccuracy() summarize works',{

    # expect_true(nrow(calcSeriesAccuracy(series_obj = rt_series,
    #                                     test_method = 'insample',
    #                                     test_type = 'rt')) == 112082)

    expect_true(nrow(calcSeriesAccuracy(series_obj = rt_series,
                                        test_method = 'insample',
                                        test_type = 'rt',
                                        summarize = TRUE,
                                        in_place = TRUE)$accuracy) == 5102)
  })

#### Forecast --------------------------------------------------------------------

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
                                                   forecast_length = 2,
                                                   train = FALSE)) == 960)

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

    # Bad forecast_length
    expect_error(is_data <- buildForecastIDs(time_cut = 33,
                                             forecast_length = 'x',
                                             hpi_df = hed_index$data,
                                             train = TRUE))

  })

context('calcForecastError()')

  test_that('forecast fails with bad arguments',{

    # Bad is_obj
    expect_error(rt_acc <- calcForecastError(is_obj = hed_index,
                                             pred_df = rt_index$data))

    # Bad pred_df
    expect_error(rt_acc <- calcForecastError(is_obj = hed_series,
                                             pred_df = rt_index))

    # Smooth when not present
    expect_error(hed_acc <- calcForecastError(is_obj = hed_series,
                                              pred_df = rt_index$data,
                                              smooth = TRUE))

    # Smooth when not present
    expect_error(hed_acc <- calcForecastError(is_obj = hed_series,
                                              pred_df = rt_index$data,
                                              forecast_length = 'x'))

  })

  test_that('Forecast works',{

    # All data
    expect_is(hed_acc <- calcForecastError(is_obj = hed_series,
                                           pred_df = rt_index$data),
              'hpiaccuracy')

    # All data, longer forecast length
    expect_is(hed_acc <- calcForecastError(is_obj = hed_series,
                                           pred_df = rt_index$data,
                                           forecast_length = 3),
              'seriesaccuracy')

    # All data, smoothed
    expect_is(rt_acc <- calcForecastError(is_obj = rt_series,
                                          pred_df = rt_index$data,
                                          smooth = TRUE),
              'hpiaccuracy')

    # # All data, smoothed, longer forecast length
    # expect_is(rt_acc <- calcForecastError(is_obj = rt_series,
    #                                       pred_df = rt_index$data,
    #                                       smooth = TRUE,
    #                                       forecast_length = 4),
    #           'seriesaccuracy')

    # Sparse data
    expect_is(rt_acc <- calcForecastError(is_obj = rt_series,
                                          pred_df = rt_index$data[1:40, ]),
              'hpiaccuracy')

    # No data
    expect_is(rt_acc <- calcForecastError(is_obj = rt_series,
                                            pred_df = rt_index$data[0, ],
                                            smooth=TRUE),
              'hpiaccuracy')

  })

  test_that('calcSeriesAccuracy works with forecast',{

    # # Returns a series with accuracy
    # expect_is(hed_acc <- calcSeriesAccuracy(series_obj = hed_series,
    #                                         test_type = 'rt',
    #                                         test_method = 'forecast',
    #                                         pred_df = rt_index$data),
    #           'seriesaccuracy')

    # Returns a series with accuracy: smooth and in place
    expect_is(rt_series <- calcSeriesAccuracy(series_obj = rt_series,
                                              test_type = 'rt',
                                              test_method = 'forecast',
                                              pred_df = rt_series$data,
                                              smooth = TRUE,
                                              in_place = TRUE),
              'serieshpi')
    expect_is(rt_series$accuracy_smooth, 'seriesaccuracy')
  })

### Revision Functions --------------------------------------------------------------

context('calcRevision()')

  test_that('calcRevision() works',{

    # Standard series object
    expect_is(hed_rev <- calcRevision(series_obj = hed_series),
              'seriesrevision')

    # In place
    expect_is(hed_series <- calcRevision(series_obj = hed_series,
                                         in_place = TRUE),
              'serieshpi')
    expect_is(hed_series$revision, 'seriesrevision')

    # With smooth
    expect_is(rt_rev_s <- calcRevision(series_obj = rt_series,
                                       smooth = TRUE),
              'seriesrevision')

    # With smooth in place
    expect_is(rt_series <- calcRevision(series_obj = rt_series,
                                        smooth = TRUE,
                                        in_place = TRUE),
              'serieshpi')
    expect_is(rt_series$revision_smooth, 'seriesrevision')

  })

  test_that('Bad arguments create errors',{

    # Bad series_obj
    expect_error(hed_rev <- calcRevision(series_obj = hed_series$data))

    # Bad smooth = TRUE
    expect_error(hed_rev <- calcRevision(series_obj = hed_series,
                                         smooth = TRUE))

  })
