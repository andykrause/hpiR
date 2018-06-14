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
  sales_df <- dateToPeriod(sales_df = sales,
                           date = 'sale_date',
                           periodicity = 'monthly')

  # Hedonic Data
  hed_df <- hedCreateSales(sales_df = sales,
                           prop_id = 'pinx',
                           sale_id = 'sale_id',
                           price = 'sale_price',
                           date = 'sale_date',
                           periodicity = 'monthly')

  # Repeat Sales Data
  rs_df <- rsCreateSales(sales_df = sales_df,
                         prop_id = 'pinx',
                         sale_id = 'sale_id',
                         price = 'sale_price')

  # Full hedonic Index
  hed_index <- hedIndex(sales_df = hed_df,
                        estimator = 'weighted',
                        log_dep = FALSE,
                        dep_var = 'price',
                        ind_var = c('tot_sf', 'beds', 'baths'),
                        weights = runif(nrow(hed_df), 0, 1))

  # Full repeat sales index
  rs_index <- rsIndex(sales_df = rs_df,
                      estimator = 'base',
                      log_dep = TRUE,
                      periodicity = 'monthly')

### Volatility Function -------------------------------------------------------------

context('calcIndexVolatility()')

  # Sample 'hed' hpi object for further testing

  test_that('Volatility Function works with a variety of inputs',{

    # Standard Input (ts object)
    expect_is(index_vol <- calcIndexVolatility(index = hed_index$index$index,
                                               window = 3),
              'indexvol')

    # Hpi Index object
    expect_is(index_vol <- calcIndexVolatility(index = hed_index$index,
                                               window = 3),
              'indexvol')

    # Full HPI Object
    expect_is(index_vol <- calcIndexVolatility(index = hed_index,
                                               window = 3),
              'indexvol')

  })

  test_that('Errors are given when index is bad',{

    # Non-sensical index
    expect_error(index_vol <- calcIndexVolatility(index = 'abc',
                                                  window = 3))

    # Negative Window
    expect_error(index_vol <- calcIndexVolatility(index = hed_index$index,
                                                  window = -1))

    # Char Window
    expect_error(index_vol <- calcIndexVolatility(index = hed_index$index,
                                                  window = 'x'))

    # NA Window
    expect_error(index_vol <- calcIndexVolatility(index = hed_index$index,
                                                  window = NA_integer_))

  })

  test_that('Returning in place works',{

    # Standard Input (ts object)
    expect_is(index_vol <- calcIndexVolatility(index = hed_index$index$index,
                                               window = 3,
                                               in_place = TRUE),
              'indexvol')

    # Add it to the Hpi Index object
    expect_is(hed_index$index <- calcIndexVolatility(index = hed_index$index,
                                                     window = 3,
                                                     in_place = TRUE),
              'hpiindex')

    # Add it to the Full HPI Object (to the hpiindex object)
    expect_is(hed_index <- calcIndexVolatility(index = hed_index,
                                               window = 3,
                                               in_place = TRUE),
              'hpi')

    # Add it to the Full HPI Object (to the hpiindex object) with new name
    expect_is(hed_index <- calcIndexVolatility(index = hed_index,
                                               window = 3,
                                               in_place = TRUE,
                                               in_place_name = 'xxx'),
              'hpi')
    expect_is(hed_index$index$xxx, 'indexvol')


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

   expect_is(rs_series <- calcIndexSeries(hpi_obj = rs_index,
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
    expect_true(names(rs_series <- calcIndexSeries(hpi_obj = rs_index,
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
                                             hpi_data = hed_index$data,
                                             train = TRUE)) == 11863)

    expect_true(length(is_data <- buildForecastIDs(time_cut = 33,
                                             hpi_data = rs_index$data,
                                             train = TRUE)) == 287)

    expect_true(length(is_data <- buildForecastIDs(time_cut = 33,
                                             hpi_data = hed_index$data,
                                             train = FALSE)) == 437)

    expect_true(length(is_data <- buildForecastIDs(time_cut = 33,
                                             hpi_data = rs_index$data,
                                             train = FALSE)) == 21)

  })

  test_that('buildForecastIDs() does not work with bad arguments',{

    # Bad Data
    expect_error(is_data <- buildForecastIDs(time_cut = 33,
                                             hpi_data = hed_index,
                                             train = TRUE))

    # Bad time cut
    expect_error(is_data <- buildForecastIDs(time_cut = -1,
                                             hpi_data = hed_index$data,
                                             train = TRUE))

  })

### Test Revision Functions --------------------------------------------------------------



### Test Accuracy Functions --------------------------------------------------------------

  # Kfold
  # Forecast

### Test Blending Functions --------------------------------------------------------------



