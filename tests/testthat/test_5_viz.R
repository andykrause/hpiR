#*****************************************************************************************
#
#   Unit tests for hpiR package - Setup Functions
#
#*****************************************************************************************

library(hpiR)
library(testthat)

## Load Data

data(seattle_sales)
sales <- seattle_sales

## Create test objects

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
                    periodicity = 'monthly',
                    smooth = TRUE)


# ### Test all plot functions --------------------------------------------------------------

context('Plot functions')


  test_that('plot.hpiindex works', {

    expect_is(plot(rt_index$index), 'plotindex')

    expect_is(plot(rt_index$index, show_imputed = TRUE), 'plotindex')

    rt_index$index$smooth <- NULL
    expect_message(plot(rt_index$index, smooth = TRUE), 'No smoothed')

  })


  test_that('plot.hpi works', {

      expect_is(plot(rt_index), 'plotindex')
      expect_is(plot(rt_index, smooth=TRUE), 'plotindex')

  })

 ## Volatility

  index_vol <- calcVolatility(index = hed_index$index$value,
                              window = 3)

  test_that('plot.indexvolatility works', {

    expect_is(plot(index_vol), 'plotvolatility')

  })

 ## Series

  rt_series <- createSeries(hpi_obj = rt_index,
                            train_period = 24)

  test_that('plot.serieshpi works', {

    expect_is(plot(rt_series), 'plotseries')

  })

 ## Revision

  rev_obj <- calcRevision(series_obj = rt_series)

  test_that('plot.hpirevision works', {

    expect_is(plot(rev_obj), 'plotrevision')

  })

 ## Accuracy

  rt_is_error <- calcAccuracy(hpi_obj = rt_index,
                              test_type = 'rt',
                              test_method = 'insample',
                              pred_df = rt_index$data)

  rt_kf_error <- calcAccuracy(hpi_obj = rt_index,
                              test_type = 'rt',
                              test_method = 'kfold',
                              pred_df = rt_index$data)

  test_that('plot.hpiaccuracy works', {

    expect_is(plot(rt_is_error, return_plot = TRUE), 'plotaccuracy')
    expect_is(plot(rt_kf_error, return_plot = TRUE), 'plotaccuracy')

  })

  rt_fc_error <- calcSeriesAccuracy(series_obj = rt_series,
                                    test_type = 'rt',
                                    test_method = 'forecast',
                                    pred_df = rt_index$data)

  test_that('plot.seriesaccuracy works', {

    expect_is(plot(rt_fc_error, return_plot = TRUE), 'plotaccuracy')

  })


