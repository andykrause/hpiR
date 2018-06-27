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
                    periodicity = 'monthly')


# ### Test all plot functions --------------------------------------------------------------

context('Plot functions')


  test_that('plot.hpiindex works', {

    expect_is(plot(rt_index$index), 'plotindex')

  })

  test_that('plot.hpi works', {

      expect_is(plot(rt_index), 'plotindex')

  })

 ## Smooth

  index_smooth <- smoothIndex(index_obj = hed_index$index,
                              order = 4)

  test_that('plot.indexsmooth works', {

    expect_is(plot(index_smooth), 'smoothplot')

  })

 ## Volatility

  index_vol <- calcVolatility(index = hed_index$index$index,
                              window = 3)

  test_that('plot.indexvolatility works', {

    expect_is(plot(index_vol), 'volatilityplot')

  })

 ## Series

  rt_series <- createSeries(hpi_obj = rt_index,
                            train_period = 24)

  test_that('plot.hpiseries works', {

    expect_is(plot(rt_series), 'seriesplot')

  })

 ## Revision

  rev_obj <- calcRevision(series_obj = rt_series)

  test_that('plot.hpirevision works', {

    expect_is(plot(rev_obj), 'revisionplot')

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

  rt_fc_error <- calcAccuracy(hpi_obj = rt_index,
                              test_type = 'rt',
                              test_method = 'forecast',
                              pred_df = rt_index$data)

  test_that('plot.indexerrors works', {

    expect_is(plot(rt_is_error, return_plot = TRUE), 'errorplot')
    expect_is(plot(rt_kf_error, return_plot = TRUE), 'errorplot')
    expect_is(plot(rt_fc_error, return_plot = TRUE), 'errorplot')

  })

 ## Blend

  blend_index <- blendIndexes(index_list = list(rt_index$index,
                                                hed_index$index))

  test_that('plot.hpiblend works', {

    expect_is(plot(blend_index), 'blendplot')

  })
