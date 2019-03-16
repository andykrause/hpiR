#*****************************************************************************************
#                                                                                        *
#   Unit tests for hpiR package - Preparatory Functions                                  *
#                                                                                        *
#*****************************************************************************************

  library(hpiR)
  library(testthat)

 ## Load Data

  sales <- get(data(ex_sales))

### dateToPeriod() -----------------------------------------------------------------------

context('Unit: dateToPeriod()')

test_that("Basic dateToPeriod() functionality (monthly) is working", {

  expect_is(sales_df <- dateToPeriod(trans_df = sales,
                                     date = 'sale_date',
                                     periodicity = 'monthly'),
            'hpidata')
  expect_true('trans_period' %in% names(sales_df))
  expect_true('trans_date' %in% names(sales_df))
  expect_true(attr(sales_df, 'periodicity') == 'monthly')
  expect_true(nrow(attr(sales_df, 'period_table')) == 84)
  expect_true(ncol(attr(sales_df, 'period_table')) == 3)

})

# Test sales_df and date field
test_that('Handling of sales_df and date field is working', {

  # Non-df as sales_df
  expect_error(dateToPeriod(trans_df=sales[[pinx]],
                            date = 'sale_date'))

  # Non date field
  expect_error(dateToPeriod(trans_df=sales,
                            date = 'sale_price'))

  # With a POSIXt field
  sales$date_p <- lubridate::as_datetime(sales$sale_date, tz='UTC')
  expect_is(dateToPeriod(trans_df=sales,
                         date = 'date_p'), 'hpidata')
  expect_true(dateToPeriod(trans_df=sales,
                           date = 'date_p')$trans_period[1] == 2)
})

# Test periodicity
test_that('Handling of periodicity field is working', {

  # Annual
  expect_is(dateToPeriod(trans_df=sales,
                         date = 'sale_date',
                         periodicity='annual'), 'hpidata')
  expect_is(dateToPeriod(trans_df=sales,
                         date = 'sale_date',
                         periodicity='yearly'), 'hpidata')
  expect_is(dateToPeriod(trans_df=sales,
                         date = 'sale_date',
                         periodicity='A'), 'hpidata')
  expect_is(dateToPeriod(trans_df=sales,
                         date = 'sale_date',
                         periodicity='Y'), 'hpidata')

  # Quarterly
  expect_is(dateToPeriod(trans_df=sales,
                         date = 'sale_date',
                         periodicity='Q'), 'hpidata')
  expect_is(dateToPeriod(trans_df=sales,
                         date = 'sale_date',
                         periodicity='quarterly'), 'hpidata')

  # Monthly
  expect_is(dateToPeriod(trans_df=sales,
                         date = 'sale_date',
                         periodicity='M'), 'hpidata')
  expect_is(dateToPeriod(trans_df=sales,
                         date = 'sale_date',
                         periodicity='montHly'), 'hpidata')

  # Weekly
  expect_is(dateToPeriod(trans_df=sales,
                         date = 'sale_date',
                         periodicity='Weekly'), 'hpidata')
  expect_true(attr(dateToPeriod(trans_df=sales,
                                date = 'sale_date',
                                periodicity='Weekly'), 'periodicity') == 'weekly')
  expect_is(dateToPeriod(trans_df=sales,
                         date = 'sale_date',
                         periodicity='W'), 'hpidata')

  # Daily
  expect_error(dateToPeriod(trans_df=sales,
                            date = 'sale_date',
                            periodicity = 'daily'))

})

# Test min and max clips
test_that('Min and Max Date clips are working', {

  # Min_date adj
  expect_is(dateToPeriod(trans_df=sales,
                         date = 'sale_date',
                         periodicity='monthly',
                         min_date = as.Date('2010-01-03')),
            'hpidata')
  expect_true(attr(dateToPeriod(trans_df=sales,
                                date = 'sale_date',
                                periodicity='monthly',
                                min_date = as.Date('2010-01-03')), 'min_date') ==
                '2010-01-03')
  expect_true(attr(dateToPeriod(trans_df=sales,
                                date = 'sale_date',
                                periodicity='monthly',
                                min_date = as.Date('2010-01-01')), 'min_date') ==
                '2010-01-01')

  # Min_date clip
  expect_is(dateToPeriod(trans_df=sales,
                         date = 'sale_date',
                         periodicity='monthly',
                         min_date = as.Date('2010-01-03'),
                         adj_type='clip'),
            'hpidata')
  expect_true(attr(dateToPeriod(trans_df=sales,
                                date = 'sale_date',
                                periodicity='monthly',
                                min_date = as.Date('2010-01-03'),
                                adj_type='clip'), 'min_date') ==
                '2010-01-03')
  expect_true(attr(dateToPeriod(trans_df=sales,
                                date = 'sale_date',
                                periodicity='monthly',
                                min_date = as.Date('2010-01-01'),
                                adj_type='clip'), 'min_date') ==
                '2010-01-01')

  # Max_date adj
  expect_is(dateToPeriod(trans_df=sales,
                         date = 'sale_date',
                         periodicity='monthly',
                         max_date = as.Date('2016-12-21')),
            'hpidata')
  expect_true(attr(dateToPeriod(trans_df=sales,
                                date = 'sale_date',
                                periodicity='monthly',
                                max_date = as.Date('2016-12-29')), 'max_date') ==
                '2016-12-29')
  expect_true(attr(dateToPeriod(trans_df=sales,
                                date = 'sale_date',
                                periodicity='monthly',
                                max_date = as.Date('2010-12-27')), 'max_date') ==
                '2016-12-22')

  # Max_date clip
  expect_is(dateToPeriod(trans_df=sales,
                         date = 'sale_date',
                         periodicity='monthly',
                         max_date = as.Date('2016-12-21'),
                         adj_type='clip'),
            'hpidata')
  expect_true(attr(dateToPeriod(trans_df=sales,
                                date = 'sale_date',
                                periodicity='monthly',
                                max_date = as.Date('2016-12-21'),
                                adj_type='clip'), 'max_date') ==
                '2016-12-21')
  expect_true(attr(dateToPeriod(trans_df=sales,
                                date = 'sale_date',
                                periodicity='monthly',
                                max_date = as.Date('2016-12-29'),
                                adj_type='clip'), 'max_date') ==
                '2016-12-29')
})

# Test Annual
test_that('Annual periodicity works', {
  expect_true(dateToPeriod(trans_df=sales,
                           date = 'sale_date',
                           periodicity='annual')$trans_period[1] == 2)
  expect_true(dateToPeriod(trans_df=sales,
                           date = 'sale_date',
                           min_date = as.Date('2009-12-31'),
                           periodicity='annual')$trans_period[1] == 3)
  expect_true(dateToPeriod(trans_df=sales,
                           date = 'sale_date',
                           min_date = as.Date('2011-12-31'),
                           adj_type = 'clip',
                           periodicity='annual')$trans_period[1] == 4)
})

# Test Monthly
test_that('Monthly periodicity works', {
  expect_true(max(dateToPeriod(trans_df=sales,
                               date = 'sale_date',
                               periodicity='monthly')$trans_period) == 84)
  expect_true(max(dateToPeriod(trans_df=sales,
                               date = 'sale_date',
                               min_date = as.Date('2009-12-31'),
                               periodicity='monthly')$trans_period) == 85)
  expect_true(max(dateToPeriod(trans_df=sales,
                               date = 'sale_date',
                               min_date = as.Date('2011-12-31'),
                               adj_type = 'clip',
                               periodicity='monthly')$trans_period) == 61)
})

# Test Quarterly
test_that('Quarterly periodicity works', {
  expect_true(max(dateToPeriod(trans_df=sales,
                           date = 'sale_date',
                           periodicity='Q')$trans_period) == 28)
  expect_true(max(dateToPeriod(trans_df=sales,
                           date = 'sale_date',
                           min_date = as.Date('2009-12-31'),
                           periodicity='Q')$trans_period) == 29)
  expect_true(max(dateToPeriod(trans_df=sales,
                           date = 'sale_date',
                           min_date = as.Date('2011-12-31'),
                           adj_type = 'clip',
                           periodicity='q')$trans_period) == 21)
  expect_true(max(dateToPeriod(trans_df=sales,
                           date = 'sale_date',
                           min_date = as.Date('2012-01-01'),
                           adj_type = 'clip',
                           periodicity='q')$trans_period) == 20)
})

# Test Weekly
test_that('Weekly periodicity works', {
  expect_true(max(dateToPeriod(trans_df=sales,
                           date = 'sale_date',
                           periodicity='w')$trans_period) == 363)
  expect_true(max(dateToPeriod(trans_df=sales,
                           date = 'sale_date',
                           min_date = as.Date('2009-12-31'),
                           periodicity='W')$trans_period) == 364)
  expect_true(max(dateToPeriod(trans_df=sales,
                           date = 'sale_date',
                           min_date = as.Date('2011-12-31'),
                           adj_type = 'clip',
                           periodicity='weekly')$trans_period) == 260)
  expect_true(max(dateToPeriod(trans_df=sales,
                           date = 'sale_date',
                           min_date = as.Date('2012-01-01'),
                           adj_type = 'clip',
                           periodicity='Weekly')$trans_period) == 259)

})

# Test Missing period check
test_that('Missing period warning work', {
  expect_message(dateToPeriod(trans_df=sales,
                              date = 'sale_date',
                              max_date = as.Date('2018-03-31'),
                              periodicity='M')$trans_period[1])
  expect_message(dateToPeriod(trans_df=sales,
                              date = 'sale_date',
                              min_date = as.Date('2005-03-31'),
                              periodicity='M'))
})

# Bad Date arguments
test_that('Bad date arguments are converted or give errors', {

  # Conversion works for a single string/date
  expect_is(dateToPeriod(trans_df=sales,
                         date = 'sale_date',
                         min_date = '2011-11-01',
                         max_date = Sys.time()),
            'hpidata')

  # Conversion works for a vector
  sales_x <- sales
  sales_x$sale_date <- as.character(sales_x$sale_date)
  expect_is(dateToPeriod(trans_df=sales_x,
                         date = 'sale_date',
                         min_date = '2011-11-01',
                         max_date = Sys.time()),
            'hpidata')

  # Conversion fails for a bad string
  expect_error(dateToPeriod(trans_df=sales,
                            date = 'sale_date',
                            min_date = 'xxx'))

  # Conversion fails for a bad vector
  expect_error(dateToPeriod(trans_df=sales,
                            date = 'sale_price'))

})
