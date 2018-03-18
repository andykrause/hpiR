#*****************************************************************************************
#
#   Unit tests for hpiR package
#
#*****************************************************************************************

  library(hpiR)
  library(testthat)

  ## Load Data

  sales <- get(data(seattle_sales))

## Setup ---------------------------------------------------------------------------------

  context('dateToPeriod')

  # Test Setup
  test_that("Basic dateToPeriod() functionality is working", {

    expect_is(sales_df <- dateToPeriod(sales_df = sales,
                                       date = 'sale_date',
                                       periodicity = 'monthly'), 'salesdf')
    sales_df <- dateToPeriod(sales_df = sales,
                             date = 'sale_date',
                             periodicity = 'monthly')
    expect_true(!is.null(attr(sales_df, 'period_table')))
    expect_true(digest::digest(sales_df$date_period) ==
                  'ad2b649a8470371eff6f18cfdad93c92')
  })

  # Test sales_df and date field
  test_that('Handling of sales_df and date field is working', {

    # Non-df as sales_df
    expect_error(dateToPeriod(sales_df=sales[[pinx]],
                              date = 'sale_price'))

    # Non date field
    expect_error(dateToPeriod(sales_df=sales,
                              date = 'sale_price'))

    # With a POSIXt field
    sales$date_p <- lubridate::as_datetime(sales$sale_date, tz='UTC')
    expect_is(dateToPeriod(sales_df=sales,
                           date = 'date_p'), 'salesdf')
    expect_true(dateToPeriod(sales_df=sales,
                            date = 'date_p')$date_period[1] == 4)
  })

  # Test periodicity
  test_that('Handling of periodicity field is working', {

    # Annual
    expect_is(dateToPeriod(sales_df=sales,
                           date = 'sale_date',
                           periodicity='annual'), 'salesdf')
    expect_is(dateToPeriod(sales_df=sales,
                           date = 'sale_date',
                           periodicity='yearly'), 'salesdf')
    expect_is(dateToPeriod(sales_df=sales,
                           date = 'sale_date',
                           periodicity='A'), 'salesdf')
    expect_is(dateToPeriod(sales_df=sales,
                           date = 'sale_date',
                           periodicity='Y'), 'salesdf')
    # expect_true(digest::digest(dateToPeriod(sales_df=sales,
    #                                         date = 'sale_date',
    #                                         periodicity='Y')) ==
    #               'd76cc7eea986978bc78fb2dea56ab40f')

    # Quarterly
    expect_is(dateToPeriod(sales_df=sales,
                           date = 'sale_date',
                           periodicity='Q'), 'salesdf')
    expect_is(dateToPeriod(sales_df=sales,
                           date = 'sale_date',
                           periodicity='quarterly'), 'salesdf')
    # expect_true(digest::digest(dateToPeriod(sales_df=sales,
    #                                         date = 'sale_date',
    #                                         periodicity='Q')) ==
    #               '02f06d62a31d9c11786b65e12b99730f')

    # Monthly
    expect_is(dateToPeriod(sales_df=sales,
                           date = 'sale_date',
                           periodicity='M'), 'salesdf')
    expect_is(dateToPeriod(sales_df=sales,
                           date = 'sale_date',
                           periodicity='montHly'), 'salesdf')
    # expect_true(digest::digest(dateToPeriod(sales_df=sales,
    #                                         date = 'sale_date',
    #                                         periodicity='M')) ==
    #               '75de9f879c80f7b0a00c3a22fb755921')
    # Weekly
    expect_is(dateToPeriod(sales_df=sales,
                           date = 'sale_date',
                           periodicity='Weekly'), 'salesdf')
    expect_true(attr(dateToPeriod(sales_df=sales,
                                  date = 'sale_date',
                                  periodicity='Weekly'), 'periodicity') == 'weekly')
    expect_is(dateToPeriod(sales_df=sales,
                           date = 'sale_date',
                           periodicity='W'), 'salesdf')
    # expect_true(digest::digest(dateToPeriod(sales_df=sales,
    #                                         date = 'sale_date',
    #                                         periodicity='W')) ==
    #               '74008c16c769670c75de7e55e437af0d')

    # Daily
    expect_error(dateToPeriod(sales_df=sales,
                              date = 'sale_date',
                              periodicity = 'daily'))

  })

  # Test min and max clips
  test_that('Min and Max Date clips are working', {

    # Min_date adj
    expect_is(dateToPeriod(sales_df=sales,
                           date = 'sale_date',
                           periodicity='monthly',
                           min_date = as.Date('2010-01-03')),
              'salesdf')
    expect_true(attr(dateToPeriod(sales_df=sales,
                                  date = 'sale_date',
                                  periodicity='monthly',
                                  min_date = as.Date('2010-01-03')), 'min_date') ==
                '2010-01-02')
    expect_true(attr(dateToPeriod(sales_df=sales,
                                  date = 'sale_date',
                                  periodicity='monthly',
                                  min_date = as.Date('2010-01-01')), 'min_date') ==
                  '2010-01-01')

    # Min_date clip
    expect_is(dateToPeriod(sales_df=sales,
                           date = 'sale_date',
                           periodicity='monthly',
                           min_date = as.Date('2010-01-03'),
                           adj_type='clip'),
              'salesdf')
    expect_true(attr(dateToPeriod(sales_df=sales,
                                  date = 'sale_date',
                                  periodicity='monthly',
                                  min_date = as.Date('2010-01-03'),
                                  adj_type='clip'), 'min_date') ==
                  '2010-01-03')
    expect_true(attr(dateToPeriod(sales_df=sales,
                                  date = 'sale_date',
                                  periodicity='monthly',
                                  min_date = as.Date('2010-01-01'),
                                  adj_type='clip'), 'min_date') ==
                  '2010-01-01')

    # Max_date adj
    expect_is(dateToPeriod(sales_df=sales,
                           date = 'sale_date',
                           periodicity='monthly',
                           max_date = as.Date('2016-12-21')),
              'salesdf')
    expect_true(attr(dateToPeriod(sales_df=sales,
                                  date = 'sale_date',
                                  periodicity='monthly',
                                  max_date = as.Date('2016-12-29')), 'max_date') ==
                  '2016-12-29')
    expect_true(attr(dateToPeriod(sales_df=sales,
                                  date = 'sale_date',
                                  periodicity='monthly',
                                  max_date = as.Date('2010-12-27')), 'max_date') ==
                  '2016-12-28')

    # Max_date clip
    expect_is(dateToPeriod(sales_df=sales,
                           date = 'sale_date',
                           periodicity='monthly',
                           max_date = as.Date('2016-12-21'),
                           adj_type='clip'),
              'salesdf')
    expect_true(attr(dateToPeriod(sales_df=sales,
                                  date = 'sale_date',
                                  periodicity='monthly',
                                  max_date = as.Date('2016-12-21'),
                                  adj_type='clip'), 'max_date') ==
                  '2016-12-21')
    expect_true(attr(dateToPeriod(sales_df=sales,
                                  date = 'sale_date',
                                  periodicity='monthly',
                                  max_date = as.Date('2016-12-29'),
                                  adj_type='clip'), 'max_date') ==
                  '2016-12-29')
  })

  # Test Annual
  test_that('Annual periodicity works', {
    expect_true(dateToPeriod(sales_df=sales,
                             date = 'sale_date',
                             periodicity='annual')$date_period[1] == 4)
    expect_true(dateToPeriod(sales_df=sales,
                             date = 'sale_date',
                             min_date = as.Date('2009-12-31'),
                             periodicity='annual')$date_period[1] == 5)
    expect_true(dateToPeriod(sales_df=sales,
                             date = 'sale_date',
                             min_date = as.Date('2011-12-31'),
                             adj_type = 'clip',
                             periodicity='annual')$date_period[1] == 3)
  })

  # Test Monthly
  test_that('Monthly periodicity works', {
    expect_true(dateToPeriod(sales_df=sales,
                             date = 'sale_date',
                             periodicity='monthly')$date_period[1] == 38)
    expect_true(dateToPeriod(sales_df=sales,
                             date = 'sale_date',
                             min_date = as.Date('2009-12-31'),
                             periodicity='monthly')$date_period[1] == 39)
    expect_true(dateToPeriod(sales_df=sales,
                             date = 'sale_date',
                             min_date = as.Date('2011-12-31'),
                             adj_type = 'clip',
                             periodicity='monthly')$date_period[1] == 15)
  })

  # Test Quarterly
  test_that('Quarterly periodicity works', {
    expect_true(dateToPeriod(sales_df=sales,
                             date = 'sale_date',
                             periodicity='Q')$date_period[1] == 13)
    expect_true(dateToPeriod(sales_df=sales,
                             date = 'sale_date',
                             min_date = as.Date('2009-12-31'),
                             periodicity='Q')$date_period[1] == 14)
    expect_true(dateToPeriod(sales_df=sales,
                             date = 'sale_date',
                             min_date = as.Date('2011-12-31'),
                             adj_type = 'clip',
                             periodicity='q')$date_period[1] == 6)
    expect_true(dateToPeriod(sales_df=sales,
                             date = 'sale_date',
                             min_date = as.Date('2012-01-01'),
                             adj_type = 'clip',
                             periodicity='q')$date_period[1] == 5)

  })

  # Test Weekly
  test_that('Weekly periodicity works', {
    expect_true(dateToPeriod(sales_df=sales,
                             date = 'sale_date',
                             periodicity='w')$date_period[1] == 162)
    expect_true(dateToPeriod(sales_df=sales,
                             date = 'sale_date',
                             min_date = as.Date('2009-12-31'),
                             periodicity='W')$date_period[1] == 163)
    expect_true(dateToPeriod(sales_df=sales,
                             date = 'sale_date',
                             min_date = as.Date('2011-12-31'),
                             adj_type = 'clip',
                             periodicity='weekly')$date_period[1] == 59)
    expect_true(dateToPeriod(sales_df=sales,
                             date = 'sale_date',
                             min_date = as.Date('2012-01-01'),
                             adj_type = 'clip',
                             periodicity='Weekly')$date_period[1] == 58)

  })

  # Test Missing period check
  test_that('Missing period warning work', {
    expect_message(dateToPeriod(sales_df=sales,
                                date = 'sale_date',
                                max_date = as.Date('2018-03-31'),
                                periodicity='M')$date_period[1])
    expect_message(dateToPeriod(sales_df=sales,
                                date = 'sale_date',
                                min_date = as.Date('2005-03-31'),
                                periodicity='M'))
  })

## Setup rsCreateSales -------------------------------------------------------------------

  context('rsCreateSales')

  # Test Setup
  test_that("Can take a functional 'salesdf' object", {

    sales_df <- dateToPeriod(sales_df = sales,
                             date = 'sale_date',
                             periodicity = 'monthly')
    expect_is(rs_df <- rsCreateSales(sales_df=sales_df,
                                     prop_id='pinx',
                                     sale_id='sale_id',
                                     price='sale_price'), 'rs')
    expect_true(nrow(rs_df) == 5102)
  })

  # Test Setup
  test_that("Can create own salesdf object", {

    expect_is(rs_df <- rsCreateSales(sales_df=sales,
                                     prop_id='pinx',
                                     sale_id='sale_id',
                                     price='sale_price',
                                     date='sale_date',
                                     periodicity='monthly'), 'rs')
    expect_true(nrow(rs_df) == 5102)
  })

  test_that("Can use min/max dates own salesdf object", {

    expect_is(rs_df <- rsCreateSales(sales_df=sales,
                                     prop_id='pinx',
                                     sale_id='sale_id',
                                     price='sale_price',
                                     date='sale_date',
                                     periodicity='monthly',
                                     min_date = as.Date('2012-03-21')), 'rs')
    expect_true(nrow(rs_df) == 5102)

    expect_is(rs_df <- rsCreateSales(sales_df=sales,
                                     prop_id='pinx',
                                     sale_id='sale_id',
                                     price='sale_price',
                                     date='sale_date',
                                     periodicity='monthly',
                                     min_date = as.Date('2012-03-21'),
                                     adj_type='clip'), 'rs')
    expect_true(nrow(rs_df) == 2827)

    expect_is(rs_df <- rsCreateSales(sales_df=sales,
                                     prop_id='pinx',
                                     sale_id='sale_id',
                                     price='sale_price',
                                     date='sale_date',
                                     periodicity='monthly',
                                     max_date = as.Date('2015-03-21')), 'rs')
    expect_true(nrow(rs_df) == 5102)

    expect_is(rs_df <- rsCreateSales(sales_df=sales,
                                     prop_id='pinx',
                                     sale_id='sale_id',
                                     price='sale_price',
                                     date='sale_date',
                                     periodicity='monthly',
                                     max_date = as.Date('2014-03-21'),
                                     adj_type='clip'), 'rs')
    expect_true(nrow(rs_df) == 1148)

  })

  test_that("Fails if sales creation fails", {

    # Bad Date field
    expect_error(rs_df <- rsCreateSales(sales_df=sales,
                                     prop_id='pinx',
                                     sale_id='sale_id',
                                     price='sale_price',
                                     date='sale_price',
                                     periodicity='monthly'))

    # Bad Periodicity field
    expect_error(rs_df <- rsCreateSales(sales_df=sales,
                                        prop_id='pinx',
                                        sale_id='sale_id',
                                        price='sale_price',
                                        date='sale_date',
                                        periodicity='mocnthly'))

  })

  test_that("Fails if bad arguments fails", {

    # Bad prop_id field
    expect_error(rs_df <- rsCreateSales(sales_df=sales_df,
                                        prop_id='pinxx',
                                        sale_id='sale_id',
                                        price='sale_price'))

    # Bad sale_id field
    expect_error(rs_df <- rsCreateSales(sales_df=sales_df,
                                        prop_id='pinx',
                                        sale_id='salex_id',
                                        price='sale_price'))

    # Bad price field
    expect_error(rs_df <- rsCreateSales(sales_df=sales_df,
                                        prop_id='pinx',
                                        sale_id='sale_id',
                                        price='salex_price'))

  })

  test_that("Returns NULL if no repeat sales", {

    expect_is(rs_df <- rsCreateSales(sales_df=sales_df[!duplicated(sales_df$prop_id),],
                                        prop_id='pinx',
                                        sale_id='sale_id',
                                        price='sale_price'), "NULL")

    expect_is(rs_df <- rsCreateSales(sales_df=sales_df[1:3, ],
                                     prop_id='pinx',
                                     sale_id='sale_id',
                                     price='sale_price'), "NULL")
  })
