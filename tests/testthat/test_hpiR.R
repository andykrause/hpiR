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


## Test rsTimeMatrix ---------------------------------------------------------------------

 context('rsTimeMatrix')
 rs_df <- rsCreateSales(sales_df=sales_df,
                        prop_id='pinx',
                        sale_id='sale_id',
                        price='sale_price')

 test_that('Time matrix works with clean data', {
  expect_is(time_matrix <- rsTimeMatrix(rs_df), 'timematrix')
 })

 test_that('Time matrix fails without rs data', {
   expect_is(time_matrix <- rsTimeMatrix(sales_df), 'NULL')
 })

 test_that('Time matrix size is correct', {
   expect_true(nrow(rsTimeMatrix(rs_df[1:2000,])) == 2000)
   expect_true(ncol(rsTimeMatrix(rs_df[1:2000,])) == nrow(attr(rs_df,
                                                               'period_table')) - 1)
 })

## Test hpiModel.rs up to rsModel --------------------------------------------------------

 test_that('hpiModel.rs works',{
   expect_is(rs_model <- hpiModel(hpi_data = rs_df,
                                  estimator = 'base',
                                  log_dep = TRUE), 'hpimodel')
 })

 test_that('"log_dep" works both ways',{
  expect_true(hpiModel(hpi_data = rs_df,
                       estimator = 'base',
                       log_dep = TRUE)$model_obj$fitted.values[1] < 1)
  expect_true(hpiModel(hpi_data = rs_df,
                       estimator = 'base',
                       log_dep = FALSE)$model_obj$fitted.values[1] > 10000)
  })

  test_that('Check for zero or negative prices works',{
    rs_dfx <- rs_df
    rs_dfx$price_1 <- 0
    expect_is(rs_model <- hpiModel(hpi_data = rs_dfx,
                                   estimator = 'base',
                                   log_dep = TRUE), 'NULL')
    expect_is(rs_model <- hpiModel(hpi_data = rs_dfx,
                                   estimator = 'base',
                                   log_dep = FALSE), 'hpimodel')
    rs_dfx$price_1 <- NA_integer_
    expect_is(rs_model <- hpiModel(hpi_data = rs_dfx,
                                   estimator = 'base',
                                   log_dep = TRUE), 'NULL')
    expect_is(rs_model <- hpiModel(hpi_data = rs_dfx,
                                   estimator = 'base',
                                   log_dep = FALSE), 'NULL')
    rs_dfx$price_1 <- Inf
    expect_is(rs_model <- hpiModel(hpi_data = rs_dfx,
                                   estimator = 'base',
                                   log_dep = TRUE), 'NULL')
    expect_is(rs_model <- hpiModel(hpi_data = rs_dfx,
                                   estimator = 'base',
                                   log_dep = FALSE), 'NULL')

  })

  test_that('Check for estimator type works',{
    expect_is(rs_model <- hpiModel(hpi_data = rs_df), 'hpimodel')
    expect_true(hpiModel(hpi_data = rs_df)$estimator == 'base')
    expect_true(hpiModel(hpi_data = rs_df,
                         estimator='xxxx')$estimator == 'base')
    expect_true(hpiModel(hpi_data = rs_df,
                         estimator='robust')$estimator == 'robust')
    expect_true(hpiModel(hpi_data = rs_df,
                         estimator='weighted')$estimator == 'weighted')
  })


## Test rsModel --------------------------------------------------------------------------

 context('rsModel')
  time_matrix <- rsTimeMatrix(rs_df)
  price_diff_l <- log(rs_df$price_2) - log(rs_df$price_1)
  price_diff <- rs_df$price_2 - rs_df$price_1

  test_that('Check for errors with bad arguments',{

    # Base works with wrong  hpi_data because it isn't used
    expect_is(rs_model <- rsModel(rs_df = sales,
                                  time_matrix = time_matrix,
                                  price_diff = price_diff_l,
                                  estimator=structure('base', class='base')),
              'rsmod')
    # Robust doesn't
    expect_error(rs_model <- rsModel(rs_df = sales,
                                     time_matrix = time_matrix,
                                     price_diff = price_diff_l,
                                     estimator=structure('robust', class='robust')))
    # Weighted doesn't
    expect_error(rs_model <- rsModel(rs_df = sales,
                                     time_matrix = time_matrix,
                                     price_diff = price_diff_l,
                                     estimator=structure('weighted', class='weighted')))

    # Test bad time_matrix
    expect_error(rs_model <- rsModel(rs_df = rs_df,
                                     time_matrix = sales,
                                     price_diff = price_diff_l,
                                     estimator=structure('base', class='base')))

    # Bad price_diff
    expect_error(rs_model <- rsModel(rs_df = rs_df,
                                     time_matrix = time_matrix,
                                     price_diff = sales,
                                     estimator=structure('base', class='base')))

    # Bad estimator class
    expect_error(rs_model <- rsModel(rs_df = rs_df,
                                     time_matrix = time_matrix,
                                     price_diff = price_diff,
                                     estimator=structure('base', class='xxx')))

  })

  test_that('Performance with sparse data',{

    rs_dfx <- rs_df[1:20, ]
    time_matrixx <- rsTimeMatrix(rs_dfx)
    price_diff_lx <- log(rs_dfx$price_2) - log(rs_dfx$price_1)
    price_diffx <- rs_dfx$price_2 - rs_dfx$price_1

    # Works with base, though many NAs
    expect_is(rs_model <- rsModel(rs_df = rs_dfx,
                                  time_matrix = time_matrixx,
                                  price_diff = price_diff_lx,
                                  estimator=structure('base', class='base')),
              'rsmod')

    # Robust works but gives warning
    expect_warning(rs_model <- rsModel(rs_df = rs_dfx,
                                       time_matrix = time_matrixx,
                                       price_diff = price_diff_lx,
                                      estimator=structure('robust', class='robust')))

    # Weighted works
    expect_is(rs_model <- rsModel(rs_df = rs_dfx,
                                  time_matrix = time_matrixx,
                                  price_diff = price_diffx,
                                  estimator=structure('weighted', class='weighted')),
              'rsmod')

    # Fails with non-matching tm and pd
    expect_error(rs_model <- rsModel(rs_df = rs_dfx,
                                     time_matrix = time_matrixx,
                                     price_diff = price_diff,
                                     estimator=structure('weighted', class='weighted')))
  })

## Test hpiModel.rs after rsModel --------------------------------------------------------

 context('hpiModel.rs')

  test_that('hpiModel.rs works in both trim_model cases',{
    expect_is(rs_model <- hpiModel(hpi_data = rs_df,
                                   estimator = 'base',
                                   log_dep = TRUE,
                                   trim_model=TRUE), 'hpimodel')
    expect_is(rs_model <- hpiModel(hpi_data = rs_df,
                                   estimator = 'base',
                                   log_dep = TRUE,
                                   trim_model=FALSE), 'hpimodel')
    expect_is(rs_model <- hpiModel(hpi_data = rs_df,
                                   estimator = 'base',
                                   log_dep = FALSE,
                                   trim_model=TRUE), 'hpimodel')
    expect_is(rs_model <- hpiModel(hpi_data = rs_df,
                                   estimator = 'base',
                                   log_dep = FALSE,
                                   trim_model=FALSE), 'hpimodel')
    expect_is(rs_model <- hpiModel(hpi_data = rs_df,
                                   estimator = 'weighted',
                                   log_dep = TRUE,
                                   trim_model=FALSE), 'hpimodel')
    expect_is(rs_model <- hpiModel(hpi_data = rs_df,
                                   estimator = 'robust',
                                   log_dep = FALSE,
                                   trim_model=TRUE), 'hpimodel')
    expect_true(is.null(hpiModel(hpi_data = rs_df,
                                 estimator = 'weighted',
                                 log_dep = TRUE,
                                 trim_model=TRUE)$model_obj$qr))
    expect_true(!is.null(hpiModel(hpi_data = rs_df,
                                  estimator = 'weighted',
                                  log_dep = TRUE,
                                  trim_model=FALSE)$model_obj$qr))
  })

  test_that('hpiModel.rs outputs are correct',{
    rs_model_base <- hpiModel(hpi_data = rs_df,
                              estimator = 'base',
                              log_dep = TRUE,
                              trim_model=TRUE)
    rs_model_robust <- hpiModel(hpi_data = rs_df,
                                estimator = 'robust',
                                log_dep = TRUE,
                                trim_model=FALSE)
    rs_model_wgt <- hpiModel(hpi_data = rs_df,
                             estimator = 'weighted',
                             log_dep = FALSE,
                             trim_model=TRUE)

    # Estimators
    expect_is(rs_model_base$estimator, 'base')
    expect_is(rs_model_robust$estimator, 'robust')
    expect_is(rs_model_wgt$estimator, 'weighted')

    # Coefficients
    expect_is(rs_model_base$coefficients, 'data.frame')
    expect_is(rs_model_robust$coefficients, 'data.frame')
    expect_is(rs_model_wgt$coefficients, 'data.frame')
    expect_true(nrow(rs_model_base$coefficients) == 84)
    expect_true(max(rs_model_robust$coefficients$time) == 84)
    expect_true(rs_model_wgt$coefficients$coefficient[1] == 0)

    # Modelobj
    expect_is(rs_model_base$model_obj, 'rsmod')
    expect_is(rs_model_robust$model_obj, 'rsmod')
    expect_is(rs_model_wgt$model_obj, 'rsmod')

    # Model spec
    expect_true(is.null(rs_model_base$model_spec))
    expect_true(is.null(rs_model_robust$model_spec))
    expect_true(is.null(rs_model_wgt$model_spec))

    # base price
    expect_true(round(rs_model_base$base_price, 0) == 427785)
    expect_true(round(rs_model_robust$base_price, 0) == 427785)
    expect_true(round(rs_model_wgt$base_price, 0) == 427785)

    # Periods
    expect_is(rs_model_base$periods, 'data.frame')
    expect_is(rs_model_robust$periods, 'data.frame')
    expect_is(rs_model_wgt$periods, 'data.frame')

    # Approach
    expect_true(rs_model_base$approach == 'rs')
    expect_true(rs_model_robust$approach == 'rs')
    expect_true(rs_model_weighted$approach == 'rs')


  })
