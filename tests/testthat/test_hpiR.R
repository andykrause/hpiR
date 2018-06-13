#*****************************************************************************************
#
#   Unit tests for hpiR package
#
#*****************************************************************************************

  library(hpiR)
  library(testthat)

  ## Load Data

  data(seattle_sales)
  sales <- seattle_sales

## Setup ---------------------------------------------------------------------------------

  context('dateToPeriod')

  # Test Setup
  test_that("Basic dateToPeriod() functionality is working", {

    sales_df <- dateToPeriod(sales_df = sales,
                             date = 'sale_date',
                             periodicity = 'monthly')
    expect_is(sales_df, 'salesdf')
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

  # Bad Date arguments
  test_that('Bad date arguments are converted or give errors', {

    # Conversion works for a single string/date
    expect_is(dateToPeriod(sales_df=sales,
                           date = 'sale_date',
                           min_date = '2011-11-01',
                           max_date = Sys.time()),
              'salesdf')

    # Conversion works for a vector
    sales_x <- sales
    sales_x$sale_date <- as.character(sales_x$sale_date)
    expect_is(dateToPeriod(sales_df=sales_x,
                           date = 'sale_date',
                           min_date = '2011-11-01',
                           max_date = Sys.time()),
              'salesdf')

    # Conversion fails for a bad string
    expect_error(dateToPeriod(sales_df=sales,
                              date = 'sale_date',
                              min_date = 'xxx'))

    # Conversion fails for a bad vector
    expect_error(dateToPeriod(sales_df=sales,
                              date = 'sale_price'))

  })


## Test rsCreateSales() ------------------------------------------------------------------

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

  test_that("Sequence only (seq_only) option works", {

    expect_is(rs_df <- rsCreateSales(sales_df=sales,
                                     prop_id='pinx',
                                     sale_id='sale_id',
                                     price='sale_price',
                                     date='sale_date',
                                     periodicity='monthly',
                                     seq_only = TRUE),
              'rs')

    expect_true(nrow(rs_df) == 4823)

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

  # Create a sales_df in the global env for future use
  sales_df <- dateToPeriod(sales_df = sales,
                           date = 'sale_date',
                           periodicity = 'monthly')

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

 # Creat a full rs_data object in global environment
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

 test_that('Time matrix size is the correct size', {
   expect_true(nrow(rsTimeMatrix(rs_df[1:2000,])) == 2000)
   expect_true(ncol(rsTimeMatrix(rs_df[1:2000,])) == nrow(attr(rs_df,
                                                               'period_table')) - 1)
 })

## Test hpiModel.rs up to rsModel --------------------------------------------------------

 context('hpiModel.rs')

 test_that('hpiModel.rs works',{
   expect_is(rs_model <- hpiModel(hpi_data = rs_df,
                                  estimator = 'base',
                                  log_dep = TRUE),
             'hpimodel')
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

  # Create matrix and difference vectors in global environment
  time_matrix <- rsTimeMatrix(rs_df)
  price_diff_l <- log(rs_df$price_2) - log(rs_df$price_1)
  price_diff <- rs_df$price_2 - rs_df$price_1

  test_that('Check for errors with bad arguments',{

    # Base: Return warning if wrong rs_df
    expect_is(rs_model <- rsModel(rs_df = sales,
                                  time_matrix = time_matrix,
                                  price_diff = price_diff_l,
                                  estimator=structure('base', class='base')),
              'NULL')

    # Robust: Return warning if wrong time_matrix
    expect_is(rs_model <- rsModel(rs_df = rs_df,
                                  time_matrix = sales,
                                  price_diff = price_diff_l,
                                  estimator=structure('robust', class='robust')),
              'NULL')

    # Weighted: Dimensions of data do not match
    expect_is(rs_model <- rsModel(rs_df = rs_df,
                                  time_matrix = time_matrix,
                                  price_diff = price_diff_l[-1],
                                  estimator=structure('weighted', class='weighted')),
                 'NULL')

    # Bad estimator class
    expect_is(rs_model <- rsModel(rs_df = rs_df,
                                  time_matrix = time_matrix,
                                  price_diff = price_diff,
                                  estimator=structure('base', class='xxx')),
              'NULL')

  })

  test_that('Performance with sparse data',{

   ## Moderate Sparseness

    # Create a sparse data set
    rs_df200 <- rs_df[1:200, ]
    time_matrix200 <- rsTimeMatrix(rs_df200)
    price_diff_l200 <- log(rs_df200$price_2) - log(rs_df200$price_1)
    price_diff200 <- rs_df200$price_2 - rs_df200$price_1

    # Works with base, though many NAs
    expect_is(rs_model <- rsModel(rs_df = rs_df200,
                                  time_matrix = time_matrix200,
                                  price_diff = price_diff_l200,
                                  estimator=structure('base', class='base')),
              'rsmod')

    # Robust works but gives warning
    expect_is(rs_model <- rsModel(rs_df = rs_df200,
                                       time_matrix = time_matrix200,
                                       price_diff = price_diff_l200,
                                      estimator=structure('robust', class='robust')),
              'rsmod')

    # Weighted works
    expect_is(rs_model <- rsModel(rs_df = rs_df200,
                                  time_matrix = time_matrix200,
                                  price_diff = price_diff200,
                                  estimator=structure('weighted', class='weighted')),
              'rsmod')

    ## Check severe sparseness

    # Create data set
    rs_df20 <- rs_df[1:20, ]
    time_matrix20 <- rsTimeMatrix(rs_df20)
    price_diff_l20 <- log(rs_df20$price_2) - log(rs_df20$price_1)
    price_diff20 <- rs_df20$price_2 - rs_df20$price_1

    # Base: Works, but gives message
    expect_is(rs_model <- rsModel(rs_df = rs_df20,
                                  time_matrix = time_matrix20,
                                  price_diff = price_diff_l20,
                                  estimator=structure('base', class='base')),
              'rsmod')

    # Robust: Works, but gives warning from lmrob()
    expect_warning(rs_model <- rsModel(rs_df = rs_df20,
                                       time_matrix = time_matrix20,
                                       price_diff = price_diff_l20,
                                       estimator=structure('robust', class='robust')))

    # Weighted: works but gives message
    expect_is(rs_model <- rsModel(rs_df = rs_df20,
                                  time_matrix = time_matrix20,
                                  price_diff = price_diff20,
                                  estimator=structure('weighted', class='weighted')),
              'rsmod')

  })

## Test hpiModel.rs after rsModel --------------------------------------------------------

 context('hpiModel.rs')

  test_that('hpiModel.rs works in both trim_model cases', {
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

  test_that('hpiModel.rs outputs are correct', {

    # Run a model of each estimator type
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
    expect_true(nrow(rs_model_base$periods) == 84)
    expect_is(rs_model_robust$periods, 'data.frame')
    expect_true(nrow(rs_model_robust$periods) == 84)
    expect_is(rs_model_wgt$periods, 'data.frame')
    expect_true(nrow(rs_model_wgt$periods) == 84)

    # Approach
    expect_true(rs_model_base$approach == 'rs')
    expect_true(rs_model_robust$approach == 'rs')
    expect_true(rs_model_wgt$approach == 'rs')
  })

### Test modelToIndex --------------------------------------------------------------------

  context('modelToIndex')

  rs_model <- hpiModel(hpi_data = rs_df,
                       estimator = 'base',
                       log_dep = TRUE,
                       trim_model=TRUE)

  test_that('modelToIndex works', {

    expect_is(modelToIndex(rs_model), 'hpiindex')

  })

  test_that('modelToIndex works with other estimators and options', {

    # Robust, LogDep=T, TrimModel=T
    expect_is(modelToIndex(hpiModel(hpi_data = rs_df,
                                    estimator = 'robust',
                                    log_dep = TRUE,
                                    trim_model=TRUE)), 'hpiindex')

    # Weighted, LogDep=T, TrimModel=T
    expect_is(modelToIndex(hpiModel(hpi_data = rs_df,
                                    estimator = 'weighted',
                                    log_dep = TRUE,
                                    trim_model=TRUE)), 'hpiindex')

    # Robust, LogDep=F, TrimModel=T
    expect_is(modelToIndex(hpiModel(hpi_data = rs_df,
                                    estimator = 'robust',
                                    log_dep = FALSE,
                                    trim_model=TRUE)), 'hpiindex')

    # Weighted, LogDep=T, TrimModel=F
    expect_is(modelToIndex(hpiModel(hpi_data = rs_df,
                                    estimator = 'weighted',
                                    log_dep = TRUE,
                                    trim_model=FALSE)), 'hpiindex')
  })

  test_that('modelToIndex fails with a NULL',{
    expect_true(is.null(modelToIndex(hpimodel = 'abc')))
  })

  test_that('modelToIndex imputes properly, BASE model, LogDEP',{

    model_base <- hpiModel(hpi_data = rs_df,
                           estimator = 'base',
                           log_dep = TRUE,
                           trim_model=TRUE)

    # Impute a beginning value
    model_ex <- model_base
    model_ex$coefficients$coefficient[2] <- NA_real_
    expect_is(modelToIndex(model_ex), 'hpiindex')
    expect_true(!is.na(modelToIndex(model_ex)$index[2]))
    expect_true(modelToIndex(model_ex)$index[2] == 100)
    expect_true(modelToIndex(model_ex)$imputed[2] == 1)

    # Interpolate interior values
    model_ex <- model_base
    model_ex$coefficients$coefficient[3:5] <- NA_real_
    expect_is(modelToIndex(model_ex), 'hpiindex')
    expect_true(all(!is.na(modelToIndex(model_ex)$index[3:5])))

    # Extrapolate end periods
    model_ex <- model_base
    model_ex$coefficients$coefficient[81:84] <- NA_real_
    expect_is(modelToIndex(model_ex), 'hpiindex')
    expect_true(all(!is.na(modelToIndex(model_ex)$index[81:84])))
    expect_true(modelToIndex(model_ex)$index[80] ==
                  modelToIndex(model_ex)$index[84])

  })

  test_that('modelToIndex imputes properly, BASE model, LogDep=FALSE',{

    model_base <- hpiModel(hpi_data = rs_df,
                           estimator = 'base',
                           log_dep = FALSE,
                           trim_model=TRUE)

    # Extrapolate a beginning value
    model_ex <- model_base
    model_ex$coefficients$coefficient[2] <- NA_real_
    expect_is(modelToIndex(model_ex), 'hpiindex')
    expect_true(!is.na(modelToIndex(model_ex)$index[2]))
    expect_true(modelToIndex(model_ex)$index[2] == 100)
    expect_true(modelToIndex(model_ex)$imputed[2] == 1)

    # Impute interior values
    model_ex <- model_base
    model_ex$coefficients$coefficient[3:5] <- NA_real_
    expect_is(modelToIndex(model_ex), 'hpiindex')
    expect_true(all(!is.na(modelToIndex(model_ex)$index[3:5])))

    # Extrapolate an end value
    model_ex <- model_base
    model_ex$coefficients$coefficient[81:84] <- NA_real_
    expect_is(modelToIndex(model_ex), 'hpiindex')
    expect_true(all(!is.na(modelToIndex(model_ex)$index[81:84])))
    expect_true(modelToIndex(model_ex)$index[80] ==
                  modelToIndex(model_ex)$index[84])

  })

  test_that('modelToIndex imputes properly, Robust model, LogDEP = TRUE',{

    model_base <- hpiModel(hpi_data = rs_df,
                           estimator = 'robust',
                           log_dep = TRUE,
                           trim_model=TRUE)

    model_ex <- model_base
    model_ex$coefficients$coefficient[2] <- NA_real_
    expect_is(modelToIndex(model_ex), 'hpiindex')
    expect_true(!is.na(modelToIndex(model_ex)$index[2]))
    expect_true(modelToIndex(model_ex)$index[2] == 100)
    expect_true(modelToIndex(model_ex)$imputed[2] == 1)

    model_ex <- model_base
    model_ex$coefficients$coefficient[3:5] <- NA_real_
    expect_is(modelToIndex(model_ex), 'hpiindex')
    expect_true(all(!is.na(modelToIndex(model_ex)$index[3:5])))

    model_ex <- model_base
    model_ex$coefficients$coefficient[81:84] <- NA_real_
    expect_is(modelToIndex(model_ex), 'hpiindex')
    expect_true(all(!is.na(modelToIndex(model_ex)$index[81:84])))
    expect_true(modelToIndex(model_ex)$index[80] ==
                  modelToIndex(model_ex)$index[84])

  })

  test_that('modelToIndex imputes properly, Weighted model, LogDep=FALSE',{

    model_base <- hpiModel(hpi_data = rs_df,
                           estimator = 'weighted',
                           log_dep = FALSE,
                           trim_model=TRUE)

    model_ex <- model_base
    model_ex$coefficients$coefficient[2] <- NA_real_
    expect_is(modelToIndex(model_ex), 'hpiindex')
    expect_true(!is.na(modelToIndex(model_ex)$index[2]))
    expect_true(modelToIndex(model_ex)$index[2] == 100)
    expect_true(modelToIndex(model_ex)$imputed[2] == 1)

    model_ex <- model_base
    model_ex$coefficients$coefficient[3:5] <- NA_real_
    expect_is(modelToIndex(model_ex), 'hpiindex')
    expect_true(all(!is.na(modelToIndex(model_ex)$index[3:5])))

    model_ex <- model_base
    model_ex$coefficients$coefficient[81:84] <- NA_real_
    expect_is(modelToIndex(model_ex), 'hpiindex')
    expect_true(all(!is.na(modelToIndex(model_ex)$index[81:84])))
    expect_true(modelToIndex(model_ex)$index[80] ==
                  modelToIndex(model_ex)$index[84])

  })

  test_that('modelToIndex "max_period" cutoff works',{

    model_base <- hpiModel(hpi_data = rs_df,
                           estimator = 'weighted',
                           log_dep = FALSE,
                           trim_model=TRUE)

    # Create shortened index
    index_80 <- modelToIndex(model_base, max_period = 80)
    expect_is(index_80, 'hpiindex')
    expect_true(length(index_80$index) == 80)

  })

### Test rsIndex() wrapper ---------------------------------------------------------------

 context('rsindex() wrapper')

 test_that('Function works with proper inputs',{

   # Full case
   full_1 <- rsIndex(sales_df = sales,
                     date = 'sale_date',
                     price = 'sale_price',
                     sale_id = 'sale_id',
                     prop_id = 'pinx',
                     estimator = 'base',
                     log_dep = TRUE,
                     periodicity = 'monthly')

   expect_is(full_1, 'hpi')
   expect_true(full_1$model$estimator == 'base')

   # Giving a 'sales_df' object
   full_2 <- rsIndex(sales_df = sales_df,
                     price = 'sale_price',
                     sale_id = 'sale_id',
                     prop_id = 'pinx',
                     estimator = 'robust',
                     log_dep = TRUE)

   expect_is(full_2, 'hpi')
   expect_true(full_2$model$estimator == 'robust')

   # Giving an 'rs_df' object
   full_3 <- rsIndex(sales_df = rs_df,
                     estimator = 'weighted',
                     log_dep = TRUE)

   expect_is(full_3, 'hpi')
   expect_true(full_3$model$estimator == 'weighted')

 })

 test_that('Additional arguments in rsIndex() work',{

  ## RS Create arguments

   # Min Date Model with Clip
   mindate_index <- rsIndex(sales_df = sales,
                            date = 'sale_date',
                            price = 'sale_price',
                            sale_id = 'sale_id',
                            prop_id = 'pinx',
                            min_date = as.Date('2011-01-01'),
                            adj_type = 'clip')
   expect_true(min(mindate_index$index$period) == 2011)

   # Max Date Model with Adjust
   maxdate_index <- rsIndex(sales_df = sales,
                            date = 'sale_date',
                            price = 'sale_price',
                            sale_id = 'sale_id',
                            prop_id = 'pinx',
                            max_date = as.Date('2015-12-31'))
   expect_true(max(maxdate_index$index$period) == 2016)

   # Periodicity
   per_index <- rsIndex(sales_df = sales,
                        date = 'sale_date',
                        price = 'sale_price',
                        sale_id = 'sale_id',
                        prop_id = 'pinx',
                        periodicity = 'weekly')
   expect_true(max(per_index$index$period) == 364)

   # Sequence Only
   seq_index <- rsIndex(sales_df = sales_df,
                        date = 'sale_date',
                        price = 'sale_price',
                        sale_id = 'sale_id',
                        prop_id = 'pinx',
                        seq_only = TRUE)
   expect_true(nrow(seq_index$data) == 4823)

  ## HPI Model

   # Trim Model
   trim_index <- rsIndex(sales_df = rs_df,
                         trim_model=TRUE)
   expect_true(is.null(trim_index$model$model_obj$qr))

   # Log Dep & Robust
   ld_index <- rsIndex(sales_df = rs_df,
                       estimator = 'robust',
                       log_dep = FALSE)
   expect_true(ld_index$model$log_dep == FALSE)
   expect_true(ld_index$model$estimator == 'robust')

  ## Model to Index

   m2i_index <- rsIndex(sales_df = rs_df,
                        estimator = 'robust',
                        log_dep = FALSE,
                        max_period = 80)
   expect_true(length(m2i_index$index$index) == 80)

 })



#
 test_that("Bad arguments generate NULLs: Full Case",{

   expect_error(rsIndex(sales_df = sales,
                        date = 'sale_price',
                        price = 'sale_price',
                        sale_id = 'sale_id',
                        prop_id = 'pinx',
                        estimator = 'base',
                        log_dep = TRUE,
                        periodicity = 'monthly'))

   expect_error(rsIndex(sales_df = sales,
                       date = 'sale_date',
                       price = 'sale_price',
                       sale_id = 'sale_id',
                       prop_id = 'pinx',
                       estimator = 'base',
                       log_dep = TRUE,
                       periodicity = 'xxx'))

 })

 test_that("Bad arguments generate errors: Sales_df Case",{

   expect_error(rsIndex(sales_df = sales_df,
                        price = 'xx',
                        sale_id = 'sale_id',
                        prop_id = 'pinx',
                        estimator = 'base',
                        log_dep = TRUE))
   expect_error(rsIndex(sales_df = sales_df,
                        price = 'sale_price',
                        sale_id = 'xx',
                        prop_id = 'pinx',
                        estimator = 'base',
                        log_dep = TRUE))
   expect_error(rsIndex(sales_df = sales_df,
                        price = 'sale_price',
                        sale_id = 'sale_id',
                        prop_id = 'xx',
                        estimator = 'base',
                        log_dep = TRUE))
 })

 test_that("Bad arguments handling: rs_sales Case",{

   # Bad estimators default to 'base'
   expect_true(rsIndex(sales_df = rs_df,
                       estimator = 'basex',
                       log_dep = TRUE)$model$estimator == 'base')

   expect_error(rsIndex(sales_df = rs_df,
                        estimator = 'robust',
                        log_dep = 'a'))

   expect_error(rsIndex(sales_df = rs_df,
                        estimator = 'robust',
                        trim_model = 'a'))

   expect_error(rsIndex(sales_df = rs_df,
                        estimator = 'robust',
                        max_period = 'a'))
 })



### Test hedCreateSales() ----------------------------------------------------------------

 context('hedCreateSales')

 # Test Setup
 test_that("Can take a functional 'salesdf' object", {

    sales_df <- dateToPeriod(sales_df = sales,
                             date = 'sale_date',
                             periodicity = 'monthly')

    expect_is(hed_df <- hedCreateSales(sales_df=sales_df,
                                       prop_id='pinx',
                                       sale_id='sale_id',
                                       price='sale_price'), 'hed')
    expect_true(nrow(hed_df) == 43074)
 })

 # Test Setup
  test_that("Can create own salesdf object", {

   expect_is(hed_df <- hedCreateSales(sales_df=sales,
                                      prop_id='pinx',
                                      sale_id='sale_id',
                                      price='sale_price',
                                      date='sale_date',
                                      periodicity='monthly'), 'hed')
   expect_true(nrow(hed_df) == 43074)
   assign('hed_df', hed_df, .GlobalEnv)
  })

 test_that("Can use min/max dates own salesdf object", {

   # Min date with move

   expect_is(hed_df <- hedCreateSales(sales_df=sales,
                                       prop_id='pinx',
                                       sale_id='sale_id',
                                       price='sale_price',
                                       date='sale_date',
                                       periodicity='monthly',
                                       min_date = as.Date('2012-03-21')),
              'hed')
   expect_true(nrow(hed_df) == 43074)

   # Min date with adj
   expect_is(hed_df <- hedCreateSales(sales_df=sales,
                                      prop_id='pinx',
                                      sale_id='sale_id',
                                      price='sale_price',
                                      date='sale_date',
                                      periodicity='monthly',
                                      min_date = as.Date('2012-03-21'),
                                      adj_type='clip'), 'hed')
   expect_true(nrow(hed_df) == 33922)

   # Max with move
   expect_is(hed_df <- hedCreateSales(sales_df=sales,
                                      prop_id='pinx',
                                      sale_id='sale_id',
                                      price='sale_price',
                                      date='sale_date',
                                      periodicity='monthly',
                                      max_date = as.Date('2015-03-21')),
             'hed')
   expect_true(nrow(hed_df) == 43074)

   # Max with clip
   expect_is(hed_df <- hedCreateSales(sales_df=sales,
                                      prop_id='pinx',
                                      sale_id='sale_id',
                                      price='sale_price',
                                      date='sale_date',
                                      periodicity='monthly',
                                      max_date = as.Date('2014-03-21'),
                                      adj_type='clip'),
             'hed')
   expect_true(nrow(hed_df) == 21536)

 })

 test_that("Fails if sales creation fails", {

   # Bad Date field
   expect_error(hed_df <- hedCreateSales(sales_df=sales,
                                         prop_id='pinx',
                                         sale_id='sale_id',
                                         price='sale_price',
                                         date='sale_price',
                                         periodicity='monthly'))

   # Bad Periodicity field
   expect_error(hed_df <- hedCreateSales(sales_df=sales,
                                         prop_id='pinx',
                                         sale_id='sale_id',
                                         price='sale_price',
                                         date='sale_date',
                                         periodicity='mocnthly'))

 })

 # Create sales data to use in future tests
 sales_df <- dateToPeriod(sales_df = sales,
                          date = 'sale_date',
                          periodicity = 'monthly')

 test_that("Fails if bad arguments fails", {

   # Bad prop_id field
   expect_error(hed_df <- hedCreateSales(sales_df=sales_df,
                                         prop_id='pinxx',
                                         sale_id='sale_id',
                                         price='sale_price'))

   # Bad sale_id field
   expect_error(hed_df <- hedCreateSales(sales_df=sales_df,
                                         prop_id='pinx',
                                         sale_id='salex_id',
                                         price='sale_price'))

   # Bad price field
   expect_error(hed_df <- hedCreateSales(sales_df=sales_df,
                                         prop_id='pinx',
                                         sale_id='sale_id',
                                         price='salex_price'))

 })

 test_that("Returns NULL if no sales", {

   expect_is(hed_df <- hedCreateSales(sales_df=sales_df[0,],
                                      prop_id='pinx',
                                      sale_id='sale_id',
                                      price='sale_price'), "NULL")

 })
#
# ## Test hpiModel.hed up to hedModel ------------------------------------------------------


 # Create hed data
 hed_df <- hedCreateSales(sales_df=sales,
                          prop_id='pinx',
                          sale_id='sale_id',
                          price='sale_price',
                          date='sale_date',
                          periodicity='monthly')

context('hpiModel.hed')

 ## Test regarding hpi model

 test_that('hpi Model with Hed works', {

   # Dep/Ind variety
   expect_is(hed_model <- hpiModel(hpi_data = hed_df,
                                   estimator = 'base',
                                   dep_var = 'price',
                                   ind_var = c('tot_sf', 'beds', 'baths'),
                                   log_dep = TRUE),
             'hpimodel')

   # Full formula
   expect_is(hed_model <- hpiModel(hpi_data = hed_df,
                                   estimator = 'base',
                                   hed_spec = as.formula(paste0('log(price) ~ as.factor(baths)',
                                                                ' + tot_sf')),
                                   log_dep = TRUE),
             'hpimodel')

   # Dep/Ind variety
   expect_is(hed_model <- hpiModel(hpi_data = hed_df,
                                   estimator = 'robust',
                                   dep_var = 'price',
                                   ind_var = c('tot_sf', 'beds', 'baths'),
                                   log_dep = TRUE),
             'hpimodel')

   # Full formula
   expect_is(hed_model <- hpiModel(hpi_data = hed_df,
                                   estimator = 'robust',
                                   hed_spec = as.formula(paste0('log(price) ~ as.factor(baths)',
                                                                ' + tot_sf')),
                                   log_dep = TRUE),
             'hpimodel')

   # Dep/Ind variety
   expect_is(hed_model <- hpiModel(hpi_data = hed_df,
                                   estimator = 'weighted',
                                   dep_var = 'price',
                                   ind_var = c('tot_sf', 'beds', 'baths'),
                                   log_dep = TRUE,
                                   weights = runif(nrow(hed_df), 0, 1)),
             'hpimodel')

   # Full formula
   expect_is(hed_model <- hpiModel(hpi_data = hed_df,
                                   estimator = 'weighted',
                                   hed_spec = as.formula(paste0('log(price) ~ as.factor(baths)',
                                                                ' + tot_sf')),
                                   log_dep = TRUE,
                                   weights = runif(nrow(hed_df), 0, 1)),
             'hpimodel')

 })

 test_that('"log_dep" works both ways',{

   expect_true(hpiModel(hpi_data = hed_df,
                        estimator = 'base',
                        dep_var = 'price',
                        ind_var = c('tot_sf', 'beds', 'baths'),
                        log_dep = TRUE)$model_obj$fitted.values[1] < 20)

   expect_true(hpiModel(hpi_data = hed_df,
                        estimator = 'robust',
                        dep_var = 'price',
                        ind_var = c('tot_sf', 'beds', 'baths'),
                        log_dep = FALSE)$model_obj$fitted.values[1] > 10000)

   })

 test_that('Check for zero or negative prices works',{

   hed_dfx <- hed_df
   hed_dfx$price[1] <- 0

   # 0 in prices with Log Dep
   expect_error(hed_model <- hpiModel(hpi_data = hed_dfx,
                                   estimator = 'base',
                                   dep_var = 'price',
                                   ind_var = c('tot_sf', 'beds', 'baths'),
                                   log_dep = TRUE))

   # 0 in prices with no Log Dep (Works)
   expect_is(hed_model <- hpiModel(hpi_data = hed_dfx,
                                      estimator = 'base',
                                      dep_var = 'price',
                                      ind_var = c('tot_sf', 'beds', 'baths'),
                                      log_dep = FALSE),
             'hpimodel')

   ## NA
   hed_dfx$price[1] <- NA_integer_

   # NA in prices with Log Dep
   expect_error(hed_model <- hpiModel(hpi_data = hed_dfx,
                                      estimator = 'base',
                                      dep_var = 'price',
                                      ind_var = c('tot_sf', 'beds', 'baths'),
                                      log_dep = TRUE))

   # NA in prices with no Log Dep
   expect_error(hed_model <- hpiModel(hpi_data = hed_dfx,
                                      estimator = 'base',
                                      dep_var = 'price',
                                      ind_var = c('tot_sf', 'beds', 'baths'),
                                      log_dep = FALSE))

   ## INF
   hed_dfx$price[1] <- Inf

   # Inf in prices with Log Dep
   expect_error(hed_model <- hpiModel(hpi_data = hed_dfx,
                                      estimator = 'base',
                                      dep_var = 'price',
                                      ind_var = c('tot_sf', 'beds', 'baths'),
                                      log_dep = TRUE))

   # Inf in prices with no Log Dep
   expect_error(hed_model <- hpiModel(hpi_data = hed_dfx,
                                      estimator = 'base',
                                      dep_var = 'price',
                                      ind_var = c('tot_sf', 'beds', 'baths'),
                                      log_dep = FALSE))

 })

 test_that('Check for estimator type works',{

   # Base
   expect_true(hpiModel(hpi_data = hed_df,
                        dep_var = 'price',
                        ind_var = c('tot_sf', 'beds', 'baths'))$estimator == 'base')

   # Robust
   expect_true(hpiModel(hpi_data = hed_df,
                        estimator = 'robust',
                        dep_var = 'price',
                        ind_var = c('tot_sf', 'beds', 'baths'))$estimator == 'robust')

   # Weighted without Weights
   expect_true(hpiModel(hpi_data = hed_df,
                        estimator = 'weighted',
                        dep_var = 'price',
                        ind_var = c('tot_sf', 'beds', 'baths'))$estimator == 'base')

   # Weighted with weights
   expect_true(hpiModel(hpi_data = hed_df,
                        estimator = 'weighted',
                        dep_var = 'price',
                        ind_var = c('tot_sf', 'beds', 'baths'),
                        weights = runif(nrow(hed_df), 0, 1))$estimator == 'weighted')

 })

context('hedModel')

 test_that('Check for errors with bad arguments',{

   # Base: Return warning if wrong rs_df
   expect_is(hed_model <- hedModel(hed_df = sales,
                                   estimator=structure('base', class='base'),
                                   hed_spec = as.formula(paste0('log(price) ~ ',
                                                'as.factor(baths) + tot_sf'))),
             'NULL')

   # Weighted: Bad spec
   expect_error(hed_model <- hedModel(hed_df = hed_df,
                                   hed_spec = as.formula(paste0('log(x) ~ ',
                                                        'as.factor(baths) + tot_sf')),
                                   estimator=structure('weighted', class='weighted')))

   # Bad estimator class
   expect_is(hed_model <- hedModel(hed_df = hed_df,
                                   hed_spec = as.formula(paste0('log(price) ~ ',
                                                     'as.factor(baths) + tot_sf')),
                                   estimator=structure('fobust', class='fobust')),
             'NULL')

 })

 test_that('Performance with sparse data',{

   ## Moderate Sparseness

   # Create a sparse data set
   hed_df200 <- hed_df[1:200, ]

   # Works with base, though many NAs
   expect_is(hed_model <- hedModel(hed_df = hed_df200,
                                   estimator=structure('base', class='base'),
                                   hed_spec = as.formula(paste0('log(price) ~ ',
                                                      'as.factor(baths) + tot_sf'))),
             'hedmod')

   # Robust works but gives warning
   expect_is(hed_model <- hedModel(hed_df = hed_df200,
                                   estimator=structure('robust', class='robust'),
                                   hed_spec = as.formula(paste0('log(price) ~ ',
                                                   'as.factor(baths) + tot_sf'))),
             'hedmod')

   # Weighted works
   expect_is(hed_model <- hedModel(hed_df = hed_df200,
                                   estimator=structure('weighted', class='weighted'),
                                   hed_spec = as.formula(paste0('log(price) ~ ',
                                                    'as.factor(baths) + tot_sf')),
                                   weights = runif(nrow(hed_df200), 0, 1)),
             'hedmod')

   ## Check severe sparseness

   # Create data set
   hed_df20 <- hed_df[1:20, ]

   # Works with base, though many NAs
   expect_is(hed_model <- hedModel(hed_df = hed_df20,
                                   estimator=structure('base', class='base'),
                                   hed_spec = as.formula(paste0('log(price) ~ ',
                                                    'as.factor(baths) + tot_sf'))),
             'hedmod')

   # Robust works but gives warning
   expect_is(hed_model <- hedModel(hed_df = hed_df20,
                                   estimator=structure('robust', class='robust'),
                                   hed_spec = as.formula(paste0('log(price) ~ ',
                                                                'as.factor(baths) + tot_sf'))),
             'hedmod')

   # Weighted works
   expect_is(hed_model <- hedModel(hed_df = hed_df20,
                                   estimator=structure('weighted', class='weighted'),
                                   hed_spec = as.formula(paste0('log(price) ~ ',
                                                          'as.factor(baths) + tot_sf')),
                                   weights = runif(nrow(hed_df20), 0, 1)),
             'hedmod')

 })

## Test hpiModel.hed after hedModel --------------------------------------------------------

 context('hpiModel.hed')

 test_that('hpiModel.hed works in both trim_model cases', {

   # Base
   expect_is(hed_model <- hpiModel(hpi_data = hed_df,
                                   estimator = 'base',
                                   log_dep = TRUE,
                                   dep_var = 'price',
                                   ind_var = c('tot_sf', 'beds', 'baths'),
                                   trim_model=FALSE), 'hpimodel')
   expect_is(hed_model$model_obj$qr, 'qr')

   expect_is(hed_model <- hpiModel(hpi_data = hed_df,
                                   estimator = 'base',
                                   log_dep = TRUE,
                                   dep_var = 'price',
                                   ind_var = c('tot_sf', 'beds', 'baths'),
                                   trim_model=TRUE), 'hpimodel')
   expect_is(hed_model$model_obj$qr, 'NULL')

   # Robust
   expect_is(hed_model <- hpiModel(hpi_data = hed_df,
                                   estimator = 'robust',
                                   log_dep = TRUE,
                                   dep_var = 'price',
                                   ind_var = c('tot_sf', 'beds', 'baths'),
                                   trim_model=FALSE), 'hpimodel')
   expect_is(hed_model$model_obj$qr, 'qr')

   expect_is(hed_model <- hpiModel(hpi_data = hed_df,
                                   estimator = 'robust',
                                   log_dep = TRUE,
                                   dep_var = 'price',
                                   ind_var = c('tot_sf', 'beds', 'baths'),
                                   trim_model=TRUE), 'hpimodel')
   expect_is(hed_model$model_obj$qr, 'NULL')

   # Weighted
   expect_is(hed_model <- hpiModel(hpi_data = hed_df,
                                   estimator = 'weighted',
                                   log_dep = TRUE,
                                   dep_var = 'price',
                                   ind_var = c('tot_sf', 'beds', 'baths'),
                                   trim_model=FALSE,
                                   weights = runif(nrow(hed_df), 0, 1)),
             'hpimodel')
   expect_is(hed_model$model_obj$qr, 'qr')

   expect_is(hed_model <- hpiModel(hpi_data = hed_df,
                                   estimator = 'weighted',
                                   log_dep = TRUE,
                                   dep_var = 'price',
                                   ind_var = c('tot_sf', 'beds', 'baths'),
                                   trim_model=TRUE,
                                   weights = runif(nrow(hed_df), 0, 1)),
             'hpimodel')
   expect_is(hed_model$model_obj$qr, 'NULL')

})

 test_that('hpiModel.hed outputs are correct', {

   # Run a model of each estimator type
   hed_model_base <- hpiModel(hpi_data = hed_df,
                               estimator = 'base',
                               dep_var = 'price',
                               ind_var = c('tot_sf', 'beds', 'baths'),
                               log_dep = TRUE,
                               trim_model=TRUE)

   hed_model_robust <- hpiModel(hpi_data = hed_df,
                                estimator = 'robust',
                                dep_var = 'price',
                                ind_var = c('tot_sf', 'beds', 'baths'),
                                log_dep = TRUE,
                                trim_model=FALSE)

   hed_model_wgt <- hpiModel(hpi_data = hed_df,
                             estimator = 'weighted',
                             dep_var = 'price',
                             ind_var = c('tot_sf', 'beds', 'baths'),
                             log_dep = FALSE,
                             trim_model=TRUE,
                             weights = runif(nrow(hed_df), 0, 1))

   # Estimatohed
   expect_is(hed_model_base$estimator, 'base')
   expect_is(hed_model_robust$estimator, 'robust')
   expect_is(hed_model_wgt$estimator, 'weighted')

   # Coefficients
   expect_is(hed_model_base$coefficients, 'data.frame')
   expect_is(hed_model_robust$coefficients, 'data.frame')
   expect_is(hed_model_wgt$coefficients, 'data.frame')
   expect_true(nrow(hed_model_base$coefficients) == 84)
   expect_true(max(hed_model_robust$coefficients$time) == 84)
   expect_true(hed_model_wgt$coefficients$coefficient[1] == 0)

   # Modelobj
   expect_is(hed_model_base$model_obj, 'hedmod')
   expect_is(hed_model_robust$model_obj, 'hedmod')
   expect_is(hed_model_wgt$model_obj, 'hedmod')

   # Model spec
   expect_true(is.null(hed_model_base$model_spec))
   expect_true(is.null(hed_model_robust$model_spec))
   expect_true(is.null(hed_model_wgt$model_spec))

   # base price
   expect_true(round(hed_model_base$base_price, 0) == 462545)
   expect_true(round(hed_model_robust$base_price, 0) == 462545)
   expect_true(round(hed_model_wgt$base_price, 0) == 462545)

   # Periods
   expect_is(hed_model_base$periods, 'data.frame')
   expect_true(nrow(hed_model_base$periods) == 84)
   expect_is(hed_model_robust$periods, 'data.frame')
   expect_true(nrow(hed_model_robust$periods) == 84)
   expect_is(hed_model_wgt$periods, 'data.frame')
   expect_true(nrow(hed_model_wgt$periods) == 84)

   # Approach
   expect_true(hed_model_base$approach == 'hed')
   expect_true(hed_model_robust$approach == 'hed')
   expect_true(hed_model_wgt$approach == 'hed')
 })


 ### Test modelToIndex (HED) --------------------------------------------------------------------

 context('modelToIndex')

 hed_model <- hpiModel(hpi_data = rs_df,
                       estimator = 'base',
                       log_dep = TRUE,
                       trim_model=TRUE,
                       dep_var = 'price',
                       ind_var = c('tot_sf', 'beds', 'baths'))

 test_that('modelToIndex works', {

   expect_is(modelToIndex(hed_model), 'hpiindex')

 })

 test_that('modelToIndex works with other estimators and options', {

   # Robust, LogDep=T, TrimModel=T
   expect_is(modelToIndex(hpiModel(hpi_data = hed_df,
                                   estimator = 'robust',
                                   log_dep = TRUE,
                                   trim_model=TRUE,
                                   dep_var = 'price',
                                   ind_var = c('tot_sf', 'beds', 'baths'))),
             'hpiindex')

   # Weighted, LogDep=T, TrimModel=T
   expect_is(modelToIndex(hpiModel(hpi_data = hed_df,
                                   estimator = 'weighted',
                                   log_dep = TRUE,
                                   trim_model=TRUE,
                                   dep_var = 'price',
                                   ind_var = c('tot_sf', 'beds', 'baths'),
                                   weights=runif(nrow(hed_df), 0, 1))),
             'hpiindex')

   # Robust, LogDep=F, TrimModel=T
   expect_is(modelToIndex(hpiModel(hpi_data = hed_df,
                                   estimator = 'robust',
                                   log_dep = FALSE,
                                   trim_model=TRUE,
                                   dep_var = 'price',
                                   ind_var = c('tot_sf', 'beds', 'baths'))),
             'hpiindex')

   # Weighted, LogDep=T, TrimModel=F
   expect_is(modelToIndex(hpiModel(hpi_data = hed_df,
                                   estimator = 'weighted',
                                   log_dep = TRUE,
                                   trim_model=FALSE,
                                   dep_var = 'price',
                                   ind_var = c('tot_sf', 'beds', 'baths'),
                                   weights=runif(nrow(hed_df), 0, 1))),
             'hpiindex')
 })

 test_that('modelToIndex imputes properly, BASE model, LogDEP',{

   model_base <- hpiModel(hpi_data = hed_df,
                          estimator = 'base',
                          log_dep = TRUE,
                          trim_model=TRUE,
                          dep_var = 'price',
                          ind_var = c('tot_sf', 'beds', 'baths'))

   # Impute a beginning value
   model_ex <- model_base
   model_ex$coefficients$coefficient[2] <- NA_real_
   expect_is(modelToIndex(model_ex), 'hpiindex')
   expect_true(!is.na(modelToIndex(model_ex)$index[2]))
   expect_true(modelToIndex(model_ex)$index[2] == 100)
   expect_true(modelToIndex(model_ex)$imputed[2] == 1)

   # Interpolate interior values
   model_ex <- model_base
   model_ex$coefficients$coefficient[3:5] <- NA_real_
   expect_is(modelToIndex(model_ex), 'hpiindex')
   expect_true(all(!is.na(modelToIndex(model_ex)$index[3:5])))

   # Extrapolate end periods
   model_ex <- model_base
   model_ex$coefficients$coefficient[81:84] <- NA_real_
   expect_is(modelToIndex(model_ex), 'hpiindex')
   expect_true(all(!is.na(modelToIndex(model_ex)$index[81:84])))
   expect_true(modelToIndex(model_ex)$index[80] ==
                 modelToIndex(model_ex)$index[84])

 })

 test_that('modelToIndex imputes properly, BASE model, LogDep=FALSE',{

   model_base <- hpiModel(hpi_data = hed_df,
                          estimator = 'base',
                          log_dep = FALSE,
                          trim_model=TRUE,
                          dep_var = 'price',
                          ind_var = c('tot_sf', 'beds', 'baths'))

   # Extrapolate a beginning value
   model_ex <- model_base
   model_ex$coefficients$coefficient[2] <- NA_real_
   expect_is(modelToIndex(model_ex), 'hpiindex')
   expect_true(!is.na(modelToIndex(model_ex)$index[2]))
   expect_true(modelToIndex(model_ex)$index[2] == 100)
   expect_true(modelToIndex(model_ex)$imputed[2] == 1)

   # Impute interior values
   model_ex <- model_base
   model_ex$coefficients$coefficient[3:5] <- NA_real_
   expect_is(modelToIndex(model_ex), 'hpiindex')
   expect_true(all(!is.na(modelToIndex(model_ex)$index[3:5])))

   # Extrapolate an end value
   model_ex <- model_base
   model_ex$coefficients$coefficient[81:84] <- NA_real_
   expect_is(modelToIndex(model_ex), 'hpiindex')
   expect_true(all(!is.na(modelToIndex(model_ex)$index[81:84])))
   expect_true(modelToIndex(model_ex)$index[80] ==
                 modelToIndex(model_ex)$index[84])

 })

 test_that('modelToIndex imputes properly, Robust model, LogDEP = TRUE',{

   model_base <- hpiModel(hpi_data = hed_df,
                          estimator = 'robust',
                          log_dep = TRUE,
                          trim_model=TRUE,
                          dep_var = 'price',
                          ind_var = c('tot_sf', 'beds', 'baths'))

   model_ex <- model_base
   model_ex$coefficients$coefficient[2] <- NA_real_
   expect_is(modelToIndex(model_ex), 'hpiindex')
   expect_true(!is.na(modelToIndex(model_ex)$index[2]))
   expect_true(modelToIndex(model_ex)$index[2] == 100)
   expect_true(modelToIndex(model_ex)$imputed[2] == 1)

   model_ex <- model_base
   model_ex$coefficients$coefficient[3:5] <- NA_real_
   expect_is(modelToIndex(model_ex), 'hpiindex')
   expect_true(all(!is.na(modelToIndex(model_ex)$index[3:5])))

   model_ex <- model_base
   model_ex$coefficients$coefficient[81:84] <- NA_real_
   expect_is(modelToIndex(model_ex), 'hpiindex')
   expect_true(all(!is.na(modelToIndex(model_ex)$index[81:84])))
   expect_true(modelToIndex(model_ex)$index[80] ==
                 modelToIndex(model_ex)$index[84])

 })

 test_that('modelToIndex imputes properly, Weighted model, LogDep=FALSE',{

   model_base <- hpiModel(hpi_data = hed_df,
                          estimator = 'weighted',
                          log_dep = TRUE,
                          trim_model=FALSE,
                          dep_var = 'price',
                          ind_var = c('tot_sf', 'beds', 'baths'),
                          weights=runif(nrow(hed_df), 0, 1))

   model_ex <- model_base
   model_ex$coefficients$coefficient[2] <- NA_real_
   expect_is(modelToIndex(model_ex), 'hpiindex')
   expect_true(!is.na(modelToIndex(model_ex)$index[2]))
   expect_true(modelToIndex(model_ex)$index[2] == 100)
   expect_true(modelToIndex(model_ex)$imputed[2] == 1)

   model_ex <- model_base
   model_ex$coefficients$coefficient[3:5] <- NA_real_
   expect_is(modelToIndex(model_ex), 'hpiindex')
   expect_true(all(!is.na(modelToIndex(model_ex)$index[3:5])))

   model_ex <- model_base
   model_ex$coefficients$coefficient[81:84] <- NA_real_
   expect_is(modelToIndex(model_ex), 'hpiindex')
   expect_true(all(!is.na(modelToIndex(model_ex)$index[81:84])))
   expect_true(modelToIndex(model_ex)$index[80] ==
                 modelToIndex(model_ex)$index[84])

 })


### Test hedIndex() wrapper ---------------------------------------------------------------

 context('hedindex() wrapper')

 test_that('Function works with proper inputs',{

   # Full case
   full_1 <- hedIndex(sales_df = sales,
                      date = 'sale_date',
                      price = 'sale_price',
                      sale_id = 'sale_id',
                      prop_id = 'pinx',
                      estimator = 'base',
                      periodicity = 'monthly',
                      dep_var = 'price',
                      ind_var = c('tot_sf', 'beds', 'baths'))

   expect_is(full_1, 'hpi')
   expect_true(full_1$model$estimator == 'base')

   # Giving a 'sales_df' object
   full_2 <- hedIndex(sales_df = sales_df,
                      price = 'sale_price',
                      sale_id = 'sale_id',
                      prop_id = 'pinx',
                      estimator = 'robust',
                      dep_var = 'price',
                      ind_var = c('tot_sf', 'beds', 'baths'))

   expect_is(full_2, 'hpi')
   expect_true(full_2$model$estimator == 'robust')

   # Giving an 'rs_df' object
   full_3 <- hedIndex(sales_df = hed_df,
                      estimator = 'weighted',
                      log_dep=FALSE,
                      dep_var = 'price',
                      ind_var = c('tot_sf', 'beds', 'baths'),
                      weights=runif(nrow(hed_df), 0, 1))

   expect_is(full_3, 'hpi')
   expect_true(full_3$model$estimator == 'weighted')

 })

 test_that('Additional arguments in hedIndex() work',{

   ## HED Create arguments

   # Min Date Model with Clip
   mindate_index <- hedIndex(sales_df = sales,
                             date='sale_date',
                             price = 'sale_price',
                             sale_id = 'sale_id',
                             prop_id = 'pinx',
                             estimator = 'robust',
                             dep_var = 'price',
                             ind_var = c('tot_sf', 'beds', 'baths'),
                             min_date = as.Date('2011-01-01'),
                             adj_type = 'clip')
   expect_true(min(mindate_index$index$period) == 2011)

   # Max Date Model with Adjust
   maxdate_index <- hedIndex(sales_df = sales,
                            date='sale_date',
                            price = 'sale_price',
                            sale_id = 'sale_id',
                            prop_id = 'pinx',
                            estimator = 'robust',
                            dep_var = 'price',
                            ind_var = c('tot_sf', 'beds', 'baths'),
                            max_date = as.Date('2015-12-31'))
   expect_true(max(maxdate_index$index$period) == 2016)

   # Periodicity
   per_index <- hedIndex(sales_df = sales,
                        date='sale_date',
                        price = 'sale_price',
                        sale_id = 'sale_id',
                        prop_id = 'pinx',
                        estimator = 'robust',
                        dep_var = 'price',
                        ind_var = c('tot_sf', 'beds', 'baths'),
                        periodicity = 'weekly')
   expect_true(max(per_index$index$period) == 364)

 ## HPI Model

   # Trim Model
   trim_index <- hedIndex(sales_df = hed_df,
                          dep_var = 'price',
                          ind_var = c('tot_sf', 'beds', 'baths'),
                          trim_model=TRUE)
   expect_true(is.null(trim_index$model$model_obj$qr))

   # Log Dep & Robust
   ld_index <- hedIndex(sales_df = hed_df,
                       estimator = 'robust',
                       dep_var = 'price',
                       ind_var = c('tot_sf', 'beds', 'baths'),
                       log_dep = FALSE)
   expect_true(ld_index$model$log_dep == FALSE)
   expect_true(ld_index$model$estimator == 'robust')

   ## Model to Index
   m2i_index <- hedIndex(sales_df = hed_df,
                         estimator = 'robust',
                         log_dep = FALSE,
                         dep_var = 'price',
                         ind_var = c('tot_sf', 'beds', 'baths'),
                         max_period = 80)
   expect_true(length(m2i_index$index$index) == 80)

 })



 #
 test_that("Bad arguments generate Errors: Full Case",{

   # Bad Date
   expect_error(hedIndex(sales_df = sales,
                         date = 'sale_price',
                         price = 'sale_price',
                         sale_id = 'sale_id',
                         prop_id = 'pinx',
                         estimator = 'base',
                         log_dep = TRUE,
                         dep_var = 'price',
                         ind_var = c('tot_sf', 'beds', 'baths'),
                         periodicity = 'monthly'))

   # Bad Periodicity
   expect_error(hedIndex(sales_df = sales,
                         date = 'sale_date',
                         price = 'sale_price',
                         sale_id = 'sale_id',
                         prop_id = 'pinx',
                         estimator = 'base',
                         log_dep = TRUE,
                         dep_var = 'price',
                         ind_var = c('tot_sf', 'beds', 'baths'),
                         periodicity = 'xxx'))

 })

 test_that("Bad arguments generate errors: Sales_df Case",{

   expect_error(hedIndex(sales_df = sales_df,
                         price = 'xx',
                         sale_id = 'sale_id',
                         prop_id = 'pinx',
                         estimator = 'base',
                         log_dep = TRUE,
                         dep_var = 'price',
                         ind_var = c('tot_sf', 'beds', 'baths')))
   expect_error(hedIndex(sales_df = sales_df,
                         price = 'sale_price',
                         sale_id = 'xx',
                         prop_id = 'pinx',
                         estimator = 'base',
                         log_dep = TRUE,
                         dep_var = 'price',
                         ind_var = c('tot_sf', 'beds', 'baths')))
   expect_error(hedIndex(sales_df = sales_df,
                         price = 'sale_price',
                         sale_id = 'sale_id',
                         prop_id = 'xx',
                         estimator = 'base',
                         log_dep = TRUE,
                         dep_var = 'price',
                         ind_var = c('tot_sf', 'beds', 'baths')))
 })

 test_that("Bad arguments handling: hed_sales Case",{

   # Bad estimators default to 'base'
   expect_true(hedIndex(sales_df = hed_df,
                        estimator = 'basex',
                        log_dep = TRUE,
                        dep_var = 'price',
                        ind_var = c('tot_sf', 'beds', 'baths'))$model$estimator == 'base')

   expect_error(hedIndex(sales_df = hed_df,
                         estimator = 'robust',
                         dep_var = 'price',
                         ind_var = c('tot_sf', 'beds', 'baths'),
                         log_dep = 'a'))

   expect_error(hedIndex(sales_df = hed_df,
                         estimator = 'robust',
                         trim_model = 'a',
                         dep_var = 'price',
                         ind_var = c('tot_sf', 'beds', 'baths')))

   expect_error(hedIndex(sales_df = hed_df,
                         estimator = 'robust',
                         max_period = 'a',
                         dep_var = 'price',
                         ind_var = c('tot_sf', 'beds', 'baths')))
 })

### Test Volatility Function -------------------------------------------------------------

 # Sample 'hed' hpi object for further testing
 hed_index <- hedIndex(sales_df = hed_df,
                       estimator = 'weighted',
                       log_dep=FALSE,
                       dep_var = 'price',
                       ind_var = c('tot_sf', 'beds', 'baths'),
                       weights=runif(nrow(hed_df), 0, 1))

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

 })

### Test Smoothing Functions -------------------------------------------------------------

### Test Revision Functions --------------------------------------------------------------

### Test Series Functions ----------------------------------------------------------------

### Test Accuracy Functions --------------------------------------------------------------

 # Kfold
 # Forecast

### Test Blending Functions --------------------------------------------------------------











# ### Test all plot functions --------------------------------------------------------------
#
#
# context('Plot functions')
#
#   test_that('plot.hpiindex works', {
#     expect_is(plot(modelToIndex(hpiModel(hpi_data = rs_df))), 'gg')
#     expect_error(plot(hpiModel(hpi_data = rs_df)))
#   })
#
#   test_that('plot.hpiindex works', {
#     expect_is(plot(rsIndex(sales_df = rs_df,
#                            estimator = 'weighted',
#                            log_dep = TRUE)), 'gg')
#   })
#
# context('Plot Functions')
