#*****************************************************************************************
#                                                                                        *
#   Unit tests for hpiR package - Repeat Sales Functions                                 *
#                                                                                        *
#*****************************************************************************************

  library(hpiR)
  library(testthat)

 ## Load Data

  sales <- get(data(seattle_sales))

### rsCreateSales() ----------------------------------------------------------------------

context('rsCreateSales()')

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
                                     periodicity='monthly'),
              'rs')
    expect_true(nrow(rs_df) == 5102)
  })

  test_that("Can use min/max dates own salesdf object", {

    expect_is(rs_df <- rsCreateSales(sales_df=sales,
                                     prop_id='pinx',
                                     sale_id='sale_id',
                                     price='sale_price',
                                     date='sale_date',
                                     periodicity='monthly',
                                     min_date = as.Date('2012-03-21')),
              'rs')
    expect_true(nrow(rs_df) == 5102)

    expect_is(rs_df <- rsCreateSales(sales_df=sales,
                                     prop_id='pinx',
                                     sale_id='sale_id',
                                     price='sale_price',
                                     date='sale_date',
                                     periodicity='monthly',
                                     min_date = as.Date('2012-03-21'),
                                     adj_type='clip'),
              'rs')
    expect_true(nrow(rs_df) == 2827)

    expect_is(rs_df <- rsCreateSales(sales_df=sales,
                                     prop_id='pinx',
                                     sale_id='sale_id',
                                     price='sale_price',
                                     date='sale_date',
                                     periodicity='monthly',
                                     max_date = as.Date('2015-03-21')),
              'rs')
    expect_true(nrow(rs_df) == 5102)

    expect_is(rs_df <- rsCreateSales(sales_df=sales,
                                     prop_id='pinx',
                                     sale_id='sale_id',
                                     price='sale_price',
                                     date='sale_date',
                                     periodicity='monthly',
                                     max_date = as.Date('2014-03-21'),
                                     adj_type='clip'),
              'rs')
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

### rsTimeMatrix -------------------------------------------------------------------------

context('rsTimeMatrix()')

  # Create a full rs_data object in global environment
  rs_df <- rsCreateSales(sales_df=sales_df,
                         prop_id='pinx',
                         sale_id='sale_id',
                         price='sale_price')

  test_that('Time matrix operates properly', {

    # Work with an 'rs' object
    expect_is(time_matrix <- rsTimeMatrix(rs_df), 'timematrix')

    # Returns a NULL without
    expect_is(time_matrix <- rsTimeMatrix(sales_df), 'NULL')

    # Returns correct number of rows
    expect_true(nrow(rsTimeMatrix(rs_df[1:2000,])) == 2000)

    # Return correct number of columns
    expect_true(ncol(rsTimeMatrix(rs_df[1:2000,])) ==
                  nrow(attr(rs_df, 'period_table')) - 1)
  })

## hpiModel.rs(): Prior to rsModel() call -------------------------------------------

context('hpiModel.rs(): Prior to rsModel() call')

  test_that('hpiModel.rs works in simplest format',{
    expect_is(rs_model <- hpiModel(hpi_data = rs_df,
                                   estimator = 'base',
                                   log_dep = TRUE),
              'hpimodel')
  })

  test_that('"log_dep" argument works both ways',{

    # TRUE
    expect_true(hpiModel(hpi_data = rs_df,
                         estimator = 'base',
                         log_dep = TRUE)$model_obj$fitted.values[1] < 1)

    # FALSE
    expect_true(hpiModel(hpi_data = rs_df,
                         estimator = 'base',
                         log_dep = FALSE)$model_obj$fitted.values[1] > 10000)

  })

  test_that('Check for zero or negative prices works',{

    rs_dfx <- rs_df

    # Zero Price
    rs_dfx$price_1[1] <- 0
    expect_is(rs_model <- hpiModel(hpi_data = rs_dfx,
                                   estimator = 'base',
                                   log_dep = TRUE), 'NULL')
    expect_is(rs_model <- hpiModel(hpi_data = rs_dfx,
                                   estimator = 'base',
                                   log_dep = FALSE), 'hpimodel')

    # NA
    rs_dfx$price_1[1] <- NA_integer_
    expect_is(rs_model <- hpiModel(hpi_data = rs_dfx,
                                   estimator = 'base',
                                   log_dep = TRUE), 'NULL')
    expect_is(rs_model <- hpiModel(hpi_data = rs_dfx,
                                   estimator = 'base',
                                   log_dep = FALSE), 'NULL')

    # Infinity
    rs_dfx$price_1[1] <- Inf
    expect_is(rs_model <- hpiModel(hpi_data = rs_dfx,
                                   estimator = 'base',
                                   log_dep = TRUE), 'NULL')
    expect_is(rs_model <- hpiModel(hpi_data = rs_dfx,
                                   estimator = 'base',
                                   log_dep = FALSE), 'NULL')

  })

  test_that('Check for estimator type works',{

    # Base
    expect_true(hpiModel(hpi_data = rs_df)$estimator == 'base')

    # Convert to base with bad estimator
    expect_true(hpiModel(hpi_data = rs_df,
                         estimator='xxxx')$estimator == 'base')

    # Robust
    expect_true(hpiModel(hpi_data = rs_df,
                         estimator='robust')$estimator == 'robust')

    # Weighted
    expect_true(hpiModel(hpi_data = rs_df,
                         estimator='weighted')$estimator == 'weighted')
  })

## rsModel() ------------------------------------------------------------------------

context('rsModel()')

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

## hpiModel.rs() after rsModel --------------------------------------------------------

context('hpiModel.rs(): after rsModel()')

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

### modelToIndex --------------------------------------------------------------------

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

### rsIndex() wrapper ---------------------------------------------------------------

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

#*****************************************************************************************
#*****************************************************************************************
