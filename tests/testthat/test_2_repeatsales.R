#*****************************************************************************************
#                                                                                        *
#   Unit tests for hpiR package - Repeat Sales Functions                                 *
#                                                                                        *
#*****************************************************************************************

  library(hpiR)
  library(testthat)

 ## Load Data

  sales <- get(data(seattle_sales))

### rtCreateTrans() ----------------------------------------------------------------------

context('rtCreateTrans()')

  # Test Setup
  test_that("Can take a functional 'trans_df' object", {

    sales_df <- dateToPeriod(trans_df = sales,
                             date = 'sale_date',
                             periodicity = 'monthly')

    expect_is(rt_df <- rtCreateTrans(trans_df=sales_df,
                                     prop_id='pinx',
                                     trans_id='sale_id',
                                     price='sale_price'),
              'rtdata')
    expect_true(nrow(rt_df) == 5102)
  })

  # Test Setup
  test_that("Can create own salesdf object", {

    expect_is(rt_df <- rtCreateTrans(trans_df=sales,
                                     prop_id='pinx',
                                     trans_id='sale_id',
                                     price='sale_price',
                                     date='sale_date',
                                     periodicity='monthly'),
              'rtdata')
    expect_true(nrow(rt_df) == 5102)
  })

  test_that("Can use min/max dates own salesdf object", {

    expect_is(rt_df <- rtCreateTrans(trans_df=sales,
                                     prop_id='pinx',
                                     trans_id='sale_id',
                                     price='sale_price',
                                     date='sale_date',
                                     periodicity='monthly',
                                     min_date = as.Date('2012-03-21')),
              'rtdata')
    expect_true(nrow(rt_df) == 5102)

    expect_is(rt_df <- rtCreateTrans(trans_df=sales,
                                     prop_id='pinx',
                                     trans_id='sale_id',
                                     price='sale_price',
                                     date='sale_date',
                                     periodicity='monthly',
                                     min_date = as.Date('2012-03-21'),
                                     adj_type='clip'),
              'rtdata')
    expect_true(nrow(rt_df) == 2827)

    expect_is(rt_df <- rtCreateTrans(trans_df=sales,
                                     prop_id='pinx',
                                     trans_id='sale_id',
                                     price='sale_price',
                                     date='sale_date',
                                     periodicity='monthly',
                                     max_date = as.Date('2015-03-21')),
              'rtdata')
    expect_true(nrow(rt_df) == 5102)

    expect_is(rt_df <- rtCreateTrans(trans_df=sales,
                                     prop_id='pinx',
                                     trans_id='sale_id',
                                     price='sale_price',
                                     date='sale_date',
                                     periodicity='monthly',
                                     max_date = as.Date('2014-03-21'),
                                     adj_type='clip'),
              'rtdata')
    expect_true(nrow(rt_df) == 1148)
  })

  test_that("Sequence only (seq_only) option works", {

    expect_is(rt_df <- rtCreateTrans(trans_df=sales,
                                     prop_id='pinx',
                                     trans_id='sale_id',
                                     price='sale_price',
                                     date='sale_date',
                                     periodicity='monthly',
                                     seq_only = TRUE),
              'rtdata')
    expect_true(nrow(rt_df) == 4823)

  })

  test_that("min_period_dist argument works", {

    expect_is(rt_df <- rtCreateTrans(trans_df=sales,
                                     prop_id='pinx',
                                     trans_id='sale_id',
                                     price='sale_price',
                                     date='sale_date',
                                     periodicity='monthly',
                                     seq_only = TRUE,
                                     min_period_dist = 12),
              'rtdata')
    expect_true(nrow(rt_df) == 3795)


  })
  test_that("Fails if sales creation fails", {

    # Bad Date field
    expect_error(rt_df <- rtCreateTrans(trans_df=sales,
                                        prop_id='pinx',
                                        trans_id='sale_id',
                                        price='sale_price',
                                        date='sale_price',
                                        periodicity='monthly'))

    # Bad Periodicity field
    expect_error(rt_df <- rtCreateTrans(trans_df=sales,
                                        prop_id='pinx',
                                        trans_id='sale_id',
                                        price='sale_price',
                                        date='sale_date',
                                        periodicity='mocnthly'))

  })




  # Create a sales_df in the global env for future use
  sales_df <- dateToPeriod(trans_df = sales,
                           date = 'sale_date',
                           periodicity = 'monthly')

  test_that("Fails if bad arguments fails", {

    # Bad prop_id field
    expect_error(rt_df <- rtCreateTrans(trans_df=sales_df,
                                        prop_id='pinxx',
                                        trans_id='sale_id',
                                        price='sale_price'))

    # Bad sale_id field
    expect_error(rt_df <- rtCreateTrans(trans_df=sales_df,
                                        prop_id='pinx',
                                        trans_id='salex_id',
                                        price='sale_price'))

    # Bad price field
    expect_error(rt_df <- rtCreateTrans(trans_df=sales_df,
                                        prop_id='pinx',
                                        trans_id='sale_id',
                                        price='salex_price'))

  })

  test_that("Returns NULL if no repeat sales", {

    expect_is(rt_df <- rtCreateTrans(trans_df=sales_df[!duplicated(sales_df$prop_id),],
                                     prop_id='pinx',
                                     trans_id='sale_id',
                                     price='sale_price'), "NULL")

    expect_is(rt_df <- rtCreateTrans(trans_df=sales_df[1:3, ],
                                     prop_id='pinx',
                                     trans_id='sale_id',
                                     price='sale_price'), "NULL")
  })

### rtTimeMatrix -------------------------------------------------------------------------

context('rtTimeMatrix()')

  # Create a full rt_data object in global environment
  rt_df <- rtCreateTrans(trans_df=sales_df,
                         prop_id='pinx',
                         trans_id='sale_id',
                         price='sale_price')

  test_that('Time matrix operates properly', {

    # Work with an 'rtdata' object
    expect_is(time_matrix <- rtTimeMatrix(rt_df), 'timematrix')

    # Returns a NULL without
    expect_error(time_matrix <- rtTimeMatrix(sales_df))

    # Returns correct number of rows
    expect_true(nrow(rtTimeMatrix(rt_df[1:2000,])) == 2000)

    # Return correct number of columns
    expect_true(ncol(rtTimeMatrix(rt_df[1:2000,])) ==
                  nrow(attr(rt_df, 'period_table')) - 1)
  })

## hpiModel.rtdata(): Prior to rtModel() call -------------------------------------------

context('hpiModel.rtdata(): Prior to rtModel() call')

  test_that('hpiModel.rtdata works in simplest format',{
    expect_is(rt_model <- hpiModel(model_type = 'rt',
                                   hpi_df = rt_df,
                                   estimator = 'base',
                                   log_dep = TRUE),
              'hpimodel')
  })

  test_that('"log_dep" argument works both ways',{

    # TRUE
    expect_true(hpiModel(model_type = 'rt',
                         hpi_df = rt_df,
                         estimator = 'base',
                         log_dep = TRUE)$model_obj$fitted.values[1] < 1)

    # FALSE
    expect_true(hpiModel(model_type = 'rt',
                         hpi_df = rt_df,
                         estimator = 'base',
                         log_dep = FALSE)$model_obj$fitted.values[1] > 10000)

  })

  test_that('Check for zero or negative prices works',{

    rt_dfx <- rt_df

    # Zero Price
    rt_dfx$price_1[1] <- 0
    expect_error(rt_model <- hpiModel(model_type = 'rt',
                                      hpi_df = rt_dfx,
                                      estimator = 'base',
                                      log_dep = TRUE))
    expect_is(rt_model <- hpiModel(model_type = 'rt',
                                   hpi_df = rt_dfx,
                                   estimator = 'base',
                                   log_dep = FALSE),
              'hpimodel')

    # NA
    rt_dfx$price_1[1] <- NA_integer_
    expect_error(rt_model <- hpiModel(model_type = 'rt',
                                      hpi_df = rt_dfx,
                                      estimator = 'base',
                                      log_dep = TRUE))
    expect_error(rt_model <- hpiModel(model_type = 'rt',
                                      hpi_df = rt_dfx,
                                      estimator = 'base',
                                      log_dep = FALSE))

    # Infinity
    rt_dfx$price_1[1] <- Inf
    expect_error(rt_model <- hpiModel(model_type = 'rt',
                                      hpi_df = rt_dfx,
                                      estimator = 'base',
                                      log_dep = TRUE))
    expect_error(rt_model <- hpiModel(model_type = 'rt',
                                      hpi_df = rt_dfx,
                                      estimator = 'base',
                                      log_dep = FALSE))

  })

  test_that('Check for estimator type works',{

    # Base
    expect_true(hpiModel(model_type = 'rt',
                         hpi_df = rt_df)$estimator == 'base')

    # Convert to base with bad estimator
    expect_true(hpiModel(model_type = 'rt',
                         hpi_df = rt_df,
                         estimator='xxxx')$estimator == 'base')

    # Robust
    expect_true(hpiModel(model_type = 'rt',
                         hpi_df = rt_df,
                         estimator='robust')$estimator == 'robust')

    # Weighted
    expect_true(hpiModel(model_type = 'rt',
                         hpi_df = rt_df,
                         estimator='weighted')$estimator == 'weighted')
  })

## rtModel() ------------------------------------------------------------------------

context('rtModel()')

  # Create matrix and difference vectort in global environment
  time_matrix <- rtTimeMatrix(rt_df)
  price_diff_l <- log(rt_df$price_2) - log(rt_df$price_1)
  price_diff <- rt_df$price_2 - rt_df$price_1

  test_that('Check for errort with bad arguments',{

    # Base: Return warning if wrong rt_df
    expect_error(rt_model <- rtModel(rt_df = sales,
                                     time_matrix = time_matrix,
                                     price_diff = price_diff_l,
                                     estimator=structure('base', class='base')))

    # Robust: Return warning if wrong time_matrix
    expect_error(rt_model <- rtModel(rt_df = rt_df,
                                     time_matrix = sales,
                                     price_diff = price_diff_l,
                                     estimator=structure('robust', class='robust')))

    # Weighted: Dimensions of data do not match
    expect_error(rt_model <- rtModel(rt_df = rt_df,
                                     time_matrix = time_matrix,
                                     price_diff = price_diff_l[-1],
                                     estimator=structure('weighted', class='weighted')))

    # Bad estimator class
    expect_error(rt_model <- rtModel(rt_df = rt_df,
                                     time_matrix = time_matrix,
                                     price_diff = price_diff,
                                     estimator=structure('base', class='xxx')))

  })

  test_that('Performance with sparse data',{

    ## Moderate Sparseness

    # Create a sparse data set
    rt_df200 <- rt_df[1:200, ]
    time_matrix200 <- rtTimeMatrix(rt_df200)
    price_diff_l200 <- log(rt_df200$price_2) - log(rt_df200$price_1)
    price_diff200 <- rt_df200$price_2 - rt_df200$price_1

    # Works with base, though many NAs
    expect_is(rt_model <- rtModel(rt_df = rt_df200,
                                  time_matrix = time_matrix200,
                                  price_diff = price_diff_l200,
                                  estimator=structure('base', class='base')),
              'rtmodel')

    # Robust works but gives warning
    expect_is(rt_model <- rtModel(rt_df = rt_df200,
                                  time_matrix = time_matrix200,
                                  price_diff = price_diff_l200,
                                  estimator=structure('robust', class='robust')),
              'rtmodel')

    # Weighted works
    expect_is(rt_model <- rtModel(rt_df = rt_df200,
                                  time_matrix = time_matrix200,
                                  price_diff = price_diff200,
                                  estimator=structure('weighted', class='weighted')),
              'rtmodel')

    ## Check severe sparteness

    # Create data set
    rt_df20 <- rt_df[1:20, ]
    time_matrix20 <- rtTimeMatrix(rt_df20)
    price_diff_l20 <- log(rt_df20$price_2) - log(rt_df20$price_1)
    price_diff20 <- rt_df20$price_2 - rt_df20$price_1

    # Base: Works, but gives message
    expect_is(rt_model <- rtModel(rt_df = rt_df20,
                                  time_matrix = time_matrix20,
                                  price_diff = price_diff_l20,
                                  estimator=structure('base', class='base')),
              'rtmodel')

    # Robust: Works, but gives warning from lmrob()
    expect_warning(rt_model <- rtModel(rt_df = rt_df20,
                                       time_matrix = time_matrix20,
                                       price_diff = price_diff_l20,
                                       estimator=structure('robust', class='robust')))

    # Weighted: works but gives message
    expect_is(rt_model <- rtModel(rt_df = rt_df20,
                                  time_matrix = time_matrix20,
                                  price_diff = price_diff20,
                                  estimator=structure('weighted', class='weighted')),
              'rtmodel')

  })

## hpiModel.rtdata() after rtModel --------------------------------------------------------

context('hpiModel.rtdata(): after rtModel()')

  test_that('hpiModel.rtdata works in both trim_model cases', {
    expect_is(rt_model <- hpiModel(model_type = 'rt',
                                   hpi_df = rt_df,
                                   estimator = 'base',
                                   log_dep = TRUE,
                                   trim_model=TRUE), 'hpimodel')
    expect_is(rt_model <- hpiModel(model_type = 'rt',
                                   hpi_df = rt_df,
                                   estimator = 'base',
                                   log_dep = TRUE,
                                   trim_model=FALSE), 'hpimodel')
    expect_is(rt_model <- hpiModel(model_type = 'rt',
                                   hpi_df = rt_df,
                                   estimator = 'base',
                                   log_dep = FALSE,
                                   trim_model=TRUE), 'hpimodel')
    expect_is(rt_model <- hpiModel(model_type = 'rt',
                                   hpi_df = rt_df,
                                   estimator = 'base',
                                   log_dep = FALSE,
                                   trim_model=FALSE), 'hpimodel')
    expect_is(rt_model <- hpiModel(model_type = 'rt',
                                   hpi_df = rt_df,
                                   estimator = 'weighted',
                                   log_dep = TRUE,
                                   trim_model=FALSE), 'hpimodel')
    expect_is(rt_model <- hpiModel(model_type = 'rt',
                                   hpi_df = rt_df,
                                   estimator = 'robust',
                                   log_dep = FALSE,
                                   trim_model=TRUE), 'hpimodel')
    expect_true(is.null(hpiModel(model_type = 'rt',
                                 hpi_df = rt_df,
                                 estimator = 'weighted',
                                 log_dep = TRUE,
                                 trim_model=TRUE)$model_obj$qr))
    expect_true(!is.null(hpiModel(model_type = 'rt',
                                  hpi_df = rt_df,
                                  estimator = 'weighted',
                                  log_dep = TRUE,
                                  trim_model=FALSE)$model_obj$qr))
  })

  test_that('hpiModel.rtdata outputs are correct', {

    # Run a model of each estimator type
    rt_model_base <- hpiModel(model_type = 'rt',
                              hpi_df = rt_df,
                              estimator = 'base',
                              log_dep = TRUE,
                              trim_model=TRUE)

    rt_model_robust <- hpiModel(model_type = 'rt',
                                hpi_df = rt_df,
                                estimator = 'robust',
                                log_dep = TRUE,
                                trim_model=FALSE)

    rt_model_wgt <- hpiModel(model_type = 'rt',
                             hpi_df = rt_df,
                             estimator = 'weighted',
                             log_dep = FALSE,
                             trim_model=TRUE)

    set.seed(123)
    rt_model_wwgt <- hpiModel(model_type = 'rt',
                              hpi_df = rt_df,
                              estimator = 'weighted',
                              log_dep = FALSE,
                              weights = runif(nrow(rt_df), 0, 1),
                              trim_model=TRUE)


    # Estimatort
    expect_is(rt_model_base$estimator, 'base')
    expect_is(rt_model_robust$estimator, 'robust')
    expect_is(rt_model_wgt$estimator, 'weighted')

    # Coefficients
    expect_is(rt_model_base$coefficients, 'data.frame')
    expect_is(rt_model_robust$coefficients, 'data.frame')
    expect_is(rt_model_wgt$coefficients, 'data.frame')
    expect_true(nrow(rt_model_base$coefficients) == 84)
    expect_true(max(rt_model_robust$coefficients$time) == 84)
    expect_true(rt_model_wgt$coefficients$coefficient[1] == 0)
    expect_false(identical(
      rt_model_wgt$coefficients$coefficient,
                  rt_model_wwgt$coefficients$coefficient))

    # Modelobj
    expect_is(rt_model_base$model_obj, 'rtmodel')
    expect_is(rt_model_robust$model_obj, 'rtmodel')
    expect_is(rt_model_wgt$model_obj, 'rtmodel')

    # Model spec
    expect_true(is.null(rt_model_base$model_spec))
    expect_true(is.null(rt_model_robust$model_spec))
    expect_true(is.null(rt_model_wgt$model_spec))

    # base price
    expect_true(round(rt_model_base$base_price, 0) == 427785)
    expect_true(round(rt_model_robust$base_price, 0) == 427785)
    expect_true(round(rt_model_wgt$base_price, 0) == 427785)

    # Periods
    expect_is(rt_model_base$periods, 'data.frame')
    expect_true(nrow(rt_model_base$periods) == 84)
    expect_is(rt_model_robust$periods, 'data.frame')
    expect_true(nrow(rt_model_robust$periods) == 84)
    expect_is(rt_model_wgt$periods, 'data.frame')
    expect_true(nrow(rt_model_wgt$periods) == 84)

    # Approach
    expect_true(rt_model_base$approach == 'rt')
    expect_true(rt_model_robust$approach == 'rt')
    expect_true(rt_model_wgt$approach == 'rt')
  })

### modelToIndex --------------------------------------------------------------------

 context('modelToIndex')

 rt_model <- hpiModel(model_type = 'rt',
                      hpi_df = rt_df,
                      estimator = 'base',
                      log_dep = TRUE,
                      trim_model=TRUE)

 test_that('modelToIndex works', {

   expect_is(modelToIndex(rt_model), 'hpiindex')

 })

  test_that('modelToIndex works with other estimatort and options', {

    # Robust, LogDep=T, TrimModel=T
    expect_is(modelToIndex(hpiModel(model_type = 'rt',
                                    hpi_df = rt_df,
                                    estimator = 'robust',
                                    log_dep = TRUE,
                                    trim_model=TRUE)), 'hpiindex')

    # Weighted, LogDep=T, TrimModel=T
    expect_is(modelToIndex(hpiModel(model_type = 'rt',
                                    hpi_df = rt_df,
                                    estimator = 'weighted',
                                    log_dep = TRUE,
                                    trim_model=TRUE)), 'hpiindex')

    # Robust, LogDep=F, TrimModel=T
    expect_is(modelToIndex(hpiModel(model_type = 'rt',
                                    hpi_df = rt_df,
                                    estimator = 'robust',
                                    log_dep = FALSE,
                                    trim_model=TRUE)), 'hpiindex')

    # Weighted, LogDep=T, TrimModel=F
    expect_is(modelToIndex(hpiModel(model_type = 'rt',
                                    hpi_df = rt_df,
                                    estimator = 'weighted',
                                    log_dep = TRUE,
                                    trim_model=FALSE)), 'hpiindex')
  })

  test_that('modelToIndex fails with an error',{
    expect_error(modelToIndex(model_obj = 'abc'))
  })

  test_that('modelToIndex imputes properly, BASE model, LogDEP',{

    model_base <- hpiModel(model_type = 'rt',
                           hpi_df = rt_df,
                           estimator = 'base',
                           log_dep = TRUE,
                           trim_model=TRUE)

    # Impute a beginning value
    model_ex <- model_base
    model_ex$coefficients$coefficient[2] <- NA_real_
    expect_is(modelToIndex(model_ex), 'hpiindex')
    expect_true(!is.na(modelToIndex(model_ex)$value[2]))
    expect_true(modelToIndex(model_ex)$value[2] == 100)
    expect_true(modelToIndex(model_ex)$imputed[2] == 1)

    # Interpolate interior values
    model_ex <- model_base
    model_ex$coefficients$coefficient[3:5] <- NA_real_
    expect_is(modelToIndex(model_ex), 'hpiindex')
    expect_true(all(!is.na(modelToIndex(model_ex)$value[3:5])))

    # Extrapolate end periods
    model_ex <- model_base
    model_ex$coefficients$coefficient[81:84] <- NA_real_
    expect_is(modelToIndex(model_ex), 'hpiindex')
    expect_true(all(!is.na(modelToIndex(model_ex)$value[81:84])))
    expect_true(modelToIndex(model_ex)$value[80] ==
                  modelToIndex(model_ex)$value[84])

  })

  test_that('modelToIndex imputes properly, BASE model, LogDep=FALSE',{

    model_base <- hpiModel(model_type = 'rt',
                           hpi_df = rt_df,
                           estimator = 'base',
                           log_dep = FALSE,
                           trim_model=TRUE)

    # Extrapolate a beginning value
    model_ex <- model_base
    model_ex$coefficients$coefficient[2] <- NA_real_
    expect_is(modelToIndex(model_ex), 'hpiindex')
    expect_true(!is.na(modelToIndex(model_ex)$value[2]))
    expect_true(modelToIndex(model_ex)$value[2] == 100)
    expect_true(modelToIndex(model_ex)$imputed[2] == 1)

    # Impute interior values
    model_ex <- model_base
    model_ex$coefficients$coefficient[3:5] <- NA_real_
    expect_is(modelToIndex(model_ex), 'hpiindex')
    expect_true(all(!is.na(modelToIndex(model_ex)$value[3:5])))

    # Extrapolate an end value
    model_ex <- model_base
    model_ex$coefficients$coefficient[81:84] <- NA_real_
    expect_is(modelToIndex(model_ex), 'hpiindex')
    expect_true(all(!is.na(modelToIndex(model_ex)$value[81:84])))
    expect_true(modelToIndex(model_ex)$value[80] ==
                  modelToIndex(model_ex)$value[84])

  })

  test_that('modelToIndex imputes properly, Robust model, LogDEP = TRUE',{

    model_base <- hpiModel(model_type = 'rt',
                           hpi_df = rt_df,
                           estimator = 'robust',
                           log_dep = TRUE,
                           trim_model=TRUE)

    model_ex <- model_base
    model_ex$coefficients$coefficient[2] <- NA_real_
    expect_is(modelToIndex(model_ex), 'hpiindex')
    expect_true(!is.na(modelToIndex(model_ex)$value[2]))
    expect_true(modelToIndex(model_ex)$value[2] == 100)
    expect_true(modelToIndex(model_ex)$imputed[2] == 1)

    model_ex <- model_base
    model_ex$coefficients$coefficient[3:5] <- NA_real_
    expect_is(modelToIndex(model_ex), 'hpiindex')
    expect_true(all(!is.na(modelToIndex(model_ex)$value[3:5])))

    model_ex <- model_base
    model_ex$coefficients$coefficient[81:84] <- NA_real_
    expect_is(modelToIndex(model_ex), 'hpiindex')
    expect_true(all(!is.na(modelToIndex(model_ex)$value[81:84])))
    expect_true(modelToIndex(model_ex)$value[80] ==
                  modelToIndex(model_ex)$value[84])

  })

  test_that('modelToIndex imputes properly, Weighted model, LogDep=FALSE',{

    model_base <- hpiModel(model_type = 'rt',
                           hpi_df = rt_df,
                           estimator = 'weighted',
                           log_dep = FALSE,
                           trim_model=TRUE)

    model_ex <- model_base
    model_ex$coefficients$coefficient[2] <- NA_real_
    expect_is(modelToIndex(model_ex), 'hpiindex')
    expect_true(!is.na(modelToIndex(model_ex)$value[2]))
    expect_true(modelToIndex(model_ex)$value[2] == 100)
    expect_true(modelToIndex(model_ex)$imputed[2] == 1)

    model_ex <- model_base
    model_ex$coefficients$coefficient[3:5] <- NA_real_
    expect_is(modelToIndex(model_ex), 'hpiindex')
    expect_true(all(!is.na(modelToIndex(model_ex)$value[3:5])))

    model_ex <- model_base
    model_ex$coefficients$coefficient[81:84] <- NA_real_
    expect_is(modelToIndex(model_ex), 'hpiindex')
    expect_true(all(!is.na(modelToIndex(model_ex)$value[81:84])))
    expect_true(modelToIndex(model_ex)$value[80] ==
                  modelToIndex(model_ex)$value[84])

  })

  test_that('modelToIndex "max_period" cutoff works',{

    model_base <- hpiModel(model_type = 'rt',
                           hpi_df = rt_df,
                           estimator = 'weighted',
                           log_dep = FALSE,
                           trim_model=TRUE)

    # Create shortened index
    index_80 <- modelToIndex(model_base, max_period = 80)
    expect_is(index_80, 'hpiindex')
    expect_true(length(index_80$value) == 80)

  })

### smoothIndex()  -------------------------------------------------------------

  context('smoothIndex()')

  model_base <- hpiModel(model_type = 'rt',
                         hpi_df = rt_df,
                         estimator = 'base',
                         log_dep = TRUE,
                         trim_model=TRUE)

  index_base <- modelToIndex(model_obj = model_base)

  test_that('smoothing Function works with a variety of inputs',{

    # HPI Index object
    expect_is(index_smooth <- smoothIndex(index_obj = index_base,
                                          order = 4),
              'indexsmooth')

  })

  test_that('Errors are given when index is bad',{

    # Non-sensical index
    expect_error(index_smooth <- smoothIndex(index_obj = 'abc',
                                             order = 3))

    # Negative Order
    expect_error(index_smooth <- smoothIndex(index_obj = index_base,
                                             order = -3))

    # Char Window
    expect_error(index_smooth <- smoothIndex(index_obj = index_base,
                                             order = 'x'))

    # NA Window
    expect_error(index_smooth <- smoothIndex(index_obj = index_base,
                                             order = NA_integer_))

  })

  test_that('Returning in place works',{

    # Add it to the Full HPI Object (to the hpiindex object)
    expect_is(index_base <- smoothIndex(index = index_base,
                                        order = 3,
                                        in_place = TRUE),
              'hpiindex')
    expect_true('smooth' %in% names(index_base))

  })

### rtIndex() wrapper ---------------------------------------------------------------

context('rtindex() wrapper')

  test_that('Function works with proper inputs',{

    # Full case
    full_1 <- rtIndex(trans_df = sales,
                      date = 'sale_date',
                      price = 'sale_price',
                      trans_id = 'sale_id',
                      prop_id = 'pinx',
                      estimator = 'base',
                      log_dep = TRUE,
                      periodicity = 'monthly')

    expect_is(full_1, 'hpi')
    expect_true(full_1$model$estimator == 'base')

    # Giving a 'trans_df' object
    full_2 <- rtIndex(trans_df = sales_df,
                      price = 'sale_price',
                      trans_id = 'sale_id',
                      prop_id = 'pinx',
                      estimator = 'robust',
                      log_dep = TRUE)

    expect_is(full_2, 'hpi')
    expect_true(full_2$model$estimator == 'robust')

    # Giving an 'rt_df' object
    full_3 <- rtIndex(trans_df = rt_df,
                      estimator = 'weighted',
                      log_dep = TRUE)

    expect_is(full_3, 'hpi')
    expect_true(full_3$model$estimator == 'weighted')

  })

  test_that('Additional arguments in rtIndex() work',{

    ## rt Create arguments

    # Min Date Model with Clip
    mindate_index <- rtIndex(trans_df = sales,
                             date = 'sale_date',
                             price = 'sale_price',
                             trans_id = 'sale_id',
                             prop_id = 'pinx',
                             min_date = as.Date('2011-01-01'),
                             adj_type = 'clip')
    expect_true(min(mindate_index$index$period) == 2011)

    # Max Date Model with Adjust
    maxdate_index <- rtIndex(trans_df = sales,
                             date = 'sale_date',
                             price = 'sale_price',
                             trans_id = 'sale_id',
                             prop_id = 'pinx',
                             max_date = as.Date('2015-12-31'))
    expect_true(max(maxdate_index$index$period) == 2016)

    # Periodicity
    per_index <- rtIndex(trans_df = sales,
                         date = 'sale_date',
                         price = 'sale_price',
                         trans_id = 'sale_id',
                         prop_id = 'pinx',
                         periodicity = 'weekly')
    expect_true(max(per_index$index$period) == 364)

    # Sequence Only
    seq_index <- rtIndex(trans_df = sales_df,
                         date = 'sale_date',
                         price = 'sale_price',
                         trans_id = 'sale_id',
                         prop_id = 'pinx',
                         seq_only = TRUE)
    expect_true(nrow(seq_index$data) == 4823)

    ## HPI Model

    # Trim Model
    trim_index <- rtIndex(trans_df = rt_df,
                          trim_model=TRUE)
    expect_true(is.null(trim_index$model$model_obj$qr))

    # Log Dep & Robust
    ld_index <- rtIndex(trans_df = rt_df,
                        estimator = 'robust',
                        log_dep = FALSE)
    expect_true(ld_index$model$log_dep == FALSE)
    expect_true(ld_index$model$estimator == 'robust')

    ## Model to Index

    m2i_index <- rtIndex(trans_df = rt_df,
                         estimator = 'robust',
                         log_dep = FALSE,
                         max_period = 80)
    expect_true(length(m2i_index$index$value) == 80)

  })

  test_that("Bad arguments generate Errors: Full Case",{

    expect_error(rtIndex(trans_df = sales,
                         date = 'sale_price',
                         price = 'sale_price',
                         trans_id = 'sale_id',
                         prop_id = 'pinx',
                         estimator = 'base',
                         log_dep = TRUE,
                         periodicity = 'monthly'))

    expect_error(rtIndex(trans_df = sales,
                         date = 'sale_date',
                         price = 'sale_price',
                         prop_id = 'pinx',
                         estimator = 'base',
                         log_dep = TRUE,
                         periodicity = 'monthly'))

    expect_error(rtIndex(trans_df = sales,
                         date = 'sale_date',
                         price = 'sale_price',
                         trans_id = 'sale_id',
                         estimator = 'base',
                         log_dep = TRUE,
                         periodicity = 'monthly'))

    expect_error(rtIndex(trans_df = sales,
                         date = 'sale_date',
                         trans_id = 'sale_id',
                         prop_id = 'pinx',
                         estimator = 'base',
                         log_dep = TRUE,
                         periodicity = 'monthly'))

    expect_error(rtIndex(trans_df = sales,
                         date = 'sale_date',
                         price = 'sale_price',
                         trans_id = 'sale_id',
                         prop_id = 'pinx',
                         estimator = 'base',
                         log_dep = TRUE,
                         periodicity = 'xxx'))

    expect_error(rtIndex(trans_df = sales[1, ],
                         date = 'sale_date',
                         price = 'sale_price',
                         trans_id = 'sale_id',
                         prop_id = 'pinx',
                         estimator = 'base',
                         log_dep = TRUE,
                         periodicity = 'monthly'), 'Converting transactions')


  })

  test_that("Bad arguments generate errort: trans_df Case",{

    expect_error(rtIndex(trans_df = sales_df,
                         price = 'xx',
                         trans_id = 'sale_id',
                         prop_id = 'pinx',
                         estimator = 'base',
                         log_dep = TRUE))
    expect_error(rtIndex(trans_df = sales_df,
                         price = 'sale_price',
                         trans_id = 'xx',
                         prop_id = 'pinx',
                         estimator = 'base',
                         log_dep = TRUE))
    expect_error(rtIndex(trans_df = sales_df,
                         price = 'sale_price',
                         prop_id = 'pinx',
                         estimator = 'base',
                         log_dep = TRUE))

    expect_error(rtIndex(trans_df = sales_df,
                         price = 'sale_price',
                         trans_id = 'sale_id',
                         prop_id = 'xx',
                         estimator = 'base',
                         log_dep = TRUE))
  })

  test_that("Bad arguments handling: rt_sales Case",{

    # Bad estimatort default to 'base'
    expect_true(rtIndex(trans_df = rt_df,
                        estimator = 'basex',
                        log_dep = TRUE)$model$estimator == 'base')

    expect_error(rtIndex(trans_df = rt_df,
                         estimator = 'robust',
                         log_dep = 'a'))

    expect_error(rtIndex(trans_df = rt_df,
                         estimator = 'robust',
                         trim_model = 'a'))

    expect_error(rtIndex(trans_df = rt_df,
                         estimator = 'robust',
                         max_period = 'a'))
  })

  test_that("Smoothing in_place for 'hpi' object works",{

    full_1 <- rtIndex(trans_df = sales,
                      date = 'sale_date',
                      price = 'sale_price',
                      trans_id = 'sale_id',
                      prop_id = 'pinx',
                      estimator = 'base',
                      log_dep = TRUE,
                      periodicity = 'monthly',
                      smooth = TRUE)

    # In wrapper smoothing worked
    expect_is(full_1$index$smooth, 'indexsmooth')

    # Full HPI Object
    expect_is(index_smooth <- smoothIndex(index_obj = full_1,
                                           order = 6),
               'indexsmooth')

    # Add it to the Full HPI Object (to the hpiindex object) with new name
    expect_is(full_1s <- smoothIndex(index = full_1,
                                     order = 3,
                                     in_place = TRUE),
               'hpi')
    expect_is(full_1s$index$smooth, 'ts')
    expect_is(full_1s$index$smooth, 'indexsmooth')

  })

#*****************************************************************************************
#*****************************************************************************************
