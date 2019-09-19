#*****************************************************************************************
#                                                                                        *
#   Unit tests for hpiR package - Hedonic Price Functions                                *
#                                                                                        *
#*****************************************************************************************

  library(hpiR)
  library(testthat)

  ## Load Data

  sales <- get(data(seattle_sales))

### hedCreateTrans() ----------------------------------------------------------------

context('hedCreateTrans()')

  # Test Setup
  test_that("Can take a functional 'trans_df' object", {

    sales_df <- dateToPeriod(trans_df = sales,
                             date = 'sale_date',
                             periodicity = 'monthly')

    expect_is(hed_df <- hedCreateTrans(trans_df=sales_df,
                                       prop_id='pinx',
                                       trans_id='sale_id',
                                       price='sale_price'),
              'heddata')
    expect_true(nrow(hed_df) == 43074)
  })

  # Test Setup
  test_that("Can create own trans_df object", {

    expect_is(hed_df <- hedCreateTrans(trans_df=sales,
                                       prop_id='pinx',
                                       trans_id='sale_id',
                                       price='sale_price',
                                       date='sale_date',
                                       periodicity='monthly'),
              'heddata')
    expect_true(nrow(hed_df) == 43074)
    assign('hed_df', hed_df, .GlobalEnv)
  })

  test_that("Can use min/max dates own trans_df object", {

    # Min date with move

    expect_is(hed_df <- hedCreateTrans(trans_df=sales,
                                       prop_id='pinx',
                                       trans_id='sale_id',
                                       price='sale_price',
                                       date='sale_date',
                                       periodicity='monthly',
                                       min_date = as.Date('2012-03-21')),
              'heddata')
    expect_true(nrow(hed_df) == 43074)

    # Min date with adj
    expect_is(hed_df <- hedCreateTrans(trans_df=sales,
                                       prop_id='pinx',
                                       trans_id='sale_id',
                                       price='sale_price',
                                       date='sale_date',
                                       periodicity='monthly',
                                       min_date = as.Date('2012-03-21'),
                                       adj_type='clip'),
              'heddata')
    expect_true(nrow(hed_df) == 33922)

    # Max with move
    expect_is(hed_df <- hedCreateTrans(trans_df=sales,
                                       prop_id='pinx',
                                       trans_id='sale_id',
                                       price='sale_price',
                                       date='sale_date',
                                       periodicity='monthly',
                                       max_date = as.Date('2015-03-21')),
              'heddata')
    expect_true(nrow(hed_df) == 43074)

    # Max with clip
    expect_is(hed_df <- hedCreateTrans(trans_df=sales,
                                       prop_id='pinx',
                                       trans_id='sale_id',
                                       price='sale_price',
                                       date='sale_date',
                                       periodicity='monthly',
                                       max_date = as.Date('2014-03-21'),
                                       adj_type='clip'),
              'heddata')
    expect_true(nrow(hed_df) == 21536)

  })

  test_that("Fails if sales creation fails", {

    # Bad Date field
    expect_error(hed_df <- hedCreateTrans(trans_df=sales,
                                          prop_id='pinx',
                                          trans_id='sale_id',
                                          price='sale_price',
                                          date='sale_price',
                                          periodicity='monthly'))

    # Bad Periodicity field
    expect_error(hed_df <- hedCreateTrans(trans_df=sales,
                                          prop_id='pinx',
                                          trans_id='sale_id',
                                          price='sale_price',
                                          date='sale_date',
                                          periodicity='mocnthly'))

  })

  # Create sales data to use in future tests
  sales_df <- dateToPeriod(trans_df = sales,
                           date = 'sale_date',
                           periodicity = 'monthly')

  test_that("Fails if bad arguments fails", {

    # Bad prop_id field
    expect_error(hed_df <- hedCreateTrans(trans_df=sales_df,
                                          prop_id='pinxx',
                                          trans_id='sale_id',
                                          price='sale_price'))

    # Bad sale_id field
    expect_error(hed_df <- hedCreateTrans(trans_df=sales_df,
                                          prop_id='pinx',
                                          trans_id='salex_id',
                                          price='sale_price'))

    # Bad price field
    expect_error(hed_df <- hedCreateTrans(trans_df=sales_df,
                                          prop_id='pinx',
                                          trans_id='sale_id',
                                          price='salex_price'))

  })

  test_that("Returns NULL if no sales", {

    expect_is(hed_df <- hedCreateTrans(trans_df=sales_df[0,],
                                       prop_id='pinx',
                                       trans_id='sale_id',
                                       price='sale_price'), "NULL")

  })

## hpiModel.heddata up to hedModel ------------------------------------------------------

context('hpiModel.heddata(): before hedModel()')

  # Create hed data
  hed_df <- hedCreateTrans(trans_df=sales,
                           prop_id='pinx',
                           trans_id='sale_id',
                           price='sale_price',
                           date='sale_date',
                           periodicity='monthly')


  ## Test regarding hpi model

  test_that('hpi Model with Hed works', {

    # # Dep/Ind variety
    # expect_is(hed_model <- hpiModel(hpi_df = hed_df,
    #                                 estimator = 'base',
    #                                 dep_var = 'price',
    #                                 ind_var = c('tot_sf', 'beds', 'baths'),
    #                                 log_dep = TRUE),
    #           'hpimodel')

    # Full formula
    expect_is(hed_model <- hpiModel(model_type = 'hed',
                                    hpi_df = hed_df,
                                    estimator = 'base',
                                    mod_spec = as.formula(paste0('log(price) ~ as.factor(baths)',
                                                                 ' + tot_sf')),
                                    log_dep = TRUE),
              'hpimodel')

    # Dep/Ind variety
    expect_is(hed_model <- hpiModel(model_type = 'hed',
                                    hpi_df = hed_df,
                                    estimator = 'robust',
                                    dep_var = 'price',
                                    ind_var = c('tot_sf', 'beds', 'baths'),
                                    log_dep = TRUE),
              'hpimodel')

    # # Full formula
    # expect_is(hed_model <- hpiModel(hpi_df = hed_df,
    #                                 estimator = 'robust',
    #                                 hed_spec = as.formula(paste0('log(price) ~ as.factor(baths)',
    #                                                              ' + tot_sf')),
    #                                 log_dep = TRUE),
    #           'hpimodel')

    # Dep/Ind variety
    expect_is(hed_model <- hpiModel(model_type = 'hed',
                                    hpi_df = hed_df,
                                    estimator = 'weighted',
                                    dep_var = 'price',
                                    ind_var = c('tot_sf', 'beds', 'baths'),
                                    log_dep = TRUE,
                                    weights = runif(nrow(hed_df), 0, 1)),
              'hpimodel')

    # # Full formula
    # expect_is(hed_model <- hpiModel(hpi_df = hed_df,
    #                                 estimator = 'weighted',
    #                                 hed_spec = as.formula(paste0('log(price) ~ as.factor(baths)',
    #                                                              ' + tot_sf')),
    #                                 log_dep = TRUE,
    #                                 weights = runif(nrow(hed_df), 0, 1)),
    #           'hpimodel')

  })

  test_that('"log_dep" works both ways',{

    # expect_true(hpiModel(hpi_df = hed_df,
    #                      estimator = 'base',
    #                      dep_var = 'price',
    #                      ind_var = c('tot_sf', 'beds', 'baths'),
    #                      log_dep = TRUE)$model_obj$fitted.values[1] < 20)

    expect_true(hpiModel(model_type = 'hed',
                         hpi_df = hed_df,
                         estimator = 'robust',
                         dep_var = 'price',
                         ind_var = c('tot_sf', 'beds', 'baths'),
                         log_dep = FALSE)$model_obj$fitted.values[1] > 10000)

  })

  test_that('Check for zero or negative prices works',{

    hed_dfx <- hed_df
    hed_dfx$price[1] <- 0

    # 0 in prices with Log Dep
    expect_error(hed_model <- hpiModel(model_type = 'hed',
                                       hpi_df = hed_dfx,
                                       estimator = 'base',
                                       dep_var = 'price',
                                       ind_var = c('tot_sf', 'beds', 'baths'),
                                       log_dep = TRUE))

    # 0 in prices with no Log Dep (Works)
    expect_is(hed_model <- hpiModel(model_type = 'hed',
                                    hpi_df = hed_dfx,
                                    estimator = 'base',
                                    dep_var = 'price',
                                    ind_var = c('tot_sf', 'beds', 'baths'),
                                    log_dep = FALSE),
              'hpimodel')

    ## NA
    hed_dfx$price[1] <- NA_integer_

    # NA in prices with Log Dep
    expect_error(hed_model <- hpiModel(model_type = 'hed',
                                       hpi_df = hed_dfx,
                                       estimator = 'base',
                                       dep_var = 'price',
                                       ind_var = c('tot_sf', 'beds', 'baths'),
                                       log_dep = TRUE))

    # NA in prices with no Log Dep
    expect_error(hed_model <- hpiModel(model_type = 'hed',
                                       hpi_df = hed_dfx,
                                       estimator = 'base',
                                       dep_var = 'price',
                                       ind_var = c('tot_sf', 'beds', 'baths'),
                                       log_dep = FALSE))

    ## INF
    hed_dfx$price[1] <- Inf

    # Inf in prices with Log Dep
    expect_error(hed_model <- hpiModel(model_type = 'hed',
                                       hpi_df = hed_dfx,
                                       estimator = 'base',
                                       dep_var = 'price',
                                       ind_var = c('tot_sf', 'beds', 'baths'),
                                       log_dep = TRUE))

    # Inf in prices with no Log Dep
    expect_error(hed_model <- hpiModel(model_type = 'hed',
                                       hpi_df = hed_dfx,
                                       estimator = 'base',
                                       dep_var = 'price',
                                       ind_var = c('tot_sf', 'beds', 'baths'),
                                       log_dep = FALSE))

  })

  test_that('Check for estimator type works',{

    # Base
    expect_true(hpiModel(model_type = 'hed',
                         hpi_df = hed_df,
                         dep_var = 'price',
                         ind_var = c('tot_sf', 'beds', 'baths'))$estimator == 'base')

    # Robust
    expect_true(hpiModel(model_type = 'hed',
                         hpi_df = hed_df,
                         estimator = 'robust',
                         dep_var = 'price',
                         ind_var = c('tot_sf', 'beds', 'baths'))$estimator == 'robust')

    # Weighted without Weights
    expect_true(hpiModel(model_type = 'hed',
                         hpi_df = hed_df,
                         estimator = 'weighted',
                         dep_var = 'price',
                         ind_var = c('tot_sf', 'beds', 'baths'))$estimator == 'base')

    # Weighted with weights
    expect_true(hpiModel(model_type = 'hed',
                         hpi_df = hed_df,
                         estimator = 'weighted',
                         dep_var = 'price',
                         ind_var = c('tot_sf', 'beds', 'baths'),
                         weights = runif(nrow(hed_df), 0, 1))$estimator == 'weighted')

  })

### hedModel() ---------------------------------------------------------------------------

context('hedModel()')

  test_that('Check for errors with bad arguments',{

    # Base: Return warning if wrong rs_df
    expect_error(hed_model <- hedModel(hed_df = sales,
                                       estimator=structure('base', class='base'),
                                       hed_spec = as.formula(paste0('log(price) ~ ',
                                                          'as.factor(baths) + tot_sf'))))

    # Weighted: Bad spec
    expect_error(hed_model <- hedModel(hed_df = hed_df,
                                       hed_spec = as.formula(paste0('log(x) ~ ',
                                                                    'as.factor(baths) + tot_sf')),
                                       estimator=structure('weighted', class='weighted')))

    # Bad estimator class
    expect_error(hed_model <- hedModel(hed_df = hed_df,
                                       hed_spec = as.formula(paste0('log(price) ~ ',
                                                            'as.factor(baths) + tot_sf')),
                                       estimator=structure('fobust', class='fobust')))

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
              'hedmodel')

    # Robust works but gives warning
    expect_is(hed_model <- hedModel(hed_df = hed_df200,
                                    estimator=structure('robust', class='robust'),
                                    hed_spec = as.formula(paste0('log(price) ~ ',
                                                                 'as.factor(baths) + tot_sf'))),
              'hedmodel')

    # Weighted works
    expect_is(hed_model <- hedModel(hed_df = hed_df200,
                                    estimator=structure('weighted', class='weighted'),
                                    hed_spec = as.formula(paste0('log(price) ~ ',
                                                                 'as.factor(baths) + tot_sf')),
                                    weights = runif(nrow(hed_df200), 0, 1)),
              'hedmodel')

    ## Check severe sparseness

    # Create data set
    hed_df20 <- hed_df[1:20, ]

    # Works with base, though many NAs
    expect_is(hed_model <- hedModel(hed_df = hed_df20,
                                    estimator=structure('base', class='base'),
                                    hed_spec = as.formula(paste0('log(price) ~ ',
                                                                 'as.factor(baths) + tot_sf'))),
              'hedmodel')

    # Robust works but gives warning
    expect_is(hed_model <- hedModel(hed_df = hed_df20,
                                    estimator=structure('robust', class='robust'),
                                    hed_spec = as.formula(paste0('log(price) ~ ',
                                                                 'as.factor(baths) + tot_sf'))),
              'hedmodel')

    # Weighted works
    expect_is(hed_model <- hedModel(hed_df = hed_df20,
                                    estimator=structure('weighted', class='weighted'),
                                    hed_spec = as.formula(paste0('log(price) ~ ',
                                                                 'as.factor(baths) + tot_sf')),
                                    weights = runif(nrow(hed_df20), 0, 1)),
              'hedmodel')

  })

### hpiModel.heddata after hedModel --------------------------------------------------------

context('hpiModel.heddata()')

  test_that('hpiModel.heddata works in both trim_model cases', {

    # Base
    # expect_is(hed_model <- hpiModel(hpi_df = hed_df,
    #                                 estimator = 'base',
    #                                 log_dep = TRUE,
    #                                 dep_var = 'price',
    #                                 ind_var = c('tot_sf', 'beds', 'baths'),
    #                                 trim_model=FALSE), 'hpimodel')
    # expect_is(hed_model$model_obj$qr, 'qr')

    expect_is(hed_model <- hpiModel(model_type = 'hed',
                                    hpi_df = hed_df,
                                    estimator = 'base',
                                    log_dep = TRUE,
                                    dep_var = 'price',
                                    ind_var = c('tot_sf', 'beds', 'baths'),
                                    trim_model=TRUE), 'hpimodel')
    expect_is(hed_model$model_obj$qr, 'NULL')

    # Robust
    expect_is(hed_model <- hpiModel(model_type = 'hed',
                                    hpi_df = hed_df,
                                    estimator = 'robust',
                                    log_dep = TRUE,
                                    dep_var = 'price',
                                    ind_var = c('tot_sf', 'beds', 'baths'),
                                    trim_model=FALSE), 'hpimodel')
    expect_is(hed_model$model_obj$qr, 'qr')

    # expect_is(hed_model <- hpiModel(hpi_df = hed_df,
    #                                 estimator = 'robust',
    #                                 log_dep = TRUE,
    #                                 dep_var = 'price',
    #                                 ind_var = c('tot_sf', 'beds', 'baths'),
    #                                 trim_model=TRUE), 'hpimodel')
    # expect_is(hed_model$model_obj$qr, 'NULL')

    # Weighted
    expect_is(hed_model <- hpiModel(model_type = 'hed',
                                    hpi_df = hed_df,
                                    estimator = 'weighted',
                                    log_dep = TRUE,
                                    dep_var = 'price',
                                    ind_var = c('tot_sf', 'beds', 'baths'),
                                    trim_model=FALSE,
                                    weights = runif(nrow(hed_df), 0, 1)),
              'hpimodel')
    expect_is(hed_model$model_obj$qr, 'qr')

    # expect_is(hed_model <- hpiModel(hpi_df = hed_df,
    #                                 estimator = 'weighted',
    #                                 log_dep = TRUE,
    #                                 dep_var = 'price',
    #                                 ind_var = c('tot_sf', 'beds', 'baths'),
    #                                 trim_model=TRUE,
    #                                 weights = runif(nrow(hed_df), 0, 1)),
    #           'hpimodel')
    # expect_is(hed_model$model_obj$qr, 'NULL')

  })

  test_that('hpiModel.heddata outputs are correct', {

    # Run a model of each estimator type
    hed_model_base <- hpiModel(model_type = 'hed',
                               hpi_df = hed_df,
                               estimator = 'base',
                               dep_var = 'price',
                               ind_var = c('tot_sf', 'beds', 'baths'),
                               log_dep = TRUE,
                               trim_model=TRUE)

    hed_model_robust <- hpiModel(model_type = 'hed',
                                 hpi_df = hed_df,
                                 estimator = 'robust',
                                 dep_var = 'price',
                                 ind_var = c('tot_sf', 'beds', 'baths'),
                                 log_dep = TRUE,
                                 trim_model=FALSE)

    hed_model_wgt <- hpiModel(model_type = 'hed',
                              hpi_df = hed_df,
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
    expect_is(hed_model_base$model_obj, 'hedmodel')
    expect_is(hed_model_robust$model_obj, 'hedmodel')
    expect_is(hed_model_wgt$model_obj, 'hedmodel')

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


### modelToIndex() (HED) --------------------------------------------------------------------

  context('modelToIndex(): hed')

  hed_model <- hpiModel(model_type = 'hed',
                        hpi_df = hed_df,
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
    expect_is(modelToIndex(hpiModel(model_type = 'hed',
                                    hpi_df = hed_df,
                                    estimator = 'robust',
                                    log_dep = TRUE,
                                    trim_model=TRUE,
                                    dep_var = 'price',
                                    ind_var = c('tot_sf', 'beds', 'baths'))),
              'hpiindex')

    # # Weighted, LogDep=T, TrimModel=T
    # expect_is(modelToIndex(hpiModel(hpi_df = hed_df,
    #                                 estimator = 'weighted',
    #                                 log_dep = TRUE,
    #                                 trim_model=TRUE,
    #                                 dep_var = 'price',
    #                                 ind_var = c('tot_sf', 'beds', 'baths'),
    #                                 weights=runif(nrow(hed_df), 0, 1))),
    #           'hpiindex')

    # # Robust, LogDep=F, TrimModel=T
    # expect_is(modelToIndex(hpiModel(hpi_df = hed_df,
    #                                 estimator = 'robust',
    #                                 log_dep = FALSE,
    #                                 trim_model=TRUE,
    #                                 dep_var = 'price',
    #                                 ind_var = c('tot_sf', 'beds', 'baths'))),
    #           'hpiindex')

    # Weighted, LogDep=T, TrimModel=F
    expect_is(modelToIndex(hpiModel(model_type = 'hed',
                                    hpi_df = hed_df,
                                    estimator = 'weighted',
                                    log_dep = TRUE,
                                    trim_model=FALSE,
                                    dep_var = 'price',
                                    ind_var = c('tot_sf', 'beds', 'baths'),
                                    weights=runif(nrow(hed_df), 0, 1))),
              'hpiindex')
  })

  test_that('modelToIndex imputes properly, BASE model, LogDEP',{

    model_base <- hpiModel(model_type = 'hed',
                           hpi_df = hed_df,
                           estimator = 'base',
                           log_dep = TRUE,
                           trim_model=TRUE,
                           dep_var = 'price',
                           ind_var = c('tot_sf', 'beds', 'baths'))

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

    model_base <- hpiModel(model_type = 'hed',
                           hpi_df = hed_df,
                           estimator = 'base',
                           log_dep = FALSE,
                           trim_model=TRUE,
                           dep_var = 'price',
                           ind_var = c('tot_sf', 'beds', 'baths'))

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

  ### smoothIndex()  -------------------------------------------------------------

  context('smoothIndex()')

  model_base <- hpiModel(model_type = 'hed',
                         hpi_df = hed_df,
                         estimator = 'base',
                         log_dep = TRUE,
                         trim_model=TRUE,
                         dep_var = 'price',
                         ind_var = c('tot_sf', 'beds', 'baths'))

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

### hedIndex() wrapper ---------------------------------------------------------------

context('hedindex() wrapper')

  test_that('Function works with proper inputs',{

    # Full case
    full_1 <- hedIndex(trans_df = sales,
                       date = 'sale_date',
                       price = 'sale_price',
                       trans_id = 'sale_id',
                       prop_id = 'pinx',
                       estimator = 'base',
                       periodicity = 'monthly',
                       dep_var = 'price',
                       ind_var = c('tot_sf', 'beds', 'baths'))

    expect_is(full_1, 'hpi')
    expect_true(full_1$model$estimator == 'base')

    # Giving a 'trans_df' object
    full_2 <- hedIndex(trans_df = sales_df,
                       price = 'sale_price',
                       trans_id = 'sale_id',
                       prop_id = 'pinx',
                       estimator = 'robust',
                       hed_spec = as.formula(paste0('log(price) ~ ',
                                                    'as.factor(baths) + tot_sf')))

    expect_is(full_2, 'hpi')
    expect_true(full_2$model$estimator == 'robust')

    # Giving an 'rs_df' object
    full_3 <- hedIndex(trans_df = hed_df,
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
    addarg_index <- hedIndex(trans_df = sales,
                             date='sale_date',
                             price = 'sale_price',
                             trans_id = 'sale_id',
                             prop_id = 'pinx',
                             estimator = 'robust',
                             dep_var = 'price',
                             ind_var = c('tot_sf', 'beds', 'baths'),
                             min_date = as.Date('2011-01-01'),
                             max_date = as.Date('2015-12-31'),
                             periodicity = 'annual',
                             adj_type = 'clip')
   expect_is(addarg_index, 'hpi')
   expect_true(min(addarg_index$index$period) == 2011)

    ## HPI Model

    # Trim Model
    trim_index <- hedIndex(trans_df = hed_df,
                           dep_var = 'price',
                           ind_var = c('tot_sf', 'beds', 'baths'),
                           trim_model=TRUE)
    expect_true(is.null(trim_index$model$model_obj$qr))

    # Log Dep & Robust
    ld_index <- hedIndex(trans_df = hed_df,
                         estimator = 'robust',
                         dep_var = 'price',
                         ind_var = c('tot_sf', 'beds', 'baths'),
                         log_dep = FALSE,
                         smooth = TRUE,
                         smooth_order = 5)
    expect_true(ld_index$model$log_dep == FALSE)
    expect_true(ld_index$model$estimator == 'robust')

    ## Model to Index
    m2i_index <- hedIndex(trans_df = hed_df,
                          estimator = 'robust',
                          log_dep = FALSE,
                          dep_var = 'price',
                          ind_var = c('tot_sf', 'beds', 'baths'),
                          max_period = 80)
    expect_true(length(m2i_index$index$value) == 80)

  })

  test_that("Bad arguments generate Errors: Full Case",{

    # Bad Date
    expect_error(hedIndex(trans_df = sales,
                          date = 'sale_price',
                          price = 'sale_price',
                          trans_id = 'sale_id',
                          prop_id = 'pinx',
                          estimator = 'base',
                          log_dep = TRUE,
                          dep_var = 'price',
                          ind_var = c('tot_sf', 'beds', 'baths'),
                          periodicity = 'monthly'))

    # Bad Periodicity
    expect_error(hedIndex(trans_df = sales,
                          date = 'sale_date',
                          price = 'sale_price',
                          trans_id = 'sale_id',
                          prop_id = 'pinx',
                          estimator = 'base',
                          log_dep = TRUE,
                          dep_var = 'price',
                          ind_var = c('tot_sf', 'beds', 'baths'),
                          periodicity = 'xxx'))

    # N0 spec
    expect_error(hedIndex(trans_df = sales,
                          date = 'sale_date',
                          price = 'sale_price',
                          trans_id = 'sale_id',
                          prop_id = 'pinx',
                          estimator = 'base',
                          log_dep = TRUE,
                          periodicity = 'monthly'))
  })

  test_that("Bad arguments generate errors: trans_df Case",{

    expect_error(hedIndex(trans_df = sales_df,
                          price = 'xx',
                          trans_id = 'sale_id',
                          prop_id = 'pinx',
                          estimator = 'base',
                          log_dep = TRUE,
                          dep_var = 'price',
                          ind_var = c('tot_sf', 'beds', 'baths')))

    expect_error(hedIndex(trans_df = sales_df,
                          trans_id = 'sale_id',
                          prop_id = 'pinx',
                          estimator = 'base',
                          log_dep = TRUE,
                          dep_var = 'price',
                          ind_var = c('tot_sf', 'beds', 'baths')))

    expect_error(hedIndex(trans_df = sales_df,
                          price = 'sale_price',
                          trans_id = 'xx',
                          prop_id = 'pinx',
                          estimator = 'base',
                          log_dep = TRUE,
                          dep_var = 'price',
                          ind_var = c('tot_sf', 'beds', 'baths')))

    expect_error(hedIndex(trans_df = sales_df,
                          price = 'sale_price',
                          prop_id = 'pinx',
                          estimator = 'base',
                          log_dep = TRUE,
                          dep_var = 'price',
                          ind_var = c('tot_sf', 'beds', 'baths')))

    expect_error(hedIndex(trans_df = sales_df,
                          price = 'sale_price',
                          trans_id = 'sale_id',
                          prop_id = 'xx',
                          estimator = 'base',
                          log_dep = TRUE,
                          dep_var = 'price',
                          ind_var = c('tot_sf', 'beds', 'baths')))

    expect_error(hedIndex(trans_df = sales_df,
                          price = 'sale_price',
                          trans_id = 'sale_id',
                          estimator = 'base',
                          log_dep = TRUE,
                          dep_var = 'price',
                          ind_var = c('tot_sf', 'beds', 'baths')))
  })

  test_that("Bad arguments handling: hed_sales Case",{

    # Bad estimators default to 'base'
    expect_true(hedIndex(trans_df = hed_df,
                         estimator = 'basex',
                         log_dep = TRUE,
                         dep_var = 'price',
                         ind_var = c('tot_sf', 'beds', 'baths'))$model$estimator == 'base')

    expect_error(hedIndex(trans_df = hed_df,
                          estimator = 'robust',
                          dep_var = 'price',
                          ind_var = c('tot_sf', 'beds', 'baths'),
                          log_dep = 'a'))

    expect_error(hedIndex(trans_df = hed_df,
                          estimator = 'robust',
                          trim_model = 'a',
                          dep_var = 'price',
                          ind_var = c('tot_sf', 'beds', 'baths')))

    expect_error(hedIndex(trans_df = hed_df,
                          estimator = 'robust',
                          max_period = 'a',
                          dep_var = 'price',
                          ind_var = c('tot_sf', 'beds', 'baths')))
  })

  test_that("Smoothing in_place for 'hpi' object works",{

    # Full case
    full_1 <- hedIndex(trans_df = sales,
                       date = 'sale_date',
                       price = 'sale_price',
                       trans_id = 'sale_id',
                       prop_id = 'pinx',
                       estimator = 'base',
                       periodicity = 'monthly',
                       dep_var = 'price',
                       ind_var = c('tot_sf', 'beds', 'baths'),
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
