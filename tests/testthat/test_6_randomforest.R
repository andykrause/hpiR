#*****************************************************************************************
#
#   Unit tests for hpiR package - Random Forest Method
#
#*****************************************************************************************

  library(hpiR)
  library(testthat)

 ## Load Data

  data(ex_sales)

  hed_df <- hedCreateTrans(trans_df = ex_sales,
                                       prop_id='pinx',
                                       trans_id='sale_id',
                                       price='sale_price',
                                       date='sale_date',
                                       periodicity='monthly')

  context('hpiModel.heddata(): before hedModel()')
  test_that('hpi Model with Hed works', {

    # Dep/Ind variety
    expect_is(rf_model <- hpiModel(model_type= 'rf',
                                   hpi_df = hed_df,
                                   estimator = 'pdp',
                                   dep_var = 'price',
                                   ind_var = c('tot_sf', 'beds', 'baths'),
                                   log_dep = TRUE,
                                   ntrees= 10,
                                   sim_count = 10),
              'hpimodel')

    # Full formula
    expect_is(rf_model <- hpiModel(model_type = 'rf',
                                   hpi_df = hed_df,
                                   estimator = 'pdp',
                                   mod_spec = as.formula(paste0('price ~ baths + tot_sf')),
                                   log_dep = TRUE,
                                   ntrees= 10,
                                   sim_count = 10),
              'hpimodel')
  })

  test_that('"log_dep" works both ways',{

    expect_true(hpiModel(model_type = 'rf',
                         hpi_df = hed_df,
                         estimator = 'pdp',
                         dep_var = 'price',
                         ind_var = c('tot_sf', 'beds', 'baths'),
                         log_dep = FALSE,
                         ntrees = 10,
                         sim_count = 10)$model_obj$predictions[1] > 100)

    expect_true(hpiModel(model_type = 'rf',
                         hpi_df = hed_df,
                         estimator = 'pdp',
                         dep_var = 'price',
                         ind_var = c('tot_sf', 'beds', 'baths'),
                         log_dep = TRUE,
                         ntrees = 10,
                         sim_count = 10)$model_obj$predictions[1] < 100)
  })

  test_that('Check for zero or negative prices works',{

    hed_dfx <- hed_df
    hed_dfx$price[1] <- 0

    # 0 in prices with Log Dep
    expect_error(rf_model <- hpiModel(model_type = 'rf',
                                       hpi_df = hed_dfx,
                                       estimator = 'pdp',
                                       dep_var = 'price',
                                       ind_var = c('tot_sf', 'beds', 'baths'),
                                       log_dep = TRUE))

    # 0 in prices with no Log Dep (Works)
    expect_is(rf_model <- hpiModel(model_type = 'rf',
                                    hpi_df = hed_dfx,
                                    estimator = 'pdp',
                                    dep_var = 'price',
                                    ind_var = c('tot_sf', 'beds', 'baths'),
                                    log_dep = FALSE,
                                    ntrees = 10,
                                    sim_count = 10),
              'hpimodel')

    ## NA
    hed_dfx$price[1] <- NA_integer_

    # NA in prices with Log Dep
    expect_error(rf_model <- hpiModel(model_type = 'rf',
                                       hpi_df = hed_dfx,
                                       estimator = 'pdp',
                                       dep_var = 'price',
                                       ind_var = c('tot_sf', 'beds', 'baths'),
                                       log_dep = TRUE))

    ## INF
    hed_dfx$price[1] <- Inf

    # Inf in prices with Log Dep
    expect_error(rf_model <- hpiModel(model_type = 'rf',
                                       hpi_df = hed_dfx,
                                       estimator = 'pdp',
                                       dep_var = 'price',
                                       ind_var = c('tot_sf', 'beds', 'baths'),
                                       log_dep = TRUE))

  })

### hedModel() ---------------------------------------------------------------------------

context('rfModel()')

test_that('Check for errors with bad arguments',{

  # Return warning if wrong rf_df
  expect_error(rf_model <- rfModel(rf_df = ex_sales,
                                   estimator = structure('pdp', class='pdp'),
                                   rf_spec = as.formula(paste0('log(price) ~ ',
                                                                  'as.factor(baths) + tot_sf'))))

  # Bad spec
  expect_error(rf_model <- rfModel(rf_df = hed_df,
                                   rf_spec = as.formula(paste0('log(x) ~ ',
                                                               'as.factor(baths) + tot_sf')),
                                   estimator=structure('pdp', class='pdp')))

  # Bad estimator class
  expect_error(rf_model <- hedModel(rf_df = hed_df,
                                    rf_spec = as.formula(paste0('log(price) ~ ',
                                                                'baths + tot_sf')),
                                    estimator = structure('fobust', class='fobust')))

  })

  test_that('Performance with sparse data',{

    ## Moderate Sparseness

    # Create a sparse data set
    hed_df200 <- hed_df[1:200, ]

    # Works
    expect_is(rf_model <- rfModel(rf_df = hed_df200,
                                  estimator = structure('pdp', class='pdp'),
                                  rf_spec = as.formula(paste0('log(price) ~ ',
                                                                 'baths + tot_sf'))),
              'rfmodel')

    ## Check severe sparseness

    # Create data set
    hed_df20 <- hed_df[1:20, ]

    # Works
    expect_is(rf_model <- rfModel(rf_df = hed_df20,
                                  estimator = structure('pdp', class='pdp'),
                                  rf_spec = as.formula(paste0('log(price) ~ ',
                                                              'baths + tot_sf'))),
              'rfmodel')

  })

### hpiModel.rf after hedModel --------------------------------------------------------

  context('hpiModel.rf()')

  test_that('hpiModel.rf works in both trim_model cases', {

    expect_is(rf_model <- hpiModel(model_type = 'rf',
                                    hpi_df = hed_df,
                                    estimator = 'pdp',
                                    log_dep = TRUE,
                                    dep_var = 'price',
                                    ind_var = c('tot_sf', 'beds', 'baths'),
                                    trim_model=TRUE,
                                    ntrees = 10,
                                    sim_count = 10), 'hpimodel')
    expect_is(rf_model$model_obj$forest, 'NULL')

  })

  test_that('hpiModel.heddata outputs are correct', {

    # Run a model of each estimator type
    rf_model <- hpiModel(model_type = 'rf',
                               hpi_df = hed_df,
                               estimator = 'pdp',
                               dep_var = 'price',
                               ind_var = c('tot_sf', 'beds', 'baths'),
                               log_dep = TRUE,
                               trim_model = TRUE,
                         ntrees = 10,
                         sim_count = 10)

    # Estimatohed
    expect_is(rf_model$estimator, 'pdp')

    # Coefficients
    expect_is(rf_model$coefficients, 'data.frame')
    expect_true(nrow(rf_model$coefficients) == 84)

    # Modelobj
    expect_is(rf_model$model_obj, 'rfmodel')

    # Model spec
    expect_true(is.null(rf_model$model_spec))

    # base price
    expect_true(round(rf_model$base_price, 0) == 618792)

    # Periods
    expect_is(rf_model$periods, 'data.frame')
    expect_true(nrow(rf_model$periods) == 84)

    # Approach
    expect_true(rf_model$approach == 'rf')
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
    expect_is(modelToIndex(hpiModel(model_type = 'rf',
                                    hpi_df = hed_df,
                                    estimator = 'pdp',
                                    log_dep = TRUE,
                                    trim_model = TRUE,
                                    ntrees = 10,
                                    sim_count = 10,
                                    dep_var = 'price',
                                    ind_var = c('tot_sf', 'beds', 'baths'))),
              'hpiindex')

  })

### smoothIndex()  -------------------------------------------------------------

  context('smoothIndex()')

  model_pdp <- hpiModel(model_type = 'rf',
                        hpi_df = hed_df,
                        estimator = 'pdp',
                         log_dep = TRUE,
                         trim_model=TRUE,
                         sim_count = 10,
                         ntrees = 10,
                         dep_var = 'price',
                         ind_var = c('tot_sf', 'beds', 'baths'))

  index_pdp <- modelToIndex(model_obj = model_pdp)

  test_that('smoothing Function works with a variety of inputs',{

    # HPI Index object
    expect_is(index_smooth <- smoothIndex(index_obj = index_pdp,
                                          order = 4),
              'indexsmooth')

  })

  ### hedIndex() wrapper ---------------------------------------------------------------

  context('rfindex() wrapper')

  test_that('Function works with proper inputs',{

    # Full case
    full_1 <- rfIndex(trans_df = ex_sales,
                       date = 'sale_date',
                       price = 'sale_price',
                       trans_id = 'sale_id',
                       prop_id = 'pinx',
                       estimator = 'pdp',
                       periodicity = 'monthly',
                       dep_var = 'price',
                       ind_var = c('tot_sf', 'beds', 'baths'),
                      sim_count = 10,
                      ntrees = 10)

    expect_is(full_1, 'hpi')
    expect_true(full_1$model$estimator == 'pdp')

    # Giving an 'rs_df' object
    full_2 <- rfIndex(trans_df = hed_df,
                      estimator = 'pdp',
                      log_dep = FALSE,
                      rf_spec = as.formula(price ~ baths + beds),
                      sim_count = 10,
                      ntrees = 10)

    expect_is(full_2, 'hpi')
    expect_true(full_2$model$estimator == 'pdp')

    # Giving an 'rs_df' object
    full_3 <- rfIndex(trans_df = hed_df,
                      estimator = 'pdp',
                      log_dep = FALSE,
                      rf_spec = as.formula(price ~ baths + beds),
                      sim_count = 10,
                      ntrees = 10,
                      smooth = TRUE,
                      smooth_order = 5)

    expect_is(full_3, 'hpi')
    expect_true(full_3$model$estimator == 'pdp')


  })

  test_that('Additional arguments in hedIndex() work',{

    ## HED Create arguments

    # Min Date Model with Clip
    addarg_index <- rfIndex(trans_df = ex_sales,
                            date='sale_date',
                            price = 'sale_price',
                            trans_id = 'sale_id',
                            prop_id = 'pinx',
                            estimator = 'pdp',
                             dep_var = 'price',
                             ind_var = c('tot_sf', 'beds', 'baths'),
                             min_date = as.Date('2011-01-01'),
                             max_date = as.Date('2015-12-31'),
                             periodicity = 'annual',
                             adj_type = 'clip',
                            sim_per = 0.01,
                            ntrees = 17)
    expect_is(addarg_index, 'hpi')
    expect_true(min(addarg_index$index$period) == 2011)

  })

  test_that("Bad arguments generate Errors: Full Case",{

    # Bad Date
    expect_error(rfIndex(trans_df = ex_sales,
                          date = 'sale_price',
                          price = 'sale_price',
                          trans_id = 'sale_id',
                          prop_id = 'pinx',
                          estimator = 'pdp',
                          log_dep = TRUE,
                          dep_var = 'price',
                          ind_var = c('tot_sf', 'beds', 'baths'),
                          periodicity = 'monthly'))

    expect_error(rfIndex(trans_df = ex_sales,
                         date = 'sale_date',
                         trans_id = 'sale_id',
                         prop_id = 'pinx',
                         estimator = 'pdp',
                         log_dep = TRUE,
                         dep_var = 'price',
                         ind_var = c('tot_sf', 'beds', 'baths'),
                         periodicity = 'monthly'))

    expect_error(rfIndex(trans_df = sales_df,
                          price = 'xx',
                          trans_id = 'sale_id',
                          prop_id = 'pinx',
                          estimator = 'pdp',
                          log_dep = TRUE,
                          dep_var = 'price',
                          ind_var = c('tot_sf', 'beds', 'baths')))

    expect_error(rfIndex(trans_df = sales_df,
                         price = 'sale_price',
                         prop_id = 'pinx',
                         estimator = 'pdp',
                         log_dep = TRUE,
                         dep_var = 'price',
                         ind_var = c('tot_sf', 'beds', 'baths')))

    expect_error(rfIndex(trans_df = sales_df,
                         price = 'sale+price',
                         trans_id = 'sale_id',
                         estimator = 'pdp',
                         log_dep = TRUE,
                         dep_var = 'price',
                         ind_var = c('tot_sf', 'beds', 'baths')))

    expect_error(rfIndex(trans_df = ex_sales,
                         date = 'sale_date',
                         price = 'sale_price',
                         trans_id = 'sale_id',
                         prop_id = 'pinx',
                         estimator = 'pdp',
                         log_dep = TRUE,
                         periodicity = 'monthly'))

  })

  test_that("Smoothing in_place for 'hpi' object works",{

    # Full case
    full_1 <- rfIndex(trans_df = ex_sales,
                       date = 'sale_date',
                       price = 'sale_price',
                       trans_id = 'sale_id',
                       prop_id = 'pinx',
                       estimator = 'pdp',
                       periodicity = 'monthly',
                       dep_var = 'price',
                       ind_var = c('tot_sf', 'beds', 'baths'),
                       smooth = TRUE,
                       sim_ids = 1:10,
                       ntrees = 16)

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
