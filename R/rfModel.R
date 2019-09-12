#'
#' Estimate hedonic model for index creation
#'
#' Estimate coefficients for an index via the hedonic approach (generic method)
#'
#' @param estimator Type of model to estimates (base, robust, weighted)
#' @param rf_df Transactions dataset from hedCreateSales()
#' @param rf_spec Model specification (`formula` object)
#' @param ntrees [200] Set number of trees to use
#' @param seed [1] Random seed for reproducibility
#' @param ... Additional arguments
#' @return `hedmodel` object: model object of the estimator (ex.: `lm`)
#' @importFrom utils methods
#' @importFrom stats median lm as.formula
#' @section Further Details:
#' `estimator` argument must be in a class of 'base', 'weighted' or 'robust'
#' This function is not generally called directly, but rather from `hpiModel()`
#' @examples
#'
#'  # Load example data
#'  data(ex_sales)
#'
#'  # Create hedonic data
#'  hed_data <- hedCreateTrans(trans_df = ex_sales,
#'                            prop_id = 'pinx',
#'                            trans_id = 'sale_id',
#'                            price = 'sale_price',
#'                            date = 'sale_date',
#'                            periodicity = 'monthly')
#'
#'  # Estimate Model
#'  rf_model <- rfModel(estimator = structure('base', class = 'base'),
#'                      rf_df = hed_data,
#'                      rf_spec = as.formula(log(price) ~ baths + tot_sf))
#'
#' @export

rfModel <- function(estimator,
                    rf_df,
                    rf_spec,
                    ntrees = 200,
                    seed = 1,
                    ...){

  ## Check for proper classes

  # hed_df object
  if (!any(class(rf_df) == 'heddata')){
    message('\nIncorrect class for "rf_df" object. "rf" estimator use hedonic data (class "hed")')
    stop()
  }

  # Check that class is available
  if (!paste0('rfModel.', class(estimator)) %in% utils::methods(rfModel)){
    message('\nInvalid estimator type: "', class(estimator), '" method not available.')
    stop()
  }

  # Check for sparseness
  if (nrow(rf_df) < nrow(attr(rf_df, 'period_table'))){
    message('\nYou have fewer observations (', nrow(rf_df), ') than number of periods (',
            nrow(attr(rf_df, 'period_table')), '). Results will likely be unreliable.')
  }

  # Check for
  UseMethod("rfModel", estimator)
}

#'
#' Hedonic model approach with base estimator
#'
#' Use of base estimator in hedonic model approach
#'
#' @section Further Details:
#' See `?rfModel` for more information
#' @inherit rfModel params
#' @method rfModel pdp
#' @importFrom ranger ranger
#' @importFrom pdp partial
#' @export

rfModel.pdp <- function(estimator,
                         rf_df,
                         rf_spec,
                         ntrees = 200,
                         seed = 1,
                         ...){

  # Estimate model
  rf_model <- ranger::ranger(rf_spec,
                             data = rf_df,
                             num.tree = ntrees,
                             seed = seed)


  # Get Simulation DF
  sim_df <- rfSimDf(rf_df = rf_df,
                    seed = seed,
                    ...)

  # Get simulation observations
  pdp_df <- pdp::partial(object = rf_model,
                         train = sim_df,
                         pred.var = "trans_period",
                         pred.grid = data.frame(trans_period = 1:max(rf_df$trans_period)))

  # Add 'coefficients'

  log_dep <- ifelse(grepl('log', rf_spec[2]), TRUE, FALSE)

    if(log_dep){
    coefs <- pdp_df$yhat - pdp_df$yhat[1]
  } else {
    coefs <- pdp_df$yhat / pdp_df$yhat[1]
  }
  rf_model$coefficients <- data.frame(time = 1:max(rf_df$trans_period),
                                      coefficient = coefs)

  # Structure and return
  structure(rf_model, class = c('rfmodel', class(rf_model)))

}

#'
#' Create simulation data for Random forest approach
#'
#' Create data to use in PDP simulation
#'
#' @section Further Details:
#' See `?rfModel` for more information
#' @param rf_df Full training dataset
#' @param seed Random seed for reproducibility
#' @param ... Additional arguments
#' @export

rfSimDf <- function(rf_df,
                    seed,
                    ...){

  # If no filters
  if (is.null(list(...)$sim_ids) &
      is.null(list(...)$sim_count) &
      is.null(list(...)$sim_per)){
    return(rf_df)
  }

  # If by sim id
  if (!is.null(list(...)$sim_ids)) return(rf_df[sim_ids, ])

  set.seed(seed)

  # If a sim count is provided
  if (!is.null(list(...)$sim_count)){
    return(rf_df[sample(1:nrow(rf_df), list(...)$sim_count, replace = TRUE), ])
  }

  # If just a sim_par is applied

  sim_count <- floor(list(...)$sim_per * nrow(rf_df))
  rf_df[sample(1:nrow(rf_df), sim_count, replace = TRUE), ]
}



#'
#' Hedonic model approach with base estimator
#'
#' Use of base estimator in hedonic model approach
#'
#' @section Further Details:
#' See `?rfModel` for more information
#' @inherit rfModel params
#' @method rfModel shap
#' @importFrom ranger ranger
#' @importFrom pdp partial
#' @export

rfModel.shap <- function(estimator,
                         rf_df,
                         rf_spec,
                         ntrees = 200,
                         seed = 1,
                         shap_k = 10,
                         ...){

    n <- 1

  # Estimate model
  mod_df <-  rf_df[, unique(c(list(...)$ind_var, 'trans_period', 'price'))]
  mod_df$price <- log(mod_df$price)

  regr.task = makeRegrTask(id = "aa", data = mod_df, target = "price")
  regr.lrn = mlr::makeLearner("regr.ranger", par.vals = list(num.trees = ntrees))
  rf_model = mlr::train(regr.lrn, regr.task)

  shap_df <- mod_df %>%
    dplyr::mutate(row_id = 1:nrow(.)) %>%
    dplyr::group_by(trans_period) %>%
    dplyr::slice(1:shap_k) %>%
    dplyr::arrange(row_id)

  shapvalue_df <- shapleyR::getShapleyValues(
    shapley(shap_df$row_id,
            task = regr.task,
            model = rf_model)) %>%
    dplyr::mutate(period = shap_df$trans_period) %>%
    dplyr::group_by(period) %>%
    dplyr::summarize(value = mean(trans_period)) %>%
    dplyr::filter(period %in% rf_df$trans_period)

  rf_model$coefficients <- data.frame(time = 1:max(rf_df$trans_period)) %>%
    dplyr::left_join(shapvalue_df %>%
                       dplyr::select(time = period,
                                     coefficient = value),
                     by = 'time') %>%
    dplyr::mutate(coefficient = coefficient - coefficient[1])
  #
  # a <- as.data.frame(cbind(X$trans_period[kk], x$trans_period))
  #
  # # Add 'coefficients'
  #
  # log_dep <- ifelse(grepl('log', rf_spec[2]), TRUE, FALSE)
  #
  # if(log_dep){
  #   coefs <- pdp_df$yhat - pdp_df$yhat[1]
  # } else {
  #   coefs <- pdp_df$yhat / pdp_df$yhat[1]
  # }
  # rf_model$coefficients <- data.frame(time = 1:max(rf_df$trans_period),
  #                                     coefficient = coefs)

  # Structure and return
  structure(rf_model, class = c('rfmodel', class(rf_model)))

}











#'
#' Hedonic model approach with base estimator
#'
#' Use of base estimator in hedonic model approach
#'
#' @section Further Details:
#' See `?rfModel` for more information
#' @inherit rfModel params
#' @method rfModel sim
#' @importFrom ranger ranger
#' @export

rfModel.sim <- function(estimator,
                         rf_df,
                         rf_spec,
                         ntrees = 200,
                         seed = 1,
                         ...){

  set.seed(seed)

  # Estimate model
  rf_model <- ranger::ranger(rf_spec,
                             data = rf_df,
                             num.tree = ntrees,
                             seed = seed)

  # Add class
  class(rf_model) <- c('rfmodel', class(rf_model))

  log_dep <- ifelse(grepl('log', rf_spec[2]), TRUE, FALSE)


  rfSimulate(rf_obj = rf_model,
             rf_df = rf_df,
             log_dep = log_dep,
             ...)
}

#'
#' Simulate selected properties
#'
#' Handle simulation of all chosen properties
#' '
#' @param rf_obj A `ranger` random forest object
#' @param rf_df Full data.frame used to build the random forest
#' @param sim_type ['random'] Sampling type to use
#' @param sim_per [0.1] Percentage of the total set to simulate
#' @param sim_count [NULL] If not giving a percentage, the total number of properties to simulate
#' @param seed [1] Seed for reproducibility
#' @param ... Additional arguments
#' @importFrom purrr map
#' @export

rfSimulate <- function(rf_obj,
                       rf_df,
                       sim_type = 'random',
                       sim_per = .1,
                       sim_count = NULL,
                       seed = 1,
                       ...){

  # if no count of simulation is given
  if (is.null(sim_count)) sim_count <- floor(sim_per * nrow(rf_df))

  # Get simulation observations
  set.seed(seed)
  sim_df <- rf_df[sample(1:nrow(rf_df), sim_count, replace = TRUE), ]

  # Calculate individual price movements
  sim_coefs <- purrr::map(.x = sim_df %>% split(., sim_df$trans_id),
                          .f = rfSim,
                          rf_obj = rf_obj,
                          periods = 1:max(rf_df$trans_period),
                          ...)

  rf_obj$coefficients <- data.frame(time = 1:max(rf_df$trans_period),
                                    coefficient = Reduce('+', sim_coefs) / length(sim_coefs) - 1)

  rf_obj

}

#'
#' Simulation engine
#'
#' Helper function to simulate each example proeprty over the time period(s)
#' '
#' @param rf_obj A `ranger` random forest object
#' @param sim_df Single property to simulate over time
#' @param periods Time periods to simulate over
#' @param log_dep [fALSE] Is the dependent variables in log form?
#' @param ... Additional arguments
#' @importFrom dplyr mutate
#' @importFrom stats predict
#' @export

rfSim <- function(rf_obj,
                  sim_df,
                  periods,
                  log_dep = FALSE,
                  ...){

  new_data <- sim_df[rep(1,length(periods)), ] %>%
    dplyr::mutate(trans_period = periods)

  pred <- stats::predict(rf_obj, new_data)$prediction
  if (log_dep) pred <- exp(pred)
  pred / pred[1]

}
