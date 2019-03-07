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

  set.seed(seed)

  # Estimate model
  rf_model <- ranger::ranger(rf_spec,
                             data = rf_df,
                             num.tree = ntrees,
                             seed = seed)

  # Add class
  class(rf_model) <- c('rfmodel', class(rf_model))

  log_dep <- ifelse(grepl('log', rf_spec[2]), TRUE, FALSE)

  pred_grid <- data.frame(trans_period = 1:max(rf_df$trans_period))
  pdp_df <- pdp::partial(rf_model,
                         train = rf_df,
                         pred.var = "trans_period",
                         pred.grid = pred_grid)

  if(log_dep){
    coefs <- pdp_df$yhat - pdp_df$yhat[1]
  } else {
    coefs <- pdp_df$yhat / pdf_df$yhat[1]
  }

  rf_model$coefficients <- data.frame(time = 1:max(rf_df$trans_period),
                                      coefficient = coefs)

  rf_model

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
