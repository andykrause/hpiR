#'
#' Estimate random forest model for index creation
#'
#' Estimate coefficients for an index via the random forest approach (generic method)
#'
#' @param estimator Type of model to estimates (pdp)
#' @param rf_df Transactions dataset from hedCreateSales()
#' @param rf_spec Model specification (`formula` object)
#' @param ntrees [200] Set number of trees to use
#' @param seed [1] Random seed for reproducibility
#' @param ... Additional arguments
#' @return `rfmodel` object: model object of the estimator (ex.: `lm`)
#' @importFrom utils methods
#' @importFrom stats median lm as.formula
#' @section Further Details:
#' `estimator` argument must be in a class of 'pdp'
#' This function is not generally called directly, but rather from `hpiModel()`
#' @examples
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
#'  rf_model <- rfModel(estimator = structure('pdp', class = 'pdp'),
#'                      rf_df = hed_data,
#'                      rf_spec = as.formula(log(price) ~ baths + tot_sf),
#'                      ntrees = 10,
#'                      sim_count = 1)
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
#' Random forest model approach with pdp estimator
#'
#' Use of pdp estimator in random forest approach
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
#' @param sim_ids row ids to simulate
#' @param sim_count number of random rows to simulate
#' @param sim_per percent of rows to randomly simulate
#' @param ... Additional arguments
#' @export

rfSimDf <- function(rf_df,
                    seed,
                    sim_ids = NULL,
                    sim_count = NULL,
                    sim_per = NULL,
                    ...){

  # If no filters
  if (is.null(sim_ids) & is.null(sim_count) & is.null(sim_per)) return(rf_df)

  # If by sim id
  if (!is.null(sim_ids)) return(rf_df[sim_ids, ])


  # If a sim percentage is provided
  if (is.null(sim_count)){
    sim_count <- floor(sim_per * nrow(rf_df))
  }

  # Take sample and return
  set.seed(seed)
  rf_df[sample(1:nrow(rf_df), sim_count, replace = TRUE), ]
}
