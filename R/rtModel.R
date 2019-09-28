#'
#' Estimate repeat transaction model for index creation
#'
#' Estimate coefficients for an index via the repeat transaction approach (generic method)
#'
#' @param rt_df Repeat transactions dataset from rtCreateTrans()
#' @param time_matrix Time matrix object from rtTimeMatrix()
#' @param price_diff Difference in price between the two transactions
#' @param estimator Type of model to estimates (base, robust, weighted).  Must be in that class.
#' @param lm_recover (TRUE) Allows robust model to use linear model if it fails
#' @param ... Additional arguments
#' @return `rtmodel` object
#' @importFrom stats lm fitted residuals median
#' @importFrom MASS rlm
#' @importFrom robustbase lmrob
#' @importFrom utils methods
#' @section Further Details:
#' Three available specific methods: 'base', 'robust' and 'weighted'
#' @examples
#'
#'   # Load data
#'   data(ex_sales)
#'
#'   # With a raw transaction data.frame
#'   rt_data <- rtCreateTrans(trans_df = ex_sales,
#'                            prop_id = 'pinx',
#'                            trans_id = 'sale_id',
#'                            price = 'sale_price',
#'                            periodicity = 'monthly',
#'                            date = 'sale_date')
#'
#'   # Calc price differences
#'   price_diff <- rt_data$price_2 - rt_data$price_1
#'
#'   # Create time matrix
#'   rt_matrix <- rtTimeMatrix(rt_data)
#'
#'   # Calculate model
#'   rt_model <- rtModel(rt_df = rt_data,
#'                       price_diff = price_diff,
#'                       time_matrix = rt_matrix,
#'                       estimator = structure('base', class='base'))
#'
#' @export

rtModel <- function(rt_df,
                    time_matrix,
                    price_diff,
                    estimator,
                    lm_recover = TRUE,
                    ...){

  ## Check for proper classes

  # rt_df object
  if (!'rtdata' %in% class(rt_df)){
    message('\nIncorrect class for "rt_df" object.  Must be of class "rtdata"')
    stop()
  }

  # timematrix
  if (!'timematrix' %in% class(time_matrix)){
    message('\nIncorrect class for "time_matrix" object.  Must be of class "timematrix"')
    stop()
  }

  # Agreement between lengths of rt_df, time_matrix and price_diff
  if (length(unique(c(nrow(rt_df), nrow(time_matrix), length(price_diff)))) > 1){
    message('\n# of Observations of "rt_df", "time_matrix" and "price_diff" do not match')
    stop()
  }

  # Check that class is available
  if (!paste0('rtModel.', class(estimator)) %in% utils::methods(rtModel)){
    message('\nInvalid estimator type: "', class(estimator), '" method not available.')
    stop()
  }

  # Check for sparseness
  if (nrow(rt_df) < nrow(attr(rt_df, 'period_table'))){
    message('\nYou have fewer observations (', nrow(rt_df), ') than number of periods (',
            nrow(attr(rt_df, 'period_table')), '). Results will likely be unreliable.')
  }

  # Dispatch to specfic method
  UseMethod("rtModel", estimator)

}

#'
#' Repeat transaction model approach with base estimator
#'
#' Use of base estimator in repeat transactions model approach
#'
#' @section Further Details:
#' See `?rtModel` for more information
#' @inherit rtModel params
#' @method rtModel base
#' @importFrom stats lm
#' @export

rtModel.base <- function(rt_df,
                         time_matrix,
                         price_diff,
                         estimator,
                         ...){

  # Estimate the model
  rt_model <- stats::lm(price_diff ~ time_matrix + 0)

  # Assign Class
  class(rt_model) <- 'rtmodel'

  # Return
  rt_model

}

#'
#' Repeat transaction model approach with robust estimator
#'
#' Use of robust estimator in repeat transactions model approach
#'
#' @section Further Details:
#' See `?rtModel` for more information
#' @inherit rtModel params
#' @method rtModel robust
#' @importFrom stats median
#' @importFrom MASS rlm
#' @importFrom robustbase lmrob
#' @export

rtModel.robust <- function(rt_df,
                           time_matrix,
                           price_diff,
                           estimator,
                           lm_recover = TRUE,
                           ...){

  # Determine 'sparseness' of the data
  time_size <- stats::median(table(c(rt_df$period_1, rt_df$period_2)))

  # Use different robust packages based on sparseness
   rt_model <- tryCatch({MASS::rlm(price_diff ~ time_matrix + 0)},
                        error = function(e) e)
   if ('simpleError' %in% class(rt_model)){
    rt_model <- tryCatch({robustbase::lmrob(price_diff ~ time_matrix + 0, setting="KS2014")},
                         error = function(e) e)
   }
   if (sum(rt_model$coefficients, na.rm = TRUE) == 0){
     if (lm_recover){
       rt_model <- lm(price_diff ~ time_matrix + 0)
     } else {
       #cat(rt_model)
       stop()
     }
   }

  # Add class
  class(rt_model) <- 'rtmodel'

  # Return
  rt_model

}

#'
#' Repeat transaction model approach with weighted estimator
#'
#' Use of weighted estimator in repeat transactions model approach
#'
#' @section Further Details:
#' See `?rtModel` for more information
#' @inherit rtModel params
#' @method rtModel weighted
#' @importFrom stats lm residuals fitted
#' @export

rtModel.weighted <- function(rt_df,
                             time_matrix,
                             price_diff,
                             estimator,
                             ...){

  if (is.null(list(...)$weights)){
    # Run base model
    lm_model <- stats::lm(price_diff ~ time_matrix + 0)

    # Estimate impact of time dif on errors
    rt_df$time_diff <- rt_df$period_2 - rt_df$period_1
    err_fit <- stats::lm((stats::residuals(lm_model) ^ 2) ~ rt_df$time_diff)

    # Implement weights
    wgts <- stats::fitted(err_fit)
    wgts <- ifelse(wgts > 0, 1 / wgts, 0)
  } else {

    wgts <- list(...)$weights
  }

  # Re-run model
  rt_model <- stats::lm(price_diff ~ time_matrix + 0, weights=wgts)
  # Add Class
  class(rt_model) <- 'rtmodel'

  # Return
  rt_model

}

