#' @title rtModel
#' @description Estimate repeat transactions model (method based on estimator class). Generic method.
#' @param rt_df Repeat transactions dataset from rtCreateTrans()
#' @param time_matrix Time matrix object from rtTimeMatrix()
#' @param price_diff Difference in price betwen the two transactions
#' @param estimator Type of model to estimates (base, robust, weighted).  Must be in that class.
#' @param ... Additional arguments
#' @return `rtmod` object
#' @section Further Details:
#' @examples
#' rt_model <- rtModel(rt_df = rt_sales,
#'                     time_matrix = time_matrix,
#'                     price_diff = price_diff,
#'                     estimator = structure('base', class='base'))
#' @export

## Generic Method

rtModel <- function(rt_df,
                    time_matrix,
                    price_diff,
                    estimator,
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
  if (!paste0('rtModel.', class(estimator)) %in% methods(rtModel)){
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

#' @title rtModel.base
#' @description Estimate repeat sales model (method based on estimator class). Generic method.
#' @export

rtModel.base <- function(rt_df,
                         time_matrix,
                         price_diff,
                         estimator,
                         ...){

  # Estimate the model
  rt_model <- lm(price_diff ~ time_matrix + 0)

  # Assign Class
  class(rt_model) <- 'rtmod'

  # Return
  rt_model

}

#' @title rtModel.robust
#' @description Estimate repeat sales model (method based on estimator class). Generic method.
#' @export

rtModel.robust <- function(rt_df,
                           time_matrix,
                           price_diff,
                           estimator,
                           ...){

  # Determine 'sparseness' of the data
  time_size <- median(table(c(rt_df$period_1, rt_df$period_2)))

  # Use different robust packages based on sparseness
  if(time_size > 5){
    rt_model <- MASS::rlm(price_diff ~ time_matrix + 0)
  } else {
    rt_model <- robustbase::lmrob(price_diff ~ time_matrix + 0, setting="KS2014")
  }

  # Add class
  class(rt_model) <- 'rtmod'

  # Return
  rt_model

}

#' @title rtModel.weighted
#' @description Estimate repeat sales model (method based on estimator class). Generic method.
#' @export

rtModel.weighted <- function(rt_df,
                             time_matrix,
                             price_diff,
                             estimator,
                             ...){

  # Run base model
  lm_model <- lm(price_diff ~ time_matrix + 0)

  # Estimate impact of time dif on errors
  rt_df$time_diff <- rt_df$period_2 - rt_df$period_1
  err_fit <- lm((residuals(lm_model) ^ 2) ~ rt_df$time_diff)

  # Implement weights
  wgts <- fitted(err_fit)
  wgts <- ifelse(wgts > 0, 1 / wgts, 0)

  # Re-run model
  rt_model <- lm(price_diff ~ time_matrix + 0, weights=wgts)

  # Add Class
  class(rt_model) <- 'rtmod'

  # Return
  rt_model

}

