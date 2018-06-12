#' @title rsModel
#' @description Estimate repeat sales model (method based on estimator class). Generic method.
#' @param rs_df Repeat sales dataset from rsCreateSales()
#' @param time_matrix Time matrix object from rsTimeMatrix()
#' @param price_diff Difference in price betwen the two sales
#' @param estimator Type of model to estimates (base, robust, weighted).  Must be in that class.
#' @param ... Additional arguments
#' @return rs model object
#' @section Further Details:
#' @examples
#' rs_model <- rsModel(rs_df = rs_sales,
#'                     time_matrix = time_matrix,
#'                     price_diff = price_diff,
#'                     estimator = structure('base', class='base'))
#' @export

## Generic Method

rsModel <- function(rs_df,
                    time_matrix,
                    price_diff,
                    estimator,
                    ...){

  ## Check for proper classes

  # rs_df object
  if (!'rs' %in% class(rs_df)){
    message('\nIncorrect class for "rs_df" object.  Must be of class "rs"')
    return(NULL)
  }

  # timematrix
  if (!'timematrix' %in% class(time_matrix)){
    message('\nIncorrect class for "time_matrix" object.  Must be of class "timematrix"')
    return(NULL)
  }

  # Agreement between lengths of rs_df, time_matrix and price_diff
  if (length(unique(c(nrow(rs_df), nrow(time_matrix), length(price_diff)))) > 1){
    message('\n# of Observations of "rs_df", "time_matrix" and "price_diff" do not match')
    return(NULL)
  }

  # Check that class is available
  if (!paste0('rsModel.', class(estimator)) %in% methods(rsModel)){
    message('\nInvalid estimator type: "', class(estimator), '" method not available.')
    return(NULL)
  }

  # Check for sparseness
  if (nrow(rs_df) < nrow(attr(rs_df, 'period_table'))){
    message('\nYou have fewer observations (', nrow(rs_df), ') than number of periods (',
            nrow(attr(rs_df, 'period_table')), '). Results will likely be unreliable.')
  }

  # Dispatch to specfic method
  UseMethod("rsModel", estimator)

}

#' @title rsModel.base
#' @description Estimate repeat sales model (method based on estimator class). Generic method.
#' @export

rsModel.base <- function(rs_df,
                         time_matrix,
                         price_diff,
                         estimator,
                         ...){

  # Estimate the model
  rs_model <- lm(price_diff ~ time_matrix + 0)

  # Assign Class
  class(rs_model) <- 'rsmod'

  # Return
  rs_model

}

#' @title rsModel.robust
#' @description Estimate repeat sales model (method based on estimator class). Generic method.
#' @export

rsModel.robust <- function(rs_df,
                           time_matrix,
                           price_diff,
                           estimator,
                           ...){

  # Determine 'sparseness' of the data
  time_size <- median(table(c(rs_df$period_1, rs_df$period_2)))

  # Use different robust packages based on sparseness
  if(time_size > 5){
    rs_model <- MASS::rlm(price_diff ~ time_matrix + 0)
  } else {
    rs_model <- robustbase::lmrob(price_diff ~ time_matrix + 0, setting="KS2014")
  }

  # Add class
  class(rs_model) <- 'rsmod'

  # Return
  rs_model

}

#' @title rsModel.weighted
#' @description Estimate repeat sales model (method based on estimator class). Generic method.
#' @export

rsModel.weighted <- function(rs_df,
                             time_matrix,
                             price_diff,
                             estimator,
                             ...){

  # Run base model
  lm_model <- lm(price_diff ~ time_matrix + 0)

  # Estimate impact of time dif on errors
  rs_df$time_diff <- rs_df$period_2 - rs_df$period_1
  err_fit <- lm((residuals(lm_model) ^ 2) ~ rs_df$time_diff)

  # Implement weights
  wgts <- fitted(err_fit)
  wgts <- ifelse(wgts > 0, 1 / wgts, 0)

  # Re-run model
  rs_model <- lm(price_diff ~ time_matrix + 0, weights=wgts)

  # Add Class
  class(rs_model) <- 'rsmod'

  # Return
  rs_model

}

