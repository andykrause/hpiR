#' @title rsModel
#' @description Estimate repeat sales model (method based on estimator class). Generic method.
#' @param estimator Type of model to estimates (base, robust, weighted)
#' @param rs_df Repeat sales dataset from rsCreateSales()
#' @param time_matrix Time matrix object from rsTimeMatrix()
#' @param price_diff Difference in price betwen the two sales
#' @param ... Additional arguments
#' @return rs model object
#' @section Further Details:
#' @examples
#' rs_model <- rsModel(estimator = 'base',
#'                     rs_df = rs_sales,
#'                     time_matrix = time_matrix,
#'                     price_diff = price_diff)
#' @export

## Generic Method

rsModel <- function(estimator,
                    rs_df,
                    time_matrix,
                    price_diff,
                    ...){
  UseMethod("rsModel")

}

## For Base Estimator

rsModel.base <- function(estimator,
                         rs_df,
                         time_matrix,
                         price_diff,
                         ...){

  rs_model <- lm(price_diff ~ time_matrix + 0)

  class(rs_model) <- 'rsmod'

  rs_model

}

## For Robust Estimator

rsModel.robust <- function(estimator,
                           rs_df,
                           time_matrix,
                           price_diff,
                           ...){

  # Determine 'sparseness' of the data
  time_size <- median(table(c(rs_df$date_1, rs_df$date_2)))

  # Use different robust packages based on sparseness
  if(time_size > 5){
    rs_model <- MASS::rlm(price_diff ~ time_matrix + 0)
  } else {
    rs_model <- robustbase::lmrob(price_diff ~ time_matrix + 0)
  }

  class(rs_model) <- 'rsmod'

  rs_model

}

## For Weigthed Estimator

rsModel.weighted <- function(estimator,
                             rs_df,
                             time_matrix,
                             price_diff,
                             ...){

  # Run base model
  lm_model <- lm(price_diff ~ time_matrix + 0)

  # Estimate impact of time dif on errors
  rs_df$time_diff <- rs_df$date_2 - rs_df$date_1
  err_fit <- lm((residuals(lm_model) ^ 2) ~ rs_df$time_diff)

  # Implement weights
  wgts <- fitted(err_fit)
  wgts <- ifelse(wgts > 0, 1 / wgts, 0)

  # Re-run model
  rs_model <- lm(price_diff ~ time_matrix + 0, weights=wgts)

  class(rs_model) <- 'rsmod'

  rs_model

}

