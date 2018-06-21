#' @title calcInSampleError
#' @description Estimate the predictive error of an index via an in-sample approach. Generic method.
#' @usage calcInSampleError(pred_df, index)
#' @param pred_df Set of sales against which to test predictions
#' @param index Index (of class) to be tested for accuracy
#' @param ... Additional Arguments
#' @return object of class `indexerrors` inheriting from class `data.frame` containing the following fields:
#' \item{prop_id}
#' \item{pred_price}
#' \item{pred_error}
#' \item{pred_period}
#' @section Further Details:
#' In addition to being a stand-alone function, it is also used by `calcForecastError` and `calcKFoldError``
#' @examples
#' \dontrun{
#' index_error <- calcInSampleError(pred_df = rt_index$data,
#'                                  index = rt_index$index$index)
#'}
#'@export

calcInSampleError <- function(pred_df,
                              index,
                              ...){

  if (!'ts' %in% class(index)){
    message('"index" argument must be of class "ts"')
    stop()
  }

  if (!any('data.frame' %in% class(pred_df)) ||
        !any(class(pred_df) %in% c('rt', 'hed'))){
    message('"pred_df" argument must be a data.frame with additional class of ',
            ' "rt" or "hed"')
    stop()
  }

  UseMethod("calcInSampleError")

}

#' @title calcInSampleError.rt
#' @export

calcInSampleError.rt <- function(pred_df,
                                 index,
                                 ...){

  # Calculate the index adjustment to apply
  adj <- index[pred_df$period_2] / index[pred_df$period_1]

  # Calculate a prediction price
  pred_price <- pred_df$price_1 * adj

  # Measure the error (difference from actual)
  error <- (pred_price - pred_df$price_2) / pred_df$price_2

  # Return Values
  error_df <- data.frame(prop_id = pred_df$prop_id,
                         pred_price=pred_price,
                         pred_error=error,
                         pred_period=pred_df$period_2,
                         stringsAsFactors=FALSE)

  # Add classes
  class(error_df) <- c('indexerrors', 'data.frame')

  # Add attribute
  attr(error_df, 'test_method') <- 'insample'

  # Return
  error_df

}

#' @title calcInSampleError.rt
#' @export

calcInSampleError.hed <- function(pred_df,
                                  index,
                                  ...){

  # Future method

}
