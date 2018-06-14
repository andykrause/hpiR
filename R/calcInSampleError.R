#' @title calcInSampleError
#' @description Estimate the predictive error of an index. Generic method.
#' @usage Lorem Ipsum...
#' @param pred_data Set of sales against which to test predictions
#' @param index Index to be tested for accuracy
#' @param ... Additional Arguments
#' @return hpimodel object
#' @section Further Details:
#' Note that ensure prediction types (holdout vs forecast) are done outside of this function
#' @examples
#' calcHPIError(pred_data=rs_data,
#'              index=rs_index)
#' @export

calcInSampleError <- function(pred_data,
                              index,
                              ...){

  if (!'ts' %in% class(index)){
    message('"index" argument must be of class "ts"')
    stop()
  }

  if (!any('data.frame' %in% class(pred_data)) ||
        !any(class(pred_data) %in% c('rs', 'hed'))){
    message('"pred_data" argument must be a data.frame with additional class of ',
            ' "rs" or "hed"')
    stop()
  }

  UseMethod("calcInSampleError")

}

#' @title calcInSampleError.rs
#' @description Estimate the predictive error of an index by predicting second sale price of a repeat sale
#' @usage Lorem Ipsum...
#' @param pred_data Set of sales against which to test predictions
#' @param index Index to be tested for accuracy
#' @param ... Additional Arguments
#' @return data.frame of property id, predicted price and error
#' @section Further Details:
#' Note that ensure prediction types (holdout vs forecast) are done outside of this function
#' @examples
#' calcHPIError(pred_data=rs_data,
#'              index=rs_index)
#' @export

calcInSampleError.rs <- function(pred_data,
                                 index,
                                 ...){

  # Calculate the index adjustment to apply
  adj <- index[pred_data$period_2] / index[pred_data$period_1]

  # Calculate a prediction price
  pred_price <- pred_data$price_1 * adj

  # Measure the error (difference from actual)
  error <- (pred_price - pred_data$price_2) / pred_data$price_2

  # Return Values
  error_df <- data.frame(prop_id = pred_data$prop_id,
                         pred_price=pred_price,
                         pred_error=error,
                         pred_period=pred_data$period_2)

  # Add classes
  class(error_df) <- c('indexerrors', 'data.frame')

  # Add attribute
  attr(error_df, 'test_method') <- 'insample'

  # Return
  error_df

}

#' @title calcInSampleError.hed
#' @description Estimate the predictive error of an index by relative improvment in a hedonic price model
#' @usage Lorem Ipsum...
#' @param pred_data Set of sales against which to test predictions
#' @param index Index to be tested for accuracy
#' @param ... Additional Arguments
#' @return data.frame of property id, predicted price and error
#' @section Further Details:
#' Note that ensure prediction types (holdout vs forecast) are done outside of this function
#' @export

calcInSampleError.hed <- function(pred_data,
                                  index,
                                  ...){

  # Future method

}
