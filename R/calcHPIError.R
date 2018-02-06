#' @title calcHPIError
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

calcHPIError <- function(pred_data,
                         index,
                         ...){

  UseMethod("calcHPIError")

}

#' @title calcHPIError.rs
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


calcHPIError.rs <- function(pred_data,
                            index,
                            ...){

  # Calculate the index adjustment to apply
  adj <- index[pred_data$period_2] / index[pred_data$period_1]

  # Calculate a prediction price
  pred_price <- pred_data$price_1 * adj

  # Measure the error (difference from actual)
  error <- (pred_price - pred_data$price_2) / pred_data$price_2

  # Return Values
  cbind(data.frame(prop_id = pred_data$prop_id,
                   pred_price=pred_price,
                   pred_error=error))

}

#' @title calcHPIError.hed
#' @description Estimate the predictive error of an index by relative improvment in a hedonic price model
#' @usage Lorem Ipsum...
#' @param pred_data Set of sales against which to test predictions
#' @param index Index to be tested for accuracy
#' @param ... Additional Arguments
#' @return data.frame of property id, predicted price and error
#' @section Further Details:
#' Note that ensure prediction types (holdout vs forecast) are done outside of this function
#' @examples
#' calcHPIError(pred_data=hed_data,
#'              index=hed_index)
#' @export

calcHPIError.hed <- function(pred_data,
                             index,
                             ...){

  # Future method

}
