#' Calculate index errors in sample
#'
#' Estimate the predictive error of an index via an in-sample approach.
#'
#' @param pred_df Set of sales against which to test predictions
#' @param index Index (of class `ts`) to be tested for accuracy
#' @param ... Additional Arguments
#' @return object of class `hpiaccuracy` inheriting from class `data.frame`
#' containing the following fields:
#' \describe{
#'   \item{pair_id}{Uniq Pair ID number}
#'   \item{price}{Transaction Price}
#'   \item{pred_price}{Predicted price}
#'   \item{error}{(Prediction - Actual) / Actual}
#'   \item{log_error}{log(prediction) - log(actual)}
#'   \item{pred_period}{Period of the prediction}
#' }
#' @section Further Details:
#' In addition to being a stand-alone function, it is also used by `calcForecastError`
#' and `calcKFoldError``
#' @examples
#'
#'  # Load example data
#'  data(ex_sales)
#'
#'  # Create index with raw transaction data
#'  rt_index <- rtIndex(trans_df = ex_sales,
#'                      periodicity = 'monthly',
#'                      min_date = '2010-06-01',
#'                      max_date = '2015-11-30',
#'                      adj_type = 'clip',
#'                      date = 'sale_date',
#'                      price = 'sale_price',
#'                      trans_id = 'sale_id',
#'                      prop_id = 'pinx',
#'                      estimator = 'robust',
#'                      log_dep = TRUE,
#'                      trim_model = TRUE,
#'                      max_period = 48,
#'                      smooth = FALSE)
#'
#'  # Calculate accuracy
#'  in_accr <- calcInSampleError(pred_df = rt_index$data,
#'                               index = rt_index$index$value)
#'
#'@export

calcInSampleError <- function(pred_df,
                              index,
                              ...){

  if (!'ts' %in% class(index)){
    message('"index" argument must be of class "ts"')
    stop()
  }

  if (!any('data.frame' %in% class(pred_df)) ||
        !any(class(pred_df) %in% c('rtdata', 'heddata'))){
    message('"pred_df" argument must be a data.frame with additional class of ',
            ' "rtdata" or "heddata"')
    stop()
  }

  UseMethod("calcInSampleError")

}

#' Calculate index errors in sample (rt approach)
#'
#' Estimate the predictive error of an index via an in-sample approach (rt approach)
#'
#' @method calcInSampleError rtdata
#' @inherit calcInSampleError params
#' @export

calcInSampleError.rtdata <- function(pred_df,
                                     index,
                                     ...){

  # Calculate the index adjustment to apply
  adj <- index[pred_df$period_2] / index[pred_df$period_1]

  # Calculate a prediction price
  pred_price <- pred_df$price_1 * adj

  # Measure the error (difference from actual)
  error <- (pred_price - pred_df$price_2) / pred_df$price_2
  logerror <- log(pred_price) - log(pred_df$price_2)

  # Return Values
  error_df <- data.frame(pair_id = pred_df$pair_id,
                         rt_price = pred_df$price_2,
                         pred_price = pred_price,
                         error = error,
                         log_error = logerror,
                         pred_period = pred_df$period_2,
                         stringsAsFactors = FALSE)

  # Add classes
  class(error_df) <- c('hpiaccuracy', 'data.frame')

  # Add attribute
  attr(error_df, 'test_method') <- 'insample'

  # Return
  error_df

}

#' Calculate index errors in sample (hed approach)
#'
#' Estimate the predictive error of an index via an in-sample approach (hed approach)
#'
#' @method calcInSampleError heddata
#' @inherit calcInSampleError params
#' @export

calcInSampleError.heddata <- function(pred_df,
                                      index,
                                      ...){

  # Future method

}
