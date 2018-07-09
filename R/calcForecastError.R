#' @title calcForecastError
#' @description Estimate out-of-sample index errors using a forecast method
#' @usage hed_error <- calcForecastError(is_obj = hed_series, pred_df = rt_data)
#' @param is_obj Object of class 'hpiseries'
#' @param pred_df Set of sales to be used for predicitive quality of index
#' @param return_forecasts default = FALSE; return the forecasted indexes
#' @param forecast_length default = 1; Length of period(s) in time to forecast
#' @param ... Additional Arguments
#' @return object of class `hpiaccuracy` inheriting from class `data.frame` containing the following fields:
#' \describe{
#' \item{prop_id}{Property Identification number}
#' \item{pred_price}{Predicted price}
#' \item{pred_error}{(Prediction - Actual) / Actual}
#' \item{pred_period}{Period of the prediction}
#' }
#' @section Further Details:
#' If you set `return_forecasts` = TRUE, the forecasted indexes for each period will be returned
#' in the `forecasts` attribute of the `hpiaccuracy` object. (attr(accr_obj, 'forecasts')
#'
#' For now, the `pred_df` object must be a set of repeat transactions with the class `rt`,
#' inheriting from `hpidata`
#'
#'@examples
#'# Load example series
#'  data(ex_serieshpi)
#'
#'# Load prediction data
#'  data(ex_rtdata)
#'
#'# Calculate forecast accuracty
#'  fc_accr <- calcForecastError(is_obj = ex_serieshpi,
#'                               pred_df = ex_rtdata)
#' @export

calcForecastError <- function(is_obj,
                              pred_df,
                              return_forecasts = FALSE,
                              forecast_length = 1,
                              ...){

  # Check Classes

  if (!'serieshpi' %in% class(is_obj)){
    message('"is_obj" argument must be of class "serieshpi"')
    stop()
  }

  if (!any('data.frame' %in% class(pred_df)) ||
      !any(class(pred_df) %in% c('rtdata', 'heddata'))){
    message('"pred_df" argument must be a data.frame with additional class of ',
            ' "rtdata" or "heddata"')
    stop()
  }

  # Set start and end
  start <- end(is_obj$hpis[[1]]$index$value)[1] + 1
  end <- end(is_obj$hpis[[length(is_obj$hpis)]]$index$value)[1] + 1
  time_range <- start:end

  # Get data
  fc_preddata <- purrr::map(.x = time_range,
                            hpi_df = pred_df,
                            forecast_length = forecast_length,
                            train=FALSE,
                            .f=buildForecastIDs)

  # Predict value
  if ('smooth' %in% names(list(...)) &&
       isTRUE(list(...)$smooth) &&
       'smooth' %in% names(is_obj$hpis[[1]]$index)){
    index_name <- 'smooth'
  } else {
    if ('smooth' %in% names(list(...)) && isTRUE(list(...)$smooth)){
      message('No smoothed indexes found.  Create them with "smoothSeries()" and ',
              'try again')
      stop()
    }
    index_name <- 'value'
  }

  fc_forecasts <- purrr::map(.x=is_obj$hpis,
                             .f=function(x){
                                 new_x <- forecast(ets(x$index[[index_name]],
                                                       model='ANN'), h=forecast_length)
                                 ts(c(x$index[[index_name]], new_x$mean), start=start(x),
                                                   frequency=frequency(x))
                              }
                           )

  # Iterate through score and calc errors
  fc_error <- purrr::map2(.x=fc_preddata,
                          .y=fc_forecasts,
                          pred_df=pred_df,
                          .f=function(x, y, pred_df){
                            calcInSampleError(pred_df=pred_df[x, ],
                                              index=y)
                            })

  error_df <- bind_rows(fc_error)
  class(error_df) <- unique(c('seriesaccuracy', 'hpiaccuracy', class(error_df)))
  attr(error_df, 'test_method') <- 'forecast'

  # If returnning errors
  if (return_forecasts){
    attr(error_df, 'forecasts') <- fc_forecasts
  }

  # Return Values
  error_df

}

#' @title buildForecastIDs
#' @description Create training or scoring data for the forecast error calculations
#' @usage buildForecastIDs(time_cut, hpi_df, ...)
#' @param time_cut Period after which to cut off data
#' @param hpi_df Data to be converted to training or scoring
#' @param forecast_length default = 1; Lenght of forecasting to do
#' @param train Default=TRUE; Create training data?  FALSE = Scoring data
#' @param ... Additional Arguments
#' @return vector of row_ids indicating inclusion in the forecasting data as either the training
#' set (train = TRUE) or the scoring set (train = FALSE)
#' @section Further Details:
#' This function is rarely (if ever) used directly.  Most often called by `calcForecastError()`
#'
#' It is a generic method that dispatches on the `hpi_df` object.
#' @examples
#' # Load example data
#'   data(ex_rtdata)
#'
#' # Create ids
#'   fc_ids <- buildForecastIDs(time_cut = 27,
#'                              hpi_df = ex_rtdata,
#'                              forecast_length = 2,
#'                              train = TRUE)
#' @export

buildForecastIDs <- function(time_cut,
                             hpi_df,
                             forecast_length = 1,
                             train=TRUE){

  if (!'data.frame' %in% class(hpi_df)){
    message('"hpi_df" argument must be a data.frame')
    stop()
  }

  if (!class(time_cut) %in% c('integer', 'numeric') ||
       time_cut < 0){
    message('"time_cut" must be a positive, numeric value')
    stop()
  }

  if (!class(forecast_length) %in% c('numeric', 'integer') ||
       forecast_length < 1){
    message('"forecast_length" must be a positive integer')
    stop()
  }

  UseMethod("buildForecastIDs", hpi_df)

}

#' @export
buildForecastIDs.heddata <- function(time_cut,
                                     hpi_df,
                                     forecast_length = 1,
                                     train=TRUE){

  # Extract data if given a full 'hpi' object
  if ('hpi' %in% class(hpi_df)){
    hpi_df <- hpi_df$data
  }

  if(train){
    time_ids <- which(hpi_df$trans_period < time_cut)
  } else {
    time_seq <- time_cut:(time_cut + (forecast_length - 1))
    time_ids <- which(hpi_df$trans_period %in% time_seq)
  }
  time_ids
}

#' @export
buildForecastIDs.rtdata <- function(time_cut,
                                    hpi_df,
                                    forecast_length = 1,
                                    train=TRUE){

  # Extract data if given a full 'hpi' object
  if ('hpi' %in% class(hpi_df)){
    hpi_df <- hpi_df$data
  }

  if(train){
    time_ids <- which(hpi_df$period_2 < time_cut)
  } else {
    time_seq <- time_cut:(time_cut + (forecast_length - 1))
    time_ids <- which(hpi_df$period_2 %in% time_seq)
  }
  time_ids
}

