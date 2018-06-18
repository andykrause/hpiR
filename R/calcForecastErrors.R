#' @title calcForecastError
#' @description Estimate out-of-sample index errors using a forecast method
#' @usage Lorem Ipsum...
#' @param is_obj Object of class 'hpiseries'
#' @param pred_data Set of sales to be used for predicitive quality of index
#' @param return_indexes Defaul = FALSE; return the forecasted indexes
#' @param ... Additional Arguments
#' @return x
#' @section Further Details:
#' If 'max_period' is left NULL, then it will forecast up to the end of the data
#' @examples
#' a <- 1
#' @export

calcForecastError <- function(is_obj,
                              pred_data,
                              return_indexes=FALSE,
                              ...){

  # Check Classes

  if (!'hpiseries' %in% class(is_obj)){
    message('"is_obj" argument must be of class "hpiseries"')
    stop()
  }

  if (!any('data.frame' %in% class(pred_data)) ||
      !any(class(pred_data) %in% c('rt', 'hed'))){
    message('"pred_data" argument must be a data.frame with additional class of ',
            ' "rt" or "hed"')
    stop()
  }

  # Set start and end
  start <- end(is_obj[[1]])[1] + 1
  end <- end(is_obj[[length(is_obj)]])[1]
  time_range <- start:end

  # Get data
  fc_preddata <- purrr::map(.x = time_range,
                            hpi_data = pred_data,
                            train=FALSE,
                            .f=buildForecastIDs)

  # Predict value
  fc_forecasts <- purrr::map(.x=is_obj[-length(is_obj)],
                             .f=function(x){
                                 new_x <- forecast(ets(x, model='ANN'), h=1)
                                 ts(c(x, new_x$mean), start=start(x),
                                    frequency=frequency(x))
                              }
                           )

  # Iterate through score and calc errors
  fc_error <- purrr::map2(.x=fc_preddata,
                          .y=fc_forecasts,
                          pred_data=pred_data,
                          .f=function(x, y, pred_data){
                            calcInSampleError(pred_data=pred_data[x, ],
                                              index=y)
                            })

  error_df <- bind_rows(fc_error)
  class(error_df) <- unique(c('indexerrors', class(error_df)))
  attr(error_df, 'test_method') <- 'forecast'

  # Return Values
  error_df

}

#' @title buildForecastIDs
#' @description Create training or scoring data for the forecast error calculations
#' @usage Lorem Ipsum...
#' @param time_cut Period of forecast
#' @param hpi_data Data to be converted to training or scoring
#' @param train Default=TRUE; Create training data?  FALSE = Scoring data
#' @param ... Additional Arguments
#' @return data.frame of training or scoring observations
#' @section Further Details:
#' Lorem Ispum...
#' @examples
#' a <- 1
#' @export

buildForecastIDs <- function(time_cut,
                             hpi_data,
                             train=TRUE){

  if (!'data.frame' %in% class(hpi_data)){
    message('"hpi_data" argument must be a data.frame')
    stop()
  }

  if (!class(time_cut) %in% c('integer', 'numeric') ||
       time_cut < 0){
    message('"time_cut" must be a positive, numeric value')
    stop()
  }

  UseMethod("buildForecastIDs", hpi_data)

}

#' @export
buildForecastIDs.hed <- function(time_cut,
                                 hpi_data,
                                 train=TRUE){

  if(train){
    time_ids <- which(hpi_data$date_period < time_cut)
  } else {
    time_ids <- which(hpi_data$date_period == time_cut)
  }
  time_ids

}

#' @export
buildForecastIDs.rt <- function(time_cut,
                                hpi_data,
                                train=TRUE){

  # Extract data if given a full 'hpi' object
  if ('hpi' %in% class(hpi_data)){
    hpi_data <- hpi_data$data
  }

  if(train){
    time_ids <- which(hpi_data$period_2 < time_cut)
  } else {
    time_ids <- which(hpi_data$period_2 == time_cut)
  }
  time_ids
}

