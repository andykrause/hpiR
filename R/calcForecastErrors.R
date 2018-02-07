#' @title calcForecastErrors
#' @description Estimate out-of-sample index errors using a forecast method
#' @usage Lorem Ipsum...
#' @param hpi_obj Object of class 'hpi'
#' @param pred_data Set of sales to be used for predicitive quality of index
#' @param train_range Number of periods to use as purely training before forecast starts
#' @param max_period Default=NULL; Maximum number of periods to forecast up to
#' @param ... Additional Arguments
#' @return hpimodel object
#' @section Further Details:
#' If 'max_period' is left NULL, then it will forecast up to the end of the data
#' @examples
#' a <- 1
#' @export

calcForecastErrors <- function(hpi_obj,
                               pred_data,
                               train_range,
                               max_period=NULL){


  if (is.null(max_period)){
    max_period <- max(hpi_obj$model$periods$period)
  }

  # Trim by time
  time_range <- (1 + train_range):(max_period)

  # Set up data
  fc_data <- purrr::map(.x=time_range,
                        hpi_data=hpi_obj$data,
                        train=TRUE,
                        .f=makeFCData)

  fc_preddata <- purrr::map(.x = time_range,
                            hpi_data = pred_data,
                            train=FALSE,
                            .f=makeFCData)

  # Rerun model
  fc_model <- purrr::map(.x=fc_data,
                         .f=hpiModel,
                         hed_spec=hpi_obj$model$mod_spec,
                         log_dep = hpi_obj$model$log_dep)

  # Forecast Future +1)
  fc_index <- purrr::map(.x=fc_model,
                         .f=function(x) modelToIndex(x)$index)

  # Predict value
  fc_indexx <- purrr::map(.x=fc_index,
                         .f=function(x){
                              new_x <- forecast(ets(x, model='ANN'), h=1)
                              ts(c(x, new_x$mean), start=start(x), frequency=frequency(x))
                            }
                         )

  # Iterate through score and calc errors
  fc_error <- purrr::map2(.x=fc_preddata,
                          .y=fc_indexx,
                          .f=calcHPIError)

  # Bind results together and return
  bind_rows(fc_error)

  ## TODO:::add in revisions here

}

#' @title makeFCData
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

makeFCData <- function(time_cut,
                       hpi_data,
                       train=TRUE){

  UseMethod("makeFCData", hpi_data)

}

#' @export
makeFCData.hed <- function(time_cut,
                           hpi_data,
                           train=TRUE){

  if(train){
    time_data <- hpi_data[hpi_data$date_period < time_cut, ]
  } else {
    time_data <- hpi_data[hpi_data$date_period == time_cut, ]
  }
  time_data

}

#' @export
makeFCData.rs <- function(time_cut,
                          hpi_data,
                          train=TRUE){

  if(train){
    time_data <- hpi_data[hpi_data$period_2 < time_cut, ]
  } else {
    time_data <- hpi_data[hpi_data$period_2 == time_cut, ]
  }
  time_data
}
