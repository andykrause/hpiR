#' @title calcAccuracy
#' @description Estimate index Accuracy using one of a number of measures
#' @usage Lorem Ipsum...
#' @param hpi_obj Object of class 'hpi'
#' @param series_name default = 'series'; name of the object in hpi_obj containing the series
#' @param test_method default = 'insample'; also 'kfold' or 'forecast'
#' @param test_type default = 'rs'; Type of data to use for test.  See details.
#' @param index_data = NULL; Extra data if the test_type doesn't match data in hpi_obj
#' @param
#' @param ... Additional Arguments
#' @return hpimodel object
#' @section Further Details:
#' 'srs' test type tests the ability of the index to correctly predict the second value in a sale-resale pair (ABS)
#' 'relhed' test type tests the ability of the index to improve an OLS model that doesn't account for time.  (REL)
#' @export

calcAccuracy <- function(hpi_obj,
                         series_name = 'series',
                         test_method = 'insample',
                         test_type = 'rs',
                         index_data = NULL,
                         in_place = FALSE,
                         in_place_name = 'acc',
                         ...){

  # Check for class of hpi_obj
  if (!'hpi' %in% class(hpi_obj)){
    message('"hpi_obj" must be of class "hpi"')
    stop()
  }

  # check for allowed test_method
  if (!test_method %in% c('insample', 'kfold', 'forecast')){
    message('"test_method" must be one of "insample", "kfold" or "forecast"')
    stop()
  }

  # check for allowed test_method
  if (!test_type %in% c('rs', 'hed')){
    message('"test_type" must be one of "rs", "hed"')
    stop()
  }

  # Check agreement between test_type and hpi_obj
  if (!test_type %in% class(hpi_obj$data)){
    if (is.null(index_data) ||
          class(index_data) != test_type){
       message('When "test_type" (', test_type, ') does not match the "hpi" object model ',
               'type (', class(hpi_obj$data)[1], ') you must provide an "index_data" object ',
               'of the necessary class, in this case: ', test_type)
       stop()
     }
  } else {
    index_data <- hpi_obj$data
  }

  if (is.null(hpi_obj[[series_name]]) ||
      any(!class(hpi_obj[[series_name]]) == 'hpiseries')){

    # Set default training period
    if (is.null(list(...)$train_period)){
      train_period <- as.integer(floor(length(hpi_obj$index$index) / 4))
    } else {
      train_period <- list(...)$train_period
    }

    # Set default max period
    if (is.null(list(...)$max_period)){
      max_period <- length(hpi_obj$index$index)
    } else {
      max_period <- list(...)$max_period
    }

    # Estimate Series
    hpi_obj <- calcIndexSeries(hpi_obj = hpi_obj,
                               train_period = train_period,
                               max_period = max_period,
                               in_place = TRUE,
                               in_place_name = series_name)

  }

  # Dispatch to the tests based on test_method

  # In sample
  if (test_method == 'insample'){
    error_obj <- calcInSampleError(pred_data = index_data,
                                   index = hpi_obj$index$index,
                                   ...)
  }

  # kfold
  if (test_method == 'kfold'){
    error_obj <- calcKFoldError(hpi_obj = hpi_obj,
                                pred_data = index_data,
                                ...)
  }

  # Forecast
  if (test_method == 'forecast'){
    error_obj <- calcForecastError(is_obj = hpi_obj[[series_name]],
                                   pred_data = index_data,
                                   ...)
  }

  # Return results

  if (in_place){

    hpi_obj[[in_place_name]] <- error_obj
    return(hpi_obj)
  }

  error_obj

}
