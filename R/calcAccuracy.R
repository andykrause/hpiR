#' @title calcAccuracy
#' @description Estimate index Accuracy using one of a variety of approaches
#' @usage calcAccuracy(hpi_obj, test_method, test_type, pred_df, series_name, in_place, in_place_name, ...)
#' @param hpi_obj Object of class 'hpi'
#' @param test_method default = 'insample'; also 'kfold' or 'forecast'
#' @param test_type default = 'rt'; Type of data to use for test.  See details.
#' @param pred_df = NULL; Extra data if the test_type doesn't match data in hpi_obj
#' @param series_name default = 'series'; name of the object in hpi_obj containing the series
#' @param in_place default = FALSE; Should the result be returned into an existing `hpi` object
#' @param in_place_name default = 'acc'; Name for returning in place
#' @param ... Additional Arguments
#' @return object of class `indexerrors` inheriting from class `data.frame` containing the following fields:
#' \item{prop_id}
#' \item{pred_price}
#' \item{pred_error}
#' \item{pred_period}
#' @section Further Details:
#' 'rt' test type tests the ability of the index to correctly predict the second value in a sale-resale pair
#' FUTURE: 'hed' test type tests the ability of the index to improve an OLS model that doesn't account for time.
#' (This approach is not ready yet).
#' `series_name` only needs to be supplied when running a test_method of type "forecast"
#'@examples
#'\dontrun{
#' index_error <- calcAccuracy(hpi_obj = rt_hpi,
#'                             test_type = 'rt',
#'                             test_method = 'insample')
#'}
#' @export

calcAccuracy <- function(hpi_obj,
                         test_method = 'insample',
                         test_type = 'rt',
                         pred_df = NULL,
                         smooth = FALSE,
                         series_name = 'series',
                         in_place = FALSE,
                         in_place_name = 'accuracy',
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
  if (!test_type %in% c('rt', 'hed')){
    message('"test_type" must be one of "rt", "hed"')
    stop()
  }

  # Check agreement between test_type and hpi_obj
  if (!paste0(test_type, 'data') %in% class(hpi_obj$data)){
    if (is.null(pred_df) ||
          !paste0(test_type, 'data') %in% class(pred_df)){
       message('When "test_type" (', test_type, ') does not match the "hpi" object model ',
               'type (', class(hpi_obj$data)[1], ') you must provide an "pred_df" object ',
               'of the necessary class, in this case: ', paste0(test_type, 'data'))
       stop()
     }
  } else {
    pred_df <- hpi_obj$data
  }

  # Check for smooth
  if (smooth && !'smooth' %in% names(hpi_obj$index)){
    message('"hpi_obj" has no smoothed index.  Please add one or set "smooth" to FALSE')
    stop()
  }

  # Check for series
  if (test_method == 'forecast'){

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
      hpi_obj <- createSeries(hpi_obj = hpi_obj,
                              train_period = train_period,
                              max_period = max_period,
                              in_place = TRUE,
                              in_place_name = series_name)

    } # Ends if(is.null(hpi....))
  } # Ends if(test_method == 'series')

  # Clip pred_df to size of index
  if (test_type == 'rt') {
    if (max(pred_df$period_2) > max(hpi_obj$index$period)){
      message("Trimming prediction date down to period ", max(hpi_obj$index$period),
              " and before.")
      pred_df <- pred_df %>%
        dplyr::filter(period_2 <= max(hpi_obj$index$period))
      class(pred_df) <- c('rtdata', 'data.frame')
    }
  }

  # Dispatch to the tests based on test_method

  # In sample
  if (test_method == 'insample'){

    index_name <- 'value'
    if (smooth) index_name <- 'smooth'

    accr_obj <- calcInSampleError(pred_df = pred_df,
                                   index = hpi_obj$index[[index_name]],
                                   ...)
  }

  # kfold
  if (test_method == 'kfold'){
    accr_obj <- calcKFoldError(hpi_obj = hpi_obj,
                                pred_df = pred_df,
                                ...)
  }

  # Forecast
  if (test_method == 'forecast'){
    accr_obj <- calcForecastError(is_obj = hpi_obj[[series_name]],
                                   pred_df = pred_df,
                                   ...)
  }

  # Return results

  if (in_place){

    hpi_obj[[in_place_name]] <- accr_obj
    return(hpi_obj)
  }

  accr_obj

}
