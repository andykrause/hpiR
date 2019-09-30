#' Calculate the accuracy of an index
#'
#' Estimate index accuracy using one of a variety of approaches
#'
#' @param hpi_obj Object of class 'hpi'
#' @param test_method default = 'insample'; Also 'kfold'
#' @param test_type default = 'rt'; Type of data to use for test.  See details.
#' @param pred_df default = NULL; Extra data if the test_type doesn't match data in hpi_obj
#' @param smooth default = FALSE; calculated on the smoothed index(es)
#' @param in_place default = FALSE; Should the result be returned into an existing `hpi` object
#' @param in_place_name default = 'accuracy'; Name for returning in place
#' @param ... Additional Arguments
#' @return object of class `hpiaccuracy` inheriting from class `data.frame` containing
#' the following fields:
#' \describe{
#'   \item{prop_id}{Property Identification number}
#'   \item{price}{Transaction Price}
#'   \item{pred_price}{Predicted price}
#'   \item{error}{(Prediction - Actual) / Actual}
#'   \item{log_error}{log(prediction) - log(actual)}
#'   \item{pred_period}{Period of the prediction}
#' }
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @section Further Details:
#' 'rt' test type tests the ability of the index to correctly predict the second value in
#' a repeat transaction pair
#' FUTURE: 'hed' test type tests the ability of the index to improve an OLS model that
#' doesn't account for time.
#' (This approach is not ready yet).
#'@examples
#'
#'  # Load Data
#'  data(ex_sales)
#'
#'  # Create Index
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
#'  # Calculate insample accuracy
#'  hpi_accr <- calcAccuracy(hpi_obj = rt_index,
#'                           test_type = 'rt',
#'                           test_method = 'insample')
#'
#' @export

calcAccuracy <- function(hpi_obj,
                         test_method = 'insample',
                         test_type = 'rt',
                         pred_df = NULL,
                         smooth = FALSE,
                         in_place = FALSE,
                         in_place_name = 'accuracy',
                         ...){

  # Check for class of hpi_obj
  if (!'hpi' %in% class(hpi_obj)){
    message('"hpi_obj" must be of class "hpi"')
    stop()
  }

  # check for allowed test_method
  if (!test_method %in% c('insample', 'kfold')){
    message('"test_method" must be one of "insample" or "kfold"')
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

  # Clip pred_df to size of index
  if (test_type == 'rt') {
    if (max(pred_df$period_2) > max(hpi_obj$index$period)){
      message("Trimming prediction date down to period ", max(hpi_obj$index$period),
              " and before.")
      pred_df <- pred_df %>%
        dplyr::filter(., .data$period_2 <= max(hpi_obj$index$period))
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
                                  smooth = smooth,
                                   ...)
  }

  # kfold
  if (test_method == 'kfold'){
    accr_obj <- calcKFoldError(hpi_obj = hpi_obj,
                               pred_df = pred_df,
                               smooth = smooth,
                                ...)
  }

  # Return results
  if (in_place){

    hpi_obj$index[[in_place_name]] <- accr_obj
    return(hpi_obj)
  }

  accr_obj

}

#' Calculate the accuracy of a series of indexes
#'
#' Estimate the index accuracy for a (progressive) series of indexes
#'
#' @param series_obj Serieshpi object to be analyzed
#' @param test_method default = 'insample'; Also 'kfold' or 'forecast'
#' @param test_type default = 'rt'; Type of data to use for test.  See details.
#' @param pred_df default = NULL; Extra data if the test_type doesn't match data in hpi_obj
#' @param smooth default = FALSE; Analyze the smoothed indexes
#' @param summarize default = FALSE; When multiple accuracy measurements for single observation
#' take the mean of them all.
#' @param in_place default = FALSE; Should the result be returned into an existing `hpi` object
#' @param in_place_name default = 'accuracy'; Name for returning in place
#' @param ... Additional Arguments
#' @return `seriesaccuracy` object (unless calculated 'in_place')
#' @importFrom purrr map
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr bind_rows group_by summarize ungroup
#' @section Further Details:
#' Unless using `test_method = "forecast"`` with a "forecast_length" of 1, the results
#' will have more than one accuracy estimate per observations.  Setting `summarize = TRUE`
#' will take the mean accuracy for each observation across all indexes.
#' @examples
#'
#'  # Load data
#'  data(ex_sales)
#'
#'  # Create index
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
#'   #  Create Series (Suppressing messages do to small sample size of this example)
#'   suppressMessages(
#'     hpi_series <- createSeries(hpi_obj = rt_index,
#'                                train_period = 12))
#'
#'   # Calculate insample accuracy
#'   hpi_series_accr <- calcSeriesAccuracy(series_obj = hpi_series,
#'                                         test_type = 'rt',
#'                                         test_method = 'insample')
#'
#' @export

calcSeriesAccuracy <- function(series_obj,
                               test_method = 'insample',
                               test_type = 'rt',
                               pred_df = NULL,
                               smooth = FALSE,
                               summarize = FALSE,
                               in_place = FALSE,
                               in_place_name = 'accuracy',
                               ...){

  # Bad series_obj
  if (!'serieshpi' %in% class(series_obj)){
    message('The "series_obj" must be of class "serieshpi"')
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
  if (!paste0(test_type, 'data') %in% class(series_obj$data)){
    if (is.null(pred_df) ||
        !paste0(test_type, 'data') %in% class(pred_df)){
      message('When "test_type" (', test_type, ') does not match the "hpi" object model ',
              'type (', class(series_obj$data)[1], ') you must provide an "pred_df" object ',
              'of the necessary class, in this case: ', paste0(test_type, 'data'))
      stop()
    }
  } else {
    pred_df <- series_obj$data
  }

  # If not forecast:
  if (test_method != 'forecast'){

    # Check for smooth indexes
    if (smooth && !'smooth' %in% names(series_obj$hpis[[1]]$index)){
        message('Not smoothed indexes found. Please add or set smooth to FALSE')
        stop()
    }

    # Calculate accuracy
    suppressMessages(
      accr_df <- purrr::map(.x=series_obj$hpis,
                              test_method = test_method,
                              test_type = test_type,
                              pred_df = pred_df,
                              orig_data = series_obj$data,
                              .f = function(x, test_method, test_type, pred_df,
                                            orig_data){
                                x$data <- orig_data
                                s_ind <- calcAccuracy(x,
                                                      test_method,
                                                      test_type,
                                                      pred_df,
                                                      smooth=smooth)
                                s_ind$series = length(x$index$value)
                                s_ind
                             }) %>% dplyr::bind_rows())

    # If summarizing
    if (summarize){
      accr_df <- accr_df %>%
        dplyr::group_by(., .data$pair_id) %>%
        dplyr::summarize(., pred_price = mean(.data$pred_price),
                            error = mean(.data$error),
                            log_error = mean(.data$log_error),
                            series = 0) %>%
        dplyr::ungroup()
    }

    # Add Class info
    accr_df <- structure(accr_df,
                         class = unique(c('seriesaccuracy', 'hpiaccuracy',
                                         class(accr_df))))

    # Return if not in place
    if (!in_place) return(accr_df)

    # Add to series object
    if (!smooth){
      series_obj[[in_place_name]] <- accr_df
    } else {
      if (in_place_name == 'accuracy') in_place_name <- 'accuracy_smooth'
      series_obj[[in_place_name]] <- accr_df
    }

  # If it is a forecast method

  } else {

   accr_df <- calcForecastError(is_obj = series_obj,
                                 pred_df = pred_df,
                                 smooth = smooth,
                                 ...)

   # Return if not in place
   if (!in_place) return(accr_df)

   # Add to object
   if (!smooth){
     series_obj[[in_place_name]] <- accr_df
   } else {
     if (in_place_name == 'accuracy') in_place_name <- 'accuracy_smooth'
     series_obj[[in_place_name]] <- accr_df
   }

  }

  # Return standard
  series_obj
}
