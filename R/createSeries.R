#'
#' Create a series of indexes
#'
#' Generate a series of progressive indexes
#'
#' @param hpi_obj Object of class 'hpi'
#' @param train_period default = 12; Number of periods to use as purely training before creating indexes
#' @param max_period default = NULL; Maximum number of periods to create the index up to
#' @param ... Additional Arguments
#' @return An `serieshpi` object -- a list of `hpi` objects.
#' @importFrom purrr map map2
#' @section Further Details:
#' `train_period` Represents the shortest index that you will create. For certain
#' approaches, such as a repeat transaction model, indexes shorter than 10 will likely
#' be highly unstable.
#'
#' If `max_period`` is left NULL, then it will forecast up to the end of the data.
#' @examples
#'
#'   # Load example sales
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
#'  # Create Series (Suppressing messages do to small sample size of this example)
#'   suppressMessages(
#'     hpi_series <- createSeries(hpi_obj = rt_index,
#'                                train_period = 12))
#'
#' @export

createSeries <- function(hpi_obj,
                         train_period = 12,
                         max_period = NULL,
                         ...){

  # Check for proper class
  if (!'hpi' %in% class(hpi_obj)){
    message('"hpi_obj" object must be of class "hpi"')
    stop()
  }

  # Check train_range
  if (!class(train_period) %in% c('integer', 'numeric')){
    message('"train_period" must be a single numeric value')
    stop()
  } else {
    if (length(train_period) > 1){
      message('"train_period" should be a single numeric value. Taking the first value',
              ' from what has been provided')
      train_period <- train_period[1]
    }
    train_period <- as.integer(train_period)
  }

  # Check for alternate max period and its allowed value
  if (is.null(max_period) || max_period > max(hpi_obj$model$periods$period)){
    max_period <- max(hpi_obj$model$periods$period)
  }

  # Make sure training period isn't greater than max or hpi_obj
  if (train_period >= min(c(max(hpi_obj$model$periods$period), max_period))){
    message('"train_period" is greater than the length of the "hpi_obj" and/or the ',
            '"max_period" argument')
    stop()
  }

  # Trim by time (The +1 ensures that the buildForecastIds function works properly)
  time_range <- (train_period:max_period) + 1

  ## Set up data

  # Get row ids for the training data
  is_data <- purrr::map(.x = time_range,
                        hpi_df = hpi_obj$data,
                        train = TRUE,
                        .f = buildForecastIDs)

  if (any(lapply(is_data, length) == 0)){
    warning('Some periods have no data.  Removing them from series estimation')
    time_range <- time_range[lapply(is_data, length) > 0]
    is_data <- is_data[lapply(is_data, length) > 0]
  }

  # Run models, indexes and combine into hpi objects
  is_hpis <- purrr::map2(.x=is_data,
                         .y=as.list(time_range),
                         z=hpi_obj$data,
                         mod_spec=hpi_obj$model$mod_spec,
                         log_dep = hpi_obj$model$log_dep,
                         mod_type = hpi_obj$model$approach,
                         estimator = hpi_obj$model$estimator,
                         .f=function(x, y, z, mod_spec, log_dep, mod_type, estimator, ...){
                             mod <- hpiModel(model_type = mod_type,
                                             hpi_df=z[x, ],
                                             mod_spec=mod_spec,
                                             log_dep=log_dep,
                                             estimator = estimator,
                                             ...)
                             ind <- modelToIndex(mod, max_period=y-1, ...)
                             structure(list(model = mod,
                                            index = ind),
                                       class = 'hpi')
                          },
                         ...)

  # Return Values
  structure(list(data = hpi_obj$data,
                 hpis = is_hpis),
            class='serieshpi')
}
