#' @title calcIndexSeries
#' @description Calculates a series of indexes, each one period longer than the previous
#' @usage Lorem Ipsum...
#' @param hpi_obj Object of class 'hpi'
#' @param train_range Number of periods to use as purely training before forecast starts
#' @param max_period Default=NULL; Maximum number of periods to forecast up to
#' @param name_prefix Default=NULL; Prefix to add before last time period if naming indexes
#' @param in_place Add to existing hpi object
#' @param in_place_name default = 'series'; Give it a different name than the default
#' @param ... Additional Arguments
#' @return hpimodel object
#' @section Further Details:
#' If 'max_period' is left NULL, then it will forecast up to the end of the data
#' @examples
#' a <- 1
#' @export

calcIndexSeries <- function(hpi_obj,
                            train_period,
                            max_period=NULL,
                            name_prefix=NULL,
                            in_place = FALSE,
                            in_place_name = 'series',
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

  # Trim by time
  time_range <- train_period:max_period

  ## Set up data

  # Get row ids for the training data
  is_data <- purrr::map(.x = time_range,
                        hpi_data = hpi_obj$data,
                        train = TRUE,
                        .f = buildForecastIDs)

  # Run models
  is_models <- purrr::map(.x=is_data,
                          y=hpi_obj$data,
                          hed_spec=hpi_obj$model$mod_spec,
                          log_dep = hpi_obj$model$log_dep,
                          .f=function(x, y, hed_spec, log_dep, ...){
                            hpiModel(hpi_data=y[x, ],
                                     hed_spec=hed_spec,
                                    log_dep=log_dep)
                             })

  # Convert models to indexes
  is_series <- purrr::map(.x=is_models,
                          .f=function(x) modelToIndex(x)$index)

  # Name
  if (!is.null(name_prefix)) names(is_series) <- paste0(name_prefix, time_range)

  if (in_place){
    hpi_obj[[in_place_name]] <-  structure(is_series, class='hpiseries')
    return(hpi_obj)
  }

  # Return Values
  structure(is_series, class='hpiseries')

}
