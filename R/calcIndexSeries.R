#' @title calcIndexSeries
#' @description Calculates a series of indexes, each one period longer than the previous
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

calcIndexSeries <- function(hpi_obj,
                            train_range,
                            max_period=NULL,
                            ...){

  # Check for alternate max period
  if (is.null(max_period)){
    max_period <- max(hpi_obj$model$periods$period)
  }

  # Trim by time
  time_range <- (1 + train_range):(1 + max_period)

  ## Set up data

  # Train data
  is_data <- purrr::map(.x=time_range,
                        hpi_data=hpi_obj$data,
                        train=TRUE,
                        .f=makeFCData)

  # Run models
  is_models <- purrr::map(.x=is_data,
                          .f=hpiModel,
                          hed_spec=hpi_obj$model$mod_spec,
                          log_dep = hpi_obj$model$log_dep)

  # Convert models to indexes
  is_series <- purrr::map(.x=is_models,
                          .f=function(x) modelToIndex(x)$index)

  # Return Values
  structure(is_series, class='hpiseries')

}





