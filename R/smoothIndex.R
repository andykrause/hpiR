#' @title smoothIndex
#' @description Smooths an index
#' @usage Lorem Ipsum...
#' @param index Index to be smoothed
#' @param order Number of nearby period to smooth with, multiple means multiple iterations
#' @param in_place default = FALSE; adds volatility metric to the `hpiindex` object
#' @param in_place_name default = 'vol'; name of volatility object in `hpiindex` object
#' @param ... Additional Arguments
#' @return a ts and 'smooth_index` object with smoothed index
#' @section Further Details:
#' Leaving order blank default to a moving average with order 3.
#' @examples
#' a <- 1
#' @export

smoothIndex <- function(index_obj,
                        order=3,
                        in_place = FALSE,
                        in_place_name = 'smoothed',
                        ...){

  ## Strip from hpi objects
  if ('hpi' %in% class(index_obj)){
    hpi_obj <- index_obj
    index_obj <- index_obj$index
  } else {
    if (in_place) message('In order to use "in_place" you must supply an object of ',
                          'class "hpi" to "index_obj"')
  }

  if (!'hpiindex' %in% class(index_obj)){
    message('The "index_obj" object must be of class "hpiindex"')
    stop()
  }

  # Check order
  if (all(class(order) %in% c('numeric', 'integer')) && !is.na(order) &&
      order > 0 && order <= length(index_obj$index) / 2){
    order <- as.integer(round(order, 0))
  } else {
    message('"order" argument must be a positive integer less than half the length of ',
            'the index')
    stop()
  }

  # Create Smoothed index (retain existing)
  s_index <- index_obj$index

  # Smooth with moving average (Multiple orders are possible; done sequentially)
  for(o.i in order){
    s_index <- forecast::ma(x=s_index,
                            order=o.i)
  }

  # Deal with NAs (should be NAs on the tail ends of the smoothing)
  na_smooth <- which(is.na(s_index))

  # Fill in low end NAs with original index
  na_low <- na_smooth[na_smooth < length(s_index) / 2]
  s_index[na_low] <- index_obj$index[na_low]

  # Fill in High end NAs with forecasted values (off of smoothed)
  na_high <- na_smooth[na_smooth >= length(s_index) / 2]
  high_fc <- forecast(ets(s_index[1:(na_high[1] - 1)], model='ANN'), h=length(na_high))
  new_high <- (high_fc$mean + index_obj$index[na_high]) / 2
  s_index[na_high] <- new_high

  # If returing in place
  if (in_place){

    if (in_place_name == 'is_smoothed'){
      message('"is_smoothed" is a reserved name.',
              'Please choose a different "in_place_name"')
      stop()
    }

    index_obj[[in_place_name]] <- s_index
    index_obj$is_smoothed  <- TRUE
    attr(index_obj[[in_place_name]], 'order') <- order

    if (exists('hpi_obj')){
      hpi_obj$index <- index_obj
      return(hpi_obj)
    } else {
      return(index_obj)
    }
  }

  # Add Class and Attributes
  sm_index <- index_obj[c('name', 'numeric', 'period', 'imputed')]
  sm_index$index <- s_index
  sm_index$original <- index_obj$index
  sm_index$order <- order
  sm_index$smoothed <- TRUE
  class(sm_index) <- unique(c('indexsmooth', 'hpiindex'))

  ## Return


  # If just returning result of smoothing
  sm_index

}

#' @title smoothSeries
#' @description Smooths a series of indexex
#' @usage Lorem Ipsum...
#' @param series Series to be smoothed
#' @param order Number of nearby period to smooth with
#' @param ... Additional Arguments
#' @return a list of objects ts and 'smooth_index` object with smoothed index
#' @section Further Details:
#' Leaving order blank default to a moving average with order 3.
#' @examples
#' a <- 1
#' @export


## TODO figure out passing ... to map()

smoothSeries <- function(series,
                         order=3,
                         ...){

  s_series <- purrr::map(.x=series,
                         order=order,
                         .f=smoothIndex)
  class(s_series) <- 'hpiseries'

  s_series

}

