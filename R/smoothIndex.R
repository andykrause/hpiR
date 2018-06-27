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
                        order = 3,
                        in_place = FALSE,
                        ...){

  ## Strip from hpi objects and check for hpiindex object
  if ('hpi' %in% class(index_obj)){
    hpi_obj <- index_obj
    index_obj <- index_obj$index
  } else {
    if (!'hpiindex' %in% class(index_obj)){
      message('The "index_obj" object must be of class "hpiindex" or "hpi"')
      stop()
    }
  }

  # Check order
  if (all(class(order) %in% c('numeric', 'integer')) && !is.na(order) &&
      order > 0 && order <= length(index_obj$value) / 2){
    order <- as.integer(round(order, 0))
  } else {
    message('"order" argument must be a positive integer less than half the length of ',
            'the index')
    stop()
  }

  # Create Smoothed index (retain existing)
  s_index <- index_obj$value

  # Smooth with moving average (Multiple orders are possible; done sequentially)
  for(o.i in order){
    s_index <- forecast::ma(x=s_index,
                            order=o.i)
  }

  # Deal with NAs (should be NAs on the tail ends of the smoothing)
  na_smooth <- which(is.na(s_index))

  # Fill in low end NAs with original index
  na_low <- na_smooth[na_smooth < length(s_index) / 2]
  s_index[na_low] <- index_obj$value[na_low]

  # Fill in High end NAs with forecasted values (off of smoothed)
  na_high <- na_smooth[na_smooth >= length(s_index) / 2]
  high_fc <- forecast(ets(s_index[1:(na_high[1] - 1)], model='ANN'), h=length(na_high))
  new_high <- (high_fc$mean + index_obj$value[na_high]) / 2
  s_index[na_high] <- new_high

  # Give it a structure
  sm_index <- structure(s_index, class = c('indexsmooth', 'ts'))
  attr(sm_index, 'order') <- order

  # If returing in place
  if (in_place){

    index_obj$smooth <- sm_index

    if (exists('hpi_obj')){
      hpi_obj$index <- index_obj
      return(hpi_obj)
    } else {
      return(index_obj)
    }
  }

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

smoothSeries <- function(series_obj,
                         order=3,
                         series_name = 'series',
                         in_place = FALSE,
                         in_place_name = 'smooth_series',
                         ...){

  # If an hpi object
  if ('hpi' %in% class(series_obj)){
    hpi_obj <- series_obj
    series_obj <- hpi_obj[[series_name]]
    if (is.null(series_obj) || !'hpiseries' %in% class(series_obj)){
      message('When supplying an object of class "hpi" to the "series_obj" you must ',
              ' supply a valid "series_name" (one that points to an "hpiseries" object).')
      stop()
    }
  }

  # Bad series_obj
  if (!'hpiseries' %in% class(series_obj)){
    message('"series_obj" must be of class "hpiseries" (or of "hpi")')
    stop()
  }

  # Apply smoothing to all indexes
  s_series <- purrr::map(.x=series_obj,
                         order=order,
                         .f=smoothIndex)
  class(s_series) <- 'hpiseries'

  # Return in place
  if (in_place && exists('hpi_obj')){
    hpi_obj[[in_place_name]] <- s_series
    return(hpi_obj)
  }

  # Return standard
  s_series

}

