#' @title smoothIndex
#' @description Smooths an index
#' @usage Lorem Ipsum...
#' @param index Index to be smoothed
#' @param order Number of nearby period to smooth with, multiple means multiple iterations of smoothing
#' @param ... Additional Arguments
#' @return a ts and 'smooth_index` object with smoothed index
#' @section Further Details:
#' Leaving order blank default to a moving average with order 3.
#' @examples
#' a <- 1
#' @export

smoothIndex <- function(index,
                        order=3,
                        in_place = FALSE,
                        in_place_name = 'smoothed',
                        ...){

  ## Save index_obj for future returning
  index_obj <- index

  ## Strip from hpi or hpiindex objects
  if ('hpi' %in% class(index_obj)){
    index <- index$index$index
  }

  if ('hpiindex' %in% class(index_obj)){
    index <- index$index
  }

  ## Check for classes
  if (!'ts' %in% class(index)){
    message('The "index" object must be of class "ts"')
    stop()
  }

  # Check order
  if (all(class(order) %in% c('numeric', 'integer')) && !is.na(order) &&
      order > 0 && order <= length(index) / 2){
    order <- as.integer(round(order, 0))
  } else {
    message('"order" argument must be a positive integer less than half the length of ',
            'the index')
    stop()
  }

  # Create Smoothed index (retain existing)
  s_index <- index

  # Smooth with moving average (Multiple orders are possible; done sequentially)
  for(o.i in order){
    s_index <- forecast::ma(x=s_index,
                            order=o.i)
  }

  # Deal with NAs (should be NAs on the tail ends of the smoothing)
  na_smooth <- which(is.na(s_index))

  # Fill in low end NAs with original index
  na_low <- na_smooth[na_smooth < length(s_index) / 2]
  s_index[na_low] <- index[na_low]

  # Fill in High end NAs with forecasted values (off of smoothed)
  na_high <- na_smooth[na_smooth >= length(s_index) / 2]
  high_fc <- forecast(ets(s_index[1:(na_high[1] - 1)], model='ANN'), h=length(na_high))
  new_high <- (high_fc$mean + index[na_high]) / 2
  s_index[na_high] <- new_high

  # Add Class and Attributes
  class(s_index) <- append('smoothindex', class(index))
  attr(s_index, 'raw') <- index
  attr(s_index, 'order') <- order

  ## Return

  # If returing in place
  if (in_place){

    if ('hpi' %in% class(index_obj)){
      index_obj$index$smoothed <- s_index
      return(index_obj)
    }

    if ('hpiindex' %in% class(index_obj)){
      index_obj$smoothed <- s_index
      return(index_obj)
    }
  }

  # If just returning result of volatility calculation

  s_index

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

smoothSeries <- function(series,
                         order=3,
                         ...){

  s_series <- purrr::map(.x=series,
                         order=order,
                         .f=smoothIndex)
  class(s_series) <- 'hpiseries'

  s_series

}

