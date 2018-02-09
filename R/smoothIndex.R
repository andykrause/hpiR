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
                        ...){

 s_index <- index

 for(o.i in order){
   s_index <- forecast::ma(x=s_index,
                           order=o.i)
  }

 na_smooth <- which(is.na(s_index))
 na_low <- na_smooth[na_smooth < length(s_index)/2]
 s_index[na_low] <- index[na_low]
 na_high <- na_smooth[na_smooth > length(s_index)/2]

 high_fc <- forecast(ets(s_index[1:(na_high[1] - 1)], model='ANN'), h=length(na_high))
 new_high <- (high_fc$mean + index[na_high]) / 2
 s_index[na_high] <- new_high

 class(s_index) <- append('smoothindex', class(index))
 attr(s_index, 'raw') <- index

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

