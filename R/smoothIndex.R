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

 s_index[is.na(s_index)] <- index[is.na(s_index)]

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

