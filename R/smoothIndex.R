#' @title smoothIndex
#' @description Smooths an index
#' @usage Lorem Ipsum...
#' @param index Index to be smoothed
#' @param order Number of nearby period to smooth with
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

 smooth_index <- forecast::ma(x=index,
                              order=order)

 smooth_index[is.na(smooth_index)] <- index[is.na(smooth_index)]

 class(smooth_index) <- append('smoothindex', class(index))
 attr(smooth_index, 'raw') <- index

 smooth_index

}
