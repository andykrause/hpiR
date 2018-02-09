#' @title calcIndexVolatility
#' @description Calculate index volatility
#' @param index time-series index object
#' @param window periods over which to calculate the volatility
#' @param ... Additional arguments
#' @return A metric of volatility
#' @section Further Details:
#' @examples
#' index_vol <- calcIndexVolatility(index=index,
#'                                  window=3)
#' @export

calcIndexVolatility <- function(index,
                                window,
                                ...){

  # series: series of index values
  # window:  length of time over which to measure the volatility

  ## Calc series length

  il <- length(index)

  ## Calculate changes

  deltas <- (index[-1] - index[-il]) / index[-il]

  ## Calculate mean rolling sd

  iv <- zoo::rollapply(deltas, window, sd)

  ## Historic volatility

  vol_obj <- structure(list(roll=iv,
                            mean=mean(iv)), class='indexvol')
  attr(vol_obj, 'orig') <- index
  attr(vol_obj, 'window') <- window

  vol_obj

}
