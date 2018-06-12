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

  ## Strip from hpi or hpiindex objects

  if ('hpi' %in% class(index)){
    index <- index$index$index
  }

  if ('hpiindex' %in% class(index)){
    index <- index$index
  }

  ## Check for classes
  if (! 'ts' %in% class(index)){
    message('The "index" object must be of class "ts"')
    stop()
  }

  # Check window
  if (class(window) %in% c('numeric', 'integer') && !is.na(window) &&
       window > 0 && window <= length(index) / 2){
    window <- as.integer(round(window, 0))
  } else {
    message('"window" argument must be a positive integer less than half the length of ',
            'the index')
    stop()
  }

  ## Calculate changes
  deltas <- (index[-1] - index[-length(index)]) / index[-length(index)]

  ## Calculate mean rolling sd
  iv <- zoo::rollapply(deltas, window, sd)

  ## Create objec
  vol_obj <- structure(list(roll=iv,
                            mean=mean(iv)), class='indexvol')
  attr(vol_obj, 'orig') <- index
  attr(vol_obj, 'window') <- window

  # Return
  vol_obj

}
