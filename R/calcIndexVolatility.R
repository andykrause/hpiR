#' @title calcIndexVolatility
#' @description Calculate index volatility
#' @param index time-series index object
#' @param window periods over which to calculate the volatility
#' @param in_place adds volatility metric to the `hpiindex` object
#' @param ... Additional arguments
#' @return A metric of volatility
#' @section Further Details:
#' @examples
#' index_vol <- calcIndexVolatility(index=index,
#'                                  window=3)
#' @export

calcIndexVolatility <- function(index,
                                window,
                                in_place=FALSE,
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

  ## Return

  # If returing in place
  if (in_place){

    if ('hpi' %in% class(index_obj)){
      index_obj$index$vol <- vol_obj
      return(index_obj)
    }

    if ('hpiindex' %in% class(index_obj)){
      index_obj$vol <- vol_obj
      return(index_obj)
    }
  }

  # If just returning result of volatility calculation

  vol_obj

}
