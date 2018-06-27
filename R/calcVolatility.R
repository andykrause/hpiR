#' @title calcVolatility
#' @description Calculate index volatility
#' @param index An object of class `hpiindex`
#' @param window default = 3; Rolling periods over which to calculate the volatility
#' @param in_place default = FALSE; Adds volatility metric to the `hpiindex` object
#' (may be within an `hpi` object)
#' @param in_place_name default = 'vol'; Name of volatility object in `hpiindex` object
#' @param ... Additional arguments
#' @return an `indexvolatility` (S3) object, the 'index' slot of which is a `ts` object
#' \item{roll: volatility at each rolling point}
#' \item{mean: overall mean volatility}
#' \item{median: overall median volatility}
#' @section Further Details:
#' You may also provide an `hpi` object to this function.  If you do, it will
#' extract the `hpiindex` object from the `index` slot in the `hpi` class object.
#' @examples
#' \dontrun{
#' index_vol <- calcVolatility(index = hpi_index,
#'                             window = 3)
#' }
#' @export

calcVolatility <- function(index,
                           window = 3,
                           in_place = FALSE,
                           in_place_name = 'volatility',
                           smooth = FALSE,
                           ...){

  ## Save index_obj for future returning

  index_obj <- index

  ## Strip from hpi or hpiindex objects

  if ('hpi' %in% class(index_obj)){
    if (!smooth){
      index <- index$index$value
    } else {
      index <- index$index$smooth
      # Check to make sure a NULL wasn't given by smooth
      if (is.null(index)){
        message ('No smoothed index present. Please set "smooth = FALSE"')
        stop()
      }
    }
  }

  if ('hpiindex' %in% class(index_obj)){
    if (!smooth){
      index <- index$value
    } else {
      index <- index$smooth
      # Check to make sure a NULL wasn't given by smooth
      if (is.null(index)){
        message ('No smoothed index present. Please set "smooth = FALSE"')
        stop()
      }
    }
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
                            mean=mean(iv),
                            median=median(iv)),
                       class='indexvolatility')
  attr(vol_obj, 'orig') <- index
  attr(vol_obj, 'window') <- window

  ## Return

  # If returing in place
  if (in_place){

    if (smooth & in_place_name == 'volatility') in_place_name <- 'volatility_smooth'
inde
    if ('hpi' %in% class(index_obj)){
      index_obj$index[[in_place_name]] <- vol_obj
      return(index_obj)
    }

    if ('hpiindex' %in% class(index_obj)){
      index_obj[[in_place_name]] <- vol_obj
      return(index_obj)
    }
  }

  # If just returning result of volatility calculation

  vol_obj

}
