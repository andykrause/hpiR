#' @title calcVolatility
#' @description Calculate index volatility
#' @param index An object of class `hpiindex`
#' @param window default = 3; Rolling periods over which to calculate the volatility
#' @param in_place default = FALSE; Adds volatility metric to the `hpiindex` object
#' (may be within an `hpi` object)
#' @param in_place_name default = 'vol'; Name of volatility object in `hpiindex` object
#' @param smooth default = FALSE; Calculate on the smoothed index?
#' @param ... Additional arguments
#' @return an `indexvolatility` (S3) object, the 'index' slot of which is a `ts` object
#' \describe{
#' \item{roll}{ volatility at each rolling point}
#' \item{mean}{overall mean volatility}
#' \item{median}{overall median volatility}
#' }
#' @importFrom zoo rollapply
#' @importFrom stats median
#' @importFrom stats sd
#' @section Further Details:
#' You may also provide an `hpi` object to this function.  If you do, it will
#' extract the `hpiindex` object from the `index` slot in the `hpi` class object.
#' @examples
#' # Load data
#'   data(ex_hpiindex)
#'
#' # Calculate Volatility
#'   index_vol <- calcVolatility(index = ex_hpiindex,
#'                               window = 3)
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
  iv <- zoo::rollapply(deltas, window, stats::sd)

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

#' @title calcSeriesVolatility
#' @description Calculates volatility over a series of indexes
#' @usage calcSeriesVolatility(series_obj, window, smooth)
#' @param series_obj Series object to be calculted
#' @param window default = 3; Rolling periods over which to calculate the volatility
#' @param smooth default = FALSE; Also calculate volatilities for smoothed indexes
#' @param ... Additional Arguments
#' @return `serieshpi` object
#' @importFrom purrr map
#' @section Further Details:
#' Leaving order blank default to a moving average with order 3.
#' @examples
#' # Load Data
#'   data(ex_serieshpi)
#'
#' # Calculate series volatility
#'   series_vol <- calcSeriesVolatility(series_obj = ex_serieshpi,
#'                                      window= 3)
#' @export

calcSeriesVolatility <- function(series_obj,
                                 window = 3,
                                 smooth = FALSE,
                                 ...){

  # Bad series_obj
  if (!'serieshpi' %in% class(series_obj)){
    message('The "series_obj" must be of class "serieshpi"')
    stop()
  }

  # Apply smoothing to all indexes
  s_hpis <- purrr::map(.x=series_obj$hpis,
                       window = window,
                       .f = function(x, window){
                         ind <- x$index
                         s_ind <- calcVolatility(ind, window, smooth=FALSE,
                                                 in_place=TRUE)
                         x$index <- s_ind
                         x
                       })

  if (smooth){

    s_hpis <- purrr::map(.x=s_hpis,
                         window = window,
                         .f = function(x, window){
                           ind <- x$index
                           s_ind <- calcVolatility(ind, window, smooth=TRUE,
                                                   in_place=TRUE)
                           x$index <- s_ind
                           x
                         })

  }

  # Add to series obj
  series_obj$hpis <- s_hpis

  # Return standard
  series_obj

}

