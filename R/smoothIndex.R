#' @title smoothIndex
#' @description Smooths an index
#' @usage smoothIndex(index_obj, order, in_place, ...)
#' @param index Index to be smoothed
#' @param order Number of nearby period to smooth with, multiple means multiple iterations
#' @param in_place default = FALSE; adds smoothed index to the `hpiindex` object
#' @param ... Additional Arguments
#' @return a `ts`` and 'smooth_index` object with smoothed index
#' @importFrom forecast ma
#' @importFrom forecast ets
#' @importFrom forecast forecast
#' @section Further Details:
#' Leaving order blank default to a moving average with order 3.
#' @examples
#' # Load Data
#' data(ex_hpiindex)
#'
#' # Create Smooth index
#' sm_index <- smoothIndex(index_obj = ex_hpiindex,
#'                         order = 3,
#'                         in_place = FALSE)
#'
#' # Create Smooth index (in place)
#' sm_index <- smoothIndex(index_obj = ex_hpiindex,
#'                         order = 3,
#'                         in_place = TRUE)
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
  high_fc <- forecast::forecast(forecast::ets(s_index[1:(na_high[1] - 1)],
                                              model='ANN'), h=length(na_high))
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
#' @usage smoothSeries(series_obj, order, ...)
#' @param series_obj Series to be smoothed
#' @param order Number of nearby period to smooth with
#' @param ... Additional Arguments
#' @return a `serieshpi` object with a smoothed index in each `hpiindex` object
#' @importFrom purrr map
#' @section Further Details:
#' Leaving order blank default to a moving average with order 3.
#' @examples
#' # Load Data
#' data(ex_serieshpi)
#'
#' # Smooth indexes
#' sm_series <- smoothSeries(series_obj = ex_serieshpi,
#'                           order = 5)
#' @export

smoothSeries <- function(series_obj,
                         order = 3,
                         ...){

  # Bad series_obj
  if (!'serieshpi' %in% class(series_obj)){
    message('The "series_obj" must be of class "serieshpi"')
      stop()
  }

  # Apply smoothing to all indexes
  s_hpis <- purrr::map(.x=series_obj$hpis,
                         order=order,
                         .f = function(x, order){
                           ind <- x$index
                           s_ind <- smoothIndex(ind, order, in_place=TRUE)
                           x$index <- s_ind
                           x
                         })

  # Add to series obj
  series_obj$hpis <- s_hpis

  # Return standard
  series_obj

}

