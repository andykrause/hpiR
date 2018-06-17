#' @title calcRevision
#' @description Estimate revision figures for a series of indexes
#' @usage Lorem Ipsum...
#' @param index_obj A list of progressively longer indexes (from calcForecstErrors())
#' @param series_name default = 'series'; name of the series if extracting
#' @param in_place default = FALSE; calculating in place (adding to hpi)
#' @param in_place_name default = 'rev'; name of revision object in_place
#' @param ... Additional Arguments
#' @return list containing:
#' \item{period} Mean revision by period
#' \item{total} Mean revision of all periods in all indexes
#' @section Further Details:
#' @examples
#' a <- 1
#' @export

calcRevision <- function(series_obj,
                         series_name = 'series',
                         in_place = FALSE,
                         in_place_name = 'rev',
                         ...){

  # Extract if giving full hpi object
  if ('hpi' %in% class(series_obj)){

    if (!series_name %in% names(series_obj)){
      message('The "hpi" object you provided does not have a "series" object ',
              ' or the "series_name" you provided is incorrect')
      stop()
    }
    series <- series_obj[[series_name]]
  } else {
    series <- series_obj
  }

  # Check class
  if (is.null(series) || !'hpiseries' %in% class(series)){
    message('"series_obj" must be of class "hpi_series" or a full "hpi" object with ',
            ' a "series_obj" object')
    stop()
  }

  # Calculate the differences in the indexes (n to n+1)
  index_diffs <- purrr::map(.x=2:length(series),
                            index_obj=series,
                            .f=function(x, index_obj){
                              index_obj[[x]][-length(index_obj[[x]])] - index_obj[[x - 1]]
                             })

  # Extract differences and place into lists by period (essentially transposing list)
  suppressWarnings(period_diffs <- purrr::transpose(rev(index_diffs)))

  # Convert to vector format in correct order
  period_diffs <- purrr::map(.x = period_diffs,
                             .f = function(x) rev(unlist(x)))

  # Calculate the means
  period_means <- unlist(purrr::map(.x=period_diffs,
                                    .f=mean))
  period_medians <- unlist(purrr::map(.x=period_diffs,
                                      .f=median))


  # Package and Return
  rev_obj <- structure(list(period = data.frame(period=1:length(period_means),
                                                mean=period_means,
                                                median=period_medians),
                            median = median(unlist(period_means)),
                            mean = mean(unlist(period_means))),
                       class='indexrevision')

  if (in_place && 'hpi' %in% class(series_obj)){
    series_obj[[in_place_name]] <- rev_obj
    return(series_obj)
  }

  rev_obj
}
