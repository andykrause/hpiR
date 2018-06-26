#' @title calcRevision
#' @description Estimate revision figures for a series of indexes
#' @usage calcRevision(series_obj)
#' @param index_obj A list of progressively longer indexes (an 'hpiseries' object from createSeries())
#' @param series_name default = 'series'; Name of the series if extracting an existing one
#' @param in_place default = FALSE; Calculating in place (adding to hpi)
#' @param in_place_name default = 'rev'; name of revision object in_place
#' @param ... Additional Arguments
#' @return list containing:
#' \item{period: Data.frame containing the period number, mean and median for that period}
#' \item{mean: Mean revision for all periods}
#' \item{median: Median revision for all periods}
#' @section Further Details:
#' You can provide an 'hpi' object to the `index_obj` argument and the function will extract the series from it
#' given that the `series_name` argument is correct.
#'
#' The revision object can be generate "in place" inside of the `hpi` object by setting `in_place` equal to TRUE.
#' @examples
#'\dontrun{
#' hed_rev <- calcRevision(series_obj = hed_series,
#'                         series_name = 'series_1',
#'                         in_place = TRUE,
#'                         in_place_name = 'revision')
#'}
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
    message('"series_obj" must be of class "hpiseries" or a full "hpi" object with ',
            ' an "hpiseries" object in it and identified by the "series_name" argument')
    stop()
  }

  # Calculate the differences in the indexes (n to n+1)
  index_diffs <- purrr::map(.x=2:length(series),
                            index_obj=series,
                            .f=function(x, index_obj){
                              (index_obj[[x]]$index[-length(index_obj[[x]]$index)] -
                                 index_obj[[x - 1]]$index)
                             })

  # Extract differences and place into lists by period (essentially transposing list)
  suppressWarnings(period_diffs <- purrr::transpose(rev(index_diffs)))

  # Convert to vector format in correct order
  period_diffs <- purrr::map(.x = period_diffs,
                             .f = function(x) rev(unlist(x)))

  # Calculate the mean and medians
  period_means <- unlist(purrr::map(.x=period_diffs,
                                    .f=mean))
  period_medians <- unlist(purrr::map(.x=period_diffs,
                                      .f=median))


  # Package and Return
  rev_obj <- structure(list(period = data.frame(period=1:length(period_means),
                                                mean=period_means,
                                                median=period_medians,
                                                stringsAsFactors=FALSE),
                            median = median(unlist(period_means)),
                            mean = mean(unlist(period_means))),
                       class='hpirevision')

  if (in_place && 'hpi' %in% class(series_obj)){
    series_obj[[in_place_name]] <- rev_obj
    return(series_obj)
  }

  rev_obj
}
