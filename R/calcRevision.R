#' Calculate revision values of an index
#'
#' Create estimates of the revision statistics for a house price index
#'
#' @param series_obj A list of progressively longer indexes (a `serieshpi`` object from
#' `createSeries()``)
#' @param in_place default = FALSE; Calculating in place (adding to hpi)
#' @param in_place_name default = 'rev'; Name of revision object in_place
#' @param smooth default = FALSE; Use smoothed indexes
#' @param ... Additional Arguments
#' @return list of length 3 containing:
#' \describe{
#'   \item{period}{Data.frame containing the period number, mean and median for that period}
#'   \item{mean}{Mean revision for all periods}
#'   \item{median}{Median revision for all periods}
#' }
#' @importFrom purrr map transpose
#' @section Further Details:
#' The revision object can be generate "in place" inside of the `serieshpi` object by
#' setting `in_place` equal to TRUE.
#' @examples
#'
#' # Load example sales
#'  data(ex_sales)
#'
#'  # Create Index
#'  rt_index <- rtIndex(trans_df = ex_sales,
#'                      periodicity = 'monthly',
#'                      min_date = '2010-06-01',
#'                      max_date = '2015-11-30',
#'                      adj_type = 'clip',
#'                      date = 'sale_date',
#'                      price = 'sale_price',
#'                      trans_id = 'sale_id',
#'                      prop_id = 'pinx',
#'                      estimator = 'robust',
#'                      log_dep = TRUE,
#'                      trim_model = TRUE,
#'                      max_period = 48,
#'                      smooth = FALSE)
#'
#'  # Create Series (Suppressing messages do to small sample size of this example)
#'   suppressMessages(
#'     hpi_series <- createSeries(hpi_obj = rt_index,
#'                                train_period = 12))
#'
#'  # Calculate revision
#'  series_rev <-  calcRevision(series_obj = hpi_series)
#'
#' @export

calcRevision <- function(series_obj,
                         in_place = FALSE,
                         in_place_name = 'rev',
                         smooth = FALSE,
                         ...){

  # Check class
  if (!'serieshpi' %in% class(series_obj)){
    message('"series_obj" must be of class "serieshpi"')
    stop()
  }

  if (smooth && 'smooth' %in% names(series_obj$hpis[[1]]$index)){
    index_name <- 'smooth'
  } else {
    if (smooth){
      message('No smoothed indexes found.  Create them with "smoothSeries()" and ',
              'try again')
      stop()
    }
    index_name <- 'value'
  }

  # Calculate the differences in the indexes (n to n+1)
  index_diffs <- purrr::map(.x=2:length(series_obj$hpis),
                            hpi_obj=series_obj$hpis,
                            .f=function(x, hpi_obj){
                              as.numeric((hpi_obj[[x]]$index[[index_name]][
                                -length(hpi_obj[[x]]$index[[index_name]])] -
                                 hpi_obj[[x - 1]]$index[[index_name]]))
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
                       class='seriesrevision')

  if (in_place){
    if (smooth){
      series_obj$revision_smooth <- rev_obj
    } else {
      series_obj$revision <- rev_obj
    }
    return(series_obj)
  }

  rev_obj
}
