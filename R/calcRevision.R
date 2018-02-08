#' @title calcRevision
#' @description Estimate revision figures for a series of indexes
#' @usage Lorem Ipsum...
#' @param index_obj A list of progressively longer indexes (from calcForecstErrors())
#' @param ... Additional Arguments
#' @return list containing:
#' \item{period} Mean revision by period
#' \item{total} Mean revision of all periods in all indexes
#' @section Further Details:
#' @examples
#' a <- 1
#' @export

calcRevision <- function(index_obj){

  # Calculate the differences in the indexes (n to n+1)
  index_diffs <- purrr::map(.x=2:length(index_obj),
                            index_obj=index_obj,
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

  # Return
  list(period = data.frame(period=1:length(period_means),
                           revision=period_means),
       total = mean(unlist(period_means)))

}
