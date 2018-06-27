#' @title rtTimeMatrix
#' @description Create a time matrix for a repeat transaction regression model
#' @param rt_df object of class `rt`: repeat transaction data.frame created by rtCreateTrans()
#' @return matrix to be used on the right hand side of a repeat sales regression model
#' @section Further Details:
#' Time periods are calculated from the data provided.
#' @examples
#' rep_sales <- rtCreateTrans(trans_df = seattle_sales,
#'                            prop_id = 'pinx',
#'                            sale_id = 'uniq_id',
#'                            date = 'sale_date',
#'                            price = 'sale_price',
#'                            periodicity = 'qtr')
#' rt_matrix <- rtTimeMatrix(rep_sales)
#' @export

rtTimeMatrix <- function(rt_df
                         ){

  # Check for proper class
  if (!"rtdata" %in% class(rt_df)){
    message('The rt_df object you have supplied is not of class "rtdata". You can create ',
            'an rtdata object with the rtCreateTrans() function.')
    stop()
  }

  # Extract start/end/diff
  time_start <- min(rt_df$period_1)
  time_end <- max(rt_df$period_2)
  time_diff <- time_end - time_start

  # Set up emply matrix
  time_matrix <- array(0, dim = c(nrow(rt_df), time_diff))

  # Fill in time matrix
  for (tm in seq(time_start + 1, time_end)) {
    time_matrix[rt_df$period_1 == tm, tm - time_start] <- -1
    time_matrix[rt_df$period_2 == tm, tm - time_start] <- 1
  }

  # Name Time matrix
  colnames(time_matrix) <- paste0("time_", seq(time_start + 1, time_end))
  class(time_matrix) <- 'timematrix'

  # Return Value
  time_matrix

}
