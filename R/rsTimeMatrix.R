#' @title rsTimeMatrix
#' @description Create a time matrix for a repeat sale regression model
#' @param rs_df object of class `rs`: repeat sales data.frame created by rsCreateSales()
#' @return matrix to be used on the right hand side of a repeat sales regression model
#' @section Further Details:
#' Time periods are calculated from the data provided.
#' @examples
#' rep_sales <- rsCreateSales(sales_df = seattle_sales,
#'                            prop_id = 'pinx',
#'                            sale_id = 'uniq_id',
#'                            date = 'sale_date',
#'                            price = 'sale_price',
#'                            periodicity = 'qtr')
#' rs_matrix <- rsTimeMatrix(rep_sales)
#' @export

rsTimeMatrix <- function(rs_df
                         ){

  # Check for proper class
  if(class(rs_df)[1] != 'rs'){
    message('The rs_df object you have supplied is not of class rs. You can create ',
            'an rs object with the rsCreateSales() function.')
    return(NULL)
  }

  # Extract start/end/diff
  time_start <- min(rs_df$period_1)
  time_end <- max(rs_df$period_2)
  time_diff <- time_end - time_start

  # Set up emply matrix
  time_matrix <- array(0, dim = c(nrow(rs_df), time_diff))

  # Fill in time matrix
  for (tm in seq(time_start + 1, time_end)) {
    time_matrix[rs_df$period_1 == tm, tm - time_start] <- -1
    time_matrix[rs_df$period_2 == tm, tm - time_start] <- 1
  }

  # Name Time matrix
  colnames(time_matrix) <- paste0("time_", seq(time_start + 1, time_end))
  class(time_matrix) <- 'timematrix'

  # Return Value
  time_matrix

}
