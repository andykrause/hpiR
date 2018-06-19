#' @title imputeSeries
#' @description imputes missing time periods in the results of an hpimodel object
#' @param coef_df data.frame of coefficient values from hpiModel() function
#' @param max_period Maximum number of periods that should have been estimated.
#' @param ... Additional arguments
#' @return data.frame of coefficient values
#' @section Further Details:
#' @examples
#' hpi_coef <- imputeSeries(coef_df,
#'                           max_period=84)
#' @export

imputeSeries <- function(coef_df,
                         max_period=max(coef_df$time),
                         ...){

  # Determine available period length
  all.periods <- 1:max_period

  # Determine those that are missing
  miss.periods <- all.periods[!all.periods %in% coef_df$time]

  # Create a DF of all periods
  imp.df <- data.frame(time=c(coef_df$time, miss.periods),
                       coefficient=c(coef_df$coefficient, rep(0, length(miss.periods))),
                       stringsAsFactors=FALSE)
  imp.df <- imp.df %>% dplyr::arrange(time)

  # Loop through and fix those that are missing
  for(mp in miss.periods){

    # Find nearest time period
    mp.dist <- abs(mp - coef_df$time)
    mp.near <- which(mp.dist == min(mp.dist))

    # Impute value and lo/hi
    imp.df$coefficient[mp] <- median(coef_df$coefficient[mp.near])

  }

  # Return values
  imp.df

}
