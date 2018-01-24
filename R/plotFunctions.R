#' @title plot.hpi
#' @description Simple Plot of an HPI object
#' @usage Lorem Ipsum...
#' @param hpi_obj Object of class HPI
#' @param ... Additional Arguments
#' @return ggplot object
#' @section Further Details:
#' Lorem Ipsum...
#' @examples
#' a <- 1
#' @export

plot.hpi <- function(hpi_obj,
                     ...){

  ## Extract Data
  hpi_data <- data.frame(x=attr(hpi_obj$data, 'full_periods')$values,
                         y=as.numeric(hpi_obj$index))

  ## Make the base plot object
  gg_obj <- ggplot(hpi_data, aes(x=x, y=y)) +
    geom_line() +
    ylab("Index") +
    xlab('Time')

  # Return Values
  gg_obj

}
