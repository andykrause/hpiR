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

plot.hpiindex <- function(hpi_obj,
                          ...){

  ## Extract Data
  hpi_data <- data.frame(x=rs_index$numeric,
                         y=as.numeric(rs_index$index))

  ## Make the base plot object
  gg_obj <- ggplot(hpi_data, aes(x=x, y=y)) +
    geom_line() +
    ylab("Index") +
    xlab('Time')

  # Return Values
  gg_obj

}
