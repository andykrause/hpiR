#' @title plot.hpiindex
#' @description Simple Plot of an hpiindex object
#' @usage Lorem Ipsum...
#' @param index_obj Object of class hpiindex
#' @param show_imputed Highlight the imputed points
#' @param ... Additional Arguments
#' @return ggplot object
#' @section Further Details:
#' Lorem Ipsum...
#' @examples
#' a <- 1
#' @export

plot.hpiindex <- function(index_obj,
                          show_imputed=FALSE,
                          ...){

  ## Extract Data
  hpi_data <- data.frame(x=index_obj$numeric,
                         y=as.numeric(index_obj$index),
                         imp=index_obj$imputed)

  ## Make the base plot object
  gg_obj <- ggplot(hpi_data, aes(x=x, y=y)) +
    geom_line() +
    ylab("Index") +
    xlab('Time')

  if (show_imputed){
    gg_obj <- gg_obj +
      geom_point(data=hpi_data, aes(x=x, y=y,
                                    color=as.factor(imp),
                                    size=imp)) +
      theme(legend.position="none")
  }

  # Return Values
  gg_obj

}

#' @title plot.hpi
#' @description Simple Plot of an HPI object
#' @usage Lorem Ipsum...
#' @param hpi_obj Object of class HPI
#' @param show_imputed Highlight the imputed points
#' @param ... Additional Arguments
#' @return ggplot object
#' @section Further Details:
#' Lorem Ipsum...
#' @examples
#' a <- 1
#' @export

plot.hpi <- function(hpi_obj,
                     show_imputed=FALSE,
                     ...){

  # Extract index
  index_obj <- hpi_obj$index

  ## Extract Data
  hpi_data <- data.frame(x=index_obj$numeric,
                         y=as.numeric(index_obj$index),
                         imp=index_obj$imputed)

  ## Make the base plot object
  gg_obj <- ggplot(hpi_data, aes(x=x, y=y)) +
    geom_line() +
    ylab("Index") +
    xlab('Time')

  if (show_imputed){
    gg_obj <- gg_obj +
      geom_point(data=hpi_data, aes(x=x, y=y,
                                    color=as.factor(imp),
                                    size=imp)) +
      theme(legend.position="none")
  }
  # Return Values
  gg_obj

}
