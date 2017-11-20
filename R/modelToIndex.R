#' @title modelToIndex
#' @description Converts model results into a house price index
#' @param hpimodel Model results object
#' @param max_period Maximum number of periods that should have been estimated.
#' @param ... Additional arguments
#' @return rs model object
#' @section Further Details:
#' @examples
#' hpi_index <- modelToIndex(hpi_model,
#'                           max_period=84)
#' @export

### Convert model estimates into zpiindex object -----------------------------------------

modelToIndex <- function(hpimodel,
                         max_period=max(hpimodel$coefficients$time)){

  # Convert estimate to an index value
  if(hpimodel$log_dep){
    estimate <- c(0, exp(hpimodel$coefficients$coefficient) - 1)
    index_value <- ((estimate + 1) * 100)[1:max_period]
  } else {
    estimate <- ((hpimodel$coefficients$coefficient + hpimodel$base_price) /
                    hpimodel$base_price)

    index_value <- ((estimate) * 100)[1:max_period]
  }

  return(ts(data=index_value,
            start=min(hpimodel$coefficients$time),
            end=max(hpimodel$coefficients$time)))

}
