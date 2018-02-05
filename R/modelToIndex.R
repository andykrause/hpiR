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

  index <- ts(data=index_value,
             start=min(hpimodel$coefficients$time),
             end=max(hpimodel$coefficients$time))

  imputed <- rep(0, 84)

  na.index <- is.na(index)
  if (length(which(na.index)) > 0){

    imputed[na.index] <- 1

    # Fix cases where beginning is missing
    if(1 %in% which(na.index)){
      message('Warning: You are extrapolating beginning periods')
      not.na <- which(!na.index)
      beg_imp <- which(na.index[1:(not.na[1] - 1)])
      beg_index <- na.locf(index, "nocb", 'keep')
      index[beg_imp] <- beg_index[beg_imp]
    }

    # Fix cases where end is missing
    if(length(index) %in% which(na.index)){
      message('Warning: You are extrapolating ending periods')
      not.na <- which(!na.index)
      end_imp <- which(na.index[(max(not.na) + 1):length(index)])
      end_index <- na.locf(index, "locf", 'keep')
      index[end_imp] <- end_index[end_imp]
    }

    index <- imputeTS::na.interpolation(index, option='stine')
    message('Total of ', length(which(na.index)), ' period(s) imputed')
  }

  hpi <- list(name = rs_model$periods$name,
              numeric = rs_model$periods$numeric,
              period = rs_model$periods$period,
              index = index,
              imputed = imputed)

  class(hpi) <- 'hpiindex'
  hpi
}
