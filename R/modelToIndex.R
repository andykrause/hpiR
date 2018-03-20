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

  ## Check for proper class

  if (!'hpimodel' %in% class(hpimodel)){
    message('"hpimodel" object must be of class "hpimodel"')
    return(NULL)
  }

  ## Deal with imputations

  # Extract coefficients
  coef_df <- hpimodel$coefficients

  # Set up imputation identification vector
  is_imputed <- rep(0, length(coef_df$coef))

  # Determine which index values needs to be imputed
  na_coef <- is.na(coef_df$coef)

  # If any, then work through imputation process
  if (length(which(na_coef)) > 0){

    # Set all missing to imputed
    is_imputed[na_coef] <- 1

    # Fix cases where beginning is missing
    if (min(which(na_coef)) < min(which(!na_coef & coef_df$coefficient != 0))){
      message('Warning: You are extrapolating beginning periods')
      not_na <- which(!na_coef & coef_df$coefficient != 0)
      imp_to_0 <- na_coef[which(na_coef < min(not_na))]
      coef_df$coefficient[imp_to_0] <- 0
      #na_coef <- is.na(coef_df$coef)
    }

    # Fix cases where end is missing
    if (length(coef_df$coefficient) %in% which(na_coef)){
      message('Warning: You are extrapolating ending periods')
      not_na <- which(!na_coef)
      end_imp <- which(na_coef[(max(not_na) + 1):length(coef_df$coefficient)])
      end_coef <- imputeTS::na.locf(coef_df$coefficient, "locf", 'keep')
      coef_df$coefficient[end_imp] <- end_coef[end_imp]
      #na_coef <- is.na(coef_df$coef)
    }

    coef_df$coefficient <- imputeTS::na.interpolation(coef_df$coefficient,
                                                        option='stine')
    message('Total of ', length(which(na_coef)), ' period(s) imputed')
  }

  # Convert estimate to an index value
  if (hpimodel$log_dep){
    estimate <- c(exp(coef_df$coefficient) - 1)
    index_value <- ((estimate + 1) * 100)[1:max_period]
  } else {
    estimate <- ((coef_df$coefficient + hpimodel$base_price) /
                    hpimodel$base_price)
    index_value <- ((estimate) * 100)[1:max_period]
  }

  # Convert to a time series (ts) object
  index <- ts(data=index_value,
              start=min(coef_df$time),
              end=max_period)

  # Set as classed list and return
  structure(list(name = hpimodel$periods$name,
                 numeric = hpimodel$periods$numeric,
                 period = hpimodel$periods$period,
                 index = index,
                 imputed = is_imputed),
            class = 'hpiindex')
}
