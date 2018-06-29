#' @title modelToIndex
#' @description Converts model results into a house price index
#' @param model_obj Model results object
#' @param max_period Maximum number of periods that should have been estimated.
#' @param ... Additional arguments
#' @return rs model object
#' @section Further Details:
#' @examples
#' hpi_index <- modelToIndex(hpi_model,
#'                           max_period=84)
#' @export

### Convert model estimates into zpiindex object -----------------------------------------

modelToIndex <- function(model_obj,
                         max_period=max(model_obj$coefficients$time),
                         ...){

  ## Check for proper class
  if (!'hpimodel' %in% class(model_obj)){
    message('"model_obj" object must be of class "hpimodel"')
    stop()
  }

  ## Check max period
  if (!any(class(max_period) %in% c('integer', 'numeric'))){
    message('"max_period" argument must be numeric/integer')
    stop()
  }

  # if (max_period > max(model_obj$coefficients$time)){
  #   message('"max_period" cannot be greater than maximum period in the estimated model.',
  #           ' Setting to maximum of estimated model')
  #   max_period <- max(model_obj$coefficients$time)
  # }

  ## Deal with imputations

  # Extract coefficients
  coef_df <- model_obj$coefficients[1:max_period, ]

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
    }

    # Fix cases where end is missing
    if (length(coef_df$coefficient) %in% which(na_coef)){
      message('Warning: You are extrapolating ending periods')
      not_na <- which(!na_coef)
      end_imp <- (max(not_na) + 1):length(coef_df$coefficient)
      end_coef <- imputeTS::na.locf(coef_df$coefficient, "locf", 'keep')
      coef_df$coefficient[end_imp] <- end_coef[end_imp]
    }

    coef_df$coefficient <- imputeTS::na.interpolation(coef_df$coefficient,
                                                        option='stine')
    message('Total of ', length(which(na_coef)), ' period(s) imputed')
  }

  # Convert estimate to an index value
  if (model_obj$log_dep){
    estimate <- c(exp(coef_df$coefficient) - 1)
    index_value <- ((estimate + 1) * 100)[1:max_period]
  } else {
    estimate <- ((coef_df$coefficient + model_obj$base_price) /
                    model_obj$base_price)
    index_value <- ((estimate) * 100)[1:max_period]
  }

  # Convert to a time series (ts) object
  index <- ts(data=index_value,
              start=min(coef_df$time, na.rm=TRUE),
              end=max_period)

  # Set as classed list and return
  structure(list(name = model_obj$periods$name[1:max_period],
                 numeric = model_obj$periods$numeric[1:max_period],
                 period = model_obj$periods$period[1:max_period],
                 value = index,
                 imputed = is_imputed[1:max_period]),
            class = 'hpiindex')
}
