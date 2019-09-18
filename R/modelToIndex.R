#'
#' Convert model results into a house price index
#'
#' Converts model results to standardized index objects
#'
#' @param model_obj Model results object
#' @param max_period Maximum number of periods that should have been estimated.
#' @param ... Additional arguments
#' @return `hpiindex` object containing:
#' \item{name}{vector of period names}
#' \item{numeric}{vector of period in numeric form}
#' \item{period}{vector of period numbers}
#' \item{value}{`ts` object of the index values}
#' \item{imputed}{vector of binary values indicating imputation}
#' @importFrom stats ts
#' @importFrom imputeTS na_locf
#' @importFrom imputeTS na_interpolation
#' @examples
#'
#'  # Load data
#'  data(ex_sales)
#'
#'  # With a raw transaction data.frame
#'  rt_data <- rtCreateTrans(trans_df = ex_sales,
#'                           prop_id = 'pinx',
#'                           trans_id = 'sale_id',
#'                           price = 'sale_price',
#'                           periodicity = 'monthly',
#'                           date = 'sale_date')
#'
#'  # Create model object
#'  hpi_model <- hpiModel(model_type = 'rt',
#'                        hpi_df = rt_data,
#'                        estimator = 'base',
#'                        log_dep = TRUE)
#'
#'  # Create Index
#'  hpi_index <- modelToIndex(hpi_model,
#'                            max_period = 84)
#'
#' @export

modelToIndex <- function(model_obj,
                         max_period = max(model_obj$coefficients$time),
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

  ## Deal with imputations

  # Extract coefficients
  coef_df <- data.frame(time = 1:max_period) %>%
    dplyr::left_join(model_obj$coefficients[1:max_period, ],
                     by = 'time')

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
      end_coef <- imputeTS::na_locf(coef_df$coefficient, "locf", 'keep')
      coef_df$coefficient[end_imp] <- end_coef[end_imp]
    }

    coef_df$coefficient <- imputeTS::na_interpolation(coef_df$coefficient,
                                                        option='stine')
    message('Total of ', length(which(na_coef)), ' period(s) imputed')
  }

  # Convert estimate to an index value
  if ('rfModel' %in% class(model_obj)){
    estimate <- coef_df$coefficient
    index_value <- ((estimate + 1) * 100)[1:max_period]
  } else {
    if (model_obj$log_dep){
      estimate <- c(exp(coef_df$coefficient) - 1)
      index_value <- ((estimate + 1) * 100)[1:max_period]
    } else {
      estimate <- ((coef_df$coefficient + model_obj$base_price) /
                     model_obj$base_price)
      index_value <- ((estimate) * 100)[1:max_period]
    }
  }

  # Convert to a time series (ts) object
  index <- stats::ts(data = index_value,
                     start = min(coef_df$time, na.rm=TRUE),
                     end = max_period)

  # Set as classed list and return
  structure(list(name = model_obj$periods$name[1:max_period],
                 numeric = model_obj$periods$numeric[1:max_period],
                 period = model_obj$periods$period[1:max_period],
                 value = index,
                 imputed = is_imputed[1:max_period]),
            class = 'hpiindex')
}
