#' @title hedModel
#' @description Estimate repeat sales model (method based on estimator class). Generic method.
#' @param estimator Type of model to estimates (base, robust, weighted)
#' @param hed_df Repeat sales dataset from rtCreateSales()
#' @param hed_spec Time matrix object from rtTimeMatrix()
#' @param price_diff Difference in price betwen the two sales
#' @param ... Additional arguments
#' @return `hedmodel` object: model object of the estimator (ex.: `lm`)
#' @importFrom utils methods
#' @importFrom stats median
#' @importFrom stats lm
#' @importFrom stats as.formula
#' @importFrom MASS rlm
#' @importFrom robustbase lmrob
#' @section Further Details:
#' `estimator` argument must be in a class of 'base', 'weighted' or 'robust'
#' This function is not generally called directly, but rather from `hpiModel()`
#' @examples
#' # Load Data
#' data(ex_heddata)
#'
#' # Estimate Model
#' hed_model <- hedModel(estimator = structure('base', class = 'base'),
#'                       hed_df = ex_heddata,
#'                       hed_spec = as.formula(log(price) ~ baths + tot_sf))
#' @export

hedModel <- function(estimator,
                     hed_df,
                     hed_spec,
                     ...){

  ## Check for proper classes

  # hed_df object
  if (!'heddata' %in% class(hed_df)){
    message('\nIncorrect class for "hed_df" object.  Must be of class "hed"')
    stop()
  }

  # Check that class is available
  if (!paste0('hedModel.', class(estimator)) %in% utils::methods(hedModel)){
    message('\nInvalid estimator type: "', class(estimator), '" method not available.')
    stop()
  }

  # Check for sparseness
  if (nrow(hed_df) < nrow(attr(hed_df, 'period_table'))){
    message('\nYou have fewer observations (', nrow(hed_df), ') than number of periods (',
            nrow(attr(hed_df, 'period_table')), '). Results will likely be unreliable.')
  }

  # Check for

  UseMethod("hedModel", estimator)

}

#' @title hedModel.base
#' @section Further Details:
#' See `?hedModel` for more information
#' @export

hedModel.base <- function(estimator,
                          hed_df,
                          hed_spec,
                          ...){

  # Estimate model
  hed_model <- stats::lm(hed_spec,
                         data=hed_df)

  # Add class
  class(hed_model) <- 'hedmodel'

  # Return
  hed_model

}

#' @title hedModel.robust
#' @section Further Details:
#' See `?hedModel` for more information
#' @export

hedModel.robust <- function(estimator,
                            hed_df,
                            hed_spec,
                            ...){

  # Determine 'sparseness' of the data
  time_size <- stats::median(table(hed_df$trans_period))

  # Use different robust packages based on sparseness
  if(time_size > 5){
    hed_model <- MASS::rlm(hed_spec, data=hed_df)
  } else {
    hed_model <- robustbase::lmrob(hed_spec, data=hed_df, setting="KS2014")
  }

  class(hed_model) <- 'hedmodel'

  hed_model

}

#' @title hedModel.weighted
#' @section Further Details:
#' See `?hedModel` for more information
#' @export

hedModel.weighted <- function(estimator,
                              hed_df,
                              hed_spec,
                              ...){

  # Extract weights
  wgts <- list(...)$weights

  if (length(wgts) != nrow(hed_df)){
    wgts <- rep(1, nrow(hed_df))
    message('You have supplied a set of weights that do not match the data.',
            'Model being run in base OLS format.')
  }

  # Force environment
  attr(hed_spec, '.Environment') <- environment()

  # Estimate model
  hed_model <- stats::lm(stats::as.formula(hed_spec), data=hed_df, weights=wgts)

  # Add class
  class(hed_model) <- 'hedmodel'

  # Return
  hed_model

}
