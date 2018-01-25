#' @title hedModel
#' @description Estimate repeat sales model (method based on estimator class). Generic method.
#' @param estimator Type of model to estimates (base, robust, weighted)
#' @param hed_df Repeat sales dataset from rsCreateSales()
#' @param hed_spec Time matrix object from rsTimeMatrix()
#' @param price_diff Difference in price betwen the two sales
#' @param ... Additional arguments
#' @return rs model object
#' @section Further Details:
#' @examples
#' rs_model <- rsModel(estimator = 'base',
#'                     rs_df = rs_sales,
#'                     time_matrix = time_matrix,
#'                     price_diff = price_diff)
#' @export

## Generic Method

hedModel <- function(estimator,
                     hed_df,
                     hed_spec,
                     ...){
  UseMethod("hedModel")

}

#' @title hedModel.base
#' @description Estimate repeat sales model (method based on estimator class). Generic method.
#' @param estimator Type of model to estimates (base, robust, weighted)
#' @param hed_df Repeat sales dataset from rsCreateSales()
#' @param hed_spec twen the two sales
#' @param ... Additional arguments
#' @return rs model object
#' @section Further Details:
#' @examples
#' rs_model <- rsModel(estimator = 'base',
#'                     rs_df = rs_sales,
#'                     time_matrix = time_matrix,
#'                     price_diff = price_diff)
#' @export
## For Base Estimator

hedModel.base <- function(estimator,
                          hed_df,
                          hed_spec,
                          ...){

  hed_model <- lm(hed_spec, data=hed_df)

  class(hed_model) <- 'hedmod'

  hed_model

}

#' @title hedModel.robust
#' @description Estimate repeat sales model (method based on estimator class). Generic method.
#' @param estimator Type of model to estimates (base, robust, weighted)
#' @param hed_df Repeat sales dataset from rsCreateSales()
#' @param hed_spec Difference in price betwen the two sales
#' @param ... Additional arguments
#' @return hed model object
#' @section Further Details:
#' @examples
#' rs_model <- rsModel(estimator = 'base',
#'                     rs_df = rs_sales,
#'                     time_matrix = time_matrix,
#'                     price_diff = price_diff)
#' @export
#'
## For Robust Estimator

hedModel.robust <- function(estimator,
                            hed_df,
                            hed_spec,
                            ...){

  # Determine 'sparseness' of the data
  time_size <- median(table(hed_df$date_period))

  # Use different robust packages based on sparseness
  if(time_size > 5){
    hed_model <- MASS::rlm(hed_spec, data=hed_df)
  } else {
    hed_model <- robustbase::lmrob(hed_spec, data=hed_df)
  }

  class(hed_model) <- 'hedmod'

  hed_model

}
