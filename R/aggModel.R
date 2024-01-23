#'
#' Estimate hedonic model for index creation
#'
#' Estimate coefficients for an index via the hedonic approach (generic method)
#'
#' @param estimator Type of model to estimates (base, robust, weighted)
#' @param hed_df Repeat sales dataset from hedCreateSales()
#' @param price_field Field on which to calcualate aggregat index)
#' @param ... Additional arguments
#' @return `aggmodel` object: model object of the estimator (ex.: `lm`)
#' @importFrom utils methods
#' @importFrom stats median
#' @importFrom dplyr group_by summarize mutate
#' @section Further Details:
#' `estimator` argument must be in a class of 'median' or 'mean'
#' This function is not generally called directly, but rather from `hpiModel()`
#' @examples
#'
#'  # Load example data
#'  data(ex_sales)
#'
#'  # Create hedonic data
#'  hed_data <- hedCreateTrans(trans_df = ex_sales,
#'                            prop_id = 'pinx',
#'                            trans_id = 'sale_id',
#'                            price = 'sale_price',
#'                            date = 'sale_date',
#'                            periodicity = 'monthly')
#'
#'  # Estimate Model
#'  hed_model <- hedModel(estimator = structure('base', class = 'base'),
#'                        hed_df = hed_data,
#'                        hed_spec = as.formula(log(price) ~ baths + tot_sf))
#'
#' @export

aggModel <- function(estimator,
                     hed_df,
                     price_field = 'price',
                     ...){

  ## Check for proper classes

  # hed_df object
  if (!'heddata' %in% class(hed_df)){
    message('\nIncorrect class for "hed_df" object.  Must be of class "hed"')
    stop()
  }

  # Check that class is available
  if (!paste0('aggModel.', class(estimator)) %in% utils::methods(aggModel)){
    message('\nInvalid estimator type: "', class(estimator), '" method not available.')
    stop()
  }

  # Check for sparseness
  if (nrow(hed_df) < nrow(attr(hed_df, 'period_table'))){
    message('\nYou have fewer observations (', nrow(hed_df), ') than number of periods (',
            nrow(attr(hed_df, 'period_table')), '). Results will likely be unreliable.')
  }

  # Check for

  UseMethod("aggModel", estimator)

}

#'
#' Hedonic model approach with base estimator
#'
#' Use of base estimator in hedonic model approach
#'
#' @section Further Details:
#' See `?aggModel` for more information
#' @inherit aggModel params
#' @method aggModel median
#' @importFrom stats median
#' @importFrom dplyr group_by summarize mutate
#' @export

aggModel.median <- function(estimator,
                            hed_df,
                            price_field = 'price',
                            ...){

  # Get the right field
  hed_df$agg_field <- hed_df[[price_field]]

  # Estimate model
  agg_model <- hed_df %>%
    dplyr::group_by(trans_period) %>%
    dplyr::summarize(value = median(agg_field)) %>%
    dplyr::mutate(coef = (value / value[1]) - 1)

  # Add class
  class(agg_model) <- 'aggmodel'
  attr(agg_model, "approach") <- ifelse(price_field == 'price',
                                        'agg',
                                        paste0('agg_', price_field))

  # Return
  agg_model

}

#'
#' Hedonic model approach with base estimator
#'
#' Use of base estimator in hedonic model approach
#'
#' @section Further Details:
#' See `?aggModel` for more information
#' @inherit aggModel params
#' @method aggModel mean
#' @importFrom dplyr group_by summarize mutate
#' @export

aggModel.mean <- function(estimator,
                          hed_df,
                          price_field = 'price',
                          ...){

  # Get the right field
  hed_df$agg_field <- hed_df[[price_field]]

  # Estimate model
  agg_model <- hed_df %>%
    dplyr::group_by(trans_period) %>%
    dplyr::summarize(value = mean(agg_field)) %>%
    dplyr::mutate(coef = (value / value[1]) - 1)

  # Add class
  class(agg_model) <- 'aggmodel'
  attr(agg_model, "approach") <- ifelse(price_field == 'price', 'agg_', paste0(price_field))

  # Return
  agg_model

}
