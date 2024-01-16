#'
#' Create a full index object by hedonic approach
#'
#' Wrapper to create index object via entire hedonic approach
#'
#' @param trans_df data.frame of transactions
#' @param ... Additional Arguments
#' @return `hpi`` object.  S3 list with:
#' \describe{
#' \item{data}{`hpidata` object}
#' \item{model}{`hpimodel` object}
#' \item{index}{`hpiindex` object}
#' }
#' @section Further Details:
#' Additional argument need to provide necessary argument for create `hpidata` objects if
#' the `trans_df` object is not of that class.
#' @examples
#'
#'  # Load data
#'  data(ex_sales)
#'
#'  # Create index with raw transaction data
#'  agg_index <- aggIndex(trans_df = ex_sales,
#'                        estimator = 'median')
#'
#' @export

aggIndex <- function(trans_df,
                     ...
){

  # Check if trans_df is an hed_df object
  if ('heddata' %in% class(trans_df)){

    hed_trans <- trans_df

  } else {

    if (!'hpidata' %in% class(trans_df)){

      if (is.null(list(...)$date) ||
          (!any(class(trans_df[[list(...)$date]]) %in% c('Date', 'POSIXt')))){
        stop('When supplying a raw data.frame to the "trans_df"',
                'object, a valid "date" argument must be supplied')
      }

      # Create 'trans_df' object
      trans_df <- dateToPeriod(trans_df = trans_df,
                               ...)
    } # Ends if(!trans_df...)

    if (is.null(list(...)$trans_id)){
      stop('When not supplying an "hpidata" object a valid "trans_id" argument must be supplied')
    }

    if (is.null(list(...)$prop_id)){
      stop('When not supplying an "hpidata" object a "prop_id" argument must be supplied')
    }

    if (is.null(list(...)$price)){
      stop('When not supplying an "hpidata" object a "price" argument must be supplied')
    }

    # Create Tranactions object
    hed_trans <- hedCreateTrans(trans_df = trans_df,
                               ...)
  } # Ends if/else ('heddata' %in% ...)

  if (!'heddata' %in% class(hed_trans)) stop('Converting sales data to hedonic sales object failed')

  # Estimate model
  agg_model <- hpiModel(model_type = 'agg',
                        hpi_df = hed_trans,
                        ...)

  # Convert to an index
  agg_index <- modelToIndex(agg_model,
                            max_period = max(agg_model$coefficients$time)
                            ,...)

  if (class(agg_index) != 'hpiindex') stop('Converting model results to index failed')

  if ('smooth' %in% names(list(...)) && isTRUE(list(...)$smooth)){

    if (!'smooth_order' %in% names(list(...))){
      smooth_order  <- 3
    } else {
      smooth_order <- list(...)$smooth_order
    }

    agg_index <- smoothIndex(index_obj = agg_index,
                             order = smooth_order,
                             in_place = TRUE,
                             ...)
    if (!'indexsmooth' %in% class(agg_index$smooth)) stop('Smoothing index failed')
  }

  # Return Values
  structure(list(data=hed_trans,
                 model=agg_model,
                 index=agg_index),
            class='hpi')
}
