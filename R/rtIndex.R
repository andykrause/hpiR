#'
#' Create a full index object by repeat transaction approach
#'
#' Wrapper to create index object via entire repeat transaction approach
#'
#' @param trans_df data.frame of transactions.  Can be a 'hpidata' or an 'rtdata' object.
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
#'  rt_index <- rtIndex(trans_df = ex_sales,
#'                      periodicity = 'monthly',
#'                      min_date = '2010-06-01',
#'                      max_date = '2015-11-30',
#'                      adj_type = 'clip',
#'                      date = 'sale_date',
#'                      price = 'sale_price',
#'                      trans_id = 'sale_id',
#'                      prop_id = 'pinx',
#'                      estimator = 'robust',
#'                      log_dep = TRUE,
#'                      trim_model = TRUE,
#'                      max_period = 48,
#'                      smooth = FALSE)
#'
#' @export

rtIndex <- function(trans_df,
                    ...){

  # Check if trans_df is an rt_df object
  if ('rtdata' %in% class(trans_df)){

    rt_trans <- trans_df

  } else {

    if (!'hpidata' %in% class(trans_df)){

      if (is.null(list(...)$date) ||
           (!any(class(trans_df[[list(...)$date]]) %in% c('Date', 'POSIXt')))){
        message('When supplying a raw data.frame to the "trans_df"',
                'object, a valid "date" argument must be supplied')
        stop()
      }

      # Create 'trans_df' object
      trans_df <- dateToPeriod(trans_df = trans_df,
                               ...)
    } # Ends if(!trans_df...)

    if (is.null(list(...)$trans_id)){
      message('When supplying a "trans_df" object to the "trans_df" object a ',
                '"trans_id" argument must be supplied')
      stop()
    }
    if (is.null(list(...)$prop_id)){
      message('When supplying a "trans_df" object to the "trans_df" object a ',
              '"prop_id" argument must be supplied')
      stop()
    }
    if (is.null(list(...)$price)){
      message('When supplying a "trans_df" object to the "trans_df" object a ',
              '"price" argument must be supplied')
      stop()
    }

    # Create hpi transaction object
    rt_trans <- rtCreateTrans(trans_df = trans_df,
                              # trans_id = trans_id,
                              # prop_id = prop_id,
                              # price = price,
                              ...)
  } # Ends if/else ('rtdata' %in% ...)

  if (!'rtdata' %in% class(rt_trans)){
    stop('Converting transactions data to repeat transaction object failed')
  }

  # Etimate the model
  rt_model <- hpiModel(model_type = 'rt',
                       hpi_df = rt_trans,
                       ...)

  if (class(rt_model) != 'hpimodel') stop('Estimating repeat sale model failed')

  # Convert to an index
  rt_index <- modelToIndex(rt_model,
                           ...)

  if (class(rt_index) != 'hpiindex') stop('Converting model results to index failed')

  if ('smooth' %in% names(list(...)) && isTRUE(list(...)$smooth)){

    if (!'smooth_order' %in% names(list(...))){
      smooth_order  <- 3
    } else {
      smooth_order <- list(...)$smooth_order
    }

    rt_index <- smoothIndex(index_obj = rt_index,
                            order = smooth_order,
                            in_place = TRUE,
                            ...)
    if (!'indexsmooth' %in% class(rt_index$smooth)) stop('Smoothing index failed')
  }

  # Return Values
  structure(list(data=rt_trans,
                 model=rt_model,
                 index=rt_index),
            class='hpi')
}
