#' @title rtIndex
#' @description Creates a house price index from a set of ttransactions using the
#' repeat transactions method
#' @param trans_df data.frame of transactions.  Can be a 'hpi_df' or an 'rt' object.
#' @param date Field contain the transaction date
#' @param price Field contain the transaction price
#' @param trans_id Field containing the unique transaction identifier
#' @param prop_id Field containing the property identifier
#' @param estimator default = 'base', Type of estimator to use.  'base', 'robust' or 'weighted'
#' @param log_dep default = TRUE, Should the dependent variable (price) be logged?
#' @param periodicity default = 'monthly', Periodicity of time to estimate index at
#' @param ... Additional Arguments
#' @return hpi object
#' @section Further Details:
#' @examples
#' sea_rt_index <- rtIndex(trans_df = seattle_sales,
#'                         date = 'sale_date',
#'                         price = 'sale_price',
#'                         trans_id = 'uniq_id',
#'                         prop_id = 'pinx')
#' @export

rtIndex <- function(trans_df,
                    ...){

  # Check if trans_df is an rt_df object
  if ('rt' %in% class(trans_df)){

    rt_trans <- trans_df

  } else {

    if (!'hpi_df' %in% class(trans_df)){

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
  } # Ends if/else ('rt' %in% ...)

  if (!'rt' %in% class(rt_trans)){
    message('Converting transactions data to repeat transaction object failed')
    stop()
  }

  # Etimate the model
  rt_model <- hpiModel(hpi_df = rt_trans,
                       ...)

  if (class(rt_model) != 'hpimodel'){
    message('Estimating repeat sale model failed')
    stop()
  }

  # Convert to an index
  rt_index <- modelToIndex(rt_model,
                           ...)

  if (class(rt_index) != 'hpiindex'){
    message('Converting model results to index failed')
    stop()
  }

  # Return Values
  structure(list(data=rt_trans,
                 model=rt_model,
                 index=rt_index),
            class='hpi')
}
