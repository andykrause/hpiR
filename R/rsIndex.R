#' @title rsIndex
#' @description Creates a house price index from a set of sales transactions using the
#' repeat sales method
#' @param sales_df data.frame of sale transactions.  Can be a 'salesdf' or an 'rs' object.
#' @param date Field contain the sales date
#' @param price Field contain the sale price
#' @param sale_id Field containing the unique sale identifier
#' @param prop_id Field containing the property identifier
#' @param estimator default = 'base', Type of estimator to use.  'base', 'robust' or 'weighted'
#' @param log_dep default = TRUE, Should the dependent variable (price) be logged?
#' @param periodicity default = 'monthly', Periodicity of time to estimate index at
#' @param ... Additional Arguments
#' @return hpi object
#' @section Further Details:
#' @examples
#' sea_rs_index <- rsIndex(sales_df = seattle_sales,
#'                         date = 'sale_date',
#'                         price = 'sale_price',
#'                         sale_id = 'uniq_id',
#'                         prop_id = 'pinx')
#' @export

rsIndex <- function(sales_df,
                    ...){

  # Check if sales_df is an rs_df object
  if ('rs' %in% class(sales_df)){

    rs_sales <- sales_df

  } else {

    if (!'salesdf' %in% class(sales_df)){

      if (is.null(list(...)$date) ||
           (!any(class(sales_df[[list(...)$date]]) %in% c('Date', 'POSIXt')))){
        message('When supplying a raw data.frame to the "sales_df"',
                'object, a valid "date" argument must be supplied')
        stop()
      }

      # Create 'salesdf' object
      sales_df <- dateToPeriod(sales_df = sales_df,
                               # date = date,
                               # periodicity = periodicity,
                               ...)
    } # Ends if(!salesdf...)

    if (is.null(list(...)$sale_id)){
      message('When supplying a "sales_df" object to the "sales_df" object a ',
                '"sale_id" argument must be supplied')
      stop()
    }
    if (is.null(list(...)$prop_id)){
      message('When supplying a "sales_df" object to the "sales_df" object a ',
              '"prop_id" argument must be supplied')
      stop()
    }
    if (is.null(list(...)$price)){
      message('When supplying a "sales_df" object to the "sales_df" object a ',
              '"price" argument must be supplied')
      stop()
    }

    # Create Sales object
    rs_sales <- rsCreateSales(sales_df = sales_df,
                              # sale_id = sale_id,
                              # prop_id = prop_id,
                              # price = price,
                              ...)
  } # Ends if/else ('rs' %in% ...)

  if (!'rs' %in% class(rs_sales)){
    message('Converting sales data to repeat sales object failed')
    stop()
  }

  # Etimate the model
  rs_model <- hpiModel(hpi_data = rs_sales,
                       ...)

  if (class(rs_model) != 'hpimodel'){
    message('Estimating repeat sale model failed')
    stop()
  }

  # Convert to an index
  rs_index <- modelToIndex(rs_model,
                           ...)

  if (class(rs_index) != 'hpiindex'){
    message('Converting model results to index failed')
    stop()
  }

  # Return Values
  structure(list(data=rs_sales,
                 model=rs_model,
                 index=rs_index),
            class='hpi')
}
