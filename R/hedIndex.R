#' @title hedIndex
#' @description Creates a house price index from a set of sales transactions using the
#' hedonic price index
#' @param sales_df data.frame of sale transactions
#' @param date Field contain the sales date
#' @param price Field contain the sale price
#' @param sale_id Field containing the unique sale identifier
#' @param prop_id Field containing the property identifier
#' @param estimator default = 'base', Type of estimator to use.  'base', 'robust' or 'weighted'
#' @param log_dep default = TRUE, Should the dependent variable (price) be logged?
#' @param periodicity default = 'month', Periodicity of time to estimate index at
#' @param dep_var default = NULL, dependent variable in hedonic model
#' @param ind_var default = NULL, independent variables in the hedonic model
#' @param hed_spec default = NULL, Full hedonic model specification
#' @param ... Additional Arguments
#' @return hpi object
#' @section Further Details:
#' @examples
#' sea_hed_index <- rsIndex(sales_df = seattle_sales,
#'                         date = 'sale_date',
#'                         price = 'sale_price',
#'                         sale_id = 'uniq_id',
#'                         prop_id = 'pinx')
#' @export

hedIndex <- function(sales_df,
                     # date,
                     # price,
                     # sale_id,
                     # prop_id,
                     # estimator='base',
                     # log_dep=TRUE,
                     # periodicity='month',
                     dep_var=NULL,
                     ind_var=NULL,
                     hed_spec=NULL,
                     ...
){

  # Check if sales_df is an hed_df object
  if ('hed' %in% class(sales_df)){

    hed_sales <- sales_df

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
    hed_sales <- hedCreateSales(sales_df = sales_df,
                               # sale_id = sale_id,
                               # prop_id = prop_id,
                               # price = price,
                               ...)
  } # Ends if/else ('hed' %in% ...)

  if (!'hed' %in% class(hed_sales)){
    message('Converting sales data to hedonic sales object failed')
    stop()
  }

  # Estimate model if hed_spec given
  if (!is.null(hed_spec)){

    hed_model <- hpiModel(hpi_data = hed_sales,
                          hed_spec = hed_spec,
                          ...)
  }

  # Estimate the model if dep/ind given
  if (is.null(hed_spec) & (!is.null(dep_var) & !is.null(ind_var))){

    hed_model <- hpiModel(hpi_data = hed_sales,
                          dep_var = dep_var,
                          ind_var = ind_var,
                          ...)
  }

  if (is.null(hed_spec) & is.null(dep_var) & is.null(ind_var)){
    message('Either a full specification (hed_spec) or dependent (dep_var) and ',
            'independent variables (ind_var) must be provided.')
    stop()
  }


  if(class(hed_model) != 'hpimodel'){
    message('Estimating hedonic model failed')
    stop()
  }

  # Convert to an index
  hed_index <- modelToIndex(hed_model,
                            ...)

  if(class(hed_index) != 'hpiindex'){
    message('Converting model results to index failed')
    stop()
  }

  # Return Values
  structure(list(data=hed_sales,
                 model=hed_model,
                 index=hed_index),
            class='hpi')
}
