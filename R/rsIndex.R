#' @title rsIndex
#' @description Creates a house price index from a set of sales transactions using the
#' repeat sales method
#' @param sales_df data.frame of sale transactions
#' @param date Field contain the sales date
#' @param price Field contain the sale price
#' @param sale_id Field containing the unique sale identifier
#' @param prop_id Field containing the property identifier
#' @param estimator default = 'base', Type of estimator to use.  'base', 'robust' or 'weighted'
#' @param log_dep default = TRUE, Should the dependent variable (price) be logged?
#' @param periodicity default = 'month', Periodicity of time to estimate index at
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
                    date,
                    price,
                    sale_id,
                    prop_id,
                    estimator='base',
                    log_dep=TRUE,
                    periodicity='month',
                    ...
                    ){

  # Create Sales object
  rs_sales <- rsCreateSales(sales_df = sales_df,
                            sale_id = sale_id,
                            prop_id = prop_id,
                            date = date,
                            price = price,
                            periodicity = periodicity,
                            ...)

  if(class(rs_sales)[1] != 'rs'){
    message('Converting sales data to repeat sales object failed')
    stop()
  }

  # Etimate the model
  rs_model <- hpiModel(hpi_data = rs_sales,
                       estimator=estimator,
                       log_dep=log_dep,
                       ...)

  if(class(rs_model) != 'hpimodel'){
    message('Estimating repeat sale model failed')
    stop()
  }

  # Convert to an index
  rs_index <- modelToIndex(rs_model)

  if(class(rs_index) != 'hpiindex'){
    message('Converting model results to index failed')
    stop()
  }

  # Return Values
  structure(list(data=rs_sales,
                 estimator=estimator,
                 model=rs_model,
                 index=rs_index),
           class='hpi')
}
