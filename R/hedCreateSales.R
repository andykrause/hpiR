#' @title hedCreateSales
#' @description Create a hedonic model ready data.frame of sale transactions
#' @param sales_df sales transaction in either a data.frame or a
#' sales.df class from dateToPeriod() function
#' @param prop_id field contain the unique property identification
#' @param sale_id field containing the unique sale identification
#' @param price field containing the sale price
#' @param date default=NULL, field containing the date of the sale.
#' Only necessary if not passing a sales.df object
#' @param periodicity default=NULL, field containing the desired periodicity of analysis.
#' Only necessary if not passing a sales.df object
#' @return data.frame of repeat sales. Note that a full data.frame of the possible
#' periods, their values and names can be found in the attributes to the returned `hed` object
#' @section Further Details:
#' aaa
#' @examples
#' ## With a raw data.frame
#' hed_sales <- hedCreateSales(sales_df = seattle_sales,
#'                            prop_id = 'pinx',
#'                            sale_id = 'uniq_id',
#'                            price = 'sale_price',
#'                            date = 'sale_date',
#'                            periodicity = 'monthly')
#' @export

hedCreateSales <- function(sales_df,
                           prop_id,
                           sale_id,
                           price,
                           date=NULL,
                           periodicity=NULL,
                           ...){

  # Calculate the necessary date field
  if (!'salesdf' %in% class(sales_df)){
    if (is.null(date)){
      message('You must provide the name of a field with date of sale (date=)')
      stop()
    }
    if (is.null(periodicity)){
      message('No periodicity (periodicity=) provided, defaulting to annual')
      periodicity <- 'yearly'
    }

    sales_df <- dateToPeriod(sales_df=sales_df,
                             date=date,
                             periodicity=periodicity,
                             ...)
  }

  # Check fields
  if (!prop_id %in% names(sales_df)){
    message('"prop_id" field not found')
    stop()
  }
  if (!sale_id %in% names(sales_df)){
    message('"sale_id" field not found')
    stop()
  }
  if (!price %in% names(sales_df)){
    message('"price" field not found')
    stop()
  }

  # Prepare input data
  hed_df <- sales_df %>%
    # Select fields and rename
    dplyr::rename_('prop_id' = prop_id,
                   'sale_id' = sale_id,
                   'price' = price) %>%
    # Order by id, then time, then desc by price
    dplyr::arrange(prop_id, date_period, desc(price)) %>%

    # Remove any properties sold twice in same time period
    dplyr::filter(!duplicated(paste0(prop_id, '_', date_period)))

  # Add period table
  attr(hed_df, 'period_table') <- attr(sales_df, 'period_table')

  # Message if none
  if (is.null(hed_df) | nrow(hed_df) == 0){
    message('No Hedonic Sales Created\n')
    return(NULL)
  } else {
    class(hed_df) <- append('hed', class(hed_df))
  }

  # Return _df
  hed_df
}
