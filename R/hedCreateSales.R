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
#' rep_sales <- rsCreateSales(sales_df = seattle_sales,
#'                            prop_id = 'pinx',
#'                            sale_id = 'uniq_id',
#'                            price = 'sale_price',
#'                            date = 'sale_date',
#'                            periodicity = 'month')
#'
#' ## When pre-calculating the time periods
#' sea_sales <- dateToPeriod(sales_df = seattle_sales,
#'                           date = 'sale_date',
#'                           periodicity = 'month')
#' rep_sales <- rsCreateSales(sales_df = sea_sales,
#'                            prop_id = 'pinx',
#'                            sale_id = 'uniq_id',
#'                            price = 'sale_price')
#'
#' @export

hedCreateSales <- function(sales_df,
                           prop_id,
                           sale_id,
                           price,
                           date=NULL,
                           periodicity=NULL,
                           ...){

  # Calculate the necessary date field
  if(class(sales_df)[1] != 'sales.df'){
    if(is.null(date)){
      message('You must provide the name of a field with date of sale (date=)')
      stop()
    }
    if(is.null(periodicity)){
      message('No periodicity (periodicity=) provided, defaulting to annual')
      periodicity <- 'year'
    }
    sales_df <- dateToPeriod(sales_df=sales_df,
                             date=date,
                             periodicity=periodicity)
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


  attr(hed_df, 'full_periods') <- attr(sales_df, 'full_periods')

  # Return _df
  class(hed_df) <- c('hed', class(hed_df))

  hed_df

}
