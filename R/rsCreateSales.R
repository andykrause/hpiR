#' @title rsCreateSales
#' @description Create repeat sale pairs from a data frame of sale transactions
#' @param sales_df sales transaction in either a data.frame or a
#' sales.df class from dateToPeriod() function
#' @param prop_id field contain the unique property identification
#' @param sale_id field containing the unique sale identification
#' @param price field containing the sale price
#' @param date default=NULL, field containing the date of the sale.
#' Only necessary if not passing a sales.df object
#' @param periodicity default=NULL, field containing the desired periodicity of analysis.
#' Only necessary if not passing a sales.df object
#' @return data.frame of repeat sales
#' @section Further Details:
#' Properties with greater than two sales during the period will make pairwise matches
#' among all sales.  Any property selling twice in the same period will remove the lower
#' priced of the two sales.
#' If passing a raw data.frame (not a 'sales.df' object) the "date" field should refer to
#' a field containing a vector of class POSIXt or Date.
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

rsCreateSales <- function(sales_df,
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
  sales_df <- sales_df %>%
    # Select fields and rename
    dplyr::select(prop_id = prop_id,
                  sale_id = sale_id,
                  date_period,
                  price = price) %>%
    # Order by id, then time, then desc by price
    dplyr::arrange(prop_id, date_period, desc(price)) %>%

    # Remove any properties sold twice in same time period
    dplyr::filter(!duplicated(paste0(prop_id, '_', date_period)))

  ## Make count of occurances for each property and keep those with 2 or more sales

  rs_df <- sales_df %>%
    # Group by property
    dplyr::group_by(prop_id) %>%
    # Count number of sales
    dplyr::summarize(count=n()) %>%
    # Remove solo sales
    dplyr::filter(count > 1)

  ## Split into 2 sales and greater than two sales per property

  rs2 <- rs_df %>% dplyr::filter(count == 2)
  rs3 <- rs_df %>% dplyr::filter(count > 2)

  ## Create Repeat Sales for properties with exactly 2 sales

  if(nrow(rs2) > 0){

    # Extract original sales and arrange by id, then time
    x_df <- sales_df %>%
      dplyr::filter(prop_id %in% rs2$prop_id) %>%
      dplyr::arrange(prop_id, date_period)

    # Separate into first and second sale
    id_1 <- !duplicated(x_df$prop_id)
    id_2 <- duplicated(x_df$prop_id)

    # Create data.frame of repeat sales
    d2 <- data.frame(prop_id=x_df$prop_id[id_1],
                     date_1=x_df$date_period[id_1],
                     date_2=x_df$date_period[id_2],
                     price_1=x_df$price[id_1],
                     price_2=x_df$price[id_2],
                     sale_id1=x_df$sale_id[id_1],
                     sale_id2=x_df$sale_id[id_2])

  } else {
    d2 <- NULL
  }

  ## Create Repeat sales for properties with 3 or more sales

  if(nrow(rs3) > 0){

    # Extract props with 3+ sales
    x_df <- sales_df %>% dplyr::filter(prop_id %in% rs3$prop_id)

    d3 <- x_df %>%
      # Create a data.frame of combination of repeat sales
      plyr::ddply(.variables=c('prop_id'),
                  .fun=function(x) t(combn(x$sale_id, m=2))) %>%
      # Rename fields
      dplyr::select(prop_id, sale_id1='1', sale_id2='2') %>%
      # Add time and price
      dplyr::mutate(date_1 = x_df$date_period[match(sale_id1, x_df$sale_id)]) %>%
      dplyr::mutate(date_2 = x_df$date_period[match(sale_id2, x_df$sale_id)]) %>%
      dplyr::mutate(price_1 = x_df$price[match(sale_id1, x_df$sale_id)]) %>%
      dplyr::mutate(price_2 = x_df$price[match(sale_id2, x_df$sale_id)])

  } else {
    d3 <- NULL
  }

  ## Combine and Return

  # Combine
  rs_df <- rbind(d2, d3)

  # Message if none
  if (is.null(rs_df)){
    message('No Repeat Sales Created\n')
  } else {
    class(rs_df) <- append('rs', class(rs_df))
  }

  # Return _df
  rs_df

}
