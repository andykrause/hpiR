#' @title dateToPeriod
#' @description Convert dates into time periods for use in sale-resale models
#' @param sales_df data.frame of raw sales transactions
#' @param date name of field containing the date of the sale in Date or POSIXt format
#' @param periodicity type of periodicity to use ('year', 'qtr', 'month' or 'week)
#' @return data frame with three new fields:
#' date_period: integer value counting from the minimum sale date in the periodicity selected. Base value is 1. Primarily for modeling
#' date_value: float value of year and periodicty in numeric form (primarily for plotting)
#' date_name: text value of the period in the format, "Year-Period". (primarily for labeling)
#' @section Further Details:
#' date_period conat from the minimum sale date provided.  As such the period counts
#' are relative, not absolute
#' Additionally, this function modifies the data.frame that it is given and return that same
#' data.frame that it is given and returns that data.frame with the new fields attached.
#' It does so because this function is not intended as a stand-alone function but rather
#' one to be called by the ***CreateSales set of functions with hpiR
#' @examples
#' seattle_sales <- dateToPeriod(sales_df = seattle_sales,
#'                               date = sale_date,
#'                               periodicity = 'qtr')
#' @export

dateToPeriod <- function(sales_df,
                         date,
                         periodicity = 'year',
                         min_date=NULL,
                         max_date=NULL,
                         adj_type='move',
                         ...){

  # Extract Date
  sale_date <- sales_df[[date]]

  ## Create full span of dates

  # Set minimum date
  if(is.null(min_date)){
    min_date <- min(sale_date)
  } else {
    if (min_date > min(sale_date)){
      if (adj_type == 'move'){
        message('Supplied minimum date is greater than minimum of sales. Adjusting.\n')
        min_date <- min(sale_date)
      }
      if (adj_type == 'clip'){
        message('Supplied minimum date is greater than minimum of sales. Clipping sales.\n')
        sales_df <- sales_df[sale_date >= min_date, ]
        sale_date <- sales_df[[date]]
      }
    }
  }

  # Set maximum date
  if(is.null(max_date)){
    max_date <- max(sale_date)
  } else {
    if (max_date < max(sale_date)){
      if (adj_type == 'move'){
        message('Supplied maximum date is less than maximum of sales. Adjusting.\n')
        max_date <- max(sale_date)
      }
      if (adj_type == 'clip'){
        message('Supplied maximum date is less than maximum of sales. Clipping Sales.\n')
        sales_df <- sales_df[sale_date <= max_date, ]
        sale_date <- sales_df[[date]]
      }
    }
  }

  # Make date span
  date_span <- seq(min_date, max_date, 1)

  # Create inital annual indicator
  year_period <- (lubridate::year(sale_date) - lubridate::year(min_date))

  # if Annual Periodicity
  if(periodicity == 'year'){
    sales_df$date_period <- year_period + 1
    sales_df$date_value <- lubridate::year(sale_date)
    sales_df$date_name <- as.character(lubridate::year(sale_date))
    full_periods <- data.frame(names = unique(lubridate::year(date_span)),
                               values = unique(lubridate::year(date_span)),
                               periods = unique(lubridate::year(date_span)))
  }

  # Create Month or Quarter
  if(periodicity %in% c('month', 'qtr')){

    month_period <- (12 * year_period +
                     (lubridate::month(sale_date, label=FALSE) -
                        lubridate::month(min_date)))

    if(periodicity == 'month'){
      sales_df$date_period <- month_period + 1
      sales_df$date_value <- (lubridate::year(sale_date) +
                               (lubridate::month(sale_date) - 1) / 12)
      sales_df$date_name <- paste0(lubridate::year(sale_date), '-',
                                   lubridate::month(sale_date, label = TRUE))
      full_periods <- data.frame(
        names = unique(paste0(lubridate::year(date_span), '-',
                              lubridate::month(date_span, label = TRUE))),
        values = unique((lubridate::year(date_span) +
                        (lubridate::month(date_span) - 1) / 12)),
        periods = unique((12 * (lubridate::year(date_span) -
                         min(lubridate::year(date_span))) +
                          (lubridate::month(date_span, label=FALSE) -
                            lubridate::month(min_date)) + 1)))
    }

    if(periodicity == 'qtr'){
      sales_df$date_period <- (month_period %/% 3) + 1
      sales_df$date_value <- (lubridate::year(sale_date) +
                               (lubridate::quarter(sale_date) - 1) / 4)
      sales_df$date_name <- paste0(lubridate::year(sale_date), '-Q',
                                   lubridate::quarter(sale_date))

      full_periods <- data.frame(
        names = unique(paste0(lubridate::year(date_span), '-Q',
                              lubridate::quarter(date_span))),
        values = unique((lubridate::year(date_span) +
                        (lubridate::quarter(date_span) - 1) / 4)),
        periods = unique((4 * (lubridate::year(date_span) -
                          min(lubridate::year(date_span))) +
                           ((lubridate::month(date_span, label=FALSE) -
                            lubridate::month(min_date)) %/% 3) + 1)))
    }
  }

  # Create Week
  if(periodicity == 'week'){
    week_period <- (52 * (year_period) +
                    (lubridate::week(sale_date) -
                      lubridate::week(min_date)))

    sales_df$date_period <- week_period + 1
    sales_df$date_value <- (lubridate::year(sale_date) +
                             (lubridate::week(sale_date) - 1) / 53)
    sales_df$date_name <- paste0(lubridate::year(sale_date), '-W',
                                   lubridate::week(sale_date))

    full_periods <- data.frame(
      names = unique(paste0(lubridate::year(date_span), '-W',
                            lubridate::week(date_span))),
      values = unique((lubridate::year(date_span) +
                       (lubridate::week(date_span) - 1) / 53)),
      periods = unique((53 * (lubridate::year(date_span) -
                         min(lubridate::year(date_span))) +
                          ((lubridate::week(date_span) -
                            lubridate::week(min_date))) + 1)))
  }

  attr(sales_df, 'class') <- append('sales.df', attr(sales_df, 'class'))
  attr(sales_df, 'full_periods') <- full_periods

  # Return values
  sales_df

}
