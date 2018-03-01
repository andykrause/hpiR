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
                         periodicity=NULL,
                         min_date=NULL,
                         max_date=NULL,
                         adj_type='move',
                         ...){

  # Check for data.frame in sales_df
  if (!'data.frame' %in% class(sales_df)){
    message('"sales_df" must be a data.frame (or inherit from one)')
    stop()
  }

  # Extract Date
  sale_date <- sales_df[[date]]
  if (!'Date' %in% class(sale_date)){
    if ("POSIXct" %in% class(sale_date) | "POSIXt" %in% class(sale_date)){
      sale_date <- lubridate::as_date(sale_date)
    } else {
      message('"date" field must be in "Date" or "POSIXTct/POSIXt" format.')
      stop()
    }
  }

  # Check for periodicity
  if (is.null(periodicity)){
    message('No "periodicity" supplied, defaulting to "annual"')
    periodicity <- 'annual'
  }
  periodicity <- tolower(periodicity)
  if (!periodicity %in% c('weekly', 'monthly', 'quarterly', 'annual', 'yearly',
                          'w', 'm', 'q', 'a', 'y')){
    message('"Periodicity" must be one of: "weekly", "monthly", "quarterly", or "annual"')
    stop()
  } else {
    if (periodicity == 'yearly') periodicity <- 'annual'
    if (periodicity == 'y') periodicity <- 'annual'
    if (periodicity == 'a') periodicity <- 'annual'
    if (periodicity == 'q') periodicity <- 'quarterly'
    if (periodicity == 'm') periodicity <- 'monthly'
    if (periodicity == 'w') periodicity <- 'weekly'
  }

  ## Create full span of dates to use in the analysis

  # Set minimum date
  if (is.null(min_date)){
    min_date <- min(sale_date)
  } else {
    if (min_date > min(sale_date)){
      if (adj_type == 'move'){
        message('Supplied min_date" is greater than minimum of sales. Adjusting.\n')
        min_date <- min(sale_date)
      }
      if (adj_type == 'clip'){
        message('Supplied "min_date" date is greater than minimum of sales. Clipping sales.\n')
        sales_df <- sales_df[sale_date >= min_date, ]
        sale_date <- sales_df[[date]]
      }
    }
  }

  # Set maximum date
  if (is.null(max_date)){
    max_date <- max(sale_date)
  } else {
    if (max_date < max(sale_date)){
      if (adj_type == 'move'){
        message('Supplied "max_date" is less than maximum of sales. Adjusting.\n')
        max_date <- max(sale_date)
      }
      if (adj_type == 'clip'){
        message('Supplied "max_date" is less than maximum of sales. Clipping Sales.\n')
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
  if (periodicity == 'annual'){
    sales_df$date_period <- year_period + 1
    period_table <- data.frame(names = unique(lubridate::year(date_span)),
                               values = unique(lubridate::year(date_span)),
                               periods = unique(lubridate::year(date_span)))
  }

  # Create Month or Quarter
  if (periodicity %in% c('monthly', 'quarterly')){

    month_period <- (12 * year_period +
                     (lubridate::month(sale_date, label=FALSE) -
                        lubridate::month(min_date)))

    if (periodicity == 'monthly'){
      sales_df$date_period <- month_period + 1
      period_table <- data.frame(
        name = unique(paste0(lubridate::year(date_span), '-',
                              lubridate::month(date_span, label = TRUE))),
        numeric = unique((lubridate::year(date_span) +
                        (lubridate::month(date_span) - 1) / 12)),
        period = unique((12 * (lubridate::year(date_span) -
                         min(lubridate::year(date_span))) +
                          (lubridate::month(date_span, label=FALSE) -
                            lubridate::month(min_date)) + 1)))
    }

    if (periodicity == 'quarterly'){
      min_qtr <- lubridate::quarter(min_date, with_year=TRUE)
      sale_qtr <- lubridate::quarter(sale_date, with_year=TRUE)
      all_qtr <- as.numeric(as.factor(c(min_qtr, sale_qtr)))
      sales_df$date_period <- all_qtr[-1]
      period_table <- data.frame(
        name = unique(paste0(lubridate::year(date_span), '-Q',
                              lubridate::quarter(date_span))),
        numeric = unique((lubridate::year(date_span) +
                        (lubridate::quarter(date_span) - 1) / 4)),
        period = unique((4 * (lubridate::year(date_span) -
                          min(lubridate::year(date_span))) +
                           ((lubridate::month(date_span, label=FALSE) -
                            lubridate::month(min_date)) %/% 3) + 1)))
    }
  }

  # Create Week
  if (periodicity == 'weekly'){

    # Fix 53 week issue
    if (any(grepl('12-31', c(min_date, max_date, sale_date))) |
        any(grepl('12-30', c(min_date, max_date, sale_date)))){
      sale_date <- gsub('12-31', '12-29', sale_date)
      min_date <- gsub('12-31', '12-29', min_date)
      max_date <- gsub('12-31', '12-29', max_date)
      sale_date <- gsub('12-30', '12-29', sale_date)
      min_date <- gsub('12-30', '12-29', min_date)
      max_date <- gsub('12-30', '12-29', max_date)
      message('Treating all Dec 31st and Dec 30th dates (leap years) as Dec 29th ',
              'to avoid 53 week issues')
    }

    # Create Week period
    week_period <- (52 * (year_period) +
                    (lubridate::week(sale_date) -
                      lubridate::week(min_date)))
    sales_df$date_period <- week_period + 1

    # Create period table
    name <- unique(paste0(lubridate::year(date_span), '-W',
                         lubridate::week(date_span)))
    name <- name[!grepl('W53', name)]
    period_table <- data.frame(
      name = name,
      numeric = unique((lubridate::year(date_span) +
                       (lubridate::week(date_span) - 1) / 52)),
      period = unique((52 * (lubridate::year(date_span) -
                         min(lubridate::year(date_span))) +
                          ((lubridate::week(date_span) -
                            lubridate::week(min_date))) + 1)))
  }

  # Check for missing periods %
  nbr_periods <- length(unique(sales_df$date_period))
  if (nbr_periods < nrow(period_table)){
    message("Your choice of periodicity resulted in ",
            nrow(period_table) - nbr_periods, " empty periods out of ",
            nrow(period_table), " total periods.")
    if ((nrow(period_table) - nbr_periods)/nrow(period_table) > .3){
      message('You may wish to set a coarser periodicity or ',
              'set different start and end dates\n')
    }
  }

  # Add attribute information
  attr(sales_df, 'class') <- unique(append('salesdf', attr(sales_df, 'class')))
  attr(sales_df, 'periodicity') <- periodicity
  attr(sales_df, 'min_date') <- min_date
  attr(sales_df, 'max_date') <- max_date
  attr(sales_df, 'period_table') <- period_table

  # Return values
  sales_df

}
