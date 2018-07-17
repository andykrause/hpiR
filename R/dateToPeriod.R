#'
#' Convert dates to a relative period
#'
#' Create a relative period variable from a date variable
#'
#' @param trans_df data.frame of raw transactions
#' @param date name of field containing the date of the sale in Date or POSIXt format
#' @param periodicity type of periodicity to use ('yearly', 'quarterly', 'monthly' or 'weekly)
#' @param min_date default = NULL; optional minimum date to use
#' @param max_date default = NULL; optional maximum date to use
#' @param adj_type default = 'move'; how to handle min and max dates within the range of
#' transactions.  'move' min and/or max date or 'clip' the data
#' @param ... Additional arguments
#' @return original data frame (`trans_df` object) with two new fields:
#' trans_period: integer value counting from the minimum transaction date in the
#' periodicity selected. Base value is 1. Primarily for modeling trans_date: properly
#' formatted transaction date
#' @importFrom lubridate year month week quarter
#' @section Further Details:
#'   "trans_period" counts from the minimum transaction date provided.  As such the period
#'   counts are relative, not absolute
#'
#'   Additionally, this function modifies the data.frame that it is given and return that same
#'   data.frame that it is given and returns that data.frame with the new fields attached.
#' @examples
#'
#' # Load data
#'   data(ex_sales)
#'
#' # Convert to period df
#'   hpi_data <- dateToPeriod(trans_df = ex_sales,
#'                            date = 'sale_date',
#'                            periodicity = 'monthly')
#'
#' @export

dateToPeriod <- function(trans_df,
                         date,
                         periodicity = NULL,
                         min_date = NULL,
                         max_date = NULL,
                         adj_type = 'move',
                         ...){

  # Check for data.frame in trans_df
  if (!'data.frame' %in% class(trans_df)){
    message('"trans_df" must be a "data.frame" (or inherit from one)')
    stop()
  }

  # Extract Date
  trans_date <- checkDate(trans_df[[date]], 'date')

  # Check for periodicity
  if (is.null(periodicity)){
    message('No "periodicity" supplied, defaulting to "annual"')
    periodicity <- 'annual'
  }
  periodicity <- tolower(periodicity)
  if (!periodicity %in% c('weekly', 'monthly', 'quarterly', 'annual', 'yearly',
                          'w', 'm', 'q', 'a', 'y')){
    message('"periodicity" must be one of: "weekly", "monthly", "quarterly", or "annual"')
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

  # Check min and max date
  min_date <- checkDate(min_date, 'min_date')
  max_date <- checkDate(max_date, 'max_date')

  # Set minimum date
  if (is.null(min_date)){
    min_date <- min(trans_date)
  } else {
    if (min_date > min(trans_date)){
      if (adj_type == 'move'){
        message('Supplied "min_date" is greater than minimum of transactions. ',
                'Adjusting.\n')
        min_date <- min(trans_date)
      }
      if (adj_type == 'clip'){
        message('Supplied "min_date" date is greater than minimum of transactions. ',
                'Clipping transactions.\n')
        trans_df <- trans_df[trans_date >= min_date, ]
        trans_date <- trans_df[[date]]
      }
    }
  }

  # Set maximum date
  if (is.null(max_date)){
    max_date <- max(trans_date)
  } else {
    if (max_date < max(trans_date)){
      if (adj_type == 'move'){
        message('Supplied "max_date" is less than maximum of transactions. Adjusting.\n')
        max_date <- max(trans_date)
      }
      if (adj_type == 'clip'){
        message('Supplied "max_date" is less than maximum of transactions. ',
                'Clipping transactions.\n')
        trans_df <- trans_df[trans_date <= max_date, ]
        trans_date <- trans_df[[date]]
      }
    }
  }

  # Make date span
  date_span <- seq(min_date, max_date, 1)

  # Set standardized data field
  trans_df$trans_date <- trans_date

  # Create inital annual indicator
  year_period <- (lubridate::year(trans_date) - lubridate::year(min_date))

  # if Annual Periodicity
  if (periodicity == 'annual'){
    trans_df$trans_period <- year_period + 1
    period_table <- data.frame(names = unique(lubridate::year(date_span)),
                               values = unique(lubridate::year(date_span)),
                               periods = unique(lubridate::year(date_span)),
                               stringsAsFactors=FALSE)
  }

  # Create Month or Quarter
  if (periodicity %in% c('monthly', 'quarterly')){

    month_period <- (12 * year_period +
                     (lubridate::month(trans_date, label=FALSE) -
                        lubridate::month(min_date)))

    if (periodicity == 'monthly'){
      trans_df$trans_period <- month_period + 1
      period_table <- data.frame(
        name = unique(paste0(lubridate::year(date_span), '-',
                              lubridate::month(date_span, label = TRUE))),
        numeric = unique((lubridate::year(date_span) +
                        (lubridate::month(date_span) - 1) / 12)),
        period = unique((12 * (lubridate::year(date_span) -
                         min(lubridate::year(date_span))) +
                          (lubridate::month(date_span, label=FALSE) -
                            lubridate::month(min_date)) + 1)),
        stringsAsFactors=FALSE)
    }

    if (periodicity == 'quarterly'){
      min_qtr <- lubridate::quarter(min_date, with_year=TRUE)
      trans_qtr <- lubridate::quarter(trans_date, with_year=TRUE)
      all_qtr <- as.numeric(as.factor(c(min_qtr, trans_qtr)))
      trans_df$trans_period <- all_qtr[-1]
      period_table <- data.frame(
        name = unique(paste0(lubridate::year(date_span), '-Q',
                              lubridate::quarter(date_span))),
        numeric = unique((lubridate::year(date_span) +
                        (lubridate::quarter(date_span) - 1) / 4)),
        period = seq(1:length(unique(paste0(lubridate::year(date_span), '-Q',
                                     lubridate::quarter(date_span))))),
        stringsAsFactors=FALSE)
    }
  }

  # Create Week
  if (periodicity == 'weekly'){

    # Fix 53 week issue
    if (any(grepl('12-31', c(min_date, max_date, trans_date))) |
        any(grepl('12-30', c(min_date, max_date, trans_date)))){
      trans_date <- gsub('12-31', '12-29', trans_date)
      min_date <- gsub('12-31', '12-29', min_date)
      max_date <- gsub('12-31', '12-29', max_date)
      trans_date <- gsub('12-30', '12-29', trans_date)
      min_date <- gsub('12-30', '12-29', min_date)
      max_date <- gsub('12-30', '12-29', max_date)
      message('Treating all Dec 31st and Dec 30th (in leap years) dates as Dec 29th ',
              'to avoid 53rd week issues')
    }

    # Create Week period
    week_period <- (52 * (year_period) +
                    (lubridate::week(trans_date) -
                      lubridate::week(min_date)))
    trans_df$trans_period <- week_period + 1

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
                            lubridate::week(min_date))) + 1)),
      stringsAsFactors=FALSE)
  }

  # Check for missing periods %
  nbr_periods <- length(unique(trans_df$trans_period))
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
  attr(trans_df, 'class') <- c('hpidata', attr(trans_df, 'class'))
  attr(trans_df, 'periodicity') <- periodicity
  attr(trans_df, 'min_date') <- min_date
  attr(trans_df, 'max_date') <- max_date
  attr(trans_df, 'period_table') <- period_table

  # Return values
  trans_df
}

#'
#' Validate the date argument
#'
#' Internal function to validate (or convert) the provided date field
#'
#' @param x_date Date string or vector
#' @param name Name of argument to return in error/warning message
#' @importFrom lubridate as_date
#' @return Adjusted date field
#' @examples
#'
#'  # Load Data
#'   data(ex_sales)
#'
#'  # Check date
#'   date_checked <- checkDate(x_date = ex_sales$sale_date,
#'                             name = 'sale date')
#' @export

checkDate <- function(x_date,
                      name){

  # If null, give null (for dealing with ... arguments)
  if (is.null(x_date)) return(NULL)

  # If a number, give an error (stops from converting numbers to dates)
  if (any(class(x_date) %in% c('numeric', 'integer'))){
      message(name, ' argument must be in "Date" or "POSIXTct/POSIXt" format')
      stop()
  }

  # If any form of date/time, convert down to simple Date
  if (any(class(x_date) %in% c('Date', "POSIXct", "POSIXt"))){

    x_date <- lubridate::as_date(x_date)

  } else {

    # Try to convert dates given as characters, such as '2000-01-01'
    x_date <- suppressWarnings(lubridate::as_date(x_date))
    if (any(is.na(x_date))){
      message(name, ' argument must be in "Date" or "POSIXTct/POSIXt" format')
      stop()
    }
  }

  # Return
  x_date
}
