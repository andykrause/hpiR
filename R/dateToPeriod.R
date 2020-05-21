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
#' @importFrom dplyr dense_rank
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

  # Check Date Fields
  trans_df[[date]] <- checkDate(trans_df[[date]], 'date')
  min_date <- checkDate(min_date, 'min_date')
  max_date <- checkDate(max_date, 'max_date')

  # Set minimum date
  if (is.null(min_date)){
    min_date <- min(trans_df[[date]])
  } else {
    if (min_date > min(trans_df[[date]])){
      if (adj_type == 'move'){
        message('Supplied "min_date" is greater than minimum of transactions. ',
                'Adjusting.\n')
        min_date <- min(trans_df[[date]])
      }
      if (adj_type == 'clip'){
        message('Supplied "min_date" date is greater than minimum of transactions. ',
                'Clipping transactions.\n')
        trans_df <- trans_df[trans_df[[date]] >= min_date, ]
      }
    }
  }

  # Set maximum date
  if (is.null(max_date)){
    max_date <- max(trans_df[[date]])
  } else {
    if (max_date < max(trans_df[[date]])){
      if (adj_type == 'move'){
        message('Supplied "max_date" is less than maximum of transactions. Adjusting.\n')
        max_date <- max(trans_df[[date]])
      }
      if (adj_type == 'clip'){
        message('Supplied "max_date" is less than maximum of transactions. ',
                'Clipping transactions.\n')
        trans_df <- trans_df[trans_df[[date]] <= max_date, ]
      }
    }
  }

  # Set standardized data field
  trans_df$trans_date <- trans_df[[date]]

  # Make period_table
  period_table <- periodTable(trans_df = trans_df,
                              periodicity = periodicity)

  # Add to trans_df
  trans_df$trans_period <- dplyr::dense_rank(cut(trans_df$trans_date,
                                                 c(period_table$start_date,
                                                   period_table$end_date[nrow(period_table)] + 1)))

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
#' Create a table of the periods (generic method)
#'
#' Generic method for create simple table of all selected periods.  Used within `dateToPeriod()`
#'
#' @param trans_df Transaction data.frame
#' @param periodicity Periodicity option ('weekly', 'monthly', 'quarterly', 'annually')
#' @param ... Additional Arguments
#' @return [data.frame] consisting of
#' \describe{
#' \item{period}{Period number}
#' \item{start_date}{start date of each period}
#' \item{end_date}{end date of each period}
#' \item{name}{name of the period}
#' }
#' @importFrom lubridate year month quarter week floor_date ceiling_date
#' @examples
#'
#'  # Load data
#'  data(ex_sales)
#'  ex_sales$trans_date <- checkDate(ex_sales[['sale_date']], 'date')
#'
#'  # With a raw transaction data.frame
#'  pt_df <- periodTable(trans_df = ex_sales,
#'                       periodicity = 'annual')
#' @export

periodTable <- function(trans_df,
                        periodicity,
                        ...){

  periodicity <- structure(periodicity, class = periodicity)
  UseMethod("periodTable", periodicity)
}

#'
#' Create a table of the annual periods
#'
#' Specific method for creating annual period table
#'
#' @inherit periodTable params
#' @method periodTable annual
#' @export

periodTable.annual <- function(trans_df,
                               periodicity,
                               ...){

  start_date <- seq(lubridate::floor_date(min(trans_df$trans_date), 'year'),
                    lubridate::floor_date(max(trans_df$trans_date), 'year'),
                    by = '1 year')
  end_date <- seq(lubridate::ceiling_date(min(trans_df$trans_date), 'year'),
                  lubridate::ceiling_date(max(trans_df$trans_date), 'year'),
                  by = '1 year') - 1

  data.frame(period = 1:length(start_date),
             start_date = start_date,
             end_date = end_date,
             name = unique(paste0(lubridate::year(c(start_date, end_date)))),
             stringsAsFactors=FALSE)
}

#'
#' Create a table of the quarterly periods
#'
#' Specific method for creating quarterly period table
#'
#' @inherit periodTable params
#' @method periodTable quarterly
#' @export

periodTable.quarterly <- function(trans_df,
                                  periodicity,
                                  ...){

  start_date <- seq(lubridate::floor_date(min(trans_df$trans_date), 'quarter'),
                    lubridate::floor_date(max(trans_df$trans_date), 'quarter'),
                    by = '1 quarter')
  end_date <- seq(lubridate::ceiling_date(min(trans_df$trans_date), 'quarter'),
                  lubridate::ceiling_date(max(trans_df$trans_date), 'quarter'),
                  by = '1 quarter') - 1

  data.frame(period = 1:length(start_date),
             start_date = start_date,
             end_date = end_date,
             name = unique(paste0(lubridate::year(c(start_date, end_date)), '-',
                                  lubridate::quarter(c(start_date, end_date)))),
             stringsAsFactors=FALSE)
}

#'
#' Create a table of the monthly periods
#'
#' Specific method for creating monthly period table
#'
#' @inherit periodTable params
#' @method periodTable monthly
#' @export

periodTable.monthly <- function(trans_df,
                                periodicity,
                                ...){

  start_date <- seq(lubridate::floor_date(min(trans_df$trans_date), 'month'),
                    lubridate::floor_date(max(trans_df$trans_date), 'month'),
                    by = '1 month')
  end_date <- seq(lubridate::ceiling_date(min(trans_df$trans_date), 'month'),
                  lubridate::ceiling_date(max(trans_df$trans_date), 'month'),
                  by = '1 month') - 1

  data.frame(period = 1:length(start_date),
             start_date = start_date,
             end_date = end_date,
             name = unique(paste0(lubridate::year(c(start_date, end_date)), '-',
                                  lubridate::month(c(start_date, end_date), label = TRUE))),
             stringsAsFactors=FALSE)
}

#'
#' Create a table of the weekly periods
#'
#' Specific method for creating weekly period table
#'
#' @inherit periodTable params
#' @method periodTable weekly
#' @export

periodTable.weekly <- function(trans_df,
                               periodicity,
                               ...){

  start_date <- seq(lubridate::floor_date(min(trans_df$trans_date), 'weekly'),
                    lubridate::floor_date(max(trans_df$trans_date), 'weekly'),
                    by = '1 week')
  end_date <- seq(lubridate::ceiling_date(min(trans_df$trans_date), 'weekly'),
                  lubridate::ceiling_date(max(trans_df$trans_date), 'weekly'),
                  by = '1 week') - 1

  data.frame(period = 1:length(start_date),
             start_date = start_date,
             end_date = end_date,
             name = paste0('week: ', start_date, ' to ', end_date),
             stringsAsFactors=FALSE)
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
