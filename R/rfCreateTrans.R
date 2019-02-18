#'
#' Create data for `rf` approach
#'
#' Generate standardized data for the `rf` modeling approach
#'
#' @param trans_df sales transaction in either a data.frame or a
#' trans_df class from dateToPeriod() function
#' @param prop_id field contain the unique property identification
#' @param trans_id field containing the unique transaction identification
#' @param price field containing the transaction price
#' @param date default=NULL, field containing the date of the transaction.
#' Only necessary if not passing an `hpidata` object
#' @param periodicity default=NULL, field containing the desired periodicity of analysis.
#' Only necessary if not passing a `hpidata` object
#' @param ... Additional arguments
#' @importFrom dplyr rename rename_ arrange filter desc
#' @importFrom magrittr %>%
#' @return data.frame of transactions with standardized period field. Note that a full
#' data.frame of the possible periods, their values and names can be found in the
#' attributes to the returned `hed` object
#' @examples
#'
#'  # Load example data
#'  data(ex_sales)
#'
#'  # Create Hed Data
#'  ex_rfdata <- rfCreateTrans(trans_df = ex_sales,
#'                               prop_id = 'pinx',
#'                               trans_id = 'sale_id',
#'                               price = 'sale_price',
#'                               date = 'sale_date',
#'                               periodicity = 'monthly')
#'
#' @export

rfCreateTrans <- function(trans_df,
                           prop_id,
                           trans_id,
                           price,
                           date=NULL,
                           periodicity=NULL,
                           ...){

  # Calculate the necessary date field
  if (!'hpidata' %in% class(trans_df)){
    if (is.null(date)){
      message('You must provide the name of a field with date of transaction (date=)')
      stop()
    }
    if (is.null(periodicity)){
      message('No periodicity (periodicity=) provided, defaulting to annual')
      periodicity <- 'yearly'
    }

    trans_df <- dateToPeriod(trans_df=trans_df,
                             date=date,
                             periodicity=periodicity,
                             ...)
  }

  # Check fields
  if (!prop_id %in% names(trans_df)){
    message('"prop_id" field not found')
    stop()
  }
  if (!trans_id %in% names(trans_df)){
    message('"trans_id" field not found')
    stop()
  }
  if (!price %in% names(trans_df)){
    message('"price" field not found')
    stop()
  }

  # Prepare input data
  rf_df <- trans_df %>%
    # Select fields and rename
    dplyr::rename_('prop_id' = prop_id,
                   'trans_id' = trans_id,
                   'price' = price) %>%
    # Order by id, then time, then desc by price
    dplyr::arrange(prop_id, .data$trans_period, dplyr::desc(price)) %>%

    # Remove any properties sold twice in same time period
    dplyr::filter(!duplicated(paste0(prop_id, '_', .data$trans_period)))

  # Add period table
  attr(rf_df, 'period_table') <- attr(trans_df, 'period_table')

  # Message if none
  if (is.null(rf_df) | nrow(rf_df) == 0){
    message('No Hedonic Sales Created\n')
    return(NULL)
  } else {
    class(rf_df) <- c('rfdata', 'hpidata', class(rf_df))
  }

  # Return _df
  rf_df
}
