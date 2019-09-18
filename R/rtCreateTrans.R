#'
#' Create transaction data for rt approach
#'
#' Generate standardized object for rt estimate approach
#'
#' @param trans_df transactions in either a data.frame or a `hpidata`` class from
#' dateToPeriod() function
#' @param prop_id field contain the unique property identification
#' @param trans_id field containing the unique transaction identification
#' @param price field containing the transaction price
#' @param date default=NULL, field containing the date of the sale.
#' Only necessary if not passing an `hpidata` object
#' @param periodicity default=NULL, field containing the desired periodicity of analysis.
#' Only necessary if not passing a `hpidata` object
#' @param seq_only default=FALSE, indicating whether to only include sequential repeat observations
#' 1 to 2 and 2 to 3.  False returns 1 to 2, 1 to 3 and 2 to 3.
#' @param min_period_dist [12] Minimum number of period required between repeat sales
#' @param ... Additional arguments
#' @return data.frame of repeat transactions. Note that a full data.frame of the possible
#' periods, their values and names can be found in the attributes to the returned `rtdata` object
#' @importFrom dplyr mutate filter arrange select group_by desc summarize n
#' @importFrom utils combn
#' @importFrom plyr ddply
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @section Further Details:
#' Properties with greater than two transactions during the period will make pairwise matches
#' among all sales.  Any property transacting twice in the same period will remove the lower
#' priced of the two transactions.
#' If passing a raw data.frame (not a `hpidata`` object) the "date" field should refer to
#' a field containing a vector of class POSIXt or Date.
#' @examples
#'
#'  # Load data
#'  data(ex_sales)
#'
#'  # With a raw transaction data.frame
#'  rt_data <- rtCreateTrans(trans_df = ex_sales,
#'                           prop_id = 'pinx',
#'                           trans_id = 'sale_id',
#'                           price = 'sale_price',
#'                           periodicity = 'monthly',
#'                           date = 'sale_date')
#'
#' @export

rtCreateTrans <- function(trans_df,
                          prop_id,
                          trans_id,
                          price,
                          date = NULL,
                          periodicity = NULL,
                          seq_only = FALSE,
                          min_period_dist = NULL,
                          ...){

  # Hack to pass R CMD Check
  trans_period <- NULL

  # Crate the sales_df if not provided
  if (!'hpidata' %in% class(trans_df)){
    if (is.null(date)){
      message('You must provide the name of a field with date of transaction (date=)')
      stop()
    }
    if (is.null(periodicity)){
      message('No periodicity (periodicity=) provided, defaulting to yearly')
      periodicity <- 'yearly'
    }

    # Create trans_df
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
  trans_df <- trans_df %>%
    # Select fields and rename
    dplyr::select(., prop_id = prop_id,
                     trans_id = trans_id,
                     trans_period = trans_period,
                     price = price) %>%
    # Order by id, then time, then desc by price
    dplyr::arrange(., prop_id, .data$trans_period, dplyr::desc(price)) %>%

    # Remove any properties sold twice in same time period
    dplyr::filter(., !duplicated(paste0(prop_id, '_', .data$trans_period)))

  ## Make count of occurances for each property and keep those with 2 or more sales

  rt_df <- trans_df %>%
    # Group by property
    dplyr::group_by(., .data$prop_id) %>%
    # Count number of sales
    dplyr::summarize(., count=n()) %>%
    # Remove solo sales
    dplyr::filter(., .data$count > 1)

  if (nrow(rt_df) == 0){
    message('No repeat sales found')
    return(NULL)
  }

  ## Split into 2 sales and greater than two sales per property

  rt2 <- rt_df %>% dplyr::filter(., .data$count == 2)
  rt3 <- rt_df %>% dplyr::filter(., .data$count > 2)

  ## Create Repeat Sales for properties with exactly 2 sales

  if (nrow(rt2) > 0){

    # Extract original sales and arrange by id, then time
    x_df <- trans_df %>%
      dplyr::filter(., prop_id %in% rt2$prop_id) %>%
      dplyr::arrange(., prop_id, .data$trans_period)

    # Separate into first and second sale
    id_1 <- !duplicated(x_df$prop_id)
    id_2 <- duplicated(x_df$prop_id)

    # Create data.frame of repeat sales
    d2 <- data.frame(prop_id=x_df$prop_id[id_1],
                     period_1=x_df$trans_period[id_1],
                     period_2=x_df$trans_period[id_2],
                     price_1=x_df$price[id_1],
                     price_2=x_df$price[id_2],
                     trans_id1=x_df$trans_id[id_1],
                     trans_id2=x_df$trans_id[id_2],
                     stringsAsFactors = FALSE)

  } else {
    d2 <- NULL
  }

  ## Create Repeat sales for properties with 3 or more sales

  if (nrow(rt3) > 0){

    # Extract props with 3+ sales
    x_df <- trans_df %>% dplyr::filter(prop_id %in% rt3$prop_id)

    d3 <- x_df %>%
      # Create a data.frame of combination of repeat sales
      plyr::ddply(.variables=c('prop_id'),
                  .fun=function(x) (t(utils::combn(x$trans_id, m=2)))) %>%
      # Rename fields
      dplyr::select(prop_id, trans_id1='1', trans_id2='2') %>%

      # Add time and price
      dplyr::mutate(period_1 = x_df$trans_period[match(.data$trans_id1, x_df$trans_id)]) %>%
      dplyr::mutate(period_2 = x_df$trans_period[match(.data$trans_id2, x_df$trans_id)]) %>%
      dplyr::mutate(price_1 = x_df$price[match(.data$trans_id1, x_df$trans_id)]) %>%
      dplyr::mutate(price_2 = x_df$price[match(.data$trans_id2, x_df$trans_id)])

  } else {
    d3 <- NULL
  }

  ## Combine, check seq and add id

  # Combine and order
  rt_df <- rbind(d2, d3) %>%
    dplyr::arrange(prop_id, .data$period_1, .data$period_2)

  # Check for sequential only
  if (seq_only){
    rt_df <- rt_df %>%
      dplyr::filter(!duplicated(.data$trans_id1))
  }

  if (!is.null(min_period_dist)){
    rt_df <- rt_df %>%
      dplyr::mutate(pdist = .data$period_2 - .data$period_1) %>%
      dplyr::filter(.data$pdist >= min_period_dist) %>%
      dplyr::select(-.data$pdist)
  }

  # Add Unique Id
  rt_df <- rt_df %>%
    dplyr::mutate(pair_id = 1:nrow(rt_df))

  ## Return

  # Message if none
  if (is.null(rt_df) | nrow(rt_df) == 0){
    message('No Repeat Transactions Created\n')
    return(NULL)
  } else {
    class(rt_df) <- c('rtdata', 'hpidata', class(rt_df))
  }

  # Add period table attribute
  attr(rt_df, 'period_table') <- attr(trans_df, 'period_table')

  # Return rt_df
  rt_df

}
