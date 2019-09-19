#'
#' Create a full index object by random forest approach
#'
#' Wrapper to create index object via entire random forest approach
#'
#' @param trans_df data.frame of transactions
#' @param dep_var default = NULL; Dependent variable in hedonic model
#' @param ind_var default = NULL; Independent variables in the hedonic model
#' @param rf_spec default = NULL; Full random forest model specification
#' @param ... Additional Arguments
#' @return `hpi`` object.  S3 list with:
#' \describe{
#' \item{data}{`hpidata` object}
#' \item{model}{`hpimodel` object}
#' \item{index}{`hpiindex` object}
#' }
#' @section Further Details:
#' Additional argument need to provide necessary argument for create `hpidata` objects if
#' the `trans_df` object is not of that class.
#' @examples
#'
#'
#'  # Load data
#'  data(ex_sales)
#'
#'  # Create index with raw transaction data
#'  rf_index <- rfIndex(trans_df = ex_sales,
#'                      periodicity = 'monthly',
#'                      min_date = '2010-06-01',
#'                      max_date = '2015-11-30',
#'                      adj_type = 'clip',
#'                      date = 'sale_date',
#'                      price = 'sale_price',
#'                      trans_id = 'sale_id',
#'                      prop_id = 'pinx',
#'                      estimator = 'pdp',
#'                      log_dep = TRUE,
#'                      trim_model = TRUE,
#'                      max_period = 48,
#'                      dep_var = 'price',
#'                      ind_var = c('tot_sf', 'beds', 'baths'),
#'                      smooth = FALSE,
#'                      ntrees = 10,
#'                      sim_count = 2)
#'
#' @export

rfIndex <- function(trans_df,
                     dep_var = NULL,
                     ind_var = NULL,
                     rf_spec = NULL,
                     ...){

  # Check if trans_df is an hed_df object
  if ('heddata' %in% class(trans_df)){

    rf_trans <- trans_df

  } else {

    if (!'hpidata' %in% class(trans_df)){

      if (is.null(list(...)$date) ||
          (!any(class(trans_df[[list(...)$date]]) %in% c('Date', 'POSIXt')))){
        message('When supplying a raw data.frame to the "trans_df"',
                'object, a valid "date" argument must be supplied')
        stop()
      }

      # Create 'trans_df' object
      trans_df <- dateToPeriod(trans_df = trans_df,
                               ...)
    } # Ends if(!trans_df...)

    if (is.null(list(...)$trans_id)){
      stop('When not supplying an "hpidata" object a valid "trans_id" argument must be supplied')
    }
    if (is.null(list(...)$prop_id)){
      stop('When not supplying an "hpidata" object a "prop_id" argument must be supplied')

    }
    if (is.null(list(...)$price)){
      stop('When not supplying an "hpidata" object a "price" argument must be supplied')
    }

    # Create Tranactions object
    rf_trans <- hedCreateTrans(trans_df = trans_df,
                                ...)

  } # Ends if/else ('heddata' %in% ...)

  if (!'heddata' %in% class(rf_trans)){
    stop('Converting sales data to random forest sales object failed')
  }

  # Estimate model if hed_spec given
  if (!is.null(rf_spec)){

    rf_model <- hpiModel(model_type = 'rf',
                         hpi_df = rf_trans,
                         mod_spec = rf_spec,
                         ...)
  }

  # Estimate the model if dep/ind given
  if (is.null(rf_spec) & (!is.null(dep_var) & !is.null(ind_var))){

    rf_model <- hpiModel(model_type = 'rf',
                         hpi_df = rf_trans,
                         dep_var = dep_var,
                         ind_var = ind_var,
                         ...)
  }

  if (is.null(rf_spec) & is.null(dep_var) & is.null(ind_var)){
    stop('Either a full specification (rf_spec) or dependent (dep_var) and ',
            'independent variables (ind_var) must be provided.')
  }

  if (class(rf_model) != 'hpimodel') stop('Estimating hedonic model failed')

  # Convert to an index
  rf_index <- modelToIndex(rf_model,
                            ...)

  if (class(rf_index) != 'hpiindex') stop('Converting model results to index failed')

  if ('smooth' %in% names(list(...)) && isTRUE(list(...)$smooth)){

    if (!'smooth_order' %in% names(list(...))){
      smooth_order  <- 3
    } else {
      smooth_order <- list(...)$smooth_order
    }

    rf_index <- smoothIndex(index_obj = rf_index,
                             order = smooth_order,
                             in_place = TRUE,
                             ...)
    if (!'indexsmooth' %in% class(rf_index$smooth)) stop('Smoothing index failed')
  }

  # Return Values
  structure(list(data=rf_trans,
                 model=rf_model,
                 index=rf_index),
            class='hpi')
}
