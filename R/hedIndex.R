#'
#' Create a full index object by hedonic approach
#'
#' Wrapper to create index object via entire hedonic approach
#'
#' @param trans_df data.frame of transactions
#' @param dep_var default = NULL; Dependent variable in hedonic model
#' @param ind_var default = NULL; Independent variables in the hedonic model
#' @param hed_spec default = NULL; Full hedonic model specification
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
#'\donttest{
#'  # Load data
#'  data(ex_sales)
#'
#'  # Create index with raw transaction data
#'  hed_index <- hedIndex(trans_df = ex_sales,
#'                        periodicity = 'monthly',
#'                        min_date = '2010-06-01',
#'                        max_date = '2015-11-30',
#'                        adj_type = 'clip',
#'                        date = 'sale_date',
#'                        price = 'sale_price',
#'                        trans_id = 'sale_id',
#'                        prop_id = 'pinx',
#'                        estimator = 'robust',
#'                        log_dep = TRUE,
#'                        trim_model = TRUE,
#'                        max_period = 48,
#'                        dep_var = 'price',
#'                        ind_var = c('tot_sf', 'beds', 'baths'),
#'                        smooth = FALSE)
#'}
#' @export

hedIndex <- function(trans_df,
                     dep_var = NULL,
                     ind_var = NULL,
                     hed_spec = NULL,
                     ...
){

  # Check if trans_df is an hed_df object
  if ('heddata' %in% class(trans_df)){

    hed_trans <- trans_df

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
      message('When not supplying an "hpidata" object a valid',
              '"trans_id" argument must be supplied')
      stop()
    }
    if (is.null(list(...)$prop_id)){
      message('When not supplying an "hpidata" object a ',
              '"prop_id" argument must be supplied')
      stop()
    }
    if (is.null(list(...)$price)){
      message('When not supplying an "hpidata" object a ',
              '"price" argument must be supplied')
      stop()
    }

    # Create Tranactions object
    hed_trans <- hedCreateTrans(trans_df = trans_df,
                               ...)
  } # Ends if/else ('heddata' %in% ...)

  if (!'heddata' %in% class(hed_trans)){
    message('Converting sales data to hedonic sales object failed')
    stop()
  }

  # Estimate model if hed_spec given
  if (!is.null(hed_spec)){

    hed_model <- hpiModel(hpi_df = hed_trans,
                          hed_spec = hed_spec,
                          ...)
  }

  # Estimate the model if dep/ind given
  if (is.null(hed_spec) & (!is.null(dep_var) & !is.null(ind_var))){

    hed_model <- hpiModel(hpi_df = hed_trans,
                          dep_var = dep_var,
                          ind_var = ind_var,
                          ...)
  }

  if (is.null(hed_spec) & is.null(dep_var) & is.null(ind_var)){
    message('Either a full specification (hed_spec) or dependent (dep_var) and ',
            'independent variables (ind_var) must be provided.')
    stop()
  }


  if(class(hed_model) != 'hpimodel'){
    message('Estimating hedonic model failed')
    stop()
  }

  # Convert to an index
  hed_index <- modelToIndex(hed_model,
                            ...)

  if(class(hed_index) != 'hpiindex'){
    message('Converting model results to index failed')
    stop()
  }

  if ('smooth' %in% names(list(...)) && isTRUE(list(...)$smooth)){

    if (!'smooth_order' %in% names(list(...))){
      smooth_order  <- 3
    } else {
      smooth_order <- list(...)$smooth_order
    }

    hed_index <- smoothIndex(index_obj = hed_index,
                             order = smooth_order,
                             in_place = TRUE,
                             ...)
    if (!'indexsmooth' %in% class(hed_index$smooth)){
      message('Smoothing index failed')
      stop()
    }
  }

  # Return Values
  structure(list(data=hed_trans,
                 model=hed_model,
                 index=hed_index),
            class='hpi')
}
