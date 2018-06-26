#' @title hedIndex
#' @description Creates a house price (rent) index from a set of transactions using the
#' hedonic price method
#' @param trans_df data.frame of transactions
#' @param date Field contain the transaction date
#' @param price Field contain the transaction price
#' @param trans_id Field containing the unique transaction identifier
#' @param prop_id Field containing the property identifier
#' @param estimator default = 'base', Type of estimator to use.  'base', 'robust' or 'weighted'
#' @param log_dep default = TRUE, Should the dependent variable (price) be logged?
#' @param periodicity default = 'month', Periodicity of time to estimate index at
#' @param dep_var default = NULL, dependent variable in hedonic model
#' @param ind_var default = NULL, independent variables in the hedonic model
#' @param hed_spec default = NULL, Full hedonic model specification
#' @param ... Additional Arguments
#' @return hpi object
#' @section Further Details:
#' @export

hedIndex <- function(trans_df,
                     dep_var=NULL,
                     ind_var=NULL,
                     hed_spec=NULL,
                     ...
){

  # Check if trans_df is an hed_df object
  if ('hed' %in% class(trans_df)){

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
  } # Ends if/else ('hed' %in% ...)

  if (!'hed' %in% class(hed_trans)){
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

  # Return Values
  structure(list(data=hed_trans,
                 model=hed_model,
                 index=hed_index),
            class='hpi')
}
