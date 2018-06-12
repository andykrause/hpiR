#' @title hpiModel
#' @description Estimate the model for any method of house price index.  Generic method.
#' @usage Lorem Ipsum...
#' @param hpi_data Dataset created by one of the *CreateSales() function in this package.
#' @param estimator Type of estimator to be used ('base', 'weighted', 'robust')
#' @param log_dep default TRUE, should the dependent variable (change in price) be logged?
#' @param trim_model default TRUE, should excess be trimmed from model results ('lm' or 'rlm' object)?
#' @param ... Additional Arguments
#' @return hpimodel object
#' @section Further Details:
#' Lorem Ipsum...
#' @examples
#' a <- 1
#' @export

hpiModel <- function(hpi_data,
                     estimator='base',
                     log_dep=TRUE,
                     trim_model=TRUE,
                     ...){

  UseMethod("hpiModel")

}

#' @title hpiModel.rs
#' @description Estimate the model for any method of house price index.  Generic method.
#' @usage Lorem Ipsum...
#' @param hpi_data Dataset created by one of the *CreateSales() function in this package.
#' @param estimator Type of estimator to be used ('base', 'weighted', 'robust')
#' @param log_dep default TRUE, should the dependent variable (change in price) be logged?
#' @param trim_model default TRUE, should excess be trimmed from model results ('lm' or 'rlm' object)?
#' @param ... Additional Arguments
#' @return hpimodel object
#' @section Further Details:
#' Lorem Ipsum...
#' @examples
#' sea_sales <- dateToPeriod(sales_df = seattle_sales,
#'                           date = 'sale_date',
#'                           periodicity = 'month')
#' rep_sales <- rsCreateSales(sales_df = sea_sales,
#'                            prop_id = 'pinx',
#'                            sale_id = 'uniq_id',
#'                            price = 'sale_price')
#' rs_model <- hpiModel(hpi_data = rep_sales,
#'                      estimator = 'base',
#'                      log_dep = TRUE)
#' @export

hpiModel.rs <- function(hpi_data,
                        estimator='base',
                        log_dep=TRUE,
                        trim_model=TRUE,
                        ...){

  # Create time matrix
  time_matrix <- rsTimeMatrix(hpi_data)

  # Calculate price differential
  if (log_dep){
    price_diff <- log(hpi_data$price_2) - log(hpi_data$price_1)
  } else {
    price_diff <- hpi_data$price_2 - hpi_data$price_1
  }

  # If any NA, NaN, or Inf/-Inf
  if (any(!is.finite(price_diff))){
    message('NA, negative, zero or non-finite values in the price field')
    return(NULL)
  }

  # Extract base period mean price
  base_price <- mean(hpi_data$price_1[hpi_data$period_1 == min(hpi_data$period_1)])

  ## Estimate Model

  # Check for legal estimator type
  if (!estimator %in% c('base', 'robust', 'weighted')){
    message('Provided estimator type is not supported. Allowed estimators are:',
            '"base", "robust" or "weighted".  Defaulting to "base"')
    estimator <- structure('base', class='base')
  } else {
    estimator <- structure(estimator, class=estimator)
  }

  # Set estimator class, call method
  rs_mod <- rsModel(rs_df=hpi_data,
                    time_matrix=time_matrix,
                    price_diff=price_diff,
                    estimator=estimator,
                    ...)

  # Check for successful model estimation
  if (class(rs_mod) != 'rsmod'){
    message('Model estimator was unsuccessful')
    return(NULL)
  }

  # Remove qr to keep model obj small
  if (trim_model) rs_mod$qr <- NULL

  # If successful create list of results

  # Create coefficient data.frame
  model_df <- data.frame(time=c(min(hpi_data$period_1),
                                as.numeric(gsub('time_matrixtime_', '',
                                                names(rs_mod$coefficients)))),
                         coefficient=c(0, as.numeric(rs_mod$coefficients)))

  # Combine into list with class 'hpimodel and return
  structure(list(estimator=estimator,
                 coefficients=model_df,
                 model_obj=rs_mod,
                 mod_spec=NULL,
                 log_dep=log_dep,
                 base_price=base_price,
                 periods=attr(hpi_data, 'period_table'),
                 approach='rs'),
            class='hpimodel')
}

#' @title hpiModel.hed
#' @description Estimate the model for any method of house price index.  Generic method.
#' @usage Lorem Ipsum...
#' @param hpi_data Dataset created by one of the *CreateSales() function in this package.
#' @param estimator Type of estimator to be used ('base', 'weighted', 'robust')
#' @param hed_spec default=NULL; hedonic model specification
#' @param dep_var default=NULL; dependent variable of the model
#' @param ind_var default=NULL; independent variable(s) of the model
#' @param log_dep default=TRUE; should the dependent variable (change in price) be logged?
#' @param trim_model default TRUE, should excess be trimmed from model results ('lm' or 'rlm' object)?
#' @param ... Additional Arguments
#' @return hpimodel object
#' @section Further Details:
#' Lorem Ipsum...
#' @examples
#' sea_sales <- dateToPeriod(sales_df = seattle_sales,
#'                           date = 'sale_date',
#'                           periodicity = 'month')
#' rep_sales <- rsCreateSales(sales_df = sea_sales,
#'                            prop_id = 'pinx',
#'                            sale_id = 'uniq_id',
#'                            price = 'sale_price')
#' rs_model <- hpiModel(hpi_data = rep_sales,
#'                      estimator = 'base',
#'                      log_dep = TRUE)
#' @export

hpiModel.hed <- function(hpi_data,
                         estimator='base',
                         log_dep=TRUE,
                         hed_spec=NULL,
                         dep_var=NULL,
                         ind_var=NULL,
                         trim_model=TRUE,
                         ...){

  # Create specification
  if (!is.null(hed_spec)){
    if (class(hed_spec) != 'formula'){
      message('"hed_spec" argument must be of class "formula"')
      return(NULL)
    } else {
      hed_spec <- update(hed_spec, ~ . +as.factor(date_period))
    }
  } else {

    if (is.null(dep_var) | is.null(ind_var)){
      message('"dep_var" and "ind_var" must be supplied')
      return(NULL)
    }

    if(log_dep){
      dep_var <- paste0('log(', dep_var, ')')
    }
    hed_spec <- as.formula(paste0(dep_var, ' ~ ', paste(ind_var, collapse="+"),
                           '+ as.factor(date_period)'))
  }

  # Extract base period mean price
  base_price <- mean(hpi_data$price[hpi_data$date_period == min(hpi_data$date_period)])

  ## Estimate Model

  # Check for legal estimator type
   if(!estimator %in% c('base', 'robust', 'weighted')){
     message('Provided estimator type is not supported. Allowed estimators are:',
             '"base", "robust" or "weighted".  Defaulting to "base"')
    estimator <- 'base'
   }

  # Check log dep vs data
  if ((log_dep && any(hpi_data$price <= 0)) |
       any(is.na(hpi_data$price)) |
        any(!is.finite(hpi_data$price))){
    message('Your "price" field includes invalid values')
    stop()
  }

  # Set estimator class, call method
   class(estimator) <- estimator
   if (class(estimator) == 'weighted' & is.null(list(...)$weights)){
     message('You selected a weighted model but did not supply any weights.',
             '"weights" argument is NULL. Model run in base OLS format.')
     estimator <- structure('base', class='base')
   }

   hed_mod <- hedModel(estimator=estimator,
                       hed_df=hpi_data,
                       hed_spec = hed_spec,
                       ...)

  # Check for successful model estimation
  if(class(hed_mod) != 'hedmod'){

    message('Model estimator was unsuccessful')
    stop()
  }

   # Remove qr to keep model obj small
   if (trim_model) hed_mod$qr <- NULL

  # If successful create list of results
  base_period <- min(hpi_data$date_period)

  # Period names
  p_names <- grep('date_period', names(hed_mod$coefficients))
  periods <- c(base_period,
               as.numeric(gsub('[as.factor(date_period)]', '',
                               names(hed_mod$coefficients)[p_names])))

  # Coefficients
  coefs <- c(0, as.numeric(hed_mod$coefficients)[p_names])

  model_df <- data.frame(time=periods,
                         coefficient=coefs)

  # Combine into list
  hed_model <- list(estimator=estimator,
                    coefficients=model_df,
                    model_obj=hed_mod,
                    log_dep=log_dep,
                    mod_spec=hed_spec,
                    base_price=base_price,
                    periods=attr(hpi_data, 'period_table'),
                    approach='hed')

  # Assign a class
  class(hed_model) <- 'hpimodel'

  # Return Values
  hed_model

}
