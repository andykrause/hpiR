#'
#' Wrapper to estimate model approaches (generic method)
#'
#' Generic method to estimate modeling approaches for indexes
#'
#' @param model_type Type of model to estimate ('rt', 'hed', 'rf')
#' @param hpi_df Dataset created by one of the *CreateTrans() function in this package.
#' @param estimator Type of estimator to be used ('base', 'weighted', 'robust')
#' @param log_dep default TRUE, should the dependent variable (change in price) be logged?
#' @param trim_model default TRUE, should excess be trimmed from model results ('lm' or 'rlm' object)?
#' @param mod_spec Model specification
#' @param ... Additional Arguments
#' @return hpimodel object consisting of:
#' \describe{
#' \item{estimator}{Type of estimator}
#' \item{coefficients}{Data.frame of coefficient}
#' \item{model_obj}{class `rtmodel` or `hedmodel`}
#' \item{mod_spec}{Full model specification}
#' \item{log_dep}{Binary: is the dependent variable in logged format}
#' \item{base_price}{Mean price in the base period}
#' \item{periods}{`data.frame` of periods}
#' \item{approach}{Type of model used}
#' }
#' @importFrom stats as.formula
#' @importFrom stats update
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
#'  # Create model object
#'  hpi_model <- hpiModel(model_type = 'rt',
#'                        hpi_df = rt_data,
#'                        estimator = 'base',
#'                        log_dep = TRUE)
#'
#'  # For custom weighted repeat transaction model
#'
#'  hpi_model_wgt <- hpiModel(model_type = 'rt',
#'                            hpi_df = rt_data,
#'                            estimator = 'weighted',
#'                            weights = runif(nrow(rt_data), 0, 1))
#'
#'
#' @export

hpiModel <- function(model_type,
                     hpi_df,
                     estimator='base',
                     log_dep=TRUE,
                     trim_model=TRUE,
                     mod_spec = NULL,
                     ...){

  if (!'hpidata' %in% class(hpi_df)){
    message('"hpi_df" object must be of class "hpidata"')
    stop()
  }

  model_type <- structure(model_type, class = model_type)
  UseMethod("hpiModel", model_type)
}

#'
#' Specific method for hpi modeling (rt approach)
#'
#' Estimate hpi models with rt approach
#'
#' @method hpiModel rt
#' @param model_type Type of model to estimate ('rt', 'hed', 'rf')
#' @param hpi_df Dataset created by one of the *CreateTrans() function in this package.
#' @param estimator Type of estimator to be used ('base', 'weighted', 'robust')
#' @param log_dep default TRUE, should the dependent variable (change in price) be logged?
#' @param trim_model default TRUE, should excess be trimmed from model results ('lm' or 'rlm' object)?
#' @param mod_spec Model specification
#' @param ... Additional Arguments
#' @return hpimodel object consisting of:
#' \describe{
#' \item{estimator}{Type of estimator}
#' \item{coefficients}{Data.frame of coefficient}
#' \item{model_obj}{class `rtmodel` or `hedmodel`}
#' \item{mod_spec}{Full model specification}
#' \item{log_dep}{Binary: is the dependent variable in logged format}
#' \item{base_price}{Mean price in the base period}
#' \item{periods}{`data.frame` of periods}
#' \item{approach}{Type of model used}
#' }
#' @export

hpiModel.rt <- function(model_type,
                        hpi_df,
                        estimator='base',
                        log_dep=TRUE,
                        trim_model=TRUE,
                        mod_spec = NULL,
                        ...){

  if (!'rtdata' %in% class(hpi_df)) stop('"rt" models require "rtdata" objecct.
                                         Use "rtCreateTrans()"')

  # Create time matrix
  time_matrix <- rtTimeMatrix(hpi_df)

  # Calculate price differential
  if (log_dep){
    price_diff <- log(hpi_df$price_2) - log(hpi_df$price_1)
  } else {
    price_diff <- hpi_df$price_2 - hpi_df$price_1
  }

  # If any NA, NaN, or Inf/-Inf
  if (any(!is.finite(price_diff))){
    message('NA, negative, zero or non-finite values in the price field')
    stop()
  }

  # Extract base period mean price
  base_price <- mean(hpi_df$price_1[hpi_df$period_1 == min(hpi_df$period_1)])

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
  rt_mod <- rtModel(rt_df=hpi_df,
                    time_matrix=time_matrix,
                    price_diff=price_diff,
                    estimator=estimator,
                    ...)

  # Check for successful model estimation
  if (class(rt_mod) != 'rtmodel'){
    message('Model estimator was unsuccessful')
    stop()
  }

  # Remove qr to keep model obj small
  if (trim_model) rt_mod$qr <- NULL

  # If successful create list of results

  # Create coefficient data.frame
  model_df <- data.frame(time=c(min(hpi_df$period_1),
                                as.numeric(gsub('time_matrixtime_', '',
                                                names(rt_mod$coefficients)))),
                         coefficient=c(0, as.numeric(rt_mod$coefficients)),
                         stringsAsFactors=FALSE)

  # Combine into list with class 'hpimodel and return
  structure(list(estimator=estimator,
                 coefficients=model_df,
                 model_obj=rt_mod,
                 mod_spec=NULL,
                 log_dep=log_dep,
                 base_price=base_price,
                 periods=attr(hpi_df, 'period_table'),
                 approach='rt'),
            class='hpimodel')
}

#'
#' Specific method for hpi modeling (hed) approach)
#'
#' Estimate hpi models with hed approach
#'
#' @method hpiModel hed
#' @param model_type Type of model to estimate ('rt', 'hed', 'rf')
#' @param hpi_df Dataset created by one of the *CreateSales() function in this package.
#' @param estimator Type of estimator to be used ('base', 'weighted', 'robust')
#' @param log_dep default=TRUE; should the dependent variable (change in price) be logged?
#' @param trim_model default TRUE, should excess be trimmed from model results ('lm' or 'rlm' object)?
#' @param mod_spec default=NULL; hedonic model specification
#' @param dep_var default=NULL; dependent variable of the model
#' @param ind_var default=NULL; independent variable(s) of the model
#' @param ... Additional Arguments
#' @return hpimodel object consisting of:
#' \describe{
#' \item{estimator}{Type of estimator}
#' \item{coefficients}{Data.frame of coefficient}
#' \item{model_obj}{class `rtmodel` or `hedmodel`}
#' \item{mod_spec}{Full model specification}
#' \item{log_dep}{Binary: is the dependent variable in logged format}
#' \item{base_price}{Mean price in the base period}
#' \item{periods}{`data.frame` of periods}
#' \item{approach}{Type of model used}
#' }
#' @importFrom stats update
#' @export

hpiModel.hed <- function(model_type,
                         hpi_df,
                         estimator='base',
                         log_dep=TRUE,
                         trim_model=TRUE,
                         mod_spec=NULL,
                         dep_var=NULL,
                         ind_var=NULL,
                         ...){

  # Create specification
  if (!is.null(mod_spec)){
    if (class(mod_spec) != 'formula'){
      message('"mod_spec" argument must be of class "formula"')
      stop()
    } else {
      mod_spec <- stats::update(mod_spec, ~ . +as.factor(trans_period))
    }
  } else {

    if (is.null(dep_var) | is.null(ind_var)){
      message('"dep_var" and "ind_var" must be supplied')
      stop()
    }

    if(log_dep){
      dep_var <- paste0('log(', dep_var, ')')
    }
    mod_spec <- stats::as.formula(paste0(dep_var, ' ~ ', paste(ind_var, collapse="+"),
                           '+ as.factor(trans_period)'))
  }

  # Extract base period mean price
  base_price <- mean(hpi_df$price[hpi_df$trans_period == min(hpi_df$trans_period)])

  ## Estimate Model

  # Check for legal estimator type
   if(!estimator %in% c('base', 'robust', 'weighted')){
     message('Provided estimator type is not supported. Allowed estimators are:',
             '"base", "robust" or "weighted".  Defaulting to "base"')
    estimator <- 'base'
   }

  # Check log dep vs data
  if ((log_dep && any(hpi_df$price <= 0)) |
       any(is.na(hpi_df$price)) |
        any(!is.finite(hpi_df$price))){
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
                       hed_df=hpi_df,
                       hed_spec = mod_spec,
                       ...)

  # Check for successful model estimation
  if(class(hed_mod) != 'hedmodel'){

    message('Model estimator was unsuccessful')
    stop()
  }

   # Remove qr to keep model obj small
   if (trim_model) hed_mod$qr <- NULL

  # If successful create list of results
  base_period <- min(hpi_df$trans_period)

  # Period names
  p_names <- grep('trans_period', names(hed_mod$coefficients))
  periods <- c(base_period,
               as.numeric(gsub('[as.factor(trans_period)]', '',
                               names(hed_mod$coefficients)[p_names])))

  # Coefficients
  coefs <- c(0, as.numeric(hed_mod$coefficients)[p_names])

  model_df <- data.frame(time=periods,
                         coefficient=coefs,
                         stringsAsFactors=FALSE)

  # Combine into list
  hed_model <- list(estimator=estimator,
                    coefficients=model_df,
                    model_obj=hed_mod,
                    log_dep=log_dep,
                    mod_spec=mod_spec,
                    base_price=base_price,
                    periods=attr(hpi_df, 'period_table'),
                    approach='hed')

  # Assign a class
  class(hed_model) <- 'hpimodel'

  # Return Values
  hed_model

}

#'
#' Specific method for hpi modeling (hed) approach)
#'
#' Estimate hpi models with hed approach
#'
#' @method hpiModel rf
#' @param model_type Type of model ('rt', 'hed', 'rf')
#' @param hpi_df Dataset created by one of the *CreateSales() function in this package.
#' @param estimator Type of estimator to be used ('base', 'weighted', 'robust')
#' @param log_dep default=TRUE; should the dependent variable (change in price) be logged?
#' @param trim_model default TRUE, should excess be trimmed from model results ('lm' or 'rlm' object)?
#' @param mod_spec default=NULL; hedonic model specification
#' @param dep_var default=NULL; dependent variable of the model
#' @param ind_var default=NULL; independent variable(s) of the model
#' @param ... Additional Arguments
#' @return hpimodel object consisting of:
#' \describe{
#' \item{estimator}{Type of estimator}
#' \item{coefficients}{Data.frame of coefficient}
#' \item{model_obj}{class `rtmodel` or `hedmodel`}
#' \item{mod_spec}{Full model specification}
#' \item{log_dep}{Binary: is the dependent variable in logged format}
#' \item{base_price}{Mean price in the base period}
#' \item{periods}{`data.frame` of periods}
#' \item{approach}{Type of model used}
#' }
#' @importFrom stats update
#' @export

hpiModel.rf <- function(model_type,
                        hpi_df,
                        estimator='pdp',
                        log_dep=TRUE,
                        trim_model=TRUE,
                        mod_spec=NULL,
                        dep_var=NULL,
                        ind_var=NULL,
                        ...){

  # Create specification
  if (!is.null(mod_spec)){
    if (class(mod_spec) != 'formula'){
      message('"mod_spec" argument must be of class "formula"')
      stop()
    } else {
      mod_spec <- stats::update(mod_spec, ~ . + trans_period)
    }
  } else {

    if (is.null(dep_var) | is.null(ind_var)){
      message('"dep_var" and "ind_var" must be supplied')
      stop()
    }

    if(log_dep){
      dep_var <- paste0('log(', dep_var, ')')
    }
    mod_spec <- stats::as.formula(paste0(dep_var, ' ~ ', paste(ind_var, collapse="+"),
                                         '+ trans_period'))
  }

  # Extract base period mean price
  base_price <- mean(hpi_df$price[hpi_df$trans_period == min(hpi_df$trans_period)])

  ## Estimate Model

  # Check for legal estimator type
  if(!estimator %in% c('pdp')){
    message('Provided estimator type is not supported. Allowed estimators are:',
            '"pdp".  Defaulting to "pdp"')
    estimator <- 'pdp'
  }

  # Check log dep vs data
  if ((log_dep && any(hpi_df$price <= 0)) |
      any(is.na(hpi_df$price)) |
      any(!is.finite(hpi_df$price))){
    message('Your "price" field includes invalid values')
    stop()
  }

  # Set estimator class, call method
  class(estimator) <- estimator

  rf_mod <- rfModel(estimator = estimator,
                    rf_df = hpi_df,
                    rf_spec = mod_spec,
                    ind_var = ind_var,
                    dep_var = dep_var,
                    ...)

  # Check for successful model estimation
  if (!'rfmodel' %in% class(rf_mod)){

    message('Model estimator was unsuccessful')
    stop()
  }

  # Remove qr to keep model obj small
  if (trim_model) rf_mod$forest <- NULL

  # If successful create list of results
  base_period <- min(hpi_df$trans_period)

  # Period names
  model_df <- data.frame(time = rf_mod$coefficients$time,
                         coefficient = rf_mod$coefficients$coefficient,
                         stringsAsFactors = FALSE)

  # Combine into list
  rf_model <- list(estimator=estimator,
                    coefficients=model_df,
                    model_obj=rf_mod,
                    log_dep=log_dep,
                    mod_spec=mod_spec,
                    base_price=base_price,
                    periods=attr(hpi_df, 'period_table'),
                    approach='rf')

  # Assign a class
  class(rf_model) <- 'hpimodel'

  # Return Values
  rf_model
}
