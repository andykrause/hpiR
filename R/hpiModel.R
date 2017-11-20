#' @title hpiModel
#' @description Estimate the model for any method of house price index.  Generic method.
#' @usage Lorem Ipsum...
#' @param hpi_data Dataset created by one of the *CreateSales() function in this package.
#' @param estimator Type of estimator to be used ('base', 'weighted', 'robust')
#' @param log_dep default TRUE, should the dependent variable (change in price) be logged?
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

hpiModel <- function(hpi_data,
                     estimator='base',
                     log_dep=TRUE,
                     ...){

  UseMethod("hpiModel")

}

hpiModel.rs <- function(hpi_data,
                        estimator='base',
                        log_dep=TRUE,
                        ...){


  # Create time matrix
  time_matrix <- rsTimeMatrix(hpi_data)

  # Calculate price differential
  if(log_dep){
    price_diff <- log(hpi_data$price_2) - log(hpi_data$price_1)
  } else {
    price_diff <- hpi_data$price_2 - hpi_data$price_1
  }

  # Extract base period mean price
  base_price <- mean(hpi_data$price_1[hpi_data$date_1 == min(hpi_data$date_1)])

  ## Estimate Model

  # Check for legal estimator type
  if(!estimator %in% c('base', 'robust', 'weighted')){
    message('Provided estimator type is not supported. Allowed estimators are:',
            '"base", "robust" or "weighted".  Defaulting to "base"')
    estimator <- 'base'
  }

  # Set estimator class, call method
  class(estimator) <- estimator
  rs_mod <- rsModel(estimator=estimator,
                    rs_df=hpi_data,
                    time_matrix=time_matrix,
                    price_diff=price_diff,
                    ...)

  # Check for successful model estimation
  if(class(rs_mod) != 'rsmod'){

    message('Model estimator was unsuccessful')
    stop()
  }

  # If successful create list of results
  base_period <- min(hpi_data$date_1)
  periods <- c(base_period,
               as.numeric(gsub('time_matrixtime_', '', names(rs_mod$coefficients))))
  coefs <- c(0, as.numeric(rs_mod$coefficients))
  model_df <- data.frame(time=periods,
                         coefficient=coefs)

  # Combine into list
  rs_model <- list(estimator=estimator,
                   coefficients=model_df,
                   model_obj=rs_mod,
                   log_dep=log_dep,
                   base_price=base_price,
                   approach='rs')

  # Assign a class
  class(rs_model) <- 'hpimodel'

  # Return Values
  rs_model

}
