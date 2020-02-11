#' Calculate index error with FKold (out of sample)
#'
#' Use a KFold (out of sample) approach to estimate index accuracy
#'
#' @param hpi_obj HPI object of class 'hpi'
#' @param pred_df Data.frame of sales to be used for assessing predictive quality of index
#' @param k default=10; Number of folds to apply to holdout process
#' @param seed default=1; Random seed generator to control the folding process
#' @param smooth default = FALSE; Calculate on the smoothed index
#' @param ... Additional Arguments
#' @return object of class `hpiaccuracy` inheriting from class `data.frame` containing the
#' following fields:
#' \describe{
#'   \item{pair_id}{Unique Pair ID}
#'   \item{price}{Transaction Price}
#'   \item{pred_price}{Predicted price}
#'   \item{error}{(Prediction - Actual) / Actual}
#'   \item{log_error}{log(prediction) - log(actual)}
#'   \item{pred_period}{Period of the prediction}
#' }
#' @importFrom purrr map map2
#' @importFrom dplyr bind_rows filter
#' @importFrom magrittr %>%
#' @examples
#'
#'  # Load data
#'  data(ex_sales)
#'
#'  # Create index with raw transaction data
#'  rt_index <- rtIndex(trans_df = ex_sales,
#'                      periodicity = 'monthly',
#'                      min_date = '2010-06-01',
#'                      max_date = '2015-11-30',
#'                      adj_type = 'clip',
#'                      date = 'sale_date',
#'                      price = 'sale_price',
#'                      trans_id = 'sale_id',
#'                      prop_id = 'pinx',
#'                      estimator = 'robust',
#'                      log_dep = TRUE,
#'                      trim_model = TRUE,
#'                      max_period = 48,
#'                      smooth = FALSE)
#'
#'   # Create prediction data
#'   rt_data <- rtCreateTrans(trans_df = ex_sales,
#'                           prop_id = 'pinx',
#'                           trans_id = 'sale_id',
#'                           price = 'sale_price',
#'                           periodicity = 'monthly',
#'                           date = 'sale_date')
#'
#'   # Calc Accuracy
#'   kf_accr <- calcKFoldError(hpi_obj = rt_index,
#'                             pred_df = rt_data,
#'                             k = 10,
#'                             seed = 123,
#'                             smooth = FALSE)
#'
#' @export

calcKFoldError <- function(hpi_obj,
                           pred_df,
                           k = 10,
                           seed = 1,
                           smooth = FALSE,
                           ...){

  if (!'hpi' %in% class(hpi_obj)){
    message('"hpi_obj" argument must be of class "hpi"')
    stop()
  }

  if (!any('data.frame' %in% class(pred_df)) ||
      !any(class(pred_df) %in% c('rtdata', 'heddata'))){
    message('"pred_df" argument must be a data.frame with additional class of ',
            ' "rtdata" or "heddata"')
    stop()
  }

  if (!class(k) %in% c('integer', 'numeric') ||
        k < 2){
    message('Number of folds ("k" argument) must be a positive integer greater than 1')
    stop()
  }

  if (!class(seed) %in% c('integer', 'numeric') ||
       seed < 1){
    message('"seed" must be a positive integer greater than 0')
    stop()
  }

  # Set seed
  set.seed(seed)

  # K-fold the data
  k_folds <- split(x = 1:nrow(hpi_obj$data), f = sample(1:k, nrow(hpi_obj$data), replace = TRUE))

                   # Make train and score
  k_data <- purrr::map(.x=k_folds,
                       .f=createKFoldData,
                       full_data=hpi_obj$data,
                       pred_df=pred_df)

  # Extract training and scoring into their own lists
  k_train <- purrr::map(.x=k_data,
                        .f=function(x) x$train)
  k_score <- purrr::map(.x=k_data,
                        .f=function(x) x$score)

  # Train k models
  k_model <- purrr::map(.x=k_train,
                        .f=hpiModel,
                        mod_spec=hpi_obj$model$mod_spec,
                        log_dep = hpi_obj$model$log_dep,
                        model_type = hpi_obj$model$approach,
                        estimator = hpi_obj$model$estimator,
                        ...)

  # Create K indexes (just extract index)
  k_index <- purrr::map(.x=k_model,
                        .f=modelToIndex)

  # Deal with smoothing
  if (!smooth){
    k_index <- purrr::map(.x = k_index,
                          .f = function(x) x$value)
  } else {
    smooth_order <- 3
    if ('smooth_order' %in% names(list(...))){
      smooth_order <- list(...)$smooth_order
    }
    k_index <- purrr::map(.x = k_index,
                          .f = smoothIndex,
                          order=smooth_order)
  }

  # Iterate through score and calc errors
  k_error <- purrr::map2(.x=k_score,
                         .y=k_index,
                         .f=calcInSampleError)

  # Bind results together and return
  accr_df <- dplyr::bind_rows(k_error) %>%
    dplyr::filter(!is.na(.data$pair_id))

  class(accr_df) <- unique(c('hpiaccuracy', class(accr_df)))
  attr(accr_df, 'test_method') <- 'kfold'

  # Return Values
  accr_df

}

#' Create data for KFold error test
#'
#' Generic method for creating KFold testing data
#'
#' @param score_ids Vector of row ids to be included in scoring data
#' @param full_data Complete dataset (class `hpidata``) of this model type (rt or hed)
#' @param pred_df Data to be used for prediction
#' @return list of length 2 containing:
#' \describe{
#'   \item{train}{Training data.frame}
#'   \item{score}{Scoring data.frame}
#' }
#' @section Further Details:
#' Called from `calcKFoldError()``
#' @examples
#'
#'  # Load Data
#'  data(ex_sales)
#'
#'  # Create RT Data
#'  rt_data <- rtCreateTrans(trans_df = ex_sales,
#'                           prop_id = 'pinx',
#'                           trans_id = 'sale_id',
#'                           price = 'sale_price',
#'                           periodicity = 'monthly',
#'                           date = 'sale_date')
#'  # Create folds
#'  k_folds <- split(x = 1:nrow(rt_data),
#'                   f = sample(1:10, nrow(rt_data), replace = TRUE))
#'
#'  # Create data from folds
#'  kfold_data <- createKFoldData(score_ids = k_folds[[1]],
#'                                full_data = rt_data,
#'                                pred_df = rt_data)
#'
#' @export

createKFoldData <- function(score_ids,
                            full_data,
                            pred_df){
  UseMethod("createKFoldData", pred_df)
}


#' Create data for KFold error test (rt approach)
#'
#' `rtdata` method for creating KFold testing data
#'
#' @method createKFoldData rtdata
#' @inherit createKFoldData params
#' @export

createKFoldData.rtdata <- function(score_ids,
                                   full_data,
                                   pred_df){

  train_df <- full_data[-score_ids, ]
  score_df <- matchKFold(train_df,
                         pred_df)
  list(train=train_df,
       score=score_df)
}

#' Helper function to make KFold data
#'
#' Function to help create KFold data based on approach (Generic Method)
#'
#' @param train_df Data.frame of training data
#' @param pred_df Data.frame (class `hpidata``) to be used for prediction
#' @return list
#' \describe{
#' \item{train}{Training data}
#' \item{score}{Scoring data}
#' }
#' @section Further Details:
#' Helper function called from createKFoldData
#' @export

matchKFold <- function(train_df,
                       pred_df){

  UseMethod("matchKFold")

}

#' Helper function to make KFold data
#'
#' Function to help create KFold data based on rt approach
#'
#' @method matchKFold rtdata
#' @inherit matchKFold params
#' @export

matchKFold.rtdata <- function(train_df,
                              pred_df){

  score_df <- pred_df[!paste0(pred_df$trans_id1, "_", pred_df$trans_id2) %in%
                          paste0(train_df$trans_id1, "_", train_df$trans_id2), ]
  score_df

}

#' Helper function to make KFold data
#'
#' Function to help create KFold data based on hed approach
#'
#' @method matchKFold heddata
#' @inherit matchKFold params
#' @export

matchKFold.heddata <- function(train_df,
                               pred_df){

  # Choose every other one
  x1 <- which(!pred_df$trans_id1 %in% train_df$trans_id)[c(T,F)]
  x2 <- which(!pred_df$trans_id2 %in% train_df$trans_id)[c(T,F)]

  pred_df[unique(c(x1, x2)), ]

}
