#' @title calcKFoldError
#' @description Estimate out-of-sample index errors using a KFold process
#' @usage Lorem Ipsum...
#' @param hpi_obj Object of class 'hpi'
#' @param pred_df Set of sales to be used for predicitive quality of index
#' @param k default=10; Number of folds to apply to holdout process
#' @param seed random seed generator to control the folding process
#' @param ... Additional Arguments
#' @return hpimodel object
#' @section Further Details:
#' Lorem Ipsum...
#' @examples
#' a <- 1
#' @export

calcKFoldError <- function(hpi_obj,
                           pred_df,
                           k=10,
                           seed=1,
                           ...){

  if (!'hpi' %in% class(hpi_obj)){
    message('"hpi_obj" argument must be of class "hpi"')
    stop()
  }

  if (!any('data.frame' %in% class(pred_df)) ||
      !any(class(pred_df) %in% c('rt', 'hed'))){
    message('"pred_df" argument must be a data.frame with additional class of ',
            ' "rt" or "hed"')
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
  k_folds <- caret::createFolds(y=1:nrow(hpi_obj$data),
                                k=k,
                                list=TRUE,
                                returnTrain=FALSE)
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
                        hed_spec=hpi_obj$model$mod_spec,
                        log_dep = hpi_obj$model$log_dep)

  # Create K indexes (just extract index)
  k_index <- purrr::map(.x=k_model,
                        .f=function(x) modelToIndex(x)$index)

  # Iterate through score and calc errors
  k_error <- purrr::map2(.x=k_score,
                         .y=k_index,
                         .f=calcInSampleError)

  # Bind results together and return
  error_df <- dplyr::bind_rows(k_error) %>%
    dplyr::filter(!is.na(prop_id))

  class(error_df) <- unique(c('indexerrors', class(error_df)))
  attr(error_df, 'test_method') <- 'kfold'

  # Return Values
  error_df

}

#' @title createKFoldData
#' @description create the datasets for the kfold error testing (Generic Method)
#' @usage Lorem Ipsum...
#' @param score_ids Vector of row ids to be included in scoring data
#' @param full_data Complete dataset of this model type
#' @param pred_df Data to be used for prediction
#' @return list
#' \item{train} Training data
#' \item{score} Scoring data
#' @section Further Details:
#' Lorem Ipsum...
#' @examples
#' a <- 1
#' @export

createKFoldData <- function(score_ids,
                            full_data,
                            pred_df){
  UseMethod("createKFoldData", pred_df)
}

#' @export
createKFoldData.rt <- function(score_ids,
                               full_data,
                               pred_df){

  train_df <- full_data[-score_ids, ]
  score_df <- matchKFold(train_df,
                         pred_df)
  list(train=train_df,
       score=score_df)
}

#' @title matchKFoldData
#' @description Makes specific selections of scoring data (Generic Method)
#' @usage Lorem Ipsum...
#' @param train_df Data.frame of training data
#' @param pred_df Data to be used for prediction
#' @return list
#' \item{train} Training data
#' \item{score} Scoring data
#' @section Further Details:
#' Lorem Ipsum...
#' @examples
#' a <- 1
#' @export

matchKFold <- function(train_df,
                       pred_df){

  UseMethod("matchKFold")

}

#' @export
matchKFold.rt <- function(train_df,
                          pred_df){

  score_df <- pred_df[!paste0(pred_df$trans_id1, "_", pred_df$trans_id2) %in%
                          paste0(train_df$trans_id1, "_", train_df$trans_id2), ]
  score_df

}

#' @export
matchKFold.hed <- function(train_df,
                           pred_df){

  # Choose every other one
  x1 <- which(!pred_df$trans_id1 %in% train_df$trans_id)[c(T,F)]
  x2 <- which(!pred_df$trans_id2 %in% train_df$trans_id)[c(T,F)]

  pred_df[unique(c(x1, x2)), ]

}
