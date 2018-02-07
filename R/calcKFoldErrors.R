#' @title calcKFoldErrors
#' @description Estimate out-of-sample index errors using a KFold process
#' @usage Lorem Ipsum...
#' @param hpi_obj Object of class 'hpi'
#' @param pred_data Set of sales to be used for predicitive quality of index
#' @param k default=10; Number of folds to apply to holdout process
#' @param seed random seed generator to control the folding process
#' @param ... Additional Arguments
#' @return hpimodel object
#' @section Further Details:
#' Lorem Ipsum...
#' @examples
#' a <- 1
#' @export

calcKFoldErrors <- function(hpi_obj,
                            pred_data,
                            k=10,
                            seed=1){

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
                       pred_data=pred_data)

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
                         .f=calcHPIError)

  # Bind results together and return
  bind_rows(k_error)

}

#' @title createKFoldData
#' @description create the datasets for the kfold error testing (Generic Method)
#' @usage Lorem Ipsum...
#' @param score_ids Vector of row ids to be included in scoring data
#' @param full_data Complete dataset of this model type
#' @param pred_data Data to be used for prediction
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
                            pred_data){
  UseMethod("createKFoldData", pred_data)
}

#' @export
createKFoldData.rs <- function(score_ids,
                               full_data,
                               pred_data){

  train_df <- full_data[-score_ids, ]
  score_df <- matchKFold(train_df,
                         pred_data)
  list(train=train_df,
       score=score_df)
}

#' @title matchKFoldData
#' @description Makes specific selections of scoring data (Generic Method)
#' @usage Lorem Ipsum...
#' @param train_df Data.frame of training data
#' @param pred_data Data to be used for prediction
#' @return list
#' \item{train} Training data
#' \item{score} Scoring data
#' @section Further Details:
#' Lorem Ipsum...
#' @examples
#' a <- 1
#' @export

matchKFold <- function(train_df,
                       pred_data){

  UseMethod("matchKFold")

}

#' @export
matchKFold.rs <- function(train_df,
                          pred_data){

  score_df <- pred_data[!paste0(pred_data$sale_id1, "_", pred_data$sale_id2) %in%
                          paste0(train_df$sale_id1, "_", train_df$sale_id2), ]
  score_df

}

#' @export
matchKFold.hed <- function(train_df,
                           pred_data){

  x1 <- which(!pred_data$sale_id1 %in% train_df$sale_id)[c(T,F)]
  x2 <- which(!pred_data$sale_id2 %in% train_df$sale_id)[c(T,F)]

  pred_data[c(x1, x2), ]

}
