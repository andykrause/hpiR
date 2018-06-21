#' @title blendIndexes
#' @description Blend together two or more indexes
#' @usage blendIndexes(index_list, weights)
#' @param index_list A list of identical length indexes (objects of class `hpiindex`)
#' @param weights default=NULL; A vector of weights the same length as the `index_list`
#' @param ... Additional Arguments
#' @return an `hpiblend` (S3) object, inheriting from `hpiindex` (S3) object containing:
#' \item{name: vector of period names}
#' \item{numeric: vector of period absolute values}
#' \item{period: vector of period relative values}
#' \item{index: `ts` object of the blended index values}
#' \item{imputed: list with a vector of imputation for each of parent indexes}
#' \item{blended: logical indicating that this is a blended index}
#' \item{weights: vector of the weights used to blend}
#' \item{parents: list of `ts` objects of the parent indexes}
#' @section Further Details:
#' Leaving "weights" to be NULL results in a 1/n weighting.
#'@examples
#'\dontrun{
#'blend_index <- blendIndexes(index_list = list(rt_index, hed_index),
#'                            weights = c(.6, .4))
#'}
#' @export

blendIndexes <- function(index_list,
                         weights=NULL,
                         ...){

  # Check classes
  cs <- unlist(lapply(index_list, function(x) 'hpiindex' %in% class(x)))
  if (any(!cs)){
    message('All objects in "index_list" must be objects of the class "hpiindex')
    stop()
  }


  # Check if all same length,
  lens <- lapply(index_list, function(x) length(x$index))
  if (length(unique(lens)) > 1){
    message('All indexes must be the same length')
    stop()
  }

  # Compute weights
  if (is.null(weights)){
    weights <- rep(1 / length(index_list), length(index_list))
  } else {
    if (length(weights) != length(index_list)){
      message('Weights must be the same length as the index_list')
      stop()
    }
    if (round(sum(weights), 4) != 1){
      message('Weights must sum to 1')
      stop()
    }
  }

  # Compute contribution
  index_w <- purrr::map2(.x=index_list,
                         .y=weights,
                         .f=function(x, y) x$index * y)

  # Sum and convert to TS
  index_blend <- Reduce('+', index_w)

  # Return Values
  structure(list(name = index_list[[1]]$name,
                 numeric = index_list[[1]]$numeric,
                 period = index_list[[1]]$period,
                 index = index_blend,
                 imputed = lapply(index_list, function(x) x$imputed),
                 blended = TRUE,
                 weights=weights,
                 parents=lapply(index_list, function(x) x$index)),
            class = c('hpiblend', 'hpiindex'))

}
