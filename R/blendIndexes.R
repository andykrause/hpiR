#' @title blendIndexes
#' @description Blend together two or more indexes
#' @usage Lorem Ipsum...
#' @param index_list A list of identical length indexes
#' @param weights default=NULL; A vector of weights
#' @param ... Additional Arguments
#' @return a ts object with blended index
#' @section Further Details:
#' Leaving weights to be NULL results in a 1/n weighting.
#' @examples
#' a <- 1
#' @export

blendIndexes <- function(index_list,
                         weights=NULL,
                         ...){

  # Check if all same length,
  lens <- lapply(index_list, length)
  if (length(unique(lens)) > 1){
    message('All indexes must be the same length')
    return(NULL)
  }

  # Compute weights
  if (is.null(weights)){
    weights <- rep(1 / length(index_list), length(index_list))
  } else {
    if (length(weights) != length(index_list)){
      message('Weights must be the same length as the index_list')
      return(NULL)
    }
    if (round(sum(weights), 4) != 1){
      mesage('Weights must sum to 1')
      return(NULL)
    }

  }
  # Compute contribution
  index_w <- purrr::map2(.x=index_list,
                         .y=weights,
                         .f=function(x,y) x*y)

  # Sum and convert to TS
  index_blend <- Reduce('+', index_w)

  attr(index_blend, 'class') <- c('indexblend', class(index_blend))
  attr(index_blend, 'ancestry') <- list(weights=weights,
                                      parents=index_list)
  # Return Values
  index_blend

}
