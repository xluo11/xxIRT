#' CAT Helper Functions
#' @name catHelpers
NULL


#' @rdname catHelpers
#' @description \code{cat_select_randomesque} selects an item from k most informative ones
#' @param theta the current theta estimate
#' @param pool the item pool
#' @param randomesque the randomesque parameter
cat_select_randomesque <- function(theta, pool, randomesque){
  information <- irt_stats(irt_model("3pl", theta=theta, items=pool), "info")[1,]
  randomesque <- ifelse(is.null(randomesque), 1, randomesque)
  randomesque <- min(randomesque, length(information))
  index <- order(information, decreasing=TRUE)[1:randomesque]
  if(length(index) > 1) index <- sample(index, 1)
  return(index)
}
