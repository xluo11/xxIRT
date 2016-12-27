#' Utility Functions
#' @description \code{rmse} computes the root mean squared error (rsme) of two numeric vectors/matrices
#' @param x a vector/matrix of numeric values
#' @param y a vector/matrix of numeric values
#' @examples
#' rmse(1:10 + rnorm(10), 1:10 + rnorm(10))
#' rmse(matrix(1:100 + rnorm(100), ncol=5), matrix(1:100 + rnorm(100), ncol=5))
#' @export
rmse <- function(x, y){
  x <- as.matrix(x)
  y <- as.matrix(y)
  if(any(dim(x) != dim(y))) stop("x and y have different dimensions.")
  sqrt(colMeans((x - y)^2))
}

#' @rdname rmse
#' @description \code{freq} counts the frequency using user-defined values
#' @param values a vector of valid counting values
#' @examples
#' freq(sample(1:4, 100, replace=TRUE), 5:1)
#' freq(sample(1:4, 100, replace=TRUE), 1:5)
#' @family utility
#' @export
freq <- function(x, values){
  x <- as.vector(unlist(x))
  rs <- data.frame(table(x))
  rs <- rs$Freq[match(values, rs$x)]
  rs[is.na(rs)] <- 0
  rs <- data.frame(x=values, n=rs, p=rs/sum(rs))
  return(rs)
}
