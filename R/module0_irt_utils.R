#' #' Distribution of Expected Raw Scores
#' @description Calculate the distribution of expected raw scores
#' @param t the ability parameters, 1d vector
#' @param a the item discrimination parameters, 1d vector
#' @param b the item difficulty parameters, 1d vector
#' @param c the item guessing parameters, 1d vector
#' @export
expected_raw_score_dist <- function(t, a, b, c){
  if(length(b) != length(a) || length(b) != length(c)) 
    stop('incompatible dimensions for item parameters:', length(a), length(b), length(c))
  p <- model_3pl_prob(t, a, b, c)
  rs <- 1
  for(i in 1:ncol(p))
    rs <- cbind(rs * (1 - p[,i]), 0) + cbind(0, rs * p[, i])
  rs
}


