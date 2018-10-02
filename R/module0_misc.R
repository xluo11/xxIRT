#' Miscellaneous
#' @name misc
NULL

#' @rdname misc
#' @description \code{cronbach_alpha} computes the Cronbach's alpha
#' @param u the oberved responses, a matrix
#' @examples
#' cronbach_alpha(model_3pl_gendata(1000, 20)$u)
#' cronbach_alpha(model_3pl_gendata(1000, 40)$u)
#' @importFrom stats var
#' @export
cronbach_alpha <- function(u){
  ncol(u) / (ncol(u) - 1) * (1 - sum(apply(u, 2, var, na.rm=T)) / var(rowSums(u, na.rm=T)))
}


#' @rdname misc
#' @description \code{summed_score_dist} computes the distribution of summed scores
#' @param t the ability parameters, a vector
#' @param a the item discrimination parameters, a vector
#' @param b the item difficulty parameters, a vector
#' @param c the item guessing parameters, a vector
#' @export
summed_score_dist <- function(t, a, b, c){
  if(length(b) != length(a) || length(b) != length(c)) 
    stop('incompatible lengths of item parameters:', length(a), length(b), length(c))
  p <- model_3pl_prob(t, a, b, c)
  rs <- 1
  for(i in 1:ncol(p))
    rs <- cbind(rs * (1 - p[,i]), 0) + cbind(0, rs * p[, i])
  rs
}
