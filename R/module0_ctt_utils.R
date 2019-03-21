#' Cronbach's alpha
#' @description \code{cronbach_alpha} computes Cronbach's alpha internal consistency reliability
#' @param responses the oberved responses, 2d matrix
#' @examples
#' cronbach_alpha(model_3pl_gendata(1000, 20)$u)
#' @importFrom stats var
#' @export
cronbach_alpha <- function(responses){
  k <- ncol(responses)
  total_var <- var(rowSums(responses, na.rm=T))
  item_var <- sum(apply(responses, 2, var, na.rm=T))
  k / (k - 1) * (1 - item_var / total_var)
}

#' Spearman Brown Prophecy 
#' @description Use Spearman-brown formula to compute the predicted reliability
#' when the test length is extened to n-fold or reversely the n-fold extension of
#' test length in order to reach the targeted reliability
#' @param n extend the test length to n-fold
#' @param rho the reliability of current test
#' @examples
#' spearman_brown(2, .70)
#' @export
spearman_brown <- function(n, rho){
  n * rho / (1 + (n - 1) * rho)
}

#' @rdname spearman_brown
#' @param target the targeted reliability
#' @examples 
#' spearman_brown_reverse(.70, .85)
#' @export
spearman_brown_reverse <- function(rho, target){
  target * (1 - rho) / rho / (1 - target)
}






