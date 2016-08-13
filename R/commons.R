#' IRT Object
#' @description \code{irt} creates an IRT object with given values
#' @param theta a vector of ability parameters
#' @param a a vector/number of a parameters (item discrimination)
#' @param b a vector of b parameters (item difficulty)
#' @param c a vector/number of c parameters (item pseudo-guesing)
#' @param rsp a response matrix (optional)
#' @return \code{irt} an \code{irt} object
#' @details
#' An \code{irt} object contains thetas and item parameters as well as an optional repsonse matrix. 
#' D=1.7 by default. set a=.588 for Rasch items.
#' @examples
#' # create an IRT object
#' irt(rnorm(10), a=c(.8, 1.0, 1.2), b=c(-.5, 0, .5), c=0)
#' @family commons
#' @export
irt <- function(theta, a, b, c, rsp=NULL){
  # validation
  theta <- as.vector(theta)
  b <- as.vector(b)
  if(length(a) == 1) a <- rep(a, length(b)) else a <- as.vector(a)
  if(length(c) == 1) c <- rep(c, length(b)) else c <- as.vector(c)
  if(length(a) != length(b) || length(c) != length(b)) stop("parameters have different lengths.")
  if(any(a < 0)) stop("don't know how to handle negative a parameters.")
  if(any(c < 0)) stop("don't know how to handle negative c parameters.")
  if(any(abs(b) > 5.0)) warning("CAUTION: some b parameters are too large.")
  if(any(abs(theta) > 5.0)) warning("CAUTION: some theta are too large.")
  if(!is.null(rsp))
    if(dim(rsp)[1] != length(theta) || dim(rsp)[2] != length(b)) stop("rsp dimension is different from people or items.")
  # output
  rs <- list()
  rs$thetas <- theta
  rs$items <- data.frame(a=a, b=b, c=c, stringsAsFactors=FALSE)
  if(!is.null(rsp)) rs$rsp <- as.matrix(rsp)
  class(rs) <- "irt"
  return(rs)
}

#' Generate IRT Object
#' @description Create an IRT object with generated values
#' @param n.peo the number of people to be generated
#' @param n.item the number of items to be generated
#' @param thetas a vector of ability parameters to be imported
#' @param a a vector/number of a parameters to be imported
#' @param b a vector of b parameters to be imported
#' @param c a vector/number of item c parameters to be imported
#' @param theta.mu the mean of the normal distribution from which thetas are generated
#' @param theta.sig the sd of the normal distribution from which thetas are generated
#' @param a.mu the log mean of the lognormal distribution from which a parameters are generated
#' @param a.sig the log sd of the lognormal distribution from which a parameters are generated
#' @param b.mu the mean of the normal distribution from which b parameters are generated
#' @param b.sig the sd of the normal distribution from which b parameters are generated
#' @param c.min the minumum of the uniform distribution from which c parameters are generated
#' @param c.max the maximum of the uniform distribution from which c parameters are generated
#' @return an \code{irt} object
#' @details
#' In generation, thetas are drawn from the normal distribution, a parameters from the lognormal distribution,
#' b parameters from the normal distribution, and c parameters from the uniform distribution.
#' When parameters are imported, they are not drawn from the distribution. 
#' @examples
#' gen.irt(10, 5) # generate 10 people, 5 items
#' gen.irt(10, 5, thetas=seq(.1, 1, length.out=10)) # generate data with given thetas
#' gen.irt(10, 5, a=.588, c=0)  # generate data with given iteme parameters (Rasch items)
#' @family commons
#' @export
#' @importFrom stats runif rnorm
gen.irt <- function(n.peo, n.item, thetas=NULL, a=NULL, b=NULL, c=NULL, theta.mu=0, theta.sig=1, a.mu=0, a.sig=0.15, b.mu=0, b.sig=1, c.min=0, c.max=0.15){
  if(is.null(thetas)) thetas <- rnorm(n.peo, theta.mu, theta.sig)
  if(is.null(a)) a <- exp(rnorm(n.item, a.mu, a.sig))
  if(is.null(b)) b <- rnorm(n.item, b.mu, b.sig)
  if(is.null(c)) c <- runif(n.item, c.min, c.max)
  return(irt(thetas, a, b, c))
}

#' @rdname irt
#' @description \code{gen.rsp} generates binary/dichotomous responses for a given IRT object
#' @examples
#' gen.rsp(gen.irt(10, 5)) # generate responses
#' @family commons
#' @export
#' @importFrom stats runif
gen.rsp <- function(x){
  if(class(x) != "irt") stop("put an irt object in the 1st argument.")
  n.peo <- length(x$thetas)
  n.item <- nrow(x$items)
  p <- prob(x)
  r <- matrix(runif(n.peo * n.item), nrow=n.peo)
  u <- (p >= r) * 1
  x$rsp <- u
  return(x)
}

#' @rdname irt
#' @description \code{plot} plots probability/information fuctions for items in an IRT object
#' @param x an IRT object
#' @param ... other arguments, e.g., type, total, items
#' @return \code{plot} returns a ggplot2 object
#' @examples
#' plot(gen.irt(10, 5)) # plot TCC
#' plot(gen.irt(10, 5), total=FALSE) # plot ICCs
#' plot(gen.irt(10, 5), type="information") # plot TIF
#' plot(gen.irt(10, 5), type="information", total=FALSE) # plot IIFs
#' @family commons
#' @export
#' @import ggplot2
#' @importFrom reshape2 melt
plot.irt <- function(x, ...){
  opts <- list(...)
  type <- ifelse(is.null(opts$type), "probability", opts$type)
  total <- ifelse(is.null(opts$total), TRUE, opts$total) 
  if(!is.null(opts$items)) x$items <- x$items[opts$items, ]
  x$thetas <- round(seq(-3, 3, .1), 1)
  if(tolower(type) == "probability")
    rs <- prob(x)
  else if(tolower(type) == "information")
    rs <- info(x)
  else
    stop("don't recognize the type of plot.")
  colnames(rs) <- paste("Item", 1:ncol(rs))
  if(total)
    rs <- data.frame(theta=x$thetas, value=apply(rs, 1, sum), variable="Total")
  else
    rs <-  melt(data.frame(theta=x$thetas, rs), id.var="theta")
  ggplot(data=rs, aes_string(x="theta", y="value", color="variable")) + geom_line() +
    xlab(expression(theta)) + ylab(expression(paste("P(",theta,") / I(",theta,")"))) + 
    guides(color=guide_legend("")) + theme_bw() + theme(legend.key = element_blank())
}

#' Common Computations
#' @description \code{prob} computes probability for a gve IRT object.
#' @param x an IRT object 
#' @param summary the direction of summarizing results: 1 by row (over items) and 2 by column (over people)
#' @param fun the summarizing function
#' @return a matrix or vector (when summarized) results
#' @examples
#' # probabilities
#' prob(gen.irt(10, 5))
#' # product of probabilities
#' prob(gen.irt(10, 5), summary=1, fun=prod) 
#' @family commons
#' @export
prob <- function(x, summary=NULL, fun=NULL){
  if(is.null(summary) != is.null(fun)) stop("summary and fun parameters are not both on/off.")
  if(class(x) != "irt") stop("put an irt object in the 1st argument.")
  theta <- x$thetas
  a <- x$items$a
  b <- x$items$b
  c <- x$items$c
  rs <- c + (1 - c) / (1 + exp(1.7 * a * outer(b, theta, "-")))
  rs <- t(rs)
  if(!is.null(summary)) rs <- apply(rs, summary, fun)
  return(rs)
}

#' @rdname prob
#' @description \code{info} computes information for a gve IRT object.
#' @examples
#' # information
#' info(gen.irt(10, 5)) 
#' # add information over items
#' info(gen.irt(10, 5), summary=1, fun=sum) 
#' # add information over people
#' info(gen.irt(10, 5), summary=2, fun=sum) 
#' @export
info <- function(x, summary=NULL, fun=NULL){
  if(is.null(summary) != is.null(fun)) stop("summary and fun parameters are not both on/off.")
  if(class(x) != "irt") stop("put an irt object in the 1st argument.")
  n.peo <- length(x$thetas)
  p <- prob(x)
  a <- matrix(rep(x$item$a, n.peo), nrow=n.peo, byrow=T)
  c <- matrix(rep(x$item$c, n.peo), nrow=n.peo, byrow=T)
  rs <- (1.7 * a * (p - c) / (1 - c)) ^ 2 * (1 - p) / p
  if(!is.null(summary)) rs <- apply(rs, summary, fun)
  return(rs)
}

#' @rdname prob
#' @description \code{likelihood} computes likelihood for a gve IRT object.
#' @param log TRUE to take logarithm of results
#' @examples
#' # likelihood of response vectors
#' likelihood(gen.rsp(gen.irt(10, 5)), summary=1, fun=prod)
#' # log-likelihood of response vectors
#' likelihood(gen.rsp(gen.irt(10, 5)), summary=1, fun=sum, log=TRUE)
#' @export
likelihood <- function(x, summary=NULL, fun=NULL, log=FALSE){
  if(is.null(summary) != is.null(fun)) stop("summary and fun parameters are not both on/off.")
  if(class(x) != "irt") stop("put an irt object in the 1st argument.")
  if(is.null(x$rsp)) stop("rsp is not found in the object.")
  if(is.null(x$rsp)) stop("rsp dimension is not aligned with people and items.")
  p <- prob(x)
  u <- x$rsp
  rs <- p^u * (1-p)^(1-u)
  if(log) rs <- log(rs)
  if(!is.null(summary)) rs <- apply(rs, summary, fun)
  return(rs)
}


