#' MST Simulation
#' @description \code{mst_sim} runs a MST simulation
#' @param x an assembled mst
#' @param theta.true the true value of theta parameter
#' @importFrom stats runif
#' @export
mst_sim <- function(x, theta.true){
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  if(is.null(x$items)) stop("the mst has not been assembled yet")
  
  theta.est <- 0
  used.items <- NULL
  responses <- NULL
  route <- rep(NA, x$nstage)

  panel.items <- mst_get_items(x, panel=sample(1:x$npanel, 1))
  for(i in 1:x$nstage){
    # items in the current stage and connetable modules
    if(i == 1) {
      modules <- unique(x$route[,i])
    } else {
      modules <- unique(x$route[x$route[, i-1] ==  route[i-1], i])
    }
    items <- subset(panel.items, panel.items$stage == i & panel.items$index %in% modules)
    
    # select module with maximum information
    info <- irt_stats(irt_model("3pl", theta=theta.est, items=items), "info")[1,]
    info <- aggregate(info, list(items$index), sum)
    nextmodule <- info[which.max(info[, 2]), 1]
    
    # items in the module with greatest information
    items <- subset(items, items$index == nextmodule)
    p <- irt_stats(irt_model("3pl", theta=theta.true, items=items), "prob")[1,]
    u <- (p >= runif(length(p))) * 1
    
    # append module, items and responses
    route[i] <- nextmodule
    used.items <- rbind(used.items, items)
    responses <- c(responses, u)
    
    # estimate ability
    theta.est <- estimate_people(responses, used.items, model="3pl", method="mle")$people[1,]
  }
  
  used.items <- data.frame(responses=responses, used.items)
  out <- list(panel=panel.items, items=used.items, true=theta.true, est=theta.est, route=route)
  class(out) <- "mst.sim"
  return(out)
}
