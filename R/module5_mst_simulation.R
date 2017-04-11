#' MST Simulation
#' @description \code{mst_sim} runs a MST simulation
#' @param x an assembled mst
#' @param theta.true the true value of theta parameter
#' @param rdp a list of routing decision points
#' @importFrom stats runif
#' @export
mst_sim <- function(x, theta.true, rdp=NULL){
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  if(is.null(x$items)) stop("the mst has not been assembled yet")
  
  # select a panel
  panel.items <- mst_get_items(x, panel=sample(1:x$npanel, 1))
  
  # initials
  theta.est <- 0
  used.items <- responses <- NULL
  route <- thetas <- ses <- rep(NA, x$nstage)
  
  # rdp
  if(!is.null(rdp)) {
    if(length(rdp) != x$nstage - 1) stop("invalid routing decision points.")
    rdp <- lapply(rdp, function(x) data.frame(lower=c(-Inf, x), upper=c(x, Inf)))
    rdp <- Reduce(rbind, rdp)
    rdp$index <- 2:x$nmodule
  }
  
  for(i in 1:x$nstage){
    # select next module
    if(i == 1) { # initial stage: randomly select an module in Stage 1
      next.module <- sample(unique(x$route[,i]), 1)
    } else { # later stage: use info or rdp to select a module
      # all connected modules
      next.modules <- sort(unique(x$route[x$route[, i-1] ==  route[i-1], i]))
      if(is.null(rdp)) { # maximum information
        items <- with(panel.items, subset(panel.items, stage == i & index %in% next.modules))
        info <- irt_stats(model_3pl(theta=theta.est, items=items), "info")[1,]
        info <- aggregate(info, list(items$index), sum)
        colnames(info) <- c("index", "info")
        next.module <- info$index[which.max(info$info)]
      } else { # rdp
        next.module <- subset(rdp, rdp$index %in% next.modules & theta.est < rdp$upper)
        if(nrow(next.module) != 0) {
          next.module <- min(next.module$index)
        } else {
          next.module <- subset(rdp, rdp$index %in% next.modules & theta.est > rdp$lower)
          next.module <- max(next.module$index)
        }
      }
    }
    
    # generate responses
    items <- with(panel.items, subset(panel.items, stage == i & index == next.module))
    p <- irt_stats(model_3pl(theta=theta.true, items=items), "prob")[1,]
    u <- (p >= runif(length(p))) * 1
    
    # append module, items and responses
    route[i] <- next.module
    used.items <- rbind(used.items, items)
    responses <- c(responses, u)
    
    # estimate ability
    theta.est <- estimate_people(responses, used.items, model="3pl", method="mle")$people[1,]
    thetas[i] <- theta.est
    
    # information
    info <- irt_stats(model_3pl(theta=theta.est, items=used.items), "info")
    info <- sum(info)
    se <- 1 / sqrt(info)
    ses[i] <- se
  }
  
  used.items <- data.frame(rsp=responses, used.items)
  out <- list(panel=panel.items, items=used.items, true=theta.true, est=theta.est, se=se, thetas=thetas, ses=ses, route=route)
  class(out) <- "mst.sim"
  return(out)
}
