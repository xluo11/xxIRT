#' MST Simulation
#' @description \code{mst_sim} runs a MST simulation
#' @param x an assembled mst
#' @param theta.true the true value of theta parameter
#' @importFrom stats runif
#' @import magrittr dplyr
#' @export
mst_sim <- function(x, theta.true, rdp=NULL){
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  if(is.null(x$items)) stop("the mst has not been assembled yet")
  
  # rdp
  if(!is.null(rdp)) {
    if(length(rdp) != x$nstage - 1) stop("invalid routing decision points.")
    rdp <- Map(function(x) c(-Inf, x, Inf), rdp)
  }
  
  # initials
  theta.est <- 0
  used.items <- responses <- NULL
  route <- thetas <- ses <- rep(NA, x$nstage)
  
  panel.items <- mst_get_items(x, panel=sample(1:x$npanel, 1))
  for(i in 1:x$nstage){
    # select next module
    if(i == 1) { # initial stage: randomly select an module in Stage 1
      next.module <- sample(unique(x$route[,i]), 1)
    } else { # later stage: use info or rdp to select a module
      # all connected modules
      next.modules <- sort(unique(x$route[x$route[, i-1] ==  route[i-1], i]))
      if(is.null(rdp)) { # maximum information
        info <- panel.items %>% filter(., stage == i, index %in% next.modules) %>% group_by(index) %>%
          summarise(info=(model_3pl(theta=theta.est, items=.) %>% irt_stats(., "info") %>% sum(.)))
        next.module <- info$index[which.max(info$info)]
      } else { # rdp
        if(length(rdp[[i-1]]) != length(next.modules) + 1) stop("invalid rdp in Stage ", i)
        next.module <- cut(theta.est, breaks=rdp[[i-1]]) %>% `[`(next.modules, .)
      }
    }
    
    # generate responses
    items <- filter(panel.items, stage == i, index == next.module)
    p <- model_3pl(theta=theta.true, items=items) %>% irt_stats(., "prob") %>% as.vector()
    u <- (p >= runif(length(p))) * 1
    
    # append module, items and responses
    route[i] <- next.module
    used.items <- rbind(used.items, items)
    responses <- c(responses, u)
    
    # estimate ability
    theta.est <- estimate_people(responses, used.items, model="3pl", method="mle")$people[1,]
    thetas[i] <- theta.est
    
    # information
    info <- model_3pl(theta=theta.est, items=used.items) %>% irt_stats(., "info") %>% sum()
    se <- 1 / sqrt(info)
    ses[i] <- se
  }
  
  used.items <- data.frame(rsp=responses, used.items)
  out <- list(panel=panel.items, items=used.items, true=theta.true, est=theta.est, se=se, thetas=thetas, ses=ses, route=route)
  class(out) <- "mst.sim"
  return(out)
}
