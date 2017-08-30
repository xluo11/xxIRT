#' MST Simulation
#' @description \code{mst_sim} runs a MST simulation
#' @param x the assembled MST
#' @param true the true theta parameter
#' @param rdp a list of routing decision points
#' @param ... additional option/control parameters
#' @importFrom stats runif
#' @importFrom magrittr %>%
#' @import dplyr
#' @examples 
#' \dontrun{
#' set.seed(10001)
#' pool <- model_3pl()$gendata(1,200)$items
#' pool$content <- sample(1:3, nrow(pool), replace=TRUE)
#' pool$time <- round(rlnorm(nrow(pool), log(60), .2))
#' x <- mst(pool, "1-2", 2, 'topdown', len=20, maxselect=1)
#' x <- mst_obj(x, theta=-1, indices=1)
#' x <- mst_obj(x, theta= 1, indices=2)
#' x <- mst_constraint(x, "content", 10, 10, level=1)
#' x <- mst_stage_length(x, 1, min=5)
#' x <- mst_assemble(x, timeout=10)
#' rdp <- list(stage1=0)
#' mst_sim(x, 1.0, rdp)
#' }
#' @export
mst_sim <- function(x, true, rdp=NULL, ...){
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  if(is.null(x$items)) stop("the mst has not been assembled yet")
  opts <- list(...)
  
  # inits
  panel_items <- mst_get_items(x, panel=ifelse(is.null(opts$panel), sample(1:x$npanel, 1), opts$panel))
  theta <- ifelse(is.null(opts$theta), 0, opts$theta)
  admin <- u <- NULL
  stats <- matrix(nrow=x$nstage, ncol=3, dimnames=list(NULL, c("route", "t", "se")))

  # rdp
  if(!is.null(rdp)) {
    if(length(rdp) != x$nstage - 1) 
      stop("invalid routing decision points.")
    rdp <- lapply(rdp, function(x) {
      data.frame(lower=c(-Inf, x), upper=c(x, Inf))
    }) %>% Reduce(f=rbind) %>% dplyr::mutate(index=2:x$nmodule)
  }
  
  for(i in 1:x$nstage){
    # select next module
    # initial stage: randomly select a module
    # later stage: use max_info or rdp to select a module
    if(i == 1) {
      next_module <- sample(unique(x$route[, i]), 1)
    } else {
      connected_modules <- x$route[x$route[, i-1] == stats[i-1, "route"], i] %>% unique() %>% sort()
      if(is.null(rdp)) {
        info <- irt_stats(model_3pl(theta=theta, items=panel_items), "info")[1,]
        info <- aggregate(info, by=list(panel_items$index), sum) %>% 
          dplyr::rename_(index=~Group.1, info=~x) %>%
          dplyr::filter_(~index %in% connected_modules)
        next_module <- info$index[which.max(info$info)]
      } else {
        next_module <- dplyr::filter_(rdp, ~index %in% connected_modules)
        next_module$lower[1] <- -Inf
        next_module$upper[nrow(next_module)] <- Inf
        next_module <- dplyr::filter_(next_module, ~theta < upper)
        next_module <- min(next_module$index)
      }
    }
    stats[i, "route"] <- next_module
    
    # generate responses
    items <- dplyr::filter_(panel_items, ~stage == i, ~index == next_module)
    admin <- rbind(admin, items)
    p <- irt_stats(model_3pl(theta=true, items=items), "prob")[1,]
    u <- c(u, (p >= runif(length(p))) * 1)
    
    # estimate ability
    theta <- estimate_mle(matrix(u, nrow=1), a=admin$a, b=admin$b, c=admin$c, iter=10)$t
    stats[i, "t"] <- theta
    info <- irt_stats(model_3pl(theta=theta, items=admin), "info")
    stats[i, "se"] <- 1 / sqrt(sum(info))
  }
  
  admin <- cbind(u=u, admin)
  stats <- as.data.frame(stats) %>% 
    dplyr::mutate_(lb = ~t - 1.96 * se, ub = ~t + 1.96 * se)
  stats$n <- sapply(stats$route, function(xx) sum(admin$index == xx))
  stats$pos <- cumsum(stats$n)
  rs <- list(panel=panel_items, admin=admin, stats=stats, true=true, theta=theta)
  class(rs) <- "mst.sim"
  return(rs)
}


#' @rdname mst_sim
#' @export
print.mst.sim <- function(x, ...){
  cat("mst simulation: true=", round(x$true, 2), 
      ", est.=", round(x$theta, 2), ":\n", sep="")
  print(round(x$stats, 2))
  cat("Call x$admin to see administered items ('x' is the mst.sim object name).\n")
}


#' @rdname mst_sim
#' @import ggplot2
#' @export
plot.mst.sim <- function(x, ...) {
  x$admin$position <- seq(nrow(x$admin))
  x$admin$Responses <- factor(x$admin$u, levels=c(0, 1), labels=c('Wrong', 'Right'))
  ggplot(x$admin, aes_string(x="position", y="b")) + 
    geom_point(aes_string(size="a", color="Responses")) +
    geom_pointrange(data=x$stats, aes_string(x="pos", y="t", ymin="lb", ymax="ub"), lty=2, pch=4, col="coral", alpha=.7) +
    xlab("Position") + ylab("Item Difficulty") +
    coord_cartesian(ylim=c(-3.5, 3.5)) +
    scale_size_continuous(range=c(1, 3)) +
    theme_bw() + guides(size=F, fill=F)
}


