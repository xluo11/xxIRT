#' Simulation of Multistage Testing
#' @name mst_sim
#' @examples
#' \dontrun{
#' ## assemble a MST
#' nitems <- 200
#' pool <- with(model_3pl_gendata(1, nitems), data.frame(a=a, b=b, c=c))
#' pool$content <- sample(1:3, nrow(pool), replace=TRUE)
#' x <- mst(pool, "1-2-2", 2, 'topdown', len=20, max_use=1)
#' x <- mst_obj(x, theta=-1, indices=1)
#' x <- mst_obj(x, theta=0, indices=2:3)
#' x <- mst_obj(x, theta=1, indices=4)
#' x <- mst_constraint(x, "content", 6, 6, level=1)
#' x <- mst_constraint(x, "content", 6, 6, level=2)
#' x <- mst_constraint(x, "content", 8, 8, level=3)
#' x <- mst_stage_length(x, 1:2, min=5)
#' x <- mst_assemble(x)
#' 
#' ## ex. 1: administer the MST using fixed RDP for routing
#' x_sim <- mst_sim(x, .5, list(stage1=0, stage2=0))
#' plot(x_sim)
#' 
#' ## ex. 2: administer the MST using the max. info. for routing
#' x_sim <- mst_sim(x, .5)
#' plot(x_sim, ylim=c(-5, 5))
#' }
NULL

#' @description \code{mst_sim} simulates a MST administration
#' @param x the assembled MST
#' @param true the true theta parameter (numeric)
#' @param rdp routing decision points (list)
#' @param ... additional option/control parameters
#' @importFrom stats runif
#' @export
mst_sim <- function(x, true, rdp=NULL, ...){
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  if(is.null(x$items)) stop("the mst has not been assembled yet")
  opts <- list(...)
  if(is.null(opts$t_prior)) prior <- NULL else prior <- list(t=opts$t_prior)

  # inits
  if(is.null(opts$panel)) opts$panel <- sample(1:x$num_panel, 1)
  panel_items <- mst_get_items(x, panel_ix=opts$panel)
  theta <- ifelse(is.null(opts$theta), 0, opts$theta)
  admin <- NULL
  stats <- matrix(nrow=x$num_stage, ncol=4, dimnames=list(NULL, c("route", "t", "info", "se")))

  # routing decision points
  if(!is.null(rdp)) {
    if(length(rdp) != x$num_stage - 1) stop("invalid routing decision points.")
    rdp <- Reduce(rbind, lapply(rdp, function(x) data.frame(lower=c(-Inf, x), upper=c(x, Inf))))
    rdp$index <- 2:x$num_module
  }
  
  # MST administration
  for(i in 1:x$num_stage){
    # select module
    if(i == 1) {
      next_module <- unique(x$route[, i])
      next_module <- sample(next_module, 1)
    } else {
      next_module <- x$route[x$route[, i-1] == stats[i-1, "route"], i]
      next_module <- sort(unique(next_module))
      if(is.null(rdp)) {
        info <- model_3pl_info(theta, panel_items$a, panel_items$b, panel_items$c)[1, ]
        info <- aggregate(info, by=list(module=panel_items$module), sum)
        info <- info[info$module %in% next_module, ]
        next_module <- info$module[which.max(info$x)]
      } else {
        module_rdp <- subset(rdp, rdp$index %in% next_module)
        module_rdp$lower[1] <- -Inf
        module_rdp$upper[nrow(module_rdp)] <- Inf
        next_module <- min(subset(module_rdp, theta < module_rdp$upper)$index)
      }
    }

    # generate responses
    items <- subset(panel_items, panel_items$stage == i & panel_items$module == next_module)
    rsp <- as.integer(model_3pl_prob(true, items$a, items$b, items$c)[1, ] >= runif(nrow(items)))
    admin <- rbind(admin, cbind(items, rsp=rsp))

    # estimate ability
    theta <- model_3pl_jmle(matrix(rep(admin$rsp, each=2), nrow=2), a=admin$a, b=admin$b, c=admin$c, scale=NULL, priors=prior)$t[1]
    info <- sum(model_3pl_info(theta, admin$a, admin$b, admin$c))
    se <- 1 / sqrt(info)
    stats[i, c('route', 't', 'info', 'se')] <- c(next_module, theta, info, se)
  }
  
  stats <- as.data.frame(stats)
  stats$nitems <- sapply(stats$route, function(xx) sum(admin$module == xx))
  rs <- list(panel=panel_items, admin=admin, stats=stats, true=true, theta=theta)
  class(rs) <- "mst_sim"
  rs
}


#' @rdname mst_sim
#' @export
print.mst_sim <- function(x, ...){
  cat("mst simulation: true=", round(x$true, 2), 
      ", est.=", round(x$theta, 2), ":\n", sep="")
  print(round(x$stats, 2))
  cat("Call x$admin to see administered items ('x' is the mst_sim object).\n")
}


#' @rdname mst_sim
#' @importFrom stats qnorm
#' @import ggplot2
#' @export
plot.mst_sim <- function(x, ...) {
  opts <- list(...)
  if(is.null(opts$ci_width)) opts$ci_width <- qnorm(.975)
  if(is.null(opts$ylim)) opts$ylim <- c(-3, 3)
  x$admin$Position <- seq(nrow(x$admin))
  x$admin$Responses <- factor(x$admin$rsp, levels=c(0, 1), labels=c('Wrong', 'Right'))
  x$stats$lb <- x$stats$t - opts$ci_width * x$stats$se
  x$stats$ub <- x$stats$t + opts$ci_width * x$stats$se
  x$stats$position <- cumsum(x$stats$nitems)
  
  ggplot(x$admin, aes_string(x="Position", y="b")) + 
    geom_point(aes_string(size="a", color="Responses")) +
    geom_pointrange(data=x$stats, aes_string(x="position", y="t", ymin="lb", ymax="ub"), lty=2, pch=4, col="coral") +
    xlab("Position") + ylab("Item Difficulty") + guides(size=F, fill=F) +
    coord_cartesian(ylim=opts$ylim) + scale_size_continuous(range=c(1, 3)) + theme_bw()
}

