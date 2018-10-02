#' Simulation of Computerized Adaptive Testing (CAT)
#' @name cat_sim
#' @examples
#' \dontrun{
#' ## generate a 100-item pool
#' num_items <- 100
#' pool <- with(model_3pl_gendata(1, num_items), data.frame(a=a, b=b, c=c))
#' pool$set_id <- sample(1:30, num_items, replace=TRUE)
#' pool$content <- sample(1:3, num_items, replace=TRUE)
#' pool$time <- round(rlnorm(num_items, mean=4.1, sd=.2))
#' 
#' ## MLE, EAP, and hybrid estimation rule
#' cat_sim(1.0, pool, min=10, max=20, estimate_rule=cat_estimate_mle)
#' cat_sim(1.0, pool, min=10, max=20, estimate_rule=cat_estimate_eap)
#' cat_sim(1.0, pool, min=10, max=20, estimate_rule=cat_estimate_hybrid)
#' 
#' ## SE, MI, and CI stopping rule
#' cat_sim(1.0, pool, min=10, max=20, stop_se=.3)
#' cat_sim(1.0, pool, min=10, max=20, stop_mi=.6)
#' cat_sim(1.0, pool, min=10, max=20, stop_cut=0)
#' cat_sim(1.0, pool, min=10, max=20, stop_cut=0, ci_width=2.58)
#' 
#' ## maximum information selection with item sets
#' cat_sim(1.0, pool, min=10, max=20, group="set_id")$admin
#' 
#' ## maximum information with item exposure control
#' cat_sim(1.0, pool, min=10, max=20, info_random=5)$admin
#'
#' ## Constrained-CAT selection rule with and without initial randomness
#' cat_sim(1.0, pool, min=10, max=20, select_rule=cat_select_ccat, 
#'         ccat_var="content", ccat_perc=c("1"=.2, "2"=.3, "3"=.5))
#' cat_sim(1.0, pool, min=10, max=20, select_rule=cat_select_ccat, ccat_random=5,
#'         ccat_var="content", ccat_perc=c("1"=.2, "2"=.3, "3"=.5))
#'
#' ## Shadow-test selection rule
#' cons <- data.frame(var='content', level=1:3, min=c(3,3,4), max=c(3,3,4))
#' cons <- rbind(cons, data.frame(var='time', level=NA, min=55*10, max=65*10))
#' cat_sim(1.0, pool, min=10, max=10, select_rule=cat_select_shadow, constraints=cons)
#' 
#' ## Projection-based stopping rule
#' cons <- data.frame(var='content', level=1:3, min=5, max=15)
#' cons <- rbind(cons, data.frame(var='time', level=NA, min=60*20, max=60*40))
#' cat_sim(1.0, pool, min=20, max=40, select_rule=cat_select_shadow, stop_rule=cat_stop_projection, 
#'         projection_method="diff", stop_cut=0, constraints=cons)
#' }
NULL


#' @rdname cat_sim
#' @description \code{cat_sim} runs a simulation of CAT. Use \code{theta} in options to set the starting
#' value of theta estimate.
#' @param true the true theta
#' @param pool the item pool (data.frame)
#' @param ...  option/control parameters
#' @return \code{cat_sim} returns a \code{cat} object
#' @details
#' \code{...} takes a variety of option/control parameters for the simulations from users. 
#' \code{min} and {max} are mandatory for setting limits on the test length. User-defined
#' selection, estimation, and stopping rules are also passed to the simulator via options.\cr
#' To write a new rule, the function siganiture must be: \code{function(len, theta, stats, admin, pool, opts)}.
#' See built-in rules for examples.   
#' @importFrom stats runif
#' @export
cat_sim <- function(true, pool, ...){
  if(!is.data.frame(pool)) pool <- as.data.frame(pool, stringsAsFactors=FALSE)
  if(!all(c("a", "b", "c") %in% colnames(pool))) stop("cannot find a-, b-, or c-parameters in item pool")

  opts <- list(...)
  if(is.null(opts$min)) stop("minimum length is missing")
  if(is.null(opts$max)) stop("maximum length is missing")
  if(opts$min < 0 || opts$min > opts$max) stop("invalid min/max length values: ", opts$min, " -- ", opts$max)
  if(nrow(pool) < opts$max) stop("insufficient items in item pool: ", nrow(pool))
  
  theta <- ifelse(is.null(opts$theta), 0, opts$theta)
  if(is.null(opts$D)) opts$D <- 1.702
  if(is.null(opts$select_rule)) select_rule <- cat_select_maxinfo else select_rule <- opts$select_rule
  if(is.null(opts$estimate_rule)) estimate_rule <- cat_estimate_mle else estimate_rule <- opts$estimate_rule
  if(is.null(opts$stop_rule)) stop_rule <- cat_stop_default else stop_rule <- opts$stop_rule

  len <- 0
  stats <- matrix(nrow=opts$max, ncol=4, dimnames=list(NULL, c("u", "t", "se", "info")))
  admin <- NULL

  while(len < opts$max){
    # select items and update pool
    selection <- select_rule(len, theta, stats, admin, pool, opts)
    item <- selection$item
    item <- item[0:min(nrow(item), opts$max - len), ]
    pool <- selection$pool
    n <- nrow(item)
    len <- len + n
    admin <- rbind(admin, item)
    # generate responses
    p <- model_3pl_prob(true, item$a, item$b, item$c, opts$D)[1, ]
    u <- as.integer(p > runif(n))
    stats[1:n + (len - n), "u"] <- u
    # estimate
    theta <- estimate_rule(len, theta, stats, admin, pool, opts)
    info <- sum(model_3pl_info(theta, admin$a, admin$b, admin$c, opts$D))
    se <- 1 / sqrt(info)
    stats[1:n + (len - n), "t"] <- theta
    stats[1:n + (len - n), "se"] <- se
    stats[1:n + (len - n), "info"] <- info
    # stop?
    if(stop_rule(len, theta, stats, admin, pool, opts)) break
  }
  
  admin <- cbind(stats[1:len, ], admin)
  rs <- list(pool=pool, admin=admin, true=true, theta=theta)
  class(rs) <- "cat"
  rs
}



#' @rdname cat_sim
#' @description \code{cat_estimate_mle} is the maximum likelihood estimation rule. Use 
#' \code{map_len} to apply MAP to the first K items and use \code{map_prior} to set the
#' prior for MAP.  
#' @param len the current test length
#' @param theta the current theta estimate
#' @param stats a matrix of responses, theta estimate, information and std error 
#' @param admin a data frame of administered items
#' @param opts a list of option/control parameters
#' @return an estimation rule should return a theta estimate
#' @export
cat_estimate_mle <- function(len, theta, stats, admin, pool, opts){
  u <- stats[1:len, "u"]
  u <- matrix(rep(u, each=2), nrow=2)
  if(is.null(opts$map_len)) opts$map_len <- 10
  if(is.null(opts$map_prior)) opts$map_prior <- c(0, 1)
  if (len < opts$map_len) priors <- list(t=opts$map_prior) else priors <- NULL
  with(admin, model_3pl_jmle(u=u, a=a[1:len], b=b[1:len], c=c[1:len], D=opts$D, scale=NULL, priors=priors))$t[1]
}

#' @rdname cat_sim
#' @description \code{cat_estimate_eap} is the expected a posteriori estimation rule,
#' using \code{eap_mean} and \code{eap_sd} option parameters as the prior
#' @export
cat_estimate_eap <- function(len, theta, stats, admin, pool, opts){
  eap_mean <- ifelse(is.null(opts$eap_mean), 0, opts$eap_mean)
  eap_sd <- ifelse(is.null(opts$eap_sd), 1, opts$eap_sd)
  u <- stats[1:len, "u"]
  u <- matrix(rep(u, each=2), nrow=2)
  with(admin, model_3pl_eap_scoring(u=u, a=a[1:len], b=b[1:len], c=c[1:len], D=opts$D))[1]
}

#' @rdname cat_sim
#' @description \code{cat_estimate_hybrid} is a hybrid estimation rule, which uses MLE for
#' mixed responses and EAP for all 1's or 0's responses
#' @export
cat_estimate_hybrid <- function(len, theta, stats, admin, pool, opts){
  u <- stats[1:len, "u"]
  if(all(u==0) || all(u==1)){
    theta <- cat_estimate_eap(len, theta, stats, admin, pool, opts)
  } else {
    theta <- cat_estimate_mle(len, theta, stats, admin, pool, opts)
  }
  theta
}


#' @rdname cat_sim
#' @description \code{cat_stop_default} is a three-way stopping rule. When \code{stop_se}
#' is set in the options, it uses the standard error stopping rule. When
#' \code{stop_mi} is set in the options, it uses the minimum information stopping rule. When
#' \code{stop_cut} is set in the options, it uses the confidence interval (set by \code{ci_width})
#' stopping rule.  
#' @return a stopping rule should return a boolean: \code{TRUE} to stop the CAT, \code{FALSE} to continue
#' @importFrom stats qnorm
#' @export
cat_stop_default <- function(len, theta, stats, admin, pool, opts){
  if(len < opts$min) return(FALSE)
  if(len > opts$max) return(TRUE)
  if(!is.null(opts$stop_se)){
    se <- stats[len, "se"]
    return(se <= opts$stop_se)
  } else if(!is.null(opts$stop_mi)){
    info <- model_3pl_info(theta, pool$a, pool$b, pool$c, opts$D)[1, ]
    return(max(info) <= opts$stop_mi)
  } else if(!is.null(opts$stop_cut)){
    se <- stats[len, "se"]
    ci_width <- ifelse(is.null(opts$ci_width), qnorm(.975), opts$ci_width)
    lb <- theta - ci_width * se
    ub <- theta + ci_width * se
    return(lb > opts$stop_cut || ub < opts$stop_cut)
  }
  FALSE
}


#' @rdname cat_sim
#' @description \code{cat_select_maxinfo} is the maximum information selection rule. Use \code{group}
#' (a numeric vector) to group items belonging to the same set. Use \code{info_random} to implement
#' the random-esque item exposure control method.
#' @return a selection rule should return a list of (a) the selected item and (b) the updated pool
#' @export
cat_select_maxinfo <- function(len, theta, stats, admin, pool, opts){
  if(is.null(opts$group)) group <- 1:nrow(pool) else group <- pool[, opts$group]
  info <- model_3pl_info(theta, pool$a, pool$b, pool$c, opts$D)[1, ]
  info <- aggregate(info, by=list(group), mean)
  colnames(info) <- c("group", "info")
  random <- min(ifelse(is.null(opts$info_random), 1, opts$info_random), nrow(info))
  
  index <-info$group[order(-info$info)[1:random]]
  if(length(index) > 1) index <- sample(index, 1)
  index <- group %in% index
  list(item=pool[index,], pool=pool[!index,])
}

#' @rdname cat_sim
#' @description \code{cat_select_ccat} is the constrained CAT selection rule. Use
#' \code{ccat_var} to set the content variable in the pool. Use \code{ccat_perc} to set
#' the desired content distribution, with the name of each element being the content code
#' and tue value of each element being the percentage. Use \code{ccat_random} to add randomness
#' to initial item selections.
#' @export
cat_select_ccat <- function(len, theta, stats, admin, pool, opts){
  if(is.null(opts$ccat_var)) stop("ccat_var is misisng")
  if(is.null(opts$ccat_perc)) stop("ccat_perc is missing")
  initial_random <- ifelse(is.null(opts$ccat_random), 0, opts$ccat_random)
  
  info <- data.frame(id=1:nrow(pool), domain=pool[,opts$ccat_var])
  info$info <- with(pool, model_3pl_info(theta, a, b, c, opts$D))[1, ]
  
  if(len == 0) curr_perc <- rep(0, length(opts$ccat_perc)) else curr_perc <- freq(admin[1:len, opts$ccat_var], names(opts$ccat_perc))$perc
  if(len < initial_random) domain <- sample(names(opts$ccat_perc), 1) else domain <- names(opts$ccat_perc)[which.max(opts$ccat_perc - curr_perc)]
  
  info <- info[info$domain == domain, ]
  random <- min(nrow(info), ifelse(is.null(opts$info_random), 1, opts$info_random))
  index <- info$id[order(-info$info)[1:random]]
  if(length(index) > 1) index <- sample(index, 1)
  list(item=pool[index, ], pool=pool[-index, ])
}

#' @rdname cat_sim
#' @description \code{cat_select_shadow} is the shadow-test selection rule. Use \code{shadow_id}
#' to group item sets. Use \code{constraints} to set constraints. Constraints should be in a data.frame
#' with four columns: var (variable name), level (variable level, \code{NA} for quantitative variable), 
#' min (lower bound), and max (upper bound).
#' @export
cat_select_shadow <- function(len, theta, stats, admin, pool, opts){
  if(!"shadow_id" %in% colnames(pool)) pool$shadow_id <- 1:nrow(pool)
  if(is.null(opts$constraints)) stop("constraints is missing in the options")
  if(!all(colnames(opts$constraints) %in% c("var", "level", "min", "max"))) 
    stop("shadow_constr should be a data.frame with 4 columns: var, level, min, and max")
  if(is.factor(opts$constraints$var)) opts$constraints$var <- levels(opts$constraints$var)[opts$constraints$var]
  if(is.factor(opts$constraints$level)) opts$constraints$level <- levels(opts$constraints$level)[opts$constraints$level]
  
  x <- ata(pool, 1, len=c(opts$min, opts$max), 1)
  x <- ata_obj_relative(x, theta, "max")
  for(i in 1:nrow(opts$constraints))
    x <- with(opts$constraints[i,], ata_constraint(x, var, min=min, max=max, level=level))
  if(!is.null(admin)) x <- ata_item_fixedvalue(x, match(admin$shadow_id, pool$shadow_id), min=1, forms=1)
  x <- ata_solve(x, as.list=FALSE, details=F)
  if(is.null(x$items)) stop("Failed to assemble a shadow test")
  x$items <- x$items[!x$items$shadow_id %in% admin$shadow_id, ]
  
  info <- data.frame(id=x$items$shadow_id, info=with(x$items, model_3pl_info(theta, a, b, c, opts$D))[1,])
  random <- min(nrow(info), ifelse(is.null(opts$info_random), 1, opts$info_random))
  index <- info$id[order(-info$info)[1:random]]
  if(length(index) > 1) index <- sample(index, 1)
  list(item=pool[index, ], pool=pool)
}


#' @rdname cat_sim
#' @param x a \code{cat} object
#' @export
print.cat <- function(x, ...){
  if(class(x) != "cat") stop("Not a 'cat' object.")
  
  len <- nrow(x$admin)
  cat("true=", round(x$true, 2), ", est.=", round(x$theta, 2), 
      ", se=", round(x$admin$se[len], 2), ", p=", round(mean(x$admin$u), 2),
      ", used ", len, " items (", sum(x$admin$u)," correct).\n", sep="")
  cat("Belows is a history of the CAT:\n")
  if(len <= 10) {
    print(x$admin)
  } else {
    print(x$admin[1:5, ])
    cat("...\n")
    print(x$admin[1:5 + len - 5, ])
  }
  
  invisible(x)
}


#' @rdname cat_sim
#' @import ggplot2
#' @export
plot.cat <- function(x, ...){
  if(class(x) != "cat") stop("Not a 'cat' object.")
  
  opts <- list(...)
  if(is.null(opts$ylim)) opts$ylimc <- c(-3, 3)
  len <- nrow(x$admin)
  x$admin$lb <- x$admin$t - 1.96 * x$admin$se
  x$admin$ub <- x$admin$t + 1.96 * x$admin$se
  x$admin$pos <- 1:len
  x$admin$Responses <- factor(x$admin$u, levels=c(0, 1), labels=c("Wrong", "Right"))
  
  ggplot(data=x$admin, aes_string(x="pos", y="t", color="Responses")) + 
    geom_point(aes_string(size="se")) +
    geom_linerange(aes_string(ymin="lb", ymax="ub"), linetype=3) +
    geom_point(aes(x=len, y=x$true), color="coral", pch=4, size=3) +
    coord_cartesian(ylim=opts$ylim) + scale_size_continuous(range=c(1, 3)) +
    xlab("Position") + ylab(expression(paste("Est. ", theta))) + 
    guides(size=F, alpha=F) + theme_bw() + theme(legend.key=element_blank())
}


#' @rdname cat_sim
#' @description \code{cat_stop_projection} is the projection-based stopping rule. Use 
#' \code{projection_method} to choose the projection method ('info' or 'diff'). Use
#' \code{stop_cut} to set the cut score. Use \code{constraints} to set the constraints.
#' Constraints should be a data.frame with columns: var (variable name), 
#' level (variable level, \code{NA} for quantitative varialbe), min (lower bound), max (upper bound)
#' @export
cat_stop_projection <- function(len, theta, stats, admin, pool, opts){
  if(len < opts$min)  return(FALSE)
  if(len >= opts$max) return(TRUE)
  
  method <- match.arg(opts$projection_method, c('info', 'diff'))
  if(is.null(opts$stop_cut)) stop('stop_cut is missing in the options')
  if(is.null(opts$constraints)) stop("constraints is missing in the options")
  if(!all(colnames(opts$constraints) %in% c("var", "level", "min", "max"))) 
    stop("shadow_constr should be a data.frame with 4 columns: var, level, min, and max")
  if(is.factor(opts$constraints$var)) opts$constraints$var <- levels(opts$constraints$var)[opts$constraints$var]
  if(is.factor(opts$constraints$level)) opts$constraints$level <- levels(opts$constraints$level)[opts$constraints$level]
  pool <- unique(rbind(pool, admin))
  
  if(method == 'info'){
    x <- ata(pool, 1, len=opts$max, 1)
    x <- ata_obj_relative(x, theta, "max")
    for(i in 1:nrow(opts$constraints))
      x <- with(opts$constraints, ata_constraint(x, var[i], min=min[i], max=max[i], level=level[i]))
    x <- ata_item_fixedvalue(x, admin$shadow_id, min=1, forms=1)
    x <- ata_solve(x, as.list=FALSE, details=F)
    if(is.null(x$items)) stop("Failed to assemble a projection test")
    
    u <- c(stats[1:len, "u"], rep(1, opts$max - len))
    u <- matrix(rep(u, each=2), nrow=2)
    theta_ub <- with(x$items, model_3pl_jmle(u, a=a, b=b, c=c, D=opts$D, scale=NULL, priors=NULL))$t[1]
    u <- c(stats[1:len, "u"], rep(0, opts$max - len))
    u <- matrix(rep(u, each=2), nrow=2)
    theta_lb <- with(x$items, model_3pl_jmle(u, a=a, b=b, c=c, D=opts$D, scale=NULL, priors=NULL))$t[1]
  } else if(method == 'diff'){
    if(is.null(opts$proj_width)) opts$proj_width <- 1.96
    
    x <- ata(pool, 1, len=opts$max, 1)
    x <- ata_obj_absolute(x, "b", (theta + opts$proj_width * stats[len, "se"]) * opts$max)
    for(i in 1:nrow(opts$constraints))
      x <- with(opts$constraints, ata_constraint(x, var[i], min=min[i], max=max[i], level=level[i]))
    x <- ata_item_fixedvalue(x, admin$shadow_id, min=1, forms=1)
    x <- ata_solve(x, as.list=FALSE, details=F)
    if(is.null(x$items)) stop("Failed to assemble a projection test")
    u <- c(stats[1:len, "u"], rep(1, opts$max - len))
    u <- matrix(rep(u, each=2), nrow=2)
    theta_ub <- with(x$items, model_3pl_jmle(u, a=a, b=b, c=c, D=opts$D, scale=NULL, priors=NULL))$t[1]

    x <- ata(pool, 1, len=opts$max, 1)
    x <- ata_obj_absolute(x, "b", (theta - opts$proj_width * stats[len, "se"]) * opts$max)
    for(i in 1:nrow(opts$constraints))
      x <- with(opts$constraints, ata_constraint(x, var[i], min=min[i], max=max[i], level=level[i]))
    x <- ata_item_fixedvalue(x, admin$shadow_id, min=1, forms=1)
    x <- ata_solve(x, as.list=FALSE, details=F)
    if(is.null(x$items)) stop("Failed to assemble a projection test")
    u <- c(stats[1:len, "u"], rep(0, opts$max - len))
    u <- matrix(rep(u, each=2), nrow=2)
    theta_lb <- with(x$items, model_3pl_jmle(u, a=a, b=b, c=c, D=opts$D, scale=NULL, priors=NULL))$t[1]
  }
  
  (theta_lb > opts$stop_cut || theta_ub < opts$stop_cut)
}
