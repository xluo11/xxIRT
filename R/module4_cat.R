#' Computerized Adaptive Testing (CAT) Simulation
#' @description \code{cat_sim} simulates CAT with user-defined algorithms
#' @param true the true theta
#' @param pool the item pool (data.frame)
#' @param ... additional option parameters. See details.
#' @return \code{cat_sim} returns a \code{cat} object. See details.
#' @details
#' \code{...} takes additional option/control parameters from users. \code{min} and {max} are
#' mandatory in order to control the minimum and maximum test length. \cr 
#' The selection, estimation, and stopping rules takes the same set of arguments: 
#' \code{fucntion(len, theta, stats, admin, pool, opts)}, which are the current test length, 
#' the current theta estimate, the matrix of statistics, the data.frame of administered items, 
#' the item pool, and the options. To override default rules, first write new rules using the 
#' same function signature and pass the new rule and its required parameters to \code{cat_sim()}
#' options. \cr
#' The returned \code{cat} object contains the remaining item pool, the administration history,
#' the true and estimated thetas. \cr
#' @examples
#' ## generate item pool
#' pool <- model_3pl()$gendata(1, 100)$items
#' pool$set_id <- sample(1:30, 100, replace=TRUE)
#' pool$content <- sample(1:3, 100, replace=TRUE)
#' pool$time <- round(rlnorm(100, mean=4.1, sd=.2))
#' ## randomesque to control exposure in selection
#' cat_sim(1.0, pool, min=10, max=20, randomesque=5)
#' ## use user-defined ID variable to select item sets
#' cat_sim(1.0, pool, min=10, max=20, selct_id="set")
#' ## use the mle_step estimation rule
#' cat_sim(1.0, pool, min=10, max=20, mle_step=.5, 
#'     estimate_rule=cat_estimate_mle_step)
#' ## use the hybrid estimation rule
#' cat_sim(1.0, pool, min=10, max=20, estimate_rule=cat_estimate_hybrid)
#' ## use the standard error stopping rule
#' cat_sim(1.0, pool, min=10, max=20, stop_rule=cat_stop_default, stop_se=.25)
#' ## use the 95% confidence interval classification stopping rule
#' cat_sim(1.0, pool, min=10, max=20, stop_rule=cat_stop_default, stop_cut=0)
#' ## use the constrained CAT item selection
#' cat_sim(1.0, pool, min=10, max=20, select_rule=cat_select_ccat, 
#'     ccat_var='content', ccat_perc=c('1'=.2, '2'=.3, '3'=.5))
#' ## use the constrained CAT item selection with initial randomness
#' cat_sim(1.0, pool, min=10, max=20, select_rule=cat_select_ccat, 
#'     ccat_var='content', ccat_perc=c('1'=.2, '2'=.3, '3'=.5), ccat_init_rand=5)
#' ## use the shadow-test CAT 
#' cons <- data.frame(var='content', level=1:3, min=3, max=5)
#' cons <- rbind(cons, data.frame(var='time', level=NA, min=55*10, max=65*10))
#' cat_sim(1.0, pool, min=10, max=10, shadow_constraints=cons, select_id="set_id")
#' @importFrom stats runif
#' @export
cat_sim <- function(true, pool, ...){
  pool <- as.data.frame(pool, stringsAsFactors=FALSE)
  if(!all(c("a", "b", "c") %in% colnames(pool))) stop("cannot find a-, b-, or c-parameters in item pool")

  opts <- list(...)
  if(is.null(opts$min)) stop("minimum length is missing")
  if(is.null(opts$max)) stop("maximum length is missing")
  if(opts$min < 0 || opts$min > opts$max) stop("invalid min/max length values: ", opts$min, " -- ", opts$max)
  if(nrow(pool) < opts$max) stop("insufficient items in item pool: ", nrow(pool))
  
  theta <- ifelse(is.null(opts$theta), 0, opts$theta)
  if(is.null(opts$select_rule)) select_rule <- cat_select_default else select_rule <- opts$select_rule
  if(is.null(opts$estimate_rule)) estimate_rule <- cat_estimate_default else estimate_rule <- opts$estimate_rule
  if(is.null(opts$stop_rule)) stop_rule <- cat_stop_default else stop_rule <- opts$stop_rule

  len <- 0
  stats <- matrix(nrow=opts$max, ncol=3, dimnames=list(NULL, c("u", "t", "se")))
  admin <- NULL

  while(len < opts$max){
    # select items and update pool
    selection <- select_rule(len, theta, stats, admin, pool, opts)
    item <- selection$item
    pool <- selection$pool
    item <- item[0:min(nrow(item), opts$max - len), ]   # in case it busts opts$max
    n <- nrow(item)
    len <- len + n
    admin <- rbind(admin, item)
    # generate responses
    p <- irt_stats(model_3pl(theta=true, items=item), "prob")[1, ]
    u <- (p > runif(n)) * 1
    stats[len - 1:n + 1, "u"] <- u
    # estimate
    theta <- estimate_rule(len, theta, stats, admin, pool, opts)
    info <- irt_stats(model_3pl(theta=theta, items=admin), 'info')
    se <- 1 / sqrt(sum(info))
    stats[len - 1:n + 1, "t"] <- theta
    stats[len - 1:n + 1, "se"] <- se
    # stop?
    if(stop_rule(len, theta, stats, admin, pool, opts)) break
  }
  
  admin <- cbind(stats[1:len, ], admin)
  rs <- list(pool=pool, admin=admin, true=true, theta=theta)
  class(rs) <- "cat"
  rs
}



#' @rdname cat_sim
#' @description \code{cat_estimate_default} is a maximum likelihood estimator of
#' the theta parameter
#' @param len the current test length
#' @param theta the current theta estimate
#' @param stats a matrix of responses and statistics
#' @param admin a data frame of administered item pool
#' @param opts a list of options passed in \code{cat_sim} 
#' @return the estimate rule should return a numeric value
#' @export
cat_estimate_default <- function(len, theta, stats, admin, pool, opts){
  u <- matrix(stats[1:len, "u"], nrow=1)
  theta <- estimate_mle(u=u, a=admin$a[1:len], b=admin$b[1:len], c=admin$c[1:len], iter=10)$t
  theta
}

#' @rdname cat_sim
#' @description \code{cat_estimate_mle_step} is a maximum likelihood estimator of
#' the theta parameter, with a fixed incremental/decremental change for all 1s or 
#' 0s responses
#' @export
cat_estimate_mle_step <- function(len, theta, stats, admin, pool, opts){
  if(is.null(opts$mle_step)) stop("mle_step parameter is missing")
  mle_step <- opts$mle_step
  u <- matrix(stats[1:len, "u"], nrow=1)
  if(all(u==0)){
    theta <- theta - mle_step
  } else if(all(u==1)){
    theta <- theta + mle_step
  } else {
    theta <- estimate_mle(u=u, a=admin$a[1:len], b=admin$b[1:len], c=admin$c[1:len], iter=10)$t
  }
  theta
}

#' @rdname cat_sim
#' @description \code{cat_estimate_eap} is an expected a posteriori estimator of
#' the theta parameter
#' @export
cat_estimate_eap <- function(len, theta, stats, admin, pool, opts){
  eap_mean <- ifelse(is.null(opts$eap_mean), 0, opts$eap_mean)
  eap_sd <- ifelse(is.null(opts$eap_sd), 1, opts$eap_sd)
  u <- matrix(stats[1:len, "u"], nrow=1)
  theta <- estimate_bayesian(u=u, a=admin$a[1:len], b=admin$b[1:len], c=admin$c[1:len], method="eap", iter=10, t_mu=eap_mean, t_sig=eap_sd)$t
  theta
}

#' @rdname cat_sim
#' @description \code{cat_estimate_hybrid} is a hybrid estimator of
#' the theta parameter: EAP for all 1s or 0s responses, and MLE otherwise
#' @export
cat_estimate_hybrid <- function(len, theta, stats, admin, pool, opts){
  eap_mean <- ifelse(is.null(opts$eap_mean), 0, opts$eap_mean)
  eap_sd <- ifelse(is.null(opts$eap_sd), 1, opts$eap_sd)
  u <- matrix(stats[1:len, "u"], nrow=1)
  if(all(u==0) || all(u==1)){
    theta <- estimate_bayesian(u=u, a=admin$a[1:len], b=admin$b[1:len], c=admin$c[1:len], method="eap", iter=10, t_mu=eap_mean, t_sig=eap_sd)$t
  } else {
    theta <- estimate_mle(u=u, a=admin$a[1:len], b=admin$b[1:len], c=admin$c[1:len], iter=10)$t
  }
  theta
}



#' @rdname cat_sim
#' @description \code{cat_stop_default} is a trifold stopping rule: 
#' it is the minimum standard error rule when \code{stop_se} is set in options, 
#' the minimum information rule when \code{stop_mi} is set in options, and 
#' the 95% confidence interval rule when \code{stop_cut} is set in options
#' @return the stopping rule should return a boolean with \code{TRUE} to stop the CAT
#' @export
cat_stop_default <- function(len, theta, stats, admin, pool, opts){
  if(len < opts$min) return(FALSE)
  if(len > opts$max) return(TRUE)
  if(!is.null(opts$stop_se)){
    se <- stats[len, "se"]
    return(se <= opts$stop_se)
  } else if(!is.null(opts$stop_mi)){
    info <- irt_stats(model_3pl(theta=theta, items=pool), "info")[1, ]
    return(max(info) <= opts$stop_mi)
  } else if(!is.null(opts$stop_cut)){
    se <- stats[len, "se"]
    lb <- theta - 1.96 * se
    ub <- theta + 1.96 * se
    return(lb > opts$stop_cut || ub < opts$stop_cut)
  }
  FALSE
}



#' @rdname cat_sim
#' @description \code{cat_select_default} selects the item with maximum information or 
#' the item set with maximum averaged information for the current theta estimate.
#' @return the selection rule should return a list of the selected item and the updated pool
#' @export
cat_select_default <- function(len, theta, stats, admin, pool, opts){
  if(is.null(opts$select_id)) id <- 1:nrow(pool) else id <- pool[, opts$select_id]
  info <- irt_stats(model_3pl(theta=theta, items=pool), "info")[1, ]
  info <- aggregate(info, by=list(id), mean)
  colnames(info) <- c("id", "info")
  randomesque <- ifelse(is.null(opts$randomesque), 1, opts$randomesque)
  randomesque <- min(randomesque, nrow(info))
  select_id <- info$id[order(info$info, decreasing=TRUE)[1:randomesque]]
  if(length(select_id) > 1) select_id <- sample(select_id, 1)
  select_item <- pool[id == select_id, ]
  if(!is.null(opts$enemy_var)){
    enemy <- sapply(select_item[, opts$enemy_var], function(xx) strsplit(xx, split="[,]")[[1]])
    enemy <- unique(Reduce(c, enemy))
  } else {
    enemy <- NULL
  }
  select_pool <- pool[!id %in% c(select_id, enemy), ]
  list(item=select_item, pool=select_pool)
}

#' @rdname cat_sim
#' @description \code{cat_select_ccat} implements the constrained CAT selection algorithm. 
#' The \code{'ccat_perc'} argument defines the percentage targets, and the \code{'ccat_var'}
#' argument defines the constrained variable. 
#' @export
cat_select_ccat <- function(len, theta, stats, admin, pool, opts){
  if(is.null(opts$select_id)) id <- 1:nrow(pool) else id <- pool[, opts$select_id]
  if(is.null(opts$ccat_perc)) stop("ccat_perc is missing")
  if(is.null(opts$ccat_var)) stop("ccat_var is misisng")
  # find next domain
  init_rand <- ifelse(is.null(opts$ccat_init_random), 0, opts$ccat_init_random)
  target_perc <- opts$ccat_perc
  if(len == 0) curr_perc <- rep(0, length(target_perc)) else curr_perc <- freq(admin[1:len, opts$ccat_var], names(target_perc))$perc / 100
  if(len < init_rand){
    domain <- names(target_perc)[curr_perc * len < target_perc * opts$min]
    if(length(domain) > 1) domain <- sample(domain, 1)
  } else {
    domain <- names(target_perc)[which.max(target_perc - curr_perc)]
  }
  info <- irt_stats(model_3pl(theta=theta, items=pool), "info")[1, ]
  info <- aggregate(info, by=list(id, pool[,opts$ccat_var]), mean)
  colnames(info) <- c("id", "domain", "info")
  info <- info[info$domain == domain, ]
  randomesque <- ifelse(is.null(opts$randomesque), 1, opts$randomesque)
  randomesque <- min(randomesque, nrow(info))
  select_id <- info$id[order(info$info, decreasing=TRUE)[1:randomesque]]
  if(length(select_id) > 1) select_id <- sample(select_id, 1)
  select_item <- pool[id == select_id, ]
  if(!is.null(opts$enemy_var)){
    enemy <- sapply(select_item[, opts$enemy_var], function(xx) strsplit(xx, split="[,]")[[1]])
    enemy <- unique(Reduce(c, enemy))
  } else {
    enemy <- NULL
  }
  select_pool <- pool[!id %in% c(select_id, enemy), ]
  list(item=select_item, pool=select_pool)
}

#' @rdname cat_sim
#' @description \code{cat_select_shadow} implements the shadow-test CAT selection algorithm. 
#' The \code{'shadow_constraints'} argument defines the constraints. 
#' @importFrom stats aggregate
#' @importFrom magrittr %>%
#' @import dplyr
#' @export
cat_select_shadow <- function(len, theta, stats, admin, pool, opts){
  constraints <- opts$shadow_constraints
  if(is.null(constraints)) 
    stop("shadow-test constraint is missing")
  if(!all(colnames(constraints) %in% c("var", "level", "min", "max"))) 
    stop("shadow-test constraints should have columns named 'var', 'level', 'min', and 'max'")
  if(is.factor(constraints$var)) constraints$var <- levels(constraints$var)[constraints$var]
  if(is.factor(constraints$level)) constraints$level <- levels(constraints$level)[constraints$level]
  constraints$curr <- apply(constraints, 1, function(xx) {
    if(is.null(admin)){
      curr <- 0
    } else if(is.na(xx["level"])) {
      curr <- sum(admin[, xx["var"]])
    } else {
      curr <- sum(admin[, xx["var"]] == trimws(xx["level"]))
    }
    curr
  })
  
  if(is.null(opts$select_id)) id <- 1:nrow(pool) else id <- pool[, opts$select_id]
  
  # dummy code categorical variable and transform constraints
  cons_cat <- dplyr::filter_(constraints, !is.na(~level)) 
  pool_cat <- apply(cons_cat, 1, function(xx) {
    as.integer(pool[, xx['var']] == xx['level'])
  }) %>% as.data.frame()
  colnames(pool_cat) <- paste(cons_cat$var, cons_cat$level, sep="_")
  x_pool <- cbind(pool, pool_cat) %>% 
    dplyr::mutate_(len=1, info=irt_stats(model_3pl(theta=theta, items=pool), "info")[1, ]) %>%
    aggregate(by=list(id), sum) %>% 
    dplyr::rename_(shadow_id=~Group.1)
  constraints <- dplyr::mutate_(cons_cat, var=paste(~var, ~level, sep='_'), level=NA) %>%
    rbind(dplyr::filter_(constraints, is.na(~level)))
  
  # ata
  x <- ata(x_pool, 1, len=NULL, maxselect=1)
  x <- ata_obj_relative(x, x_pool$info, mode="max")
  x <- ata_constraint(x, "len", min=opts$min - len, max=opts$max - len)
  for(i in 1:nrow(constraints))
    x <- ata_constraint(x, coef=constraints$var[i], 
                        min=constraints$min[i] - constraints$curr[i], 
                        max=max(constraints$max[i] - constraints$curr[i], 0))
  x <- ata_solve(x, as.list=FALSE, verbose="neutral")
  if(is.null(x$items)) x <- x_pool else x <- x$items
  info <- order(x$info / x$len, decreasing=TRUE)
  randomesque <- ifelse(is.null(opts$randomesque), 1, opts$randomesque)
  randomesque <- min(randomesque, length(info))
  select_id <- x$shadow_id[info[1:randomesque]]
  if(length(select_id) > 1) select_id <- sample(select_id, 1)
  select_item <- pool[id == select_id, ]
  if(!is.null(opts$enemy_var)){
    enemy <- sapply(select_item[, opts$enemy_var], function(xx) strsplit(xx, split="[,]")[[1]])
    enemy <- unique(Reduce(c, enemy))
  } else {
    enemy <- NULL
  }
  select_pool <- pool[!id %in% c(select_id, enemy), ]
  list(item=select_item, pool=select_pool)
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
  
  len <- nrow(x$admin)
  x$admin$lb <- x$admin$t - 1.96 * x$admin$se
  x$admin$ub <- x$admin$t + 1.96 * x$admin$se
  x$admin$pos <- 1:len
  x$admin$Responses <- factor(x$admin$u, levels=c(0, 1), labels=c("Wrong", "Right"))
  
  ggplot(data=x$admin, aes_string(x="pos", y="t", color="Responses")) + 
    geom_point(aes_string(size="se")) + 
    geom_point(aes(x=len, y=x$true), color="coral", pch=4, size=3) +
    geom_linerange(aes_string(ymin="lb", ymax="ub"), linetype=3) +
    coord_cartesian(ylim=c(-3, 3)) + scale_size_continuous(range=c(1, 3)) +
    xlab("Position") + ylab(expression(paste("Est. ", theta))) + 
    guides(size=F, alpha=F) + theme_bw() + 
    theme(legend.key=element_blank())
}


# cat_stop_proj <- function(len, theta, stats, admin, pool, opts){
#   method <- opts$proj_method
#   if(is.null(method) || !method %in% c('info', 'diff'))
#     stop("invalid projection method: use 'info' or 'diff'")
#   cutscore <- opts$proj_cut
#   if(is.null(cutscore))
#     stop("the projection cutscore is misisng")
#   constraints <- opts$proj_constraints
#   if(is.null(constraints)) 
#     stop("shadow-test constraint is missing")
#   if(!all(colnames(constraints) %in% c("var", "level", "min", "max"))) 
#     stop("shadow-test constraints should have columns named 'var', 'level', 'min', and 'max'")
#   if(is.factor(constraints$var)) constraints$var <- levels(constraints$var)[constraints$var]
#   if(is.factor(constraints$level)) constraints$level <- levels(constraints$level)[constraints$level]
#   constraints$curr <- apply(constraints, 1, function(xx) {
#    if(is.na(xx["level"])) curr <- sum(admin[, xx["var"]]) else curr <- sum(admin[, xx["var"]] == trimws(xx["level"]))
#     curr
#   })
#   
#   if(is.null(opts$select_id)) id <- 1:nrow(pool) else id <- pool[, opts$select_id]
#   
#   cons_cat <- filter(constraints, !is.na(level)) 
#   pool_cat <- apply(cons_cat, 1, function(xx) {
#     as.integer(pool[, xx['var']] == xx['level'])
#   }) %>% as.data.frame()
#   colnames(pool_cat) <- paste(cons_cat$var, cons_cat$level, sep="_")
#   x_pool <- cbind(pool, pool_cat) %>% 
#     mutate(len=1, info=irt_stats(model_3pl(theta=theta, items=pool), "info")[1, ]) %>%
#     aggregate(., by=list(id), sum) %>% 
#     rename(shadow_id=Group.1)
#   constraints <- mutate(cons_cat, var=paste(var, level, sep='_'), level=NA) %>%
#     rbind(., filter(constraints, is.na(level)))
#   
#   # ata
#   if(method == "info") {
#     x <- ata(x_pool, 1, len=NULL, maxselect=1)
#     x <- ata_obj_relative(x, x_pool$info, "max")
#     x <- ata_constraint(x, "len", min=opts$max - len, max=opts$max - len)
#     for(i in 1:nrow(constraints))
#       x <- ata_constraint(x, coef=constraints$var[i], min=constraints$min[i] - constraints$curr[i], max=constraints$max[i] - constraints$curr[i])
#     x <- ata_solve(x, as.list=FALSE, verbose="neutral")
#     x <- list(easy=x$items, hard=x$items)
#   } else if(method == "diff") {
#     x <- ata(x_pool, 1, len=NULL, maxselect=1)
#     x <- ata_obj_absolute(x, x_pool$b, (theta + 1.96 * stats[len, "se"]) * (opts$max - len))
#     x <- ata_constraint(x, "len", min=opts$max - len, max=opts$max - len)
#     for(i in 1:nrow(constraints))
#       x <- ata_constraint(x, coef=constraints$var[i], min=constraints$min[i] - constraints$curr[i], max=constraints$max[i] - constraints$curr[i])
#     x <- ata_solve(x, as.list=FALSE, verbose="neutral")
#     x_hard <- x$items
#     
#     x <- ata(x_pool, 1, len=NULL, maxselect=1)
#     x <- ata_obj_absolute(x, x_pool$b, (theta - 1.96 * stats[len, "se"]) * (opts$max - len))
#     x <- ata_constraint(x, "len", min=opts$max - len, max=opts$max - len)
#     for(i in 1:nrow(constraints))
#       x <- ata_constraint(x, coef=constraints$var[i], min=constraints$min[i] - constraints$curr[i], max=constraints$max[i] - constraints$curr[i])
#     x <- ata_solve(x, as.list=FALSE, verbose="neutral")
#     x_easy <- x$items
#     
#     x <- list(easy=x_easy, hard=x_hard)
#   } else {
#     stop("invalid projection method: use 'info' or 'diff'")
#   }
#   
#   # estimate thetas
#   if(is.null(x$easy)) {
#     theta_lb <- -Inf
#   } else {
#     proj_items <- pool[id %in% x$easy$shadow_id, ]
#     proj_resp <- rep(0, nrow(proj_items))
#     theta_lb <- estimate_mle(matrix(proj_resp, nrow=1), a=proj_items$a, b=proj_items$b, c=proj_items$c, iter=10)$t
#   }
#   if(is.null(x$hard)) {
#     theta_ub <- Inf
#   } else {
#     proj_items <- pool[id %in% x$hard$shadow_id, ]
#     proj_resp <- rep(1, nrow(proj_items))
#     theta_ub <- estimate_mle(matrix(proj_resp, nrow=1), a=proj_items$a, b=proj_items$b, c=proj_items$c, iter=10)$t
#   }
#   (theta_ub < cutscore || theta_lb > cutscore)
# }
