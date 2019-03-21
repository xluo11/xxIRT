#' Helper functions of ATA
#' @description miscellaneous helper functions of ATA
#' @name ata_helpers
NULL

#' @rdname ata_helpers
#' @description \code{ata_append_constraints} appends constraint definitions to the model
#' @param mat coefficient matrix
#' @param dir direction
#' @param rhs right-hand-side value
#' @keywords internal
ata_append_constraints <- function(x, mat, dir, rhs) {
  x$mat <- rbind(x$mat, mat)
  x$dir <- c(x$dir, dir)
  x$rhs <- c(x$rhs, rhs)
  x
}

#' @rdname ata_helpers
#' @description \code{ata_form_index} converts input forms into actual form indices in the model
#' @param forms indices of forms
#' @param collapse \code{TRUE} to collaspe forms into one form
#' @param internal_index \code{TRUE} to use internal form indices
#' @keywords internal
ata_form_index <- function(x, forms, collapse, internal_index){
  if(internal_index){
    if(is.null(forms))
      forms <- 1:x$num_form
    if(any(!forms %in% 1:x$num_form))
      stop('invalid form indices')
    forms <- as.matrix(forms)
  } else {
    if(is.null(forms))
      forms <- 1:nrow(x$form_map)
    if(any(!forms %in% 1:nrow(x$form_map)))
      stop('invalid form indices')
    forms <- x$form_map[forms, , drop=FALSE]
  }
  if(collapse) forms <- matrix(unique(as.vector(forms)), nrow=1)
  forms
}

#' @rdname ata_helpers
#' @description \code{ata_obj_coef} processes input coefficients of the objective functions
#' @param coef coefficients
#' @param compensate \code{TRUE} to combine coefficients
#' @importFrom stats aggregate
#' @keywords internal
ata_obj_coef <- function(x, coef, compensate){
  if(length(coef) == x$num_item){ 
    # if a vector of given values (item-group-level), then convert to matrix
    coef <- matrix(coef, nrow=1)
  } else if(length(coef) == nrow(x$pool)) { 
    # if a vector of given values (item-level), then aggregate and conver to matrix
    coef <- aggregate(coef, by=list(x$group), sum)[,-1]
    coef <- matrix(coef, nrow=1)
  } else if(is.numeric(coef)) { 
    # if a vector of theta's, then compute infomation and aggregate
    coef <- with(x$pool, model_3pl_info(coef, a, b, c, D=x$opts$D))
    coef <- aggregate(t(coef), by=list(group=x$group), sum)[,-1]
    coef <- t(as.matrix(coef))
  } else if(is.character(coef) && all(coef %in% colnames(x$pool))) { 
    # if a variable name, then retrieve value and aggregate
    coef <- aggregate(x$pool[,coef], by=list(group=x$group), sum)[,-1]
    coef <- t(as.matrix(coef))
  } else {
    stop("invalid coefficients")
  }
  if(compensate) coef <- matrix(colSums(coef), nrow=1)
  round(coef, 2)
}

#' @rdname ata_helpers
#' @description \code{ata_solve_lpsolve} solves the the MIP model using lp_solve
#' @param time_limit the time limit in seconds passed along to solvers
#' @param message \code{TRUE} to print messages from solvers
#' @param ... additional control parameters for solvers
#' @import lpSolveAPI
#' @keywords internal
ata_solve_lpsolve <- function(x, time_limit, message, ...) {
  if(class(x) != "ata") stop("not an 'ata' object")
  lp <- make.lp(0, x$num_lpvar)
  # (max): direction
  lp.control(lp, sense=ifelse(x$max, "max", "min"))
  # set bound for y: positive = (lb=0); negative = (ub = 0)
  if(x$negative) set.bounds(lp, lower=-Inf, upper=0, x$num_lpvar-1)
  # (obj): objective function
  set.objfn(lp, x$obj, seq_along(x$obj))
  # (type): x's = binary, y = continuous
  types <- sapply(x$types, function(x) switch(x, "B"="binary", "I"="integer", "C"="real"))
  for(i in seq_along(types))
    set.type(lp, i, types[i])
  # (bounds): column bounds
  if(!is.null(x$bounds$idx))
    with(x$bounds, for(i in 1:length(idx)) {
      set.bounds(lp, if(!is.na(lb[i])) lower=lb[i], if(!is.na(ub[i])) upper=ub[i], columns=idx[i])
    })
  # (mat): constraints
  for(i in 1:nrow(x$mat))
    add.constraint(lp, x$mat[i,], x$dir[i], x$rhs[i])
  # solve
  lp.control(lp, mip.gap=c(.01, .01), epsint=.10, presolve="lindep", timeout=time_limit)
  lp.control(lp, verbose=ifelse(message, 'normal', 'neutral'))
  lp.control(lp, ...)
  code <- solve(lp)
  status <- switch(as.character(code),
                   '0'="optimal solution found",
                   '1'="the model is sub-optimal",
                   '2'="the model is infeasible",
                   '3'="the model is unbounded",
                   '4'="the model is degenerate",
                   '5'="numerical failure encountered",
                   '6'="process aborted",
                   '7'="timeout",
                   '9'="the model was solved by presolve",
                   '10'="the branch and bound routine failed",
                   '11'="the branch and bound was stopped because of a break-at-first or break-at-value",
                   '12'="a feasible branch and bound solution was found",
                   '13'="no feasible branch and bound solution was found")
  optimum <- get.objective(lp)
  result <- matrix(get.variables(lp)[1:(x$num_lpvar-2)], ncol=x$num_form, byrow=FALSE)
  obj_vars <- get.variables(lp)[(x$num_lpvar-1):x$num_lpvar]
  
  if(!code %in% c(0, 1, 9)) result <- matrix(0, nrow=nrow(result), ncol=ncol(result))
  list(code=code, status=status, optimum=optimum, result=result, obj_vars=obj_vars)
}

#' @rdname ata_helpers
#' @description \code{ata_solve_glpk} solves the the MIP model using GLPK
#' @import glpkAPI
#' @keywords internal
ata_solve_glpk <- function(x, time_limit, message, ...) {
  if(class(x) != "ata") stop("not an 'ata' object")
  opts <- list(...)
  
  # set up the problem
  lp <- initProbGLPK()
  addRowsGLPK(lp, nrow(x$mat))
  addColsGLPK(lp, ncol(x$mat))
  # (max): optimization direction
  setObjDirGLPK(lp, ifelse(x$max, GLP_MAX, GLP_MIN))
  # (obj): obj functions
  setObjCoefsGLPK(lp, seq(x$num_lpvar), x$obj)
  # (types): x's = binary, y's = continuous
  for(j in seq(x$num_lpvar)[x$types == 'B'])
    setColKindGLPK(lp, j, GLP_BV)
  for(j in seq(x$num_lpvar)[x$types == 'C'])
    setColBndGLPK(lp, j, GLP_LO, 0, 0)
  if(x$negative) setColBndGLPK(lp, x$num_lpvar-1, GLP_UP, 0, 0)
  ## fixed values
  if(!is.null(x$bounds$idx))
    for(j in 1:length(x$bounds$idx))
      if(is.na(x$bound$lb[j])){
        setColBndGLPK(lp, x$bounds$idx[j], GLP_UP, 0, x$bounds$ub[j])
      } else if(is.na(x$bound$ub[j])){
        setColBndGLPK(lp, x$bounds$idx[j], GLP_LO, x$bounds$lb[j], 0)
      } else {
        setColBndGLPK(lp, x$bounds$idx[j], GLP_DB, x$bounds$lb[j], x$bounds$ub[j])
      }
  # # check column bounds
  # cbind(getColsLowBndsGLPK(lp, 1:x$num_lpvar), getColsUppBndsGLPK(lp, 1:x$num_lpvar))
  
  # (mat)
  ind <- x$mat != 0
  ia <- rep(1:nrow(x$mat), ncol(x$mat))[ind]
  ja <- rep(1:ncol(x$mat), each=nrow(x$mat))[ind]
  ar <- x$mat[ind]
  loadMatrixGLPK(lp, length(ar), ia, ja, ar)
  # (dir & rhs): row bounds
  dir <- sapply(x$dir, function(x) switch(x, '>='=GLP_LO, '<='=GLP_UP, '='=GLP_FX))
  setRowsBndsGLPK(lp, 1:nrow(x$mat), x$rhs, x$rhs, dir)
  # # check row bounds
  # cbind(getRowsLowBndsGLPK(lp, 1:nrow(x$mat)), getRowsUppBndsGLPK(lp, 1:nrow(x$mat)))
  
  # solve
  setMIPParmGLPK(PRESOLVE, GLP_ON)
  setMIPParmGLPK(MIP_GAP, 0.01)
  setMIPParmGLPK(TM_LIM, 1000 * time_limit)
  setMIPParmGLPK(MSG_LEV, ifelse(message, GLP_MSG_ON, GLP_MSG_OFF))
  for(i in seq_along(opts)) setMIPParmGLPK(get(names(opts)[i]), opts[[i]])
  code <- solveMIPGLPK(lp)
  status <- switch(as.character(code),
                   '0'="optimal solution found",
                   '1'='invalid basis',
                   '2'='singular matrix',
                   '3'='ill-conditioned matrix',
                   '4'='invalid bounds',
                   '5'='solver failed',
                   '6'='objective lower limit reached',
                   '7'='objective upper limit reached',
                   '8'='iteration limit exceeded',
                   '9'='time limit exceeded',
                   '10'='no primal feasible solution',
                   '11'='no dual feasible solution',
                   '12'='root LP optimum not provided',
                   '13'='search terminated by application',
                   '14'='relative mip gap tolerance reached',
                   '15'='no primal/dual feasible solution',
                   '16'='no convergence',
                   '17'='numerical instability',
                   '18'='invalid data',
                   '19'='result out of range')
  optimum <- mipObjValGLPK(lp)
  result <- matrix(mipColsValGLPK(lp)[1:(x$num_lpvar-2)], ncol=x$num_form, byrow=FALSE)
  obj_vars <- mipColsValGLPK(lp)[(x$num_lpvar-1):x$num_lpvar]
  
  list(code=code, status=status, optimum=optimum, result=result, obj_vars=obj_vars)
}
