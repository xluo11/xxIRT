#' Root Mean Squared Error
#' @description Root mean squared error (RMSE) of two numeric vectors/matrices
#' @param x a numeric vector/matrix
#' @param y a numeric vector/matrix
#' @export
rmse <- function(x, y){
  x <- as.matrix(x)
  y <- as.matrix(y)
  if(any(dim(x) != dim(y))) stop("x and y have different dimensions")
  sqrt(colMeans((x - y)^2))
}

#' Frequency Counts
#' @description Frequency counts of a vector
#' @param x a numeric or character vector
#' @param values valid values, \code{NULL} to include all values
#' @param rounding round percentage to n-th decimal places
#' @export
freq <- function(x, values=NULL, rounding=NULL){
  if(is.null(values)) values <- sort(unique(x))
  rs <- data.frame(table(factor(x, levels=values, labels=values)), stringsAsFactors=F)
  colnames(rs) <- c("value", "freq")
  
  rs$perc <- rs$freq / sum(rs$freq)
  rs$cum_freq <- cumsum(rs$freq)
  rs$cum_perc <- cumsum(rs$perc)
  
  if(!is.null(rounding)){
    rs$perc <- round(rs$perc, rounding)
    rs$cum_perc <- round(rs$cum_perc, rounding)
  }
  rs
}

#' Hermite-Gauss Quadrature
#' @description Pre-computed hermite gaussian quadratures points and weights
#' @param degree the degree of hermite-gauss quadrature: '20', '11', '7'
#' @keywords internal
hermite_gauss <- function(degree=c('20', '11', '7')){
  switch(match.arg(degree),
         '20'=list(t=c(-5.38748089001123,-4.60368244955074,-3.94476404011562,-3.34785456738321,-2.78880605842813,-2.25497400208927,-1.73853771211658,-1.23407621539532,-0.737473728545394,-0.245340708300901,0.245340708300901,0.737473728545394,1.23407621539532,1.73853771211658,2.25497400208927,2.78880605842813,3.34785456738321,3.94476404011562,4.60368244955074,5.38748089001123),
                   w=c(2.22939364553415E-13,4.39934099227318E-10,1.08606937076928E-07,7.80255647853206E-06,0.000228338636016353,0.00324377334223786,0.0248105208874636,0.109017206020023,0.286675505362834,0.46224366960061,0.46224366960061,0.286675505362834,0.109017206020023,0.0248105208874636,0.00324377334223786,0.000228338636016353,7.80255647853206E-06,1.08606937076928E-07,4.39934099227318E-10,2.22939364553415E-13)),
         '11'=list(t=c(-3.66847084655958,-2.78329009978165,-2.02594801582575,-1.32655708449493,-0.656809566882099,0,0.656809566882099,1.32655708449493,2.02594801582575,2.78329009978165,3.66847084655958), 
                   w=c(1.43956039371425E-06,0.000346819466323345,0.0119113954449115,0.117227875167708,0.429359752356125,0.654759286914591,0.429359752356125,0.117227875167708,0.0119113954449115,0.000346819466323345,1.43956039371425E-06)),
         '7'=list(t=c(-2.651961356835233492447,-1.673551628767471445032,-0.8162878828589646630387,0,0.8162878828589646630387,1.673551628767471445032,2.651961356835233492447),
                  w=c(9.71781245099519154149E-4,0.05451558281912703059218,0.4256072526101278005203,0.810264617556807326765,0.4256072526101278005203,0.0545155828191270305922,9.71781245099519154149E-4)))
  }

