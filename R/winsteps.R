#' Interface to Winsteps
#' @description Run a winsteps calibration in R
#' @param u a reponse matrix
#' @param codes a vector of valid response values
#' @param ws.path the winstep installation directory
#' @param prefix a prefix attached to temporarily generated files
#' @param noprint TRUE to remove temporary files
#' @param peo.mean the mean of centered person parameters
#' @param item.mean the mean of centered item parameters
#' @param scale the sd of centering variable
#' @param item.anchor a two-column maxtrix of anchored item values: index, value
#' @param peo.anchor a two-column matrix of anchored person values: index, value
#' @param out.vars a vector of variables to be extracted from winstep outputs
#' @return a list of theta, b, step parameters
#' @details
#' Winsteps is a commerical software pacakge that calibrates Rach-family models.
#' See Winsteps Manual for more detail regarding its syntax and functionality.
#' @examples
#' \dontrun{
#' u <- gen.rsp(gen.irt(200, 20, a.mu=log(.5882), a.sig=0, c.min=0, c.max=0))
#' winsteps(u$rsp)
#' }
#' @export
#' @importFrom reshape2 acast
#' @importFrom utils write.table read.table
winsteps <- function(u, codes=c(0, 1), noprint=TRUE, ws.path="C:/Winsteps/", prefix=paste("wstmp",as.integer(Sys.Date()),sep=""), peo.mean=0, item.mean=NULL, scale=1, peo.anchor=NULL, item.anchor=NULL, out.vars=NULL){
  # ckeck winsteps path
  if(!any(file.exists(paste(ws.path, "/winsteps.exe", sep="")))) stop("winsteps is not found in ws.path.")
  if(getwd() != ws.path) file.copy(from=paste(ws.path, "/winsteps.exe", sep=""), to=getwd())
  # check anchors
  if(!is.null(peo.anchor)){
    peo.anchor <- as.matrix(peo.anchor)
    if(ncol(peo.anchor) != 2) stop("peo.anchor must have two columns: index, value")
    write.table(peo.anchor, paste(prefix,"_peoanchor.txt",sep=""), sep=" ", row.names=F, col.names=F)
  }
  if(!is.null(item.anchor)){
    item.anchor <- as.matrix(item.anchor)
    if(ncol(item.anchor) != 2) stop("item.anchor must have two columns: index, value")
    write.table(item.anchor, paste(prefix,"_itemanchor.txt",sep=""), sep=" ", row.names=F, col.names=F)
  }
  # check response matrix
  u <- as.matrix(u)
  write.table(u, paste(prefix, "_data.txt", sep=""), sep="", row.names=F, col.names=F)
  
  # write control
  ws <- paste("TITLE = 'WINSTEPS CALIBRATION';\n",
              "DATA = '", prefix, "_data.txt';\n",
              ifelse(is.null(item.anchor), "", paste("IAFILE = '", prefix, "_itemanchor.txt';\n", sep="")),
              ifelse(is.null(peo.anchor), "", paste("PAFILE = '", prefix, "_peoanchor.txt';\n", sep="")),
              "IFILE = '", prefix, "_item.txt';\n",
              "PFILE = '", prefix, "_peo.txt';\n",
              "SFILE = '", prefix, "_step.txt;\n",
              "NI = ", ncol(u), ";\n",
              "NAME1 = 1;\n", "ITEM1 = 1;\n", 
              paste("CODES =", paste(codes,collapse=""),";\n", sep=""), 
              ifelse(is.null(item.mean), "", paste("UIMEAN = ", item.mean, ";\n",sep="")),
              ifelse(is.null(peo.mean), "", paste("UPMEAN = ", peo.mean, ";\n",sep="")),
              "USCALE = ", scale, ";\n",
              "ISGROUP = 0;\n",
              "&END;", sep="")
  write.table(ws,paste(prefix, "_control.txt",sep=""), sep="", row.names=F, col.names=F, quote=F)
  
  # call WINSTEPS
  command <- paste("\"", ws.path, "/winsteps\"", " BATCH=YES ", prefix,"_control.txt ", prefix, "_output.txt", sep="")
  system(command)
  
  # extract item and people parameters
  output <- list()
  output$t <- read.table(paste(prefix, "_peo.txt", sep=""), skip=1, header=T)
  output$b <- read.table(paste(prefix, "_item.txt", sep=""), skip=1, header=T)
  output$s <- read.table(paste(prefix, "_step.txt", sep=""), skip=2, header=F, col.names=c("item","step","par"))
  output$s <- acast(output$s, item ~ step, value.var="par")
  if(is.null(out.vars)) {
    output$t <- output$t[,2]
    output$b <- output$b[,2]
  } else {
    out.vars <- toupper(out.vars)
    out.index <- match(out.vars, colnames(output$t))
    output$t <- output$t[ , out.index]
    output$b <- output$b[ , out.index]
  }
  if(length(codes) == 2) output$s <- NULL
  
  # remove temporary files
  if(noprint) unlink(paste(prefix, "*", sep=""))
  if(getwd() != ws.path) unlink("winsteps.exe")
  
  # return
  cat("Estimation is Completed.\n")
  if(!is.null(out.vars)) cat("[", paste(out.vars,collapse=",",sep=""), "] are retrieved.\n")
  if(!is.null(item.anchor)) cat("Scale is anchored with b parameters.\n")
  else if(!is.null(peo.anchor)) cat("Scale is anchored with ability parameters.\n")
  else if(!is.null(item.mean)) cat("Center b parameters at", item.mean, "\n")
  else if(!is.null(peo.mean)) cat("Center ability parameters at ", peo.mean, "\n")
  return(output)
}

