combine.fits <- function(fit.list, param){

  temp.matrix <- sapply(fit.list, function(x){ x[[param]] })

  myparam <- function(x, FUN, ...){
    FUN <- match.fun(FUN)
    if (any(!is.na(x))){
      result <- FUN(x, ...)
    } else {
      result <- NA
    }
    invisible(result)
  }
  
  temp.mean   <- apply(temp.matrix, 1, myparam, mean, na.rm=TRUE)
  temp.median <- apply(temp.matrix, 1, myparam, median, na.rm=TRUE)
  temp.sd     <- apply(temp.matrix, 1, myparam, sd, na.rm=TRUE)
  temp.min    <- apply(temp.matrix, 1, myparam, min, na.rm=TRUE)
  temp.max    <- apply(temp.matrix, 1, myparam, max, na.rm=TRUE)

  invisible(data.frame(L=fit.list[[1]]$L,
                       M=fit.list[[1]]$M,
                       mean=temp.mean,
                       median=temp.median,
                       sd=temp.sd,
                       min=temp.min,
                       max=temp.max))
}
