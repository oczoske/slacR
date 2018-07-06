plot.spectrum <- function(x, add=FALSE, offset=0, scale=1, norm=FALSE,
                          noise=FALSE, contsub=FALSE, contdiv=FALSE,
                          mask=FALSE, ...){

  lambda <- x$lambda

  if (contsub){
    if (is.null(x$contsub)){
      cat("Continuum subtracted spectrum not available\n")
      return(invisible())
    }  
    value <- x$contsub
  } else if (contdiv){
    if (is.null(x$contdiv)){
      cat("Continuum divided spectrum not available\n")
      return(invisible())
    }
    value <- x$contdiv
  } else if (noise){
    if (is.null(x$noise)){
      cat("Noise spectrum not available\n")
      return(invisible())
    }
    value <- x$noise
  } else {
    value <- x$value
  }
  
  if (norm){  ## normalise - except for continuum divided spectr
    if (contsub){
      scale <- scale/sd(value, na.rm=TRUE)
    } else if (noise){
      scale <- scale/mean(x$value, na.rm=TRUE)
    } else if (!contdiv){
      scale <- scale/mean(value, na.rm=TRUE)
    }
    cat (paste("Scale:", scale, "\n"))
  }

  value <- value * scale + offset
  if (!add){
    plot(lambda, value, type='l', ...)
  } else {
    lines(lambda, value, ...)
  }

  if (!is.logical(mask)){
    mask <- convert.mask(lambda, mask)
    points(lambda[!mask], value[!mask], col=6, pch=20, cex=0.5)
  } else if (mask){
    points(lambda[!x$mask], value[!x$mask], col=6, pch=20, cex=0.5)
  }
}

