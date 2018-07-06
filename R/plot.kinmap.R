# $Id: plot.with.bar.R,v 1.1 2007/08/08 11:07:45 oczoske Exp $

plot.kinmap <- function(x, param, zlim=NULL, xlab="L", ylab="M",
                        zlab=NULL, color.table=heat.colors(100), logz=FALSE,
                        ...){ 

  if (missing(param)){
    stop("Please specify which parameter to plot!")
  }
  
  csi <- par()$csi
  opar <- par(no.readonly=TRUE)
  on.exit(par(opar))

  xvec <- x[["L"]]
  yvec <- x[["M"]]
  if (is.null(z <- x[[param]])){
    stop(paste("Parameter", param, "does not exist in kinmap"))
  }

  if (logz)
      z <- log10(z)
  
  if (is.null(zlim))
      zlim= range(z, na.rm=TRUE)

  x.bar <- c(0,1)
  y.bar <- seq(zlim[1], zlim[2], length.out=25)
  
  xy.bar <- expand.grid(x.bar, y.bar)

  z.bar <- matrix(apply(xy.bar, 1, function(x){x[2]}), nrow=length(x.bar))
  
  w <- (2 + opar$mar[2]) * csi * 2.54
  layout(matrix(c(2, 1), ncol = 2), widths = c(1, lcm(w)))

  mar <- opar$mar
  mar[4] <- mar[2]
  mar[2] <- 0
  par(mar = mar)
  image(x.bar, y.bar, z.bar, zlim=zlim, bty="c", xlab="", ylab="", xaxt="n",
        yaxt="n", col=color.table)

  ## Add axis on the right, need to expand the tick vector
  ticks <- axTicks(4)
  ticks <- c(ticks[1] - median(diff(ticks)),
             ticks,
             ticks[-1] + median(diff(ticks)))
  axis(4, at=ticks)
  
  padj.value <- 4
  if (missing(zlab) | is.null(zlab)){
    padj.value <- 3
    if (param == "sigma"){
      zlab <- expression(paste(sigma, " [km/s]"))
    } else if (param == "v"){
      zlab <- expression(paste(v, " [km/s]"))
    } else if (param == "gof"){
      padj.value <- 2
      zlab <- expression(chi^2)
    } else if (param == "SN"){
      zlab <- expression(S/N)
    } else {
      zlab <- param
    }
  }
  mtext(zlab, side=4, padj=padj.value)

  mar <- opar$mar
  mar[4] <- 1
  par(mar=mar)
  if (any(diff(xvec) <= 0) || any(diff(yvec) <= 0)) {
    xvec <- unique(xvec)
    yvec <- unique(yvec)
  }

  ## Sort the kinmap such that L is the fastest changing index
  z <- z[order(x[['M']], x[['L']])]

  #### Alternative method: check whether order requires creating the
  #### matrix by row or by column. This requires the kinmap to be sorted
  #### in one way or another. 
  ##  if (all(expand.grid(xvec, yvec) == cbind(x[['L']], x[['M']]))){
  ##      byrow <- FALSE
  ##  } else {
  ##      byrow <- TRUE
  ##  }
  
  image(xvec, yvec, matrix(z, nrow=length(xvec), byrow=FALSE), xlab=xlab,
        ylab=ylab, col=color.table, zlim=zlim, ...)

}


