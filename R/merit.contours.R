merit.contours <- function(galaxy, template, fit=TRUE, p, v.range, s.range,
                         plot=TRUE, mask=TRUE, ...){

  if (!is.null(galaxy$noise)){
    noise <- galaxy$noise
  } else {
    noise <- 1
  }
  
  if (fit || (missing(p) && (missing(v.range) || missing(s.range)))){
    best.fit <- fit.spectrum(galaxy, template, mask=mask, verbose=TRUE, ...)
    p <- c(best.fit$v, best.fit$sigma)
    noise <- best.fit$noise
    mask <- best.fit$mask
  } 
  
  if (missing(v.range)){
    v.range <- c(p[1] - 50, p[1] + 50)
  }
  if (missing(s.range)){
    s.range <- c(p[2] - 50, p[2] + 50)
  }
  v <- seq(v.range[1], v.range[2], length=20)
  sigma <- seq(s.range[1], s.range[2], length=20)

  i <- 0
  j <- 1
  gof <- outer(v, sigma, Vectorize(function(x, y){
    mf <- merit.function(c(x, y),  galaxy, template, noise=noise, mask=mask)
    if(i == 0){
      cat(sprintf("%3d  ", j))
    }
    cat(".")
    i <<- i+1
    if (i == length(v)){
      cat("\n")
      i <<- 0
      j <<- j+1
    }
    invisible(mf)
  }))

  if (plot){
    contour(v, sigma, gof, nlevels=40, xlab="v", ylab="sigma")
    points(p[1], p[2], pch=20, col=2)
  } else {
    list(v=v, sigma=sigma, gof=gof)
  }
  
}
