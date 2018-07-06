### Tensor virial theorem

`Wxx.Wzz` <-
  function(epsilon){
    
    ecc <- sqrt(1 - (1-epsilon)^2)
    
    numerator <- asin(ecc) - ecc * sqrt(1 - ecc^2)
    denominator <- 2 * (ecc * sqrt(1 - ecc^2) - (1 - ecc^2) * asin(ecc))
    
    result <- numerator/denominator
    invisible(result)
  }

`v.sig.1d` <-
  function(epsilon, delta = 0){
    v.sig.2 <- 2 * (1-delta) * Wxx.Wzz(epsilon) - 2
    return(ifelse(v.sig.2 <0, NA, sqrt(abs(v.sig.2))))
  }

`v.sig.2d` <- 
  function(epsilon, delta=0, alpha=0.15){
    Omega <- Wxx.Wzz(epsilon)
    v.sig.2 <- ((1-delta) * Omega - 1)/(alpha*(1-delta)*Omega + 1)
    return(ifelse(v.sig.2 <0, NA, sqrt(abs(v.sig.2))))
  }
  
`delta.anisotropy` <-
  function(epsilon, V.sigma, alpha=0.15){
    Omega <- Wxx.Wzz(epsilon)
    delta <- 1 - (1 + V.sigma^2)/(1 - alpha*V.sigma^2)/Omega
    return (delta)
}

`v.sig.eps.plot` <-
  function (eps.data=NULL, vsig.data=NULL, twoD=TRUE, xlim, ylim, ...){
    
    epsilon <- seq(0, 0.95, length=600)
    if (twoD){
      v.sig.fn <- v.sig.2d
    } else {
      v.sig.fn <- v.sig.1d
    }

    if (missing(xlim)){
      if(is.null(eps.data)){
        xlim <- c(0, 0.7)
      } else {
        xlim <- range(eps.data)
      }
    }

    if (missing(ylim)){
      if(is.null(vsig.data)){
        ylim <- c(0, 1.3)
      } else {
        ylim <- range(vsig.data)
      }
    }
    
    plot(NULL, xlim=xlim, ylim=ylim, xlab=expression(epsilon), ylab=expression(italic(v/sigma)))
    
    for(delta in seq(0, 0.5, by=0.1)){
      if (twoD){
        lines(epsilon, v.sig.2d(epsilon, alpha=0, delta=delta))
        lines(epsilon, v.sig.2d(epsilon, alpha=0.2, delta=delta), lty=2)
      } else {
        lines(epsilon, v.sig.1d(epsilon, delta=delta))
      }
    }

    if (!is.null(eps.data) & !is.null(vsig.data)){
      points(eps.data, vsig.data, ...)
    }
    
  }

