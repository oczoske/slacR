convolve.spectrum.velocity <-
  function(spectrum, p, LOSVD=c("gauss", "gh", "gauss.hermite"), nsig=8){

    LOSVD <- match.arg(LOSVD)

    z <- p[1]/c.light
    
    if (diff(range(diff(log10(spectrum$lambda)))) > 15*.Machine$double.eps){
      warning("Need logarithmically binned spectrum")
      return(NULL)
    }
    
    dloglambda <- median(diff(log10(spectrum$lambda)))
    dv <- c.light * dloglambda * log(10)

    p[1] <- p[1]/dv
    p[2] <- p[2]/dv
    if(LOSVD == "gauss"){
      p[3] <- 0
      p[4] <- 0
    }
    
    kern <- build.kernel(p, nsig)
    
    spectrum <- convolve.spectrum(spectrum, kern)

    ## Apply redshift simply to lambda array
#    spectrum$lambda <- spectrum$lambda*(1+z)
    
    return(spectrum)
  }

convolve.spectrum.Angstrom <-
  function(spectrum, sigma, nsig=8){

    if (diff(range(diff(spectrum$lambda))) > min(diff(spectrum$lambda))){
      warning ("Need linearly binned spectrum")
      return(NULL)
    }

    dlambda <- median(diff(spectrum$lambda))
    kern <- build.kernel(c(0, sigma/dlambda, 0, 0), nsig)

    spectrum <- convolve.spectrum(spectrum, kern)

    return(spectrum)
    
  }

convolve.spectrum <-
  function(spectrum, kern){
    
    if(length(kern$y) >= length(spectrum$value)){
      return(NA)
    }
    if (!is.null(spectrum$value)){
      spectrum$value <-  as.vector(filter(spectrum$value, kern$y))
    }
    if (!is.null(spectrum$contsub)){
      spectrum$contsub <-  as.vector(filter(spectrum$contsub, kern$y))
    }
    if (!is.null(spectrum$contdiv)){
      spectrum$contdiv <-  as.vector(filter(spectrum$contdiv, kern$y))
    }
    
    return(spectrum)
  }


    
