`merit.function` <-
  function(p, galaxy, template, LOSVD=c("gauss", "gh", "gauss-hermite"),
           mask=TRUE, noise=NULL,         
           plot=FALSE, verbose=FALSE, return.fit=FALSE,
           sigma.max=1000, ...){

    LOSVD <- match.arg(LOSVD)
    
    ## Assign the parameters
    v <- p[1]
    sigma <- p[2]
    h3 <- p[3]
    h4 <- p[4]

    if (verbose){
      if (LOSVD == "gauss"){
        cat(paste("Working on v =", v, ", sigma =", sigma, "\n"))
      } else if (LOSVD == "gh" || LOSVD == "gauss.hermite"){
        cat(paste("Working on\n", "   v     =", v, "\n",
                  "   sigma =", sigma, "\n", "   h3    =", h3, "\n",
                  "   h4    =", h4, "\n"))
      }
    }

    ## Check validity of parameters
    if (sigma <= 0 || sigma > sigma.max){
      return(NA)
    }
    
    ## Prepare noise vector if specifically given
    if (!is.null(noise)){
      if (length(noise) == 1){
        galaxy$noise <- rep(noise, length(galaxy$lambda))
      } else {
        galaxy$noise <- noise
      }
    }
    
    ## AND in additional mask
    mask <- convert.mask(galaxy$lambda, mask)
    galaxy$mask <- galaxy$mask & mask
    
    ## Build the selected model
    galaxy <- build.model(template, galaxy, p, LOSVD=LOSVD, ...)
    
    if (is.logical(galaxy) && is.na(galaxy)){
      return(NA)
    }
    
    ## Calculation of goodness of fit
    good.resid <- !is.na(galaxy$residuals)
    if (LOSVD == "gauss"){
      npar <- 2
    } else {
      npar <- 4
    }
    ngood <- length(galaxy$residuals[good.resid & galaxy$mask]) -
      npar - length(coef(galaxy$lm.fit))
    
    galaxy$gof <- sum(galaxy$residuals[good.resid & galaxy$mask]^2 /
                      ngood / galaxy$noise[good.resid & galaxy$mask]^2,
                      na.rm=TRUE)
    
    ## rms of residuals, for comparison to input noise
    galaxy$rms <- sd(galaxy$residuals[galaxy$mask], na.rm=TRUE)
    
    ## Plot spectrum and best model if requested
    if (plot){
      plot(galaxy)
    }
    
    ## Output
    if (return.fit){
      return(galaxy)
    } else {
      return(galaxy$gof)
    }
  }

