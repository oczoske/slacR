`prepare.template` <-
  function(config, file, z.temp=0, lambda.lim=NULL, dv=25, 
           contsub=TRUE, contdiv = TRUE, order=16, smooth.kernel=NULL,
           log=TRUE, ...){
    
    if (!missing(config)){

      if (is.character(config)){
        source(config, local=TRUE)
      } else if (is.list(config)){
        configuration <- config
      } else {
        stop("config is not in recognisable format\n")
      }
      
      if (missing(file)){
        file <- configuration$template.file
      }
      if (missing(lambda.lim)){
        lambda.lim <- configuration$lambda.temp.lim
      }
    }

    ## Read in the template spectrum
    template <- prepare.spectrum(file, lambda.lim=lambda.lim,
                                 contsub=contsub, contdiv=contdiv,
                                 order=order, ...)
    
###    ## Linear interpolation on NAs
###    if (any(is.na(template$value))){
###      cat("Warning: template contains NAs, I'll fix them\n")
###      sapply(which(is.na(template$value)), function(x){
###        template$value[x] <- template$value[x-1]
###      })
###    }

    
    if (!is.null(smooth.kernel)){
      template <- convolve.spectrum.Angstrom(template, sigma=smooth.kernel)
    }
    
    if (log){
      ## If no limits are given use full range
      if (is.null(lambda.lim))
        lambda.lim <- range(template$lambda)
      
      ## Build spectrum
      lambda <- 10^seq(log10(lambda.lim[1]), log10(lambda.lim[2]),
                       by=dv/c.light/log(10))
      
      template <- resample.spectrum(template, lambda)
    }
    
    return(template)
  }

