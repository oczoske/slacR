`prepare.spectrum` <-
  function(file, config=NULL, noise=NULL, mask=NULL, order=16, z=NULL,
           lambda.lim=NULL, normalize=FALSE, contsub=FALSE, contdiv=FALSE,
           verbose=TRUE, object=""){

    configuration <- NULL
    
    if (!is.null(config)){
      source(config, local=TRUE)
      if (missing(file))
        file <- configuration$spectrum
      if (is.null(noise))
        noise <- configuration$noise.1d
      if (is.null(mask))
        mask <- configuration$mask.file
      if (is.null(z))
        z <- configuration$z.lens
      if (is.null(lambda.lim))
        lambda.lim <- configuration$lambda.spec.lim
      if (object == "")
        object <- configuration$object
    }
    
    if (verbose)
      cat(paste("Spectrum:", file, "\n"))

    ## Test if spectrum is a fits file
    ## file may contain extensions which need to be split off
    file.base <- strsplit(file, "'\\[")[[1]]
    fits <- ifelse (length(system(paste("file", file.base, "| grep FITS"), intern=TRUE)) == 1, TRUE, FALSE)

    ## Read in the spectrum
    if (fits){
      cat(paste("fits2R ", file, "\n"))
      spectrum <- as.list(read.table(pipe(paste("fits2R ", file)), header=TRUE, as.is=TRUE))
    } else {
      spectrum <- as.list(read.table(file, header = TRUE))
    }
    if (!is.null(spectrum$Error.)){
      cat("Error: No spectrum read.\n")
      return(NULL)
    }
    ## cast to class spectrum
    class(spectrum) <- c("spectrum", "list")

    ## This line appears to be necessary in R 3.1.0. ???
    spectrum$lambda <- as.numeric(spectrum$lambda)
    
    spectrum$object <- object
    spectrum$name <- file
    spectrum$z <- z

    ## Default redshift
    if (is.null(z))
      z <- 0
    
    if (z != 0){
      if (verbose)
        cat(paste("Deredshifting spectrum: z =", z, "\n"))
      spectrum$lambda <- spectrum$lambda/(1+z)
    }
    
    ## Read in the noise if necessary
    if (is.null(noise)){
      spectrum$noise <- NULL
    } else {

      if (is.character(noise)){
        if (verbose)
          cat(paste("Noise:", noise, "\n"))
        
        noise.spectrum <- as.list(read.table(pipe(paste("fits2R ", noise)),
                                             header=TRUE))
        spectrum$noise <- noise.spectrum$value
        
      } else if (is(noise, "spectrum")){   # Check on identity of lambda
                                           # would be okay...
        spectrum$noise <- noise$value
        
      } else if (is.numeric(noise)){
        
        if (length(noise) == 1){
          spectrum$noise <- rep(noise, length(spectrum$lambda))
        } else {
          spectrum$noise <- noise
        }
        
      }
      ## had problems with noise=0 in SDSS spectra - check for this
      spectrum$noise[spectrum$noise==0] <- 100
    }

    ## Read mask
    if (is.null(mask)){
      spectrum$mask <- rep(TRUE, length(spectrum$lambda))
    } else {
      if (verbose & is.character(mask))
        cat(paste("Mask:", mask, "\n"))
      spectrum$mask <- convert.mask(spectrum, mask)
    }

    ## Cut by restframe wavelength
    if (!is.null(lambda.lim))
      spectrum <- trim.spectrum(spectrum, lambda.lim)
    

    ## Normalize the spectrum if required
    if (is.logical(normalize)){
      if (normalize){
        norm <- mean(spectrum$value, na.rm=TRUE)
        spectrum$value <- spectrum$value/norm
        spectrum$noise <- spectrum$noise/norm
      }
    } else if (is.numeric(normalize)){
      spectrum$value <- spectrum$value/normalize
      spectrum$noise <- spectrum$noise/normalize
    } else {
      cat (normalize)
      warning ("Don't know what to do with normalize\n")
    }
        
    ## optionally add continuum subtracted and/or divided spectra
    if (contsub | contdiv){
      
      ## simple polynomial fit to the continuum
      cont.fit <- lm(value ~ poly(lambda, order), data=spectrum)
      
      if (contsub){
        spectrum$contsub <- residuals(cont.fit)
      }
      if (contdiv){
        spectrum$contdiv <- spectrum$value/cont.fit$fitted.values
      }
    }
    
    return(spectrum)
  }

