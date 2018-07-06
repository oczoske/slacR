`prepare.cube` <-
  function(datacube, config=NULL, extension=0, input.table=NULL, noisecube=NULL,
           mask=NULL, z=NULL, lambda.lim=NULL, verbose = FALSE, save.cube=FALSE,
           object=NULL, ...){ 

    configuration <- NULL
    
### Check the configuration
    if ( !missing(config) & !is.null(config)) {
      
      ## config can be the name of a file or a configuration list
      if(is.character(config)){
        source(config, local=TRUE)
      } else if (is.list(config)){
        configuration <- config
      } else {
        stop ("config is not in recognisable format\n")
      }
      
    } else if ( missing(datacube) | missing(input.table) ) {
      stop ("No configuration: need at least datacube and input.table\n")
    } else {
      configuration <- NULL
    }
    

### Parameters are taken from config, from arguments or set to defaults

    ## file containing the data cube
    if ( missing(datacube) ){
      datacube <- configuration$data.cube
    }
    
    ## file containing the noise cube (not required)
    if (is.null(noisecube) & !is.null(configuration$noise.cube)){
      noisecube <- configuration$noise.cube
    }

    ## file containing the mask
    if (is.null(mask)){
      mask <- configuration$mask
    }
    
    ## the table of input fibres
    if(is.null(input.table)){
      input.table <- configuration$input.tab
    }
    
    ## the wavelength range to be used
    if (is.null(lambda.lim)){
      if (!is.null(configuration$lambda.spec.lim)){
        lambda.lim <- configuration$lambda.spec.lim
      } else {      # default value
        lambda.lim <- c(-999999, 999999)
      }
    }
    
    ## the redshift of the galaxy
    if (is.null(z)){
      if (!is.null(configuration$z.lens)){
        z <- configuration$z.lens
      } else {      # default value
        z <- 0
      }
    }
    
    if (is.null(object) & !is.null(configuration$object))
      object <- configuration$object
    
    
    ## read and convert input table if necessary
    if (is.data.frame(input.table)){
      intab <- input.table
    } else {
      intab <- read.table(input.table, header=TRUE)
    }


    if (verbose){
      cat(paste("Data cube:   ", datacube, "\n"))
      cat(paste("Noise cube:  ", ifelse(is.null(noisecube), "none", noisecube),
                "\n"))
      cat(paste("Redshift:    ", z, "\n"))
      cat(paste("Lambda range:", lambda.lim[1], "to", lambda.lim[2], "\n"))
    }
      
    ## read in the data cube along with the noise cube
    galspec <- apply(intab, 1, function(x){
      
      line <- paste(datacube, "'[", extension, "][*,", x["F"], ":", x["F"], "]'", sep="")
      
      noiseline <-
        ifelse(is.null(noisecube), "",
               paste(noisecube,
                     "'[", extension, "][*,", x["F"], ":", x["F"], "]'",
                     sep=""))
      
      if (!is.null(object))
        obj.string <- paste(object, "L =", x["L"], " M =", x["M"])

      if (verbose)
        cat(paste("Reading", line, "and", noiseline, "\n"))
      galaxy <- prepare.spectrum(line, noise=noiseline, mask=mask,
                                 order=order, z=z, lambda.lim=lambda.lim,
                                 verbose=verbose, 
                                 object=obj.string, ...)
      
      galaxy$L <- x["L"]
      galaxy$M <- x["M"]
      galaxy$F <- x["F"]
      galaxy$num <- x["num"]

      galaxy
    })

    ## Save R representation of 
    if (!(is.logical(save.cube) & !save.cube)){
      object <- sub("SDSS-", "", object)
      if (is.character(save.cube)){
        save.file <- save.cube
      } else {
        save.file <- paste(object, "-cube.Rdata", sep="")
      }
      cube.name <- paste(object, ".cube", sep="")
      assign(cube.name, galspec)
      save(list=cube.name, file=save.file)
    }
    
    ## Done
    return(galspec)
  }



