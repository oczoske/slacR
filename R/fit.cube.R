`fit.cube` <-
  function(config, galspec, template, LOSVD=c("gauss", "gh", "gauss-hermite"),  
           mask, output, errors=0, SN.lim=8, dv=25,
           verbose = TRUE, sigma.max=1000, normalize=FALSE, ...){
             
    LOSVD <- match.arg(LOSVD)
             
    if ( missing(config) &
        ( missing(galspec) | missing(template) | missing(mask) ) ){
      stop("Need configuration: Either file or list\n")
    }
        
    if (is.character(config)){
      source(config, local=TRUE)
    } else if (is.list(config)){
      configuration <- config
    } else {
      stop("config is not in recognisable form\n")
    }

#### PRINT SUMMARY OF CONFIG FILE
    
    ## Read data cube unless already done
    if (missing(galspec)){
      if (verbose){
        cat(paste("Prepare cube from", config, "\n"))
      }
      galspec <- prepare.cube(config=config, normalize=normalize, verbose=verbose)
    }
    
    ## Read template and determine interpolation function
    if (missing(template)){
      template <- prepare.IndoUS(configuration$template.file, dv=dv)
    }
  
    ## Build the mask
    if (missing(mask))
      mask <- configuration$mask.file
    
    mask <- convert.mask(galspec[[1]]$lambda, mask)
    
    
    ## Do the actual fit
    galfits <- lapply(galspec, function(x){
      cat(paste("Fitting spectrum ", x$num, ": ", x$object, "\n", sep=""))
      fit.spectrum(x, template=template,
                   lambda=configuration$lambda.stat,
                   mask=mask, SN.lim=SN.lim,
                   errors=errors, sigma.max=sigma.max,
                   LOSVD=LOSVD,
                   verbose=verbose, simple=TRUE, ... )
    }) 
    

    L.vec <-  sapply(galfits, function(x){x$L})
    M.vec <-  sapply(galfits, function(x){x$M})
    SN.vec <- sapply(galfits, function(x){x$SN})
    v.vec <-  sapply(galfits, function(x){x$v})
    sigma.vec <- sapply(galfits, function(x){x$sigma})
    gof.vec <- sapply(galfits, function(x){x$gof})
    if (errors != 0){
      dv.low.vec <- sapply(galfits, function(x){x$dv.low})
      dv.high.vec <- sapply(galfits, function(x){x$dv.high})
      dsig.low.vec <- sapply(galfits, function(x){x$dsig.low})
      dsig.high.vec <- sapply(galfits, function(x){x$dsig.high})
    }
    if (errors != 0){
      galfit.results <- data.frame(L=L.vec, M=M.vec, SN=SN.vec, v=v.vec, dv.low=dv.low.vec, dv.high=dv.high.vec, sigma=sigma.vec, dsig.low=dsig.low.vec, dsig.high=dsig.high.vec, gof=gof.vec)
    } else {
      galfit.results <- data.frame(L=L.vec, M=M.vec, SN=SN.vec, v=v.vec, sigma=sigma.vec, gof=gof.vec)
    }
    ## Reshape galfits
###    col.names <- names(galfits[[1]])
###    galfits <- as.data.frame(t(matrix(unlist(galfits), nrow=length(col.names))))
###    colnames(galfits) <- col.names
###    galfits$L <- L.vec
###    galfits$M <- M.vec

##    galfit.results <- data.frame(t(sapply(galfits, function(x){
##      c(x$L, x$M, x$SN, x$v, x$sigma, x$gof, x$h3, x$h4, x$v1, x$v2, x$dv.low, x$dv.high, x$dsig.low, x$dsig.high)})))
##    names(galfit.results) <- c("L", "M", "SN", "v", "sigma", "gof", "h3", "h4", "v1", "v2", "dv.low", "dv.high", "dsig.low", "dsig.high")
##    
    if (!missing(output)){
      cat(paste("Writing result to", output, "\n"))
      write.table(galfit.results, file=output, row.names=FALSE, quote=FALSE)
    }
#####     write.table(galfits, file=output, row.names=FALSE, quote=FALSE)
#####      write.table(data.frame(L=galfits$L, M=galfits$M, SN=galfits$SN,
#####                             v=galfits$v, dv.low=galfits$dv.low,
#####                             dv.high=galfits$dv.high, sigma=galfits$sigma,
#####                             dsig.low=galfits$dv.low,
#####                             dsig.high=galfits$dsig.high,
#####                             gof=galfits$gof, rms=galfits$rms),
#####                  file=output, row.names=FALSE, quote=FALSE)

##      
#####    system(paste("perl ", slacR.path, "/rearrange.pl /tmp/temp.Rdata > ",
#####                 output))
    galfit.results <- as.list(galfit.results)
    galfit.results$object <- configuration$object
    galfit.results$template <- template$object
    galfit.results$FeH <- template$FeH
    galfit.results$Teff <- template$Teff
    galfit.results$logg <- template$logg
    galfit.results$RV <- template$RV
    galfit.results$sptype <- template$sptype
    galfit.results$o.mult <- template$o.mult
    galfit.results$o.add <- template$o.add
    class(galfit.results) <- c("kinmap", "list")
    
    return(invisible(galfit.results))
#####    invisible(galfits)
  }

