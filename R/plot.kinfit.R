plot.kinfit <- function(x, as.spectrum = FALSE, ...){

  ### If only the spectrum is desired, then we're done here
  if (as.spectrum){
    plot.spectrum(x)
    return( invisible())
  }
  
  good.resid <- !is.na(x$residuals)
  mask <- x$mask & good.resid

  value.range <- range(x$value[mask], na.rm=TRUE)
  resid.range <- 1.4*range(x$residuals[mask], na.rm=TRUE)

  if (any(is.na(value.range))){
    cat("Warning: Could not plot best fit\n")
  } else if (any(is.na(resid.range))){
    cat("Warning: Could not plot residuals\n")
  } else {
    
    par(mfrow=c(2,1))
        
    ## Spectrum and model
    plot(x$lambda[good.resid],
         x$value[good.resid],
         type = 'l',
         main = paste(x$object, "   ", x$template,
           "   sigma =", sprintf("%.1f", x$sigma),
           "   gof =", sprintf("%.3g", x$gof)),
         ylim = value.range, xlab="Wavelength", ylab="Rel. Flux")
        
    lines(x$lambda[good.resid],
          x$model[good.resid], col=2)
        
    ## Residual plot, mark masked pixels
    plot(x$lambda[good.resid],
         x$residuals[good.resid],
         type='l', main="Residuals", ylim=resid.range, xlab="Wavelength",
         ylab="Residuals")
    
    points(x$lambda[good.resid & !mask],
           x$residuals[good.resid & !mask], col=2, pch=20)
    
    abline(h=0, col=4)
    
    ## add the estimated noise spectrum
    lines(x$lambda[good.resid], x$noise[good.resid], col=5)
    par(mfrow=c(1,1))
  }
}
    

