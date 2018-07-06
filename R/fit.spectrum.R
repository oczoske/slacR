`fit.spectrum` <-
  function(galaxy, template, p0=c(0, 230, 0, 0),
           LOSVD=c("gauss", "gh", "gauss-hermite"), 
           noise=NULL, lambda=NULL,
           errors=0, mask=NULL, SN.lim=0,
           niter=3, nsig=3, verbose=FALSE, 
           plot=FALSE, plot.errors=FALSE,
           simple=FALSE, o.mult=4, o.add=6, ...){ 

##    if (verbose)
##      cat(paste("Fitting spectrum:", galaxy$name, "\n"))

    LOSVD <- match.arg(LOSVD)
    
    ## Prepare the mask if specifically given
    if (is.null(mask)){
      mask <- rep(TRUE, length(galaxy$lambda))
    } else {
      mask <- convert.mask(galaxy$lambda, mask)
    }
    galaxy$mask <- galaxy$mask & mask

    
    ## Prepare the polynomials
    p.mult <- cbind(rep(1, length(galaxy$lambda)),
                    poly(galaxy$lambda, o.mult))
    p.add <- cbind(rep(1, length(galaxy$lambda)),
                   poly(galaxy$lambda, o.add))
    

    ## Prepare noise vector if specifically given
    if (!is.null(noise)){
      if (length(noise) == 1){
        galaxy$noise <- rep(noise, length(galaxy$lambda))
      } else {
        galaxy$noise <- noise
      }
    }

    ## Some statistics
    galaxy$level <- sum(galaxy$values[galaxy$mask])
    galaxy$noise.level <- median(galaxy$noise[galaxy$mask], na.rm=TRUE)
    if (galaxy$noise.level == 0){
      galaxy$SN <- 0
    } else {
      galaxy$SN <- median(galaxy$value[galaxy$mask], na.rm=TRUE)/galaxy$noise.level
    }
    
    ## Do we have enough signal-to noise?
    if ( is.na(galaxy$SN) | (galaxy$SN < SN.lim) ){   ### ignore low-S/N spectra
      galaxy$rms <- NA
      galaxy$v <- NA
      galaxy$dv.low <- NA
      galaxy$dv.high <- NA
      galaxy$sigma <- NA
      galaxy$dsig.low <- NA
      galaxy$dsig.high <- NA
      galaxy$h3 <- NA
      galaxy$dh3.low <- NA
      galaxy$dh3.high <- NA
      galaxy$h4 <- NA
      galaxy$dh4.low <- NA
      galaxy$dh4.high <- NA
      galaxy$v1 <- NA
      galaxy$dv1.low <- NA
      galaxy$dv1.high <- NA
      galaxy$v2 <- NA
      galaxy$dv2.low <- NA
      galaxy$dv2.high <- NA
      galaxy$gof <- NA
      best.fit <- NA
    } else {                   ### the actual fit for the good spectra
      
      if (verbose)
        cat("Initial fit      ")
      
      fit <- optim(p0, merit.function,
                   galaxy=galaxy, template=template,
                   LOSVD=LOSVD,
                   p.add=p.add, p.mult=p.mult, ...)

      if (verbose){
        if (LOSVD == "gauss"){
          cat(paste("v = ", sprintf("%3.2f", fit$par[1]), "   ", "sigma = ",
                    sprintf("%4.2f", fit$par[2]), "   ", "gof = ",
                    sprintf("%5.2f", fit$value), "\n", sep=""));        
        } else if (LOSVD == "gh" || LOSVD == "gauss-hermite"){
          cat(paste("v = ", sprintf("%3.2f", fit$par[1]), "   ", "sigma = ",
                    sprintf("%4.2f", fit$par[2]), "   ", "h3 = ", fit$par[3],
                    "   ", "h4 = ", fit$par[4], "    ", "gof = ",
                    sprintf("%5.2f", fit$value), "\n", sep=""));        
        }
      }
      
      i <- 0
      while (i < niter){    ### iterative sigma-clipping 
        gof.fit <- merit.function(fit$par, galaxy=galaxy, template=template,
                                  LOSVD=LOSVD,
                                  p.add=p.add, p.mult=p.mult, 
                                  return.fit=TRUE,...)
        
        galaxy$mask <- (galaxy$mask &
                        (abs(gof.fit$residuals) <
                         nsig*sd(gof.fit$residuals[mask], na.rm=TRUE)))
        
        if (verbose)
          cat(paste("Iteration ", i+1, "/", niter, "    ", sep=""))
        
        fit <- optim(fit$par, merit.function,
                     galaxy=galaxy, template=template,
                     LOSVD=LOSVD, 
                     p.add=p.add, p.mult=p.mult, ...) 
        
        if (verbose){
          if (LOSVD == "gauss"){
            cat(paste("v = ", sprintf("%3.2f", fit$par[1]), "   ", "sigma = ",
                      sprintf("%4.2f", fit$par[2]), "   ", "gof = ",
                      sprintf("%5.2f", fit$value), "\n", sep=""));        
          } else if (LOSVD == "gh" || LOSVD == "gauss-hermite"){
            cat(paste("v = ", sprintf("%3.2f", fit$par[1]), "   ", "sigma = ",
                      sprintf("%4.2f", fit$par[2]), "   ", "h3 = ", fit$par[3],
                      "   ", "h4 = ", fit$par[4], "    ", "gof = ",
                      sprintf("%5.2f", fit$value), "\n", sep=""));        
          }
        }
         
        ## WHAT WAS THAT?
        if (fit$par[2] > 1){
          i <- i+1
        } else {
          i <- niter
        }
        
      }

      ## Assign the best fit parameters
      v <- fit$par[1]
      sigma <- fit$par[2]
      h3 <- ifelse (LOSVD == "gauss", 0, fit$par[3])
      h4 <- ifelse (LOSVD == "gauss", 0, fit$par[4])
      gof <- fit$value
      
      ## Determine the best model and plot if requested
      galaxy <- merit.function(c(v, sigma, h3, h4), galaxy=galaxy,
                               template=template,
                               LOSVD=LOSVD, plot=plot,
                               p.add=p.add, p.mult=p.mult, 
                               return.fit=TRUE, ...)
      ## velocity moments
      vel.moments <- GH.moments(v, sigma, h3, h4)
      v1 <- vel.moments[1]
      v2 <- vel.moments[2]

      galaxy$v1 <- v1
      galaxy$v2 <- v2
      
      if (verbose){
        cat("Fit results:\n")
        cat(paste("      v =", v, "\n"))
        cat(paste("  sigma =", sigma, "\n"))
        if (LOSVD == "gh" || LOSVD == "gauss-hermite"){
          cat(paste("     h3 =", h3, "\n"))
          cat(paste("     h4 =", h4, "\n"))
        }
        cat(paste("   chi2 =", gof, "\n\n"))
        cat(paste("     v1 =", v1, "\n"))
        cat(paste("     v2 =", v2, "\n\n"))
      }
      
       
      
      ## Determine the errors if requested
      if (errors > 0){
        
        ## Plot contours  NEED TO REWRITE gof.contours
        ##        if (plot.errors){
        ##          gof.contours(galaxy, template, lambda=lambda)
        ##        }

        ## Assign arrays 
        v.err <- numeric(errors)
        sig.err <- numeric(errors)
        h3.err <- numeric(errors)
        h4.err <- numeric(errors)
        gof.err <- numeric(errors)
        v1.err <- numeric(errors)
        v2.err <- numeric(errors)
        
        ## Perform the error analysis
        for (i in 1:errors){
          if (verbose){
            cat(paste("\rError run: ", i, "/", errors, sep=""))
          }
          model.err <-
            data.frame(lambda=galaxy$lambda,
                       value=galaxy$model + rnorm(length(galaxy$lambda), 0, galaxy$rms),
                       noise=galaxy$noise,
                       mask=galaxy$mask)
          class(model.err) <- c("spectrum", "list")
          
          fit.err <- optim(c(v, sigma, h3, h4),
                           merit.function,
                           galaxy=model.err,
                           template=template,
                           LOSVD=LOSVD,
                           p.add=p.add, p.mult=p.mult, ...)
          gof.err[i] <- fit.err$value
          v.err[i] <- fit.err$par[1]
          sig.err[i] <- fit.err$par[2]
          h3.err[i] <- ifelse(LOSVD=="gauss", 0, fit.err$par[3])
          h4.err[i] <- ifelse(LOSVD=="gauss", 0, fit.err$par[4])

          moments <- GH.moments(v.err[i], sig.err[i], h3.err[i], h4.err[i])
          v1.err[i] <- moments[1]
          v2.err[i] <- moments[2]
          
          if (plot.errors){
            points(v.err[i], sig.err[i], col=4, pch=20)
          }
        }
        if (verbose & errors > 0)
          cat("\n")

        
        v.limits <- quantile(v.err, probs=c(0.16, 0.84))
        galaxy$v.err <- v.err
        galaxy$dv.low <- v.limits[1]-v
        galaxy$dv.high <- v.limits[2]-v

        sig.limits <- quantile(sig.err, probs=c(0.16, 0.84))
        galaxy$sig.err <- sig.err
        galaxy$dsig.low <- sig.limits[1] - sigma
        galaxy$dsig.high <- sig.limits[2] - sigma

        galaxy$gof.err <- gof.err

        h3.limits <- quantile(h3.err, probs=c(0.16, 0.84))
        galaxy$h3.err <- h3.err
        galaxy$dh3.low <- h3.limits[1]-h3
        galaxy$dh3.high <- h3.limits[2]-h3

        h4.limits <- quantile(h4.err, probs=c(0.16, 0.84))
        galaxy$h4.err <- h4.err
        galaxy$dh4.low <- h4.limits[1]-h4
        galaxy$dh4.high <- h4.limits[2]-h4

        v1.limits <- quantile(v1.err, probs=c(0.16, 0.84))
        galaxy$v1.err <- v1.err
        galaxy$dv1.low <- v1.limits[1] - v1
        galaxy$dv1.high <- v1.limits[2] - v1

        v2.limits <- quantile(v2.err, probs=c(0.16, 0.84))
        galaxy$v2.err <- v2.err
        galaxy$dv2.low <- v2.limits[1] - v2
        galaxy$dv2.high <- v2.limits[2] - v2

        ### Print results
        cat(paste("Results from", errors, "error runs:\n"))
        cat(paste("     v =", sprintf("% 7.2f %+6.2f %+6.2f\n", galaxy$v, galaxy$dv.high, galaxy$dv.low)))
        cat(paste(" sigma =", sprintf("% 7.2f %+6.2f %+6.2f\n", galaxy$sigma, galaxy$dsig.high, galaxy$dsig.low)))
        cat(paste("    h3 =", sprintf("% 7.4f %+6.4f %+6.4f\n", galaxy$h3, galaxy$dh3.high, galaxy$dh3.low)))
        cat(paste("    h4 =", sprintf("% 7.4f %+6.4f %+6.4f\n", galaxy$h4, galaxy$dh4.high, galaxy$dh4.low)))
        cat(paste("    v1 =", sprintf("% 7.2f %+6.2f %+6.2f\n", galaxy$v1, galaxy$dv1.high, galaxy$dv1.low)))
        cat(paste("    v2 =", sprintf("% 7.2f %+6.2f %+6.2f\n", galaxy$v2, galaxy$dv2.high, galaxy$dv2.low)))
        
      }
    }  

    ## Return
    invisible(galaxy)
  }

