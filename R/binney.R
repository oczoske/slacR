`lambdaR` <-
  function(kinmap, config, SN.lim=0, SN.plot=FALSE, Lcent=NULL, Mcent=NULL, title=NULL, errors=0, error.scale=NULL, v.err = NULL, sig.err = NULL, ...){

    ### Parameter check
    if (errors > 1 & is.null(error.scale) & (is.null(v.err) | is.null(sig.err))){
      cat("Need at least error.scale or v.err and sig.err\n")
      return()
    }
    
    ### The following function computes lambdaR. It requires the radial distance
    ### R to be present in the kinematic map, which is why I hide it. 
    compute.lambdaR <- function(kinmap, good){
      if (missing(good)){
        good <- rep(TRUE, length(kinmap$v))
      }
      numerator <- sum((abs(kinmap$v) * kinmap$SB * kinmap$R)[good], na.rm=TRUE)
      denominator <- sum((kinmap$SB*kinmap$R*sqrt(kinmap$v^2 + kinmap$sigma^2))[good], na.rm=TRUE)
      return(numerator/denominator)
    }
    

    configuration <- NULL
    
    ## Get center coordinates from config file unless given
    ## as parameters
    if (is.null(Lcent) | is.null(Mcent)){
      if (missing(config)){
        warning("Need either Lcent and Mcent or a configuration file")
        return()
      }
      source(config, local=TRUE)
      if (is.null(Lcent)){
        Lcent <- configuration$Lcent
      }
      if (is.null(Mcent)){
        Mcent <- configuration$Mcent
      }
    }
    
    ## Read kinmap
    ## Check whether kinmap is a file name, a kinmap or at least a dataframe
    if (is.character(kinmap)){
      if ( file.exists(kinmap)){
        kinmap <- prepare.kinmap(kinmap)
      } else {
        warning (paste(kinmap, "does not exist\n"))
        return()
      }
    }
    if (!is(kinmap, "kinmap")){
      if (is(kinmap, "data.frame")){
        class(kinmap) <- c("kinmap", class(kinmap))
      } else {
        warning (paste(kinmap, "is neither kinmap nor file name\n"))
        return()
      }
    } 
    
    ## Distance from center point
    kinmap$R <- sqrt((kinmap$L - Lcent)^2 + (kinmap$M - Mcent)^2)


    ## Switch according to whether we want the function to plot or to compute
    ## a particular value
    if (SN.plot){
      SN <- 0:25
      lR <- sapply(SN, function(x){
        lambdaR(kinmap, SN.lim=x, SN.plot=FALSE, Lcent=Lcent, Mcent=Mcent)
      })

      lR <- unlist(lR)
      
      plot(SN, lR, ylim=c(0, 1), #ylim=c(0, max(B, na.rm=TRUE)), 
           xlab=expression((italic(S/N))[lim]), 
           ylab=expression(italic(lambda[R]) ~~ bgroup("(", italic(S/N > (S/N)[plain(lim)]), ")")), ...)
      if (!is.null(title))
        mtext(title, line=-1, adj=0.9, cex=0.5)
      
      return(invisible())

    } else {

      ## Apply signal-to-noise limit
      good <- kinmap$SN > SN.lim
      
      ## Computation
      lambdaR <- compute.lambdaR(kinmap, good)
      
      lambdaR.err <- NA
      if (errors > 0){
        
        if (is.null(v.err)){
          v.err <- error.scale*(kinmap$dv.high - kinmap$dv.low)/2
        }
        if (is.null(sig.err)){
          sig.err <- error.scale*(kinmap$dsig.high - kinmap$dsig.low)/2
        }

        lambdaR.err <- sapply(1:errors, function(x){
          temp.kinmap <- kinmap
          temp.kinmap$v <- suppressWarnings(rnorm(length(temp.kinmap$v), temp.kinmap$v, v.err))
          temp.kinmap$sigma <- suppressWarnings(rnorm(length(temp.kinmap$sigma), temp.kinmap$sigma, sig.err))
          invisible(compute.lambdaR(temp.kinmap, good))
        })
      }
      return(list(value=lambdaR, error=lambdaR.err, v.err=v.err, sig.err=sig.err))
    }
  }




`binney` <-
  function(kinmap, SN.lim=0, SN.plot=FALSE, title = NULL, ...){

    ## check whether kinmap is a file name, a kinmap or at least a dataframe
    if (is.character(kinmap)){
      if ( file.exists(kinmap)){
        kinmap <- prepare.kinmap(kinmap)
      } else {
        warning (paste(kinmap, "does not exist\n"))
        return()
      }
    }
    if (!is(kinmap, "kinmap")){
      if (is(kinmap, "data.frame")){
        class(kinmap) <- c("kinmap", class(kinmap))
      } else {
        warning (paste(kinmap, "is neither kinmap nor file name\n"))
        return()
      }
    } 

    ## We assume that kinmap contains all the necessary components
    if (SN.plot){
      SN <- 0:25
      B <- sapply(SN, function(x){
        binney(kinmap, SN.lim=x, SN.plot=FALSE)})
      plot(SN, B, ylim=c(0, 1), #ylim=c(0, max(B, na.rm=TRUE)), 
           xlab=expression((italic(S/N))[lim]), 
           ylab=expression(frac(italic(v), italic(sigma)) ~~ bgroup("(", italic(S/N > (S/N)[plain(lim)]), ")")), ...)
      if (!is.null(title))
        mtext(title, line=-1, adj=0.9, cex=0.5)

      return(invisible())
    } else {
      good <- kinmap$SN > SN.lim
      kinmap$v <- kinmap$v - mean(kinmap$v[good], na.rm=TRUE)
      v2 <- sum((kinmap$v^2 * kinmap$SB)[good], na.rm=TRUE)
      sigma2 <- sum((kinmap$sigma^2 * kinmap$SB)[good], na.rm=TRUE)
      v.sig <- sqrt(v2/sigma2)
      ##    cat(paste("v/sigma =", v.sig, "    (uncorrected)\n"))
      return(v.sig)
    }
  }
