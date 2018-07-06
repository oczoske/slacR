gauss.fit <- function(spec, lambda=range(spec$lambda),
                      continuum='flat', p0, plot=TRUE){

    ## Define the models (outside function?)
    gauss.nobg <- function(p, spec){
#        ymod <- p['A'] * dnorm(spec$lambda, p['mu'], p['sig'])
        ymod <- p['A'] * exp(-(spec$lambda - p['mu'])^2/(2*p['sig2']))
        return( ymod )
    }

    gauss.flatbg <- function(p, spec){
        ymod <- gauss.nobg(p, spec) + p['B']
        return( ymod)
    }

    gauss.linbg <- function(p, spec){
        x0 <- mean(range(lambda))
        ymod <- gauss.nobg(p, spec) + p['a'] + p['b'] * (spec$lambda - x0)
        return( ymod)
    }

    gauss.gof <- function(p, spec, gauss.mod){
        return(sum((tspec$value - gauss.mod(p, spec))^2))
    }
    
    ## Cut spectrum to desired range
    tspec <- trim.spectrum(spec, lambda)

    ## starting values
    if (continuum == 'zero'){
        gauss.mod <- gauss.nobg
        if (missing(p0)){
            p0 = c(mu=tspec$lambda[which.max(tspec$value)],
                sig2=3^2, A=diff(range(tspec$value)))
        }
    } else if (continuum == 'flat'){
        gauss.mod <- gauss.flatbg
        if (missing(p0)){
            p0 <- c(mu=tspec$lambda[which.max(tspec$value)],
                    sig2=3^2, A=diff(range((tspec$value))),
                    B=min(tspec$value))
        }
    } else if (continuum == 'linear'){
        gauss.mod <- gauss.linbg
        if (missing(p0)){
            p0 = c(mu=tspec$lambda[which.max(tspec$value)],
                sig2=3^2, A=max(diff(range(tspec$value))),
                a=min(tspec$value), b=0.)
        }
    }
    
        
    ## Do the optimization
    if (plot){
        plot(tspec)
        lines(tspec$lambda, gauss.mod(p0, tspec), col=2)
    }
    Opt <- optim(p0, gauss.gof, spec=tspec, gauss.mod=gauss.mod)

    if (plot){
        lines(tspec$lambda, gauss.mod(Opt$par, tspec), col=3)
    }
    
    return(Opt)
}
