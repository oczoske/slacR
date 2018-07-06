get.flux <- function(x){
  if (!any(class(x) == "spectrum")){
    warning("x must be a spectrum")
    return(NULL)
  }
  return(x$value)
}

get.noise <- function(x){
    if (!any(class(x) == "spectrum")){
        warning("x must be a spectrum")
        return(NULL)
    }
    return(x$noise)
}

get.lambda <- function(x){
  if (!any(class(x) == "spectrum")){
    warning("x must be a spectrum")
    return (NULL)
  }
  return(x$lambda)
}

get.mask <- function(x){
  if (!any(class(x) == "spectrum")){
    warning("x must be a spectrum")
    return (NULL)
  }
  return(x$mask)
}

spadd <- function(s1, s2){
  invisible( sarith(s1, s2, op="+") )
}

spsub <- function(s1, s2){
  invisible( sarith(s1, s2, op="-"))
}

spmult <- function(s1, s2){
  invisible( sarith(s1, s2, op="*"))
}

spdiv <- function(s1, s2){
  invisible( sarith(s1, s2, op="/"))
}

sarith <- function(s1, s2, op=NULL){
    if (!any(class(s1) == 'spectrum')){
        warning("First operand must be a spectrum")
        return(NULL)
    }
    
    res <- s1
    if (any(class(s2) == "spectrum")){
        if (any(s1$lambda != s2$lambda)){
            ## resample s2 to s1
            s2 <- resample.spectrum(s1, s2)
        }
        f2 <- get.flux(s2)
        n2 <- get.noise(s2)
        m2 <- get.mask(s2)
    } else {
        # if s2 is just a number, it has no noise
        f2 <- s2
        m2 <- TRUE
        if (any(op == c("+", "-")))
            n2 <- 0
    }

    ## perform the operation
    if (op == "+"){
        res$value <- s1$value + f2
        res$noise <- sqrt(s1$noise^2 + n2^2)
        res$mask <- s1$mask & m2
    } else if (op == "-"){
        res$value <- s1$value - f2
        res$noise <- sqrt(s1$noise^2 + n2^2)
        res$mask <- s1$mask & m2
    } else if (op == "*"){
        res$value <- s1$value * f2
        res$noise <- s1$noise * f2
        res$mask <- s1$mask & m2
    } else if (op == "/"){
        res$value <- s1$value / f2
        res$noise <- s1$noise / f2
        res$mask <- s1$mask & m2
    } else {
        warning("Unknown operation")
    }
    
    invisible(res)
}
