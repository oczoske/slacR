`build.kernel` <-
function(p, nsig=8){
  N <- 2*floor(nsig*p[2]) + 1
  x <- ((-(N-1)/2):((N-1)/2))
                                        #  y <- dnorm(x, 0, sigma.pix)
  y <- gauss.hermite(x, p[1], p[2], p[3], p[4], norm=TRUE)
  return(data.frame(x=x, y=y))
}

