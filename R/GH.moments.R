GH.moments <- function(x0, sigma, h3, h4){

  f.1 <- function(x, x0, sigma, h3, h4){
    (x) * gauss.hermite(x, x0, sigma, h3, h4, norm=TRUE)
  }

  f.2 <- function(x, xc, x0, sigma, h3, h4){
    (x-xc)^2 * gauss.hermite(x, x0, sigma, h3, h4, norm=TRUE)
  }

  ##  m1 <- tryCatch(integrate(f.1, -Inf, Inf, x0, sigma, h3, h4), NA)
  m1 <- integrate(f.1, -Inf, Inf, x0, sigma, h3, h4)
  if (m1$message == "OK"){
      m2 <- integrate(f.2, -Inf, Inf, m1$value, x0, sigma, h3, h4)

      if (m2$value >=0){
          return(c(m1$value, sqrt(m2$value)))
      } else {
          warning("Negative variance.")
          return(c(m1$value, NA))
      }
  } else {
      return(c(NA, NA))
  }
}
