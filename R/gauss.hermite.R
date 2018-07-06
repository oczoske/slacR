gauss.hermite <- function(x, x0, sigma, h3, h4, norm=TRUE){
  y <- (x-x0)/sigma
  f <- function(x){ exp(-x^2/2) * (1 + h3*H.3(x) + h4*H.4(x))}
  f.y <- f(y)
  if (norm){
    f.norm <- integrate(f, -Inf, Inf)
    if (f.norm$value != 0)
      f.y <- 1/sigma * f.y/f.norm$value
  } else {
    f.y <- f.y/sigma
  }
  f.y
}

H.3 <- function(x){
  1/sqrt(3) * ( 2 * x^3 - 3 * x)
}

H.4 <- function(x){
  1/sqrt(24) * (4*x^4 - 12 * x^2 + 3)
}
