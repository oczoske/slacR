\name{Constants}
\alias{c.light}
\title{Constants}
\description{
  Constants defined by \code{slacR}
}
\usage{
c.light
}
\details{
  Currently, only the speed of light is defined.

  \itemize{
    \item \code{c.light}: The speed of light in km/s.
  }
}
\examples{
z1 <- 0.391
z2 <- 0.395

dv <- c.light * (z2 - z1)/(1 + mean(c(z1, z2)))
dv
}
\keyword{constants}
  
