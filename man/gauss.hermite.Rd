\name{Gauss.Hermite}
\alias{gauss.hermite}
\title{ Function to compute the Gauss-Hermite series up to order 4 }
\description{
  This function returns the Gauss-Hermite series as defined by van der
  Marel & Franx (1993) up to order 4.
}
\usage{
gauss.hermite(x, x0, sigma, h3, h4, norm = TRUE)
}
\arguments{
  \item{x}{ numeric vector }
  \item{x0}{ mean of the Gaussian part of the Gauss-Hermite series }
  \item{sigma}{ sigma of the Gaussian part of the Gauss-Hermite series }
  \item{h3, h4}{ coefficients of the Hermite polynomials
    \code{\link{H.3}} and \code{\link{H.4}}  }
  \item{norm}{ logical; if \code{TRUE} (default), the Gauss-Hermite
    series is normalized to unity }
}
\details{
  The definition of the Gauss-Hermite series is that of van der Marel &
  Franx (1993) and might differ somewhat from other definitions.
}
\value{
  A numeric vector giving the values of the Gauss-Hermite series at \code{x}.
}
\references{van der Marel, R. P., Franx, M., Astroph. Journ. 407, 525 (1993) }
\author{ Oliver Czoske }
\seealso{ \code{\link{H.3}}, \code{\link{H.4}} }
\examples{
x <- seq(-500, 500, by=25)
y <- gauss.hermite(x, 0, 120, 0.1, 0.3)
plot(x, y, type='l')
}
\keyword{ math }
