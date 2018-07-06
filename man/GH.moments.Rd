\name{GH.moments}
\alias{GH.moments}
\title{ Velocity moments of a Gauss-Hermite series }
\description{
  This function computes the first and second velocity moments of a
  Gauss-Hermite series given by the parameters \code{x0}, \code{sigma},
  \code{h3} and \code{h4}.
}
\usage{
GH.moments(x0, sigma, h3, h4)
}
\arguments{
  \item{x0, sigma, h3, h4}{Parameters of the Gauss-Hermite series, as
    defined by van der Marel & Franx (1993) }
}
\value{
  A two-component vector containing the velocity moments. The second
  moment is returned as \code{NA} if the variance is negative.
}
\references{ van der Marel, R. P., Franx, M., Astroph. Journ. 407, 525
  (1993)  }
\author{ Oliver Czoske }
\seealso{ \code{\link{gauss.hermite}} }
\examples{
GH.moments(0, 230, 0.01, -0.02)
  }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
