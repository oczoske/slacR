\name{Hermite}
\alias{H.3}
\alias{H.4}
\title{ Hermite Polynomials }
\description{
  These functions give the Hermite polynomials of order 3 and 4
  according to the (non-standard) definition of van der Marel & Franx
  (1993). 
}
\usage{
H.3(x)
H.4(x)
}
\arguments{
  \item{x}{ numeric vector }
}
\details{
  The Hermite polynomials computed are

  \eqn{H_3(x) = \frac{1}{\sqrt{3}} (2 x^3 - 3 x)}

  \eqn{H_4(x) = \frac{1}{sqrt{24}} (4 x^4 - 12 x^2 + 3 )}
}
\references{ van der Marel, R. P., Franx, M., Astroph. Journ. 407, 525 (1993)  }
\author{ Oliver Czoske }
\seealso{ \code{\link{gauss.hermite}} }
\examples{
x <- seq(-3, 3, by=0.1)
h3 <- H.3(x)
h4 <- H.4(x)
plot(x, h3, type='l', ylim=range(c(h3, h4)))
lines(x, h4)
}

\keyword{ math }
