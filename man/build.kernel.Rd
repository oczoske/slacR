\name{build.kernel}
\alias{build.kernel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Build a Gaussian-Hermite kernel for convolution of a template spectrum}
\description{
  This function builds a Gauss-Hermite kernel of order 4. A Gaussian
  kernel is obtained by setting \code{p[3]=0} and \code{p[4]=0}.
}
\usage{
  build.kernel(p, nsig = 8)
}
\arguments{
  \item{p}{ vector of parameters v, sigma, h3, h4 of the Gauss-Hermite
    series to be built. }
  \item{nsig}{ One-sided extent of the Gaussian in units of standard deviation}
}
\details{
  The parameters \code{p[1]} and \code{p[2]} are expected to be
  in units of pixels, hence the physical values have to be divided by
  the velocity step \code{dv} before calling \code{build.kernel}.  }
\value{
  A data.frame with components
  \item{x }{pixel values at which the Gaussian is evaluated, centred on 0.}
  \item{y }{the values of the Gaussian kernel}
}
\author{ Oliver Czoske }
\seealso{ \code{\link{convolve.spectrum}}}
\examples{
  sigma <- 300
  dv <- 50
  nsig <- 8
  kern <- build.kernel(c(0, sigma/dv, 0, 0), nsig)
  plot(kern, type='l')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ datagen }
