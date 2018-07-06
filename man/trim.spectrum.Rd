\name{trim.spectrum}
\alias{trim.spectrum}

\title{ Trim Spectrum to Specified Wavelength Range }
\description{
  Cuts a spectified wavelength range from a spectrum. This is useful for
  fitting restricted wavelength ranges in a reasonable time.
}
\usage{
trim.spectrum(x, lambda)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ object of class \code{spectrum} with components
    \code{lambda}, \code{value} and optionally \code{noise},
    \code{contsub} and \code{mask}}
  \item{lambda}{ two-component vector giving wavelength range (in
    Angstroms) to be cut from the spectrum \code{x} }
}
\value{
  an object of class \code{spectrum}
}
\author{ Oliver Czoske }

\seealso{ \code{\link{prepare.spectrum}},
  \code{\link{prepare.template}}, \code{\link{prepare.IndoUS}} }
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
