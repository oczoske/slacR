\name{sarith}
\alias{sarith}
\alias{spadd}
\alias{spsub}
\alias{spmult}
\alias{spdiv}

\title{ Arithmetic operations on spectra }
\description{
  The functions perform the elementary arithmetic operations on the flux
  values of spectra. If \code{s2} is a spectrum sampled on a different
  wavelength vector than \code{s1}, it is first resampled. If \code{s2}
  is a numeric vector, it must have the same length as \code{s1}. 
}
\usage{
spadd(s1, s2)
spsub(s1, s2)
spmult(s1, s2)
spdiv(s1, s2)
sarith(s1, s2, op)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s1}{ object of class \code{spectrum} with components
    \code{lambda}, \code{value} and optionally \code{noise},
    \code{contsub} and \code{mask}}
  \item{s2}{object of class \code{spectrum}, a number or a numerical
    vector of the same length as the \code{value} component of
    \code{s1}}
  \item{op}{One of '+', '-', '*', '/'. }
}
\value{
  an object of class \code{spectrum}, sampled on the same wavelength
  vector as \code{s1}

  When adding or subtracting spectra, their \code{noise} vectors are
  added quadratically. If \code{s2} is a number, the \code{noise} vector
  of \code{s1} is unchanged.

  When multiplying or dividing spectra, the noise vector of \code{s1} is
  scaled by the value of \code{s2}. This is appropriate if \code{s2} is
  some wavelength-dependent scaling (like a transmission curve), but it
  is not fully general!  
}
\author{ Oliver Czoske }

\seealso{}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
