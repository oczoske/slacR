\name{convolve.spectrum}
\alias{convolve.spectrum}
\alias{convolve.spectrum.velocity}
\alias{convolve.spectrum.Angstrom}
\title{ Convolve a spectrum with a given kernel }
\description{
  The generic function \code{convolve.spectrum} convolves
  \code{spectrum} with a given kernel \code{kern}. The more
  user-friendly functions \code{convolve.spectrum.Angstrom} and
  \code{convolve.spectrum.velocity} perform convolutions with Gaussian
  and Gauss-Hermite kernels in linear and logarithmic (or velocity)
  wavelength space, respectively. 
}
\usage{
  convolve.spectrum(spectrum, kern)
  convolve.spectrum.velocity(spectrum, p, LOSVD=c("gauss", "gh",
       "gauss.hermite"), nsig=8)
  convolve.spectrum.Angstrom(spectrum, sigma, nsig=8)
}
\arguments{
  \item{spectrum}{ a data frame with columns \code{lambda}, \code{value}
    and optionally \code{contsub} containing a spectrum. The spectrum
    has to be sampled on a logarithmic wavelength grid.}
  \item{kern}{ a dataframe with components \code{x} and \code{y}
    specifying a kernel used for the convolution. A Gaussian kernel can
    be built using \code{\link{build.kernel}}}
  \item{p}{ a vector of parameters for building a Gauss-Hermite
    kernel. If \code{LOSVD=="gauss"}, \code{p[3]} and \code{p[4]} are
    ignored and set to zero. \code{p[1]} and \code{p[2]} are expected in
    physical units, km/s. }
  \item{LOSVD}{Shape of the LOSVD to be fit to the spectrum. Defaults to
    Gaussian but can also be set to a Gauss-Hermite series, to order 4. }
  \item{sigma}{the dispersion of the Gaussian kernel. For
    \code{convolve.spectrum.Angstrom} this is in the same units as
    \code{spectrum$lambda} (e.g. Angstrom).}
  \item{nsig}{ the extent of the Gaussian or Gauss-Hermite kernel in
    terms of the parameter \code{sigma} or \code{p[2]}.}
}
\details{
  The function \code{convolve.spectrum} is primarily meant to be called
  by \code{\link{convolve.spectrum.velocity}} and 
  \code{\link{convolve.spectrum.Angstrom}}, which include building the
  respective kernel.
}
\value{
  An object of class \code{spectrum} with the same components as the
  input spectrum.
}
\seealso{ \code{\link{build.kernel}}}
\author{ Oliver Czoske }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ datagen }
