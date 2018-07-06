\name{prepare.template}
\alias{prepare.template}
\title{Prepare a template spectrum for kinematic analysis}
\description{
Read in a template spectrum from a fits file and prepare for the
kinematic analysis of a galaxy spectrum.
}
\usage{
prepare.template(config, file, z.temp = 0, lambda.lim=NULL, dv = 25,
                 contsub = TRUE, contdiv = TRUE, order = 16,
                 smooth.kernel = NULL, log = TRUE, ...)
}
\arguments{
  \item{config}{name of a file or list containing relevant data on the 
    object on which the kinematic analysis is to be performed}
  \item{file}{fits file containing the template spectrum. Filters are
    allowed as understood by cfitsio}
  \item{z.temp}{redshift of the template (mostly 0)}
  \item{lambda.lim}{wavelength range to be cut from the spectrum. If
    \code{NULL} use the entire range.}
  \item{dv}{velocity step onto which the template spectrum will be
    rebinned if \code{log == TRUE}} 
  \item{contsub}{logical. If TRUE a polynomial fit to the continuum will
    be subtracted}
  \item{contdiv}{logical. If TRUE, the spectrum will be divided by a
    polynomial fit to the continuum}
  \item{order}{order of polynomial fit to the continuum}
  \item{smooth.kernel}{numeric. If non-NULL, convolve the spectrum with a
    Gaussian with sigma \code{smooth.kernel}. This only works correctly if
    the spectrum is natively sampled on a linear wavelength scale.}
  \item{log}{logical. If TRUE, the template is resampled on to a
    logarithmic wavelength scale with velocity step \code{dv}
    (default).}
  \item{...}{further arguments to be passed to
    \code{\link{prepare.spectrum}} (especially normalize)}
}
\details{
  Read in a spectrum from a fits file to be used as the template in the
  kinematic analysis of a galaxy spectrum. Optionally, a polynomial fit
  to the continuum will be subtracted. If the redshift of the template
  is not zero, deredshift the template spectrum. The template spectrum
  is by default rebinned onto a logarithmic wavelength grid with
  constant velocity step \code{dv}. 
}
\value{
  a dataframe containing the following components:
  \item{lambda }{logarithmic wavelength grid}
  \item{name }{name of fits file from which template was read}
  \item{value }{value of the spectrum interpolated to the logarithmic
    wavelength grid}
  \item{contsub }{value of the spectrum after continuum subtraction}
}
\author{Oliver Czoske}
\seealso{\code{\link{prepare.spectrum}}, \code{\link{prepare.cube}}}
\examples{
}
\keyword{file}

