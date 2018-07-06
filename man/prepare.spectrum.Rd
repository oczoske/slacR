\name{prepare.spectrum}
\alias{prepare.spectrum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Read and prepare a one-dimensional spectrum}
\description{
  Read a one-dimensional spectrum from a fits file or ASCII data table
  and prepare for the kinematic analysis
}
\usage{
prepare.spectrum(file, config = NULL, noise = NULL, mask = NULL,
  order = 16,z = NULL, lambda.lim = NULL, normalize = FALSE,
  contsub = FALSE, contdiv = FALSE, verbose = TRUE, object = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{ name of the fits file containing the
    spectrum. Filters are allowed, as understood by cfitsio.}
  \item{config}{ name of a configuration file. Specifying this is enough
    to read in a spectrum. Parameters than can be overridden explicitely
    are \code{file} (\code{configuration$spectrum}), \code{noise}
    (\code{configuration$noise.1d}), \code{z} (\code{configuration$z.lens}),
    \code{lambda.lim} (\code{configuration$lambda.spec.lim}),
    \code{object} (\code{configuration$object}).}
  \item{noise}{ name of a fits file containing the noise spectrum, or a
    noise \code{spectrum}, or a numeric vector containing the noise
    spectrum or a number giving the noise level}
  \item{mask}{ a mask in any format that can be read and converted by
    \code{\link{convert.mask}}. Most of the time, this will be a file or
    data.frame with components \code{x1} and \code{x2} giving the left
    and right limits of the wavelength regions to be masked.}
  \item{order}{ order of a polynomial fit to the continuum}
  \item{z}{ redshift of the galaxy. Defaults to 0. }
  \item{lambda.lim}{ wavelength range to be cut from the data cube. If
    \code{NULL}, use the entire range. }
  \item{normalize}{ logical. If TRUE, the spectrum is normalized to mean of
    unity over the range defined by \code{lambda.lim}}
  \item{contsub}{ logical. if TRUE a polynomial fit to the continuum
    will be subtracted from the spectrum }
  \item{contdiv}{ logical. If TRUE, the spectrum will be divided by a
    polynomial fit to the continuum}
  \item{verbose}{ logical. If TRUE, give some progress report.}
  \item{object}{ an optional name of the object }
}
\details{
  A one-dimensional spectrum is read from a file and returned in
  the form of a list. Optionally, a noise vector is added. The spectrum
  is deredshifted and optionally a polynomial fit to the continuum is
  subtracted from the spectrum. The input file can be either a fits file
  or an ASCII file in the format of a data table with at least columns
  \code{lambda} and \code{value}. 
}
\value{
  a list containing the following elements:
  \item{lambda}{the wavelength values of the deredshifted spectrum}
  \item{value}{the original values of the spectrum}
  \item{noise}{the estimated noise for each wavelength pixel}
  \item{mask}{a logical vector with \code{TRUE} for good and
    \code{FALSE} for bad pixels}
  \item{contsub}{(optional) the values of the spectrum after subtraction
    of a continuum fit}
  \item{contdiv}{(optional) the values of the spectrum after division by
    a continuum fit}
  \item{name}{the name of the input fits file}
  \item{object}{the name of the object}
}
\author{Oliver Czoske}
\seealso{\code{\link{prepare.template}}, \code{link{prepare.cube}}}
\keyword{file}
