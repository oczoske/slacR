\name{prepare.ELODIE}
\alias{prepare.ELODIE}

\title{ Read an ELODIE Spectrum as Template }
\description{
  This function provides a wrapper to \code{\link{prepare.template}}
  specifically geared to ELODIE spectra.
}
\usage{
prepare.ELODIE(ident, config.file = NULL, path = NULL, type="H",
               lambda.lim = NULL, resolution.target=NULL,
               contdiv=FALSE, contsub=FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ident}{ character string identifying an ELODIE spectrum. This
    is the basename of the corresponding fits file. }
  \item{config.file}{ name of a configuration file. This can be used to
    specify a wavelength range to be used with a given target galaxy.  }
  \item{path}{ optional character string to specify non-standard path to
    the ELODIE library}
  \item{type}{Choose high ("H") or low ("L") resolution version of
    ELODIE spectra. The high-resolution spectra have R=42,000 and a
    normalized ot the continuum. The low-resolution spectra have
    R=10,000 and are flux-calibrated.}
  \item{lambda.lim}{ two-component vector giving the wavelength range to
    be read from the template spectrum. If \code{NULL}, the entire
    spectrum is read.}
  \item{resolution.target }{Instrumental resolution (sigma!) of the
    spectrum that is to be fitted.}
  \item{contdiv}{logical. If \code{TRUE}, the spectrum will be divided
    by a polynomial fit to the continuum. Since the ELODIE spectra are 
    normalised to the continuum, this is set to \code{FALSE} by default.}
  \item{contsub}{logical. If TRUE a polynomial fit to the continuum will
    be subtracted. Since the ELODIE spectra are normalised to the
    continuum, this could be done by subtracting 1 and the parameter is
    set to \code{FALSE} by default.} 
  \item{\dots}{ Further parameters to be passed to
    \code{\link{prepare.template}}. }
}
\details{
  The ELODIE library is expected to reside in
  \code{$SLACS_HOME/ELODIE/H}. A non-standard path can be given with
  \code{path}. 

  From a given \code{resolution.target}, the routing automatically
  computes the required width of the smoothing kernel used by
  \code{\link{prepare.template}}. The instrumental resolution of the
  ELODIE spectra is ??? Angstroms. 
  
  The object name and atmospheric parameters are looked for in keywords
  OBJECT, SPTYPE, I\_FE\_H, I\_TEFF and I\_LOGG. 
}
\value{
  An object of class \code{spectrum}, resampled to a logarithmic
  wavelength vector.
}
\author{ Oliver Czoske }
\seealso{ \code{\link{prepare.template}}, \code{\link{prepare.spectrum}} }
\examples{
% c.light <- 299792.5
% template <- prepare.ELODIE("00985")
% plot.spectrum(template)


  }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ file }

