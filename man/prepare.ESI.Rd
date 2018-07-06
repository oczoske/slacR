\name{prepare.ESI}
\alias{prepare.ESI}

\title{ Read an ESI Spectrum as Template }
\description{
  This function provides a wrapper to \code{\link{prepare.template}}
  specifically geared to Keck/ESI spectra from Tommaso Treu.
}
\usage{
prepare.ESI(ident, config.file = NULL, path = NULL,
               lambda.lim = NULL , ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ident}{ character string identifying an ESI spectrum. This
    is the basename of the corresponding fits file. }
  \item{config.file}{ name of a configuration file. This can be used to
    specify a wavelength range to be used with a given target galaxy.  }
  \item{path}{ optional character string to specify non-standard path to
    the ESI library}
  \item{lambda.lim}{ two-component vector giving the wavelength range to
    be read from the template spectrum. If \code{NULL}, the entire
    spectrum is read.}
  \item{\dots}{ Further parameters to be passed to
    \code{\link{prepare.template}}. }
}
\details{
  The ESI spectra are expected to reside in
  \code{$SLACS_HOME/ESI/}. A non-standard path can be given with
  \code{path}.

  The identifier is a three-digit HR number, e.g. \code{"HR019"} for HR 19.
  This is also used as the object name.

  The atmospheric parameters are looked for in header keywords SPTYPE, TEFF,
  LOGG and FEH. These were added by OC on 2008-02-26 from SIMBAD information.
}
\value{
  An object of class \code{spectrum}, resampled to a logarithmic
  wavelength vector.
}
\author{ Oliver Czoske }
\seealso{ \code{\link{prepare.template}}, \code{\link{prepare.spectrum}} }
\examples{
%  c.light <- 299792.5
%  template <- prepare.ESI("HR019")
%  plot.spectrum(template)


  }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ file }

