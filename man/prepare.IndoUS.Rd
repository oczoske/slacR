\name{prepare.IndoUS}
\alias{prepare.IndoUS}

\title{ Read an Indo-US Spectrum as Template }
\description{
  This function provides a wrapper to \code{\link{prepare.template}}
  specifically geared to Indo-US spectra.
}
\usage{
prepare.IndoUS(ident, config.file = NULL, path = NULL,
               lambda.lim = NULL, resolution.target, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ident}{ character string identifying an Indo-US spectrum. This
    is the basename of the corresponding fits file. }
  \item{config.file}{ name of a configuration file. This can be used to
    specify a wavelength range to be used with a given target galaxy.  }
  \item{path}{ optional character string to specify non-standard path to
    the Indo-US library}
  \item{lambda.lim}{ two-component vector giving the wavelength range to
    be read from the template spectrum. If \code{NULL}, the entire
    spectrum is read.}
  \item{resolution.target}{Instrumental resolution (sigma!) of the
    spectrum that is to be fitted.}
  \item{\dots}{ Further parameters to be passed to
    \code{\link{prepare.template}}. }
}
\details{
  The Indo-US library is expected to reside in
  \code{$SLACS_HOME/Indo-US/IRAF}. A non-standard path can be given with
  \code{path}.

  From a given \code{resolution.target}, the routing automatically
  computes the required width of the smoothing kernel used by
  \code{\link{prepare.template}}. The instrumental resolution of the
  IndoUS spectra is 0.43 Angstroms. 
  
  The object name and atmospheric parameters are looked for in keywords
  OBJNAME, PICKTYPE, TEFF, LOGG and FEH.
}
\value{
  An object of class \code{spectrum}, resampled to a logarithmic
  wavelength vector.
}
\author{ Oliver Czoske }
\seealso{ \code{\link{prepare.template}}, \code{\link{prepare.spectrum}} }
\examples{
\dontrun{
template <- prepare.IndoUS("100006")
plot.spectrum(template)
}
  }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ file }

