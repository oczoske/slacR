\name{OII.model}
\alias{OII.model}
\alias{OII.model.map}
\alias{OII.fit}
\alias{OII.gof}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Collection of functions to create an OII map from VIMOS data }
\description{
  These functions determine the strength of the [OII] doublet across a
  VIMOS/IFU data cube by fitting a double Gaussian model to the
  lines. Depending on the signal-to-noise ratio of the individual
  spectra the template fit to the entire spectrum is subtracted before
  fitting [OII].
}
\usage{
OII.model.map(config.file, cube.fit, galspec, template,
              plot=FALSE, m1=NULL, m2=NULL, normalize=1e-18, \dots)
OII.model(lambda, m1 = 5277, m2 = 5281.5, s1 = 1.2, s2 = 1.2, a.rel = 1.5)
OII.fit(galaxy, template, spec.fit, model, plot=FALSE, \dots)
OII.gof(p=c(1), y, y.mod)
}
\arguments{
  \item{config.file}{name of list or file containing configuration data
    pertinent to the galaxy to be analysed}
  \item{cube.fit}{a \code{data.frame} containing the results of
    kinematic fits to the data cube, as obtained with
    \code{\link{fit.cube}}}
  \item{spec.fit}{a \code{data.frame} (with one row) or \code{list}
    containing the results from a kinematic fit to a single
    spectrum. This can be a single row from \code{cube.fit} or the
    results from \code{\link{fit.spectrum}}. The minimum requirements
    are components \code{v}, \code{sigma} and \code{SN}. }
  \item{galspec}{(optional) a list of galaxy spectra, all on the same
    wavelength vector. This would be read in from a VIMOS cube with
    \code{\link{prepare.cube}}. If missing, the name of the data cube is
    taken from \code{config} }
  \item{galaxy}{a single galaxy spectrum on which the [OII] line
    strength is to be determined}
  \item{template}{(optional) an object of class \code{spectrum} containing the
    stellar template spectrum. If missing, the name of the template file
    is taken from \code{config}}
  \item{plot}{(logical) If \code{TRUE}, \code{OII.model.map} displays
    the [OII] map; \code{OII.fit} displays the spectrum and the best
    double Gaussian fit if the function is called directly.}
  \item{m1, m2, s1, s2, a.rel}{central wavelengths, dispersions and
    relative strength of the Gaussians describing the two components of
    the [OII] doublet model.}
  \item{model}{(optional) a model for the [OII] doublet as returned by
    \code{OII.model}. \code{OII.fit} determines the scaling needed to
    match this model to the actual [OII] lines.}
  \item{normalize}{normalization constant used to bring the spectral
    values to order unity}
  \item{lambda}{ wavelength vector on which the [OII] model is built }
  \item{p}{starting value for the scaling of the model}
  \item{y, y.mod}{the data and model vectors used to compute the
    goodness of fit}
  \item{ \dots }{parameters that are passed to \code{\link{build.model}},
    i.e. affecting the kinematic model}
}
\details{
  The function \code{OII.model.map} is the one typically used
  directly. The other functions (with the possible exception of
  \code{OII.fit}) are used internally to compute the [OII] model, the
  goodness-of-fit for a given model, and to optimize the fit.
}
\value{
  \code{OII.model.map} returns a \code{data.frame} containing positions
  \code{L} and \code{M} the signal-to-noise ratio of the spectrum,
  \code{SN}, and the strength of the [OII] lines, \code{Flux}. This can
  be visualized with \code{\link{plot.kinmap}}

  \code{OII.fit} returns the same values in the shape of a list and for
  a single spectrum.

  \code{OII.model} returns a \code{spectrum}, i.e. a list with
  \code{lambda} and \code{value}.

  \code{OII.gof} returns a number, the sum of squared differences
  between data and model.
}
\references{The procedure follows an idea described by Bolton \& Burles
  (2008). } 
\author{ Oliver Czoske }
\note{ THIS IS NOT QUITE FINISHED YET!}
\seealso{ \code{\link{fit.cube}}, \code{\link{build.model}},
  \code{\link{plot.kinmap}} }
\examples{}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
