\name{merit.function}
\alias{merit.function}
\title{ Compute the chi2 merit function for template fitting}
\description{
  This function computes the noise weighted chi2 merit function for
  template fitting. The model is a stellar template spectrum convolved
  with a Gaussian or Gauss-Hermitian LOSVD. It is matched to a galaxy
  spectrum. The galaxy spectrum, the model spectrum and the residuals
  can be plotted. 
}
\usage{
merit.function(p, galaxy, template, LOSVD=c("gauss", "gh",
               "gauss-hermite"), mask = TRUE, noise = NULL,
               plot = FALSE, verbose = FALSE, return.fit = FALSE,
               sigma.max = 1000, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{ a two-component \code{vector} containing the parameters for
    the template fit: velocity \code{v} and velocity dispersion
    \code{sigma}, both in km/s. }
  \item{galaxy}{ an object of class \code{spectrum} containing the
    galaxy spectrum that is to be fitted}
  \item{template}{ an object of class \code{spectrum} containing the
    stellar template spectrum}
  \item{LOSVD}{Shape of the LOSVD to be fit to the spectrum. Defaults to
    Gaussian but can also be set to a Gauss-Hermite series, to order 4. }
  \item{mask}{ a data.frame with two columns \code{x1} and \code{x2}
    giving the left and right limits of wavelength regions that are to
    be masked; or, the name of a file containing such a data.frame; or,
    a logical vector along \code{galaxy$lambda}, with \code{TRUE} for
    pixels that shall be included in the fit}
  \item{noise}{ a vector along \code{galaxy$lambda} containing an
    estimate of the noise for each pixel; or, a single number giving the
    average noise value. If missing, the merit function will be unnormalized.}
  \item{plot}{ \code{logical}. If \code{TRUE}, the galaxy spectrum will
    be plotted along with the model and residuals. }
  \item{verbose}{ \code{logical}. If \code{TRUE}, informative messages
    will be printed }
  \item{return.fit}{ \code{logical}. If \code{FALSE}, only the value of
    the merit function will be returned, otherwise a list (see below).}
  \item{sigma.max}{ the maximum allowed value for \code{sigma}. If
    \code{sigma} is larger than \code{sigma.max}, the function will
    return \code{NA}.}
  \item{...}{ Parameters to be passed to \code{\link{build.model}}, in
    particular the orders of the multiplicative and additive polynomials }
}
\details{
  The value returned by this function does not seem to be a proper chi2 yet.
}
\value{
  If \code{return.fit} is \code{FALSE}, only the value of the merit
  function is returned. This should be the setting for optimising over
  the parameters.
  If \code{return.fit} is \code{TRUE}, a list is returned with the
  components
  \item{L, M}{ the coordinates of \code{galaxy}}
  \item{gof}{ the value of the merit function}
  \item{v, sigma, h3, h4}{the kinematic parameters used in the
    computation of the merit function}
  \item{lambda, model, residual}{ the model spectrum and the residuals
    on the wavelength vector of \code{galaxy}}
  \item{mask, noise}{the mask and noise vectors}
  \item{lm.fit}{the linear fit for the coefficients of the
    multiplicative and additive polynomials}
}
\author{ Oliver Czoske }
\note{  
  The chi2 is as yet not properly normalized.
}
\seealso{ \code{\link{build.model}}, \code{\link{fit.spectrum}} }
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }

