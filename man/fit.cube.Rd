\name{fit.cube}
\alias{fit.cube}
\title{ Function to perform kinematic analysis over a VIMOS IFU data cube }
\description{
  This function is a wrapper function, it calls
  \code{\link{fit.spectrum}} for every spectrum in a VIMOS IFU data
  cube. 
}
\usage{
fit.cube(config, galspec, template, LOSVD=c("gauss", "gh",
         "gauss-hermite"), mask, output, errors = 0, SN.lim = 8, 
         dv = 25, verbose = TRUE, sigma.max = 1000, normalize = FALSE,
         ...)
}
\arguments{
  \item{config}{ name of a file or list containing relevant data on the
    object on which the kinematic analysis is to be performed }
  \item{galspec}{ (optional) a list of galaxy spectra, all on the same
    wavelength vector. This would be read in from a VIMOS cube with
    \code{\link{prepare.cube}}. If missing, the name of the data cube is
    taken from \code{config}}
  \item{template}{ (optional) an object of class \code{spectrum} containing the
    stellar template spectrum. If missing, the name of the template file
    is taken from \code{config}}
  \item{LOSVD}{Shape of the LOSVD to be fit to the spectrum. Defaults to
    Gaussian but can also be set to a Gauss-Hermite series, to order 4. }
  \item{mask}{ a data.frame with two columns \code{x1} and \code{x2}
    giving the left and right limits of wavelength regions that are to
    be masked; or, the name of a file containing such a data.frame; or,
    a logical vector along \code{galaxy$lambda}, with \code{TRUE} for
    pixels that shall be included in the fit}
  \item{output }{the name of a file to which the resulting fit will be
    written by \code{write.table}}
  \item{errors}{ Number of Monte Carlo simulations that will be
    performed for each spectrum in the cube to give an estimate of the
    error distribution; defaults to zero }
  \item{SN.lim}{ Only spectra in the cube which have a signal-to-noise
    ratio larger than this value will be analysed by
    \code{fit.spectrum}. All others will have \code{NA} in their results }
  \item{dv}{ velocity step on which the template spectrum will be
    initially sampled; defaults to 25 km/sec }
  \item{verbose}{ \code{logical}. If \code{TRUE}, informative messages
    will be printed }
  \item{sigma.max}{ Maximum allowed value for the velocity dispersion,
    defaults to 1000 km/s. This gives in principle a constrained
    optimization, although the best fit should not be affected. }
  \item{normalize}{Normalization factor for the data cube, defaults to
    \code{FALSE}, i.e. no normalization at all}
  \item{ ... }{Further parameters to be passed to
    \code{\link{fit.spectrum}}. These would be in particular
    \code{niter}, \code{nsig} and \code{plot}.}
}
\details{
  Beware of running too many error realizations, this takes a hell of a
  long time.
}
\value{
  An object of class \code{kinmap} obtained from \code{fit.spectrum}
  with \code{simple=TRUE}. If \code{output} is 
  given, the result will be written directly to a file in the form of a
  \code{data.frame}. 
  \item{v, sig, gof, h3, h4 }{The best fitting values for velocity and
    velocity dispersion, and the corresponding value of the
    \code{\link{merit.function}}}
  \item{v1, v2}{first and second velocity moments of the best-fitting LOSVD}
  \item{dv.low, dv.high, dsig.low, dsig.high,...}{If \code{errors} is larger
    than zero, 68\% confidence limits for velocity and velocity
    dispersion and other parameters}
  \item{SN, level, noise }{The signal-to-noise, level and noise for the
    galaxy spectrum. }
  \item{rms }{The root-mean-square residuals of the best fit; this can
    be compared to noise}
  \item{L, M}{coordinates of the fibre in the data cube}
  \item{template, sptype, FeH, logg, Teff}{ some data on the template used}
}
\references{  }
\author{ Oliver Czoske }
\note{}
\seealso{ \code{\link{fit.spectrum}}}
\examples{
  }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
