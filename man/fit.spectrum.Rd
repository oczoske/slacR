\name{fit.spectrum}
\alias{fit.spectrum}
\title{ Fit a galaxy spectrum with a stellar template and a Gaussian or
  Gauss-Hermite LOSVD}
\description{
  This function determines velocity and velocity dispersion of a galaxy
  by optimizing over a \code{\link{merit.function}} given by convolving
  a stellar template spectrum and comparing to the galaxy spectrum in
  the least squares sense.  The line profile can be parameterized as
  Gaussian or a Gauss-Hermite series of order 4.
}
\usage{
fit.spectrum(galaxy, template, p0 = c(0, 230, 0, 0),
             LOSVD=c("gauss", "gh", "gauss-hermite"),
             noise = NULL, lambda = NULL, errors = 0, mask = NULL,
             SN.lim = 0, niter = 3, nsig = 3, verbose = FALSE,
             plot = FALSE, plot.errors = FALSE, 
             simple = FALSE, o.mult=4, o.add=6, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{galaxy}{ an object of class \code{spectrum} containing the
    galaxy spectrum that is to be fitted}
  \item{template}{ an object of class \code{spectrum} containing the
    stellar template spectrum}
  \item{p0}{ four-component \code{vector} containing the starting values
    for the parameters that are to be determined from the template fit:
    velocity \code{v} and velocity dispersion \code{sigma}, both in
    km/s. The Gauss-Hermite coefficients \code{h3} and \code{h4} are
    ignored if \code{LOSVD=="gauss"}. }
  \item{LOSVD}{Shape of the LOSVD to be fit to the spectrum. Defaults to
    Gaussian but can also be set to a Gauss-Hermite series, to order 4. }
  \item{noise}{ an object of class \code{spectrum} or a vector along
    \code{galaxy$lambda} containing an estimate of the noise for each
    pixel; or, a single number giving the average noise value. If
    missing, the noise will be estimated from the wavelength region
    \code{lambda}. If noise is a \code{spectrum}, then the wavelength
    component should be identical to \code{galaxy$lambda}} 
  \item{lambda}{ a two-component \code{vector} giving the limits of a
    wavelength region within which level, noise and signal-to-noise of
    the galaxy spectrum will be determined }
  \item{errors}{ Number of Monte Carlo simulations that will be
    performed to give an estimate of the error distribution; defaults to
    zero }
  \item{mask}{ Normally, the mask used in the fit is included in the
    spectrum \code{galaxy}. It is possible to specify an additional
    \code{mask}, which is combined with \code{galaxy$mask} by logical
    AND. Any format that is understood by \code{\link{convert.mask}} is
    allowed: a data.frame with two columns \code{x1} and \code{x2}
    giving the left and right limits of wavelength regions that are to
    be masked; or, the name of a file containing such a data.frame; or,
    a logical vector along \code{galaxy$lambda}, with \code{TRUE} for
    pixels that shall be included in the fit}
  \item{SN.lim}{Only galaxy spectra which have signal-to-noise larger
    than this value will be analysed; defaults to zero, i.e. all spectra
    will be analysed.}
  \item{niter}{ number of iterations in the sigma-clipping }
  \item{nsig}{ data points with residuals larger than \code{nsig} times
    the standard deviation of the fit will be masked }
  \item{verbose}{ \code{logical}. If \code{TRUE}, informative messages
    will be printed }
  \item{plot}{ \code{logical}. If \code{TRUE}, the galaxy spectrum will
    be plotted along with the best fitting model and residuals. }
  \item{plot.errors}{ \code{logical}. If \code{TRUE}, then a contour
    plot of the \code{\link{merit.function}} in the
    \code{v}--\code{sigma} plane will be plotted and the estimates from
    the error runs will be marked }
  \item{simple}{ \code{logical}. If \code{TRUE}, a list of only scalar
    vectors will be returned (this is required when the function is
    called by \code{fit.cube}) }
  \item{o.mult}{ order of polynomial multiplying the convolved template spectrum }
  \item{o.add}{ order of polynomial added to the convolved template spectrum }
  \item{\dots}{ Further parameters to be passed to
    \code{\link{merit.function}} }
}
\details{
  Plotting the errors does not work yet, rewrite of \code{gof.contours}
  is required.
}
\value{
  An object of class \code{kinfit}. This is the original spectrum with
  added parameters describing the fit and its results.
  Of particular importance are the first and second velocity moments,
  \code{v1} and \code{v2}, computed by integrating over the best-fit
  LOSVD. 
  
  If \code{errors} is larger than zero, the list includes summaries of
  the error distribution:
  \item{dv1.low, dv1.high, dv2.low, dv2.high}{68\% confidence levels for
    the first and second velocity moments}
  \item{dv.low, dv.high }{68\% confidence limits for velocity }
  \item{dsig.low, dsig.high }{68\% confidence limits for velocity
    dispersion}
  \item{v1.err, v2.err, v.err, sig.err, gof.err, h3.err,
    h4.err}{velocities, velocity   dispersions and corresponding
    \code{merit.function} values determined from each Monte Carlo
    simulation (only if \code{simple=FALSE})} 
}
\references{  }
\author{ Oliver Czoske }
\seealso{ \code{\link{merit.function}}, \code{\link{fit.cube}}}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
