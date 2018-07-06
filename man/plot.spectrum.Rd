\name{plot.spectrum}
\alias{plot.spectrum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Line Plot Of A Spectrum }
\description{
  Plots a spectrum (i.e. a dataframe with entries \code{lambda} and
  \code{value} or \code{contsub}). This is a convenience wrapper for
  \code{\link{plot.default}} which selects the correct columns
  automatically and defaults to \code{type='l'}. 
}
\usage{
\method{plot}{spectrum}(x, add = FALSE, offset = 0, scale = 1,
              norm = FALSE, noise = FALSE, contsub = FALSE,
              contdiv = FALSE, mask=FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a \code{"spectrum"} object containing the
    components \code{lambda} and \code{value} and possibly \code{noise} or
    \code{contsub}}
  \item{add}{ add the spectrum to an existing plot }
  \item{scale}{ multiply the spectrum values by this number }
  \item{offset}{ add a constant offset to the (scaled) spectrum }
  \item{norm}{ Normalize the spectrum to either a mean value of unity
    (default) or standard deviation of unity (if uning \code{contsub} }
  \item{noise }{if \code{TRUE} then plot the noise spectrum}
  \item{contsub }{if \code{TRUE} then plot the continuum subtracted spectrum}
  \item{contdiv }{if \code{TRUE} then plot the continuum divided
    spectrum}
  \item{mask }{if not \code{FALSE}, then mark all masked points in the
    spectrum in magenta. The mask is typically contained in the
    \code{spectrum} \code{x}. Otherwise, \code{mask} can be any format
    accepted by \code{\link{convert.mask}}}
  \item{\dots}{Further graphical parameters (see 'par') may also be supplied
    as arguments. }
}
\details{
  The spectrum can be scaled, offset or normalized. The normalization
  is the mean of the spectrum \code{value}, except if the continuum
  subtracted spectrum, when the standard deviation of that is used.
}
\author{ Oliver Czoske }
\seealso{ \code{\link{plot}}, \code{\link{lines}}, \code{\link{par}} }
\examples{
spectrum <- data.frame(lambda=seq(3800, 6500, length=500),
value=rnorm(500), name="A random noise spectrum")

plot.spectrum(spectrum, ylim=c(-3, 5))
plot.spectrum(spectrum, add=TRUE, offset=1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
