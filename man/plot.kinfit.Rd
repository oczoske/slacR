\name{plot.kinfit}
\alias{plot.kinfit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plot of a kinematic fit }
\description{
  Plots a kinematic fit. The upper panel shows the spectrum with the
  model as determined by \code{\link{merit.function}}, the lower panel
  the residuals with the estimated noise vector overlaid.
}
\usage{
\method{plot}{kinfit}(x, as.spectrum = FALSE, ...)
}
\arguments{
  \item{x}{ a \code{"kinfit"} object containing the input
    \code{spectrum}, the \code{model} and the \code{residuals}}
  \item{as.spectrum}{ if \code{TRUE}, then only the input
    \code{spectrum} is plotted using \code{\link{plot.spectrum}} }
  \item{\dots}{ Further graphical parameters (see 'par') may also be supplied
    as arguments. }
}
\details{
  The default is to plot the spectrum with overlaid model in the top
  panel and the residuals in the bottom panel. Plots of the additive and
  multiplicative polynomials will be added later.
}
\author{ Oliver Czoske }
\seealso{ \code{\link{plot}}, \code{\link{plot.spectrum}}, \code{\link{par}} }
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
