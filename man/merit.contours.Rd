\name{merit.contours}
\alias{merit.contours}

\title{ Create contour plot of goodness of kinematic
  fit  }
\description{
  This function visualizes the \code{\link{merit.function}} as a
  function of \code{v} and \code{sigma} around specified values \code{p}
  (in general the best-fit values).
}
\usage{
merit.contours(galaxy, template, fit=TRUE, p, v.range, s.range,
  plot=TRUE, mask=TRUE,...)
}

\arguments{
  \item{galaxy}{ data.frame containing the galaxy spectrum }
  \item{template}{ data.frame containing the template spectrum }
  \item{fit}{ logical. If \code{TRUE} determine best-fit values
    automatically via \code{\link{fit.spectrum}}.}
  \item{p}{ two-component vector containing the central \code{v} and
    \code{sigma} values. Only required if \code{v.range} and
    \code{s.range} are not given and \code{fit=FALSE}. }
  \item{v.range}{ two-component vector giving range of velocity
    \code{v}. Defaults to 100 km/s around \code{p[1]}.}
  \item{s.range}{ two-component vector givingrange of velocity
    dispersion \code{sigma}. Defaults to 100 km/s around \code{p[2]}}
  \item{plot}{ logical specifying whether to plot contours or not}
  \item{mask}{ mask that is to be applied to the spectrum. Can be in
    any format accepted by \code{\link{convert.mask}}. If \code{fit} is
    \code{TRUE}, then the updated mask from the fitting is used. }
  \item{\dots}{Further arguments to be passed to \code{\link{fit.spectrum}}}
}
\details{
  The function computes \code{\link{merit.function}} on a grid of 20x20
  of velocity and velocity dispersion. The resulting matrix is either
  plotted as a contour plot or returned along with the velocity and
  velocity dispersion vectors.
}
\value{
  \item{v }{Vector of velocity values}
  \item{sigma }{Vector of velocity dispersion values}
  \item{gof }{Matrix of values of \code{\link{merit.function}}}
}
\author{ Oliver Czoske }
\seealso{ \code{\link{merit.function}}, \code{\link{fit.spectrum}} }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## None for now.

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
