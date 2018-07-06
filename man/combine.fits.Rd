\name{combine.fits}
\alias{combine.fits}
\title{ Combine and compute statistics of a number of kinematic maps }
\description{
  Occasionally, one wants to create several kinematic maps of the same
  IFU data cube using, say, different templates. This function
  computes the mean, median and other combinations of these maps, in
  particular the standard deviation. 
}
\usage{
  combine.fits(fit.list, param)
}
\arguments{
  \item{fit.list}{ A \code{list} of kinematic maps as created by
    \code{\link{fit.cube}}. These are data frames with at least columns
    \code{L}, \code{M} and the measurement specified by \code{param}.}
  \item{param}{ A character string specifying the measurement to be
    combined,~e.g.\ \code{sigma} or \code{v}.}
}
\value{
  A data frame with the following columns:
  \item{L, M }{spaxel coordinates, taken from the first member of the
    list \code{fit.list}}
  \item{mean, median, sd, min, max }{the respective combinations of the
    values of \code{param} for the given spaxel in \code{fit.list}. All
    the statistics are computed with \code{na.rm=TRUE}.}
}
\author{ Oliver Czoske }
\seealso{ \code{\link{fit.cube}} }
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
