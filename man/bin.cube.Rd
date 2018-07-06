\name{bin.cube}
\alias{bin.cube}
\title{Compute the average of spectra in a cube }
\description{
    Compute the average or sum of spectra residing in \code{cube}. The spectra to work on are identified by coordinates \code{L} and \code{M} in \code{bin}. 
}
\usage{
  bin.cube(cube, bin, add=FALSE)
}
\arguments{
  \item{cube}{ A \code{list} of spectra, all on the same wavelength
    vector. This would be read in from a VIMOS cube with
    \code{\link{prepare.cube}}}. 
  \item{bin}{ A \code{data.frame} with columns \code{L} and \code{M}
    identifying the spectra to be summed or average.}
  \item{add}{ If \code{TRUE} sum the spectra, otherwise (default)
    compute the average. }
}
\value{
  The summed or averaged \code{spectrum}.
}
\author{ Oliver Czoske }
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
