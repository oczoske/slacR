\name{print.kinmap}
\alias{print.kinmap}
\title{Pretty-print a kinematic map }
\description{
  Prints a kinematic map in table format with fixed columns. If the
  kinematic map is created from binned spectra, the table lists the
  bins, otherwise pairs (\code{L},\code{M}). If the kinematic maps
  contains errors, they are symmetrized. 
}
\usage{
\method{print}{kinmap}(x, ...)
}
\arguments{
  \item{x }{an object of class \code{kinmap} created with
    \code{\link{fit.cube}} or \code{\link{bin.cube}}. A \code{kinmap} is
    a list that contains at least the elements \code{L} and \code{M} for
    unbinned maps, or element \code{bin} for binned maps. } 
  \item{... }{parameters passed to \code{\link{print.data.frame}}}
}
\author{ Oliver Czoske}
\seealso{\code{\link{print.data.frame}}}
\keyword{print}
