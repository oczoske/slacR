\name{binney}
\Rdversion{1.1}
\alias{binney}
\title{
  Function to compute <v/sigma> as defined by Binney (2005)
}
\description{
  This function computes \eqn{\langle v/\sigma\rangle}{<v/sigma>} as
  appropriate for two-dimensional kinematic data. 
}
\usage{
binney(kinmap, SN.lim = 0, SN.plot = FALSE, title = NULL, ...)
}
\arguments{
  \item{kinmap}{a kinematic map with velocity and velocity dispersion,
    given as a file name, an object of class \code{kinmap} or as a
    \code{data.frame} containing the relevant columns}
  \item{SN.lim}{Lower signal-to-noise limit of data points that are to
    be used in the computation.} 
  \item{SN.plot}{Create a plot of \eqn{\langle v/\sigma
      \rangle}{<v/sigma>} against signal-to-noise limit. This clarifies
    the dependence on \code{SN.lim} and may help in choosing an
    appropriate value.
  }
  \item{title}{
    A title for the \code{SN.plot}.
  }
  \item{\dots}{further arguments passed to or from other methods.}
}
\value{Just a number}
\references{
  Binney, J.: "Rotation and anisotropy of galaxies revisited", MNRAS
  363, 937 (2005)
}
\author{
  Oliver Czoske
}
\seealso{
  \code{\link{lambdaR}}, \code{\link{v.sig.2d}}
}
\examples{
}
\keyword{ univar }
