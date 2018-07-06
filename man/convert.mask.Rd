\name{convert.mask}
\alias{convert.mask}
\title{ Utility function to create a mask vector from a variety of input
  formats}
\description{
  The function makes sure that a mask is a vector along a given
  wavelength vector, converting input such as a file name or a
  data.frame. 
}
\usage{
convert.mask(x, mask)
}
\arguments{
  \item{x}{ a wavelength vector or an object of class \code{spectrum}}
  \item{mask}{ Normally the name of a file or of a
    \code{data.frame} with components \code{x1} and \code{x2} giving the
    left and right limits of the wavelength regions to be masked. Can
    also be a logical scalar or vector, but then the function does not
    do much. }
}
\details{
  
}
\value{
  a logical vector along the wavelength vector \code{x} or
  \code{x$lambda}, with \code{TRUE} for pixels outside the wavelength
  regions given by \code{mask}}
\author{ Oliver Czoske }
\seealso{ \code{\link{fit.spectrum}}}
\examples{
lambda <- seq(3750, 5000, by=5)
mask.df <- data.frame(x1=c(4000, 4500), x2=c(4050, 4600))
mask <- convert.mask(lambda, mask.df)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }

