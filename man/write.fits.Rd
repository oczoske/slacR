\name{write.fits}
\alias{write.fits}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Write data to a fits file }
\description{
  This is a very basic function to write some data from R to a fits
  file. The function is a wrapper for the C program \code{R2fits}. 
}
\usage{
write.fits(x, outfile, nx, ny, na.val = 0)
}
\arguments{
  \item{x}{ The input data. This can be a vector of length (at least) nx
  x ny, or a matrix with dimensions nx and ny.}
  \item{outfile}{ The name of the fits file to be written. To force
    overwriting an existing file us a leading "!". }
  \item{nx, ny}{ The x and y dimensions of the image to be
    written. These have to be specified if \code{x} is a vector. If
    \code{x} is a matrix, its dimensions are used. \code{nx} and
    \code{ny} can also be vectors giving the world coordinates of the
    image pixels. In this case, the number of pixels in each direction
    is computed automatically.}
  \item{na.val}{ Numerical value to replace \code{NA} in the input data
    \code{x}.}
}
\details{
  The output created by \code{R2fits} and hence by this function is a
  very basic fits file whose header only contains the image dimensions
  and the date of writing. Any extra information on the data will have
  to be added automatically. 
}
\value{
  None.
}
\author{Oliver Czoske}
\seealso{ \code{\link{write.kinmap}} }
\examples{
x <- y <- seq(-2, 2, by=0.2)
z <- outer(x, y, function(x, y){exp(-x^2 - y^2/2^2)})
file <- paste("/tmp/slacR.tmp.", floor(10000*runif(1)), sep="")

write.fits(z, file)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ file }

