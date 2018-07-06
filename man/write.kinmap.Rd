\name{write.kinmap}
\alias{write.kinmap}
\title{ Function to write a kinematic map to a fits file }
\description{
  The function uses \code{\link{write.fits}} to write one or more
  components of the kinematic maps to fits files. A WCS is created if
  its reference point is defined through function arguments. If a
  configuration is given, details on the object and the template are
  also written to the fits header. 
}
\usage{
write.kinmap(x, component, outfile, na.val = 0, overwrite = FALSE,
             configuration = NULL, crval1 = NULL, crval2 = NULL,
             crpix1 = NULL, crpix2 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ A kinematic map as created by \code{\link{fit.cube}}. More
    generally, a data frame with at least the \code{component} to be
    written, and coordinate vectors \code{x$L} and \code{x$M}.}
  \item{component}{ One or more components of the kinematic map \code{x}
    for which fits files are to be written. If more than one, these are
    to be given as a vector of character strings. }
  \item{outfile}{ The name(s) of one or more fits files that are to be
    created. The order corresponds to that of \code{component}. If the
    lengths of \code{component} and \code{outfile} are not the same,
    files are written, until one of the vectors is exhausted. }
  \item{na.val}{ Numerical value to replace \code{NA} in the data. }
  \item{overwrite}{ If \code{TRUE}, an existing file will be overwritten. }
  \item{configuration}{A \code{list} of configuration details on the
    object or the name of a configuration file. Currently the components
    \code{object}, \code{data.cube}, \code{noise.cube} and
    \code{template.file} are written to the fits header. } 
  \item{crval1, crval2}{ The world coordinates of the reference point. }
  \item{crpix1, crpix2}{ The physical coordinates, i.e.~\code{x$L} and
    \code{x$M}, of the reference point. }
}
\details{
  The function rotates the kinematic map so that north is along the
  positive y axis and east along the negative x axis in the fits file. 
  NOTE THAT THIS MIGHT CHANGE AGAIN!
  
  A WCS is created if the coordinates of the reference point are
  explicitely given. The linear matrix is implicitely assumed and
  correponds to that of VIMOS data reduced with VIPGI. This means that
  \code{x$L} increases in the north direction and \code{x$M} increases
  in the east direction. The pixel scale is 0.67 arcsec.

  The physical coordinates \code{x$L} and \code{x$M} are in principle
  preserved using the keywords \code{LTM1} and \code{LTM2} in the fits
  header. They are, however, interchanged. Matching the kinematic maps
  to the 2D image reconstruction therefore needs a rotation.  
}
\value{
  None.
}
\author{Oliver Czoske}
\seealso{ \code{\link{write.fits}}, \code{\link{fit.cube}} }
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ file }
