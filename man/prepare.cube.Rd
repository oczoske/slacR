\name{prepare.cube}
\alias{prepare.cube}
\title{Prepare a data cube for kinematic analysis}
\description{
  Read in an IFS data cube and prepare for kinematic analysis.
}
\usage{
prepare.cube(datacube, config = NULL, extension = 0, input.table = NULL,
             noisecube = NULL, mask = NULL, z = NULL, lambda.lim = NULL,
             verbose = FALSE, save.cube=FALSE, object = NULL, ...)
}
\arguments{
  \item{datacube}{ name of fits file containing the "data cube" in
    row-stacked form. Overrides name given in \code{config.file}. }
  \item{config}{ name of list or file containing configuration data
    pertinent to the galaxy to be analysed}
  \item{extension}{ name or number of the MEF extension of to be
    read from \code{datacube}. Defaults to 0 which is appropriate for
    the final reduced VIPGI cubes.}
  \item{input.table}{ file or dataframe containing list of rows from
    "data cube" to be read in as well as translation to L/M coordinates }
  \item{noisecube}{ name of fits file containing the "noise cube" in
    row-stacked form. Overrides name given in \code{config.file}. }
  \item{mask}{ a mask in any format that can be read and converted by
    \code{\link{convert.mask}}. Most of the time, this will be a file or
    data.frame with components \code{x1} and \code{x2} giving the left
    and right limits of the wavelength regions to be masked.}
  \item{z}{ redshift of the galaxy. Overrides keyword \code{z.lens} from
    \code{config}.}
  \item{lambda.lim}{Wavelength range to be cut from the data
    cube. Overrides keyword \code{lambda.spec.lim} from \code{config}.}
  \item{verbose}{be talkative}
  \item{save.cube}{logical or character string. Save the R
    representation of the data cube to a file using
    \code{\link{save}}. If \code{TRUE}, then the object and file names
    are generated automatically from the variable \code{object},
    dropping the prefix "SDSS-". It is also possible to 
    specify the file name as a character string. The file can later
    simply be \code{\link{load}}ed, which is significantly faster than
    using \code{prepare.cube} again.}
  \item{object}{optional name of the object. Overrides name given in
    \code{config.file}} 
  \item{...}{Further parameters to be passed to
    \code{\link{prepare.spectrum}}, notably \code{normalize} and
    \code{order}. }
}
\details{
  Read in a subset of spectra from a "data cube" (actually a row-stacked
  representation of IFS data) as prepared by VIPGI. The list of rows to
  be read as well as the identification with two-dimensional position on
  the sky is given in an input table. A second data cube containing
  corresponding noise spectra can also be given.
}
\value{
  a list of galaxy spectra. Each spectrum is in turn a list containing
  the following components:
  \item{lambda }{wavelength grid}
  \item{value }{value of the spectrum}
  \item{noise }{value of the estimated noise}
  \item{contsub }{value of the spectrum after continuum subtraction}
  \item{name }{name of the fits file, including the filter to identify
    the row in the row-stacked representation}
  \item{object }{name of the object, including L and M}
  \item{L, M, F }{two-dimensional coordinates of the fibre in the data
    cube and the row-number in the row-stacked representation}
}
\author{ Oliver Czoske}
\seealso{\code{\link{prepare.spectrum}}, \code{\link{prepare.template}}}
\keyword{file}
