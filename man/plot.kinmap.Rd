\name{plot.kinmap}
\alias{plot.kinmap}
\title{Plot image of a matrix with adjoining colour bar }
\description{
  Plots an image of a matrix with adjoining colour bar. 
}
\usage{
\method{plot}{kinmap}(x, param, zlim=NULL, xlab="L", ylab="M", zlab=NULL,
                   color.table=heat.colors(100), logz=FALSE, ...)
}
\arguments{
  \item{x }{an object of class \code{kinmap} created with
    \code{\link{fit.cube}}. A \code{kinmap} is a list that contains at
    least the elements \code{L} and \code{M} as well as the measurement
    specified by \code{param}. } 
  \item{param }{a character string specifying the measurement to be
    plotted. This has to correspond to a column name present in
    \code{x}.}
  \item{zlim }{the minimum and maximum 'z' values for which colors should be
    plotted, defaulting to the range of the finite values of 'z'.}
  \item{xlab, ylab }{ each a character string giving the labels for the x and y
    axis. Defaults are "L" and "M".}
  \item{zlab}{the label for the colour bar. This defaults to
    \code{param}. For parameters \code{sigma}, \code{v} and \code{gof},
    special labels using Greek characters are hard-coded. }
  \item{color.table }{The colour table to be used. Defaults to
    \code{heat.colors(100)}}
  \item{logz }{ Logical. If \code{TRUE}, then the logarithm of the
    measurement is plotted. This is useful for surface brightness. Note
    that no checks are performed whether the measurement is positive
    everywhere!} 
  \item{... }{graphical parameters for 'plot' may also be passed as
    arguments to this function, as can the plot aspect ratio
    'asp' and 'axes' (see 'plot.window'). Note that these parameters
    only apply to the main plot, not the colour bar.}
}
\author{ Oliver Czoske}
\seealso{\code{\link{image}}, \code{\link{image.default}}}
\examples{
  x <- y <- seq(-4*pi, 4*pi, len=27)
  LM <- expand.grid(x, y)
  r <- sqrt(outer(x^2, y^2, "+"))
  cube <- list(L=LM$Var1, M=LM$Var2, r=as.vector(r))
  class(cube) <- "kinmap"
  plot(cube, "r")
}
\keyword{hplot}
\keyword{aplot}
