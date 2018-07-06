\name{resample.spectrum}
\alias{resample.spectrum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Resample a spectrum to a given wavelength vector }
\description{
  The function is basically a wrapper to \code{\link{approx}}. It takes
  a spectrum that is resampled to a wavelength vector given as the
  second argument.
}
\usage{
resample.spectrum(input, target)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{ an object of class \code{spectrum} that is to be
    resampled }
  \item{target}{ either an object of class \code{spectrum} (in which
    case only component \code{lambda} is used) or a numeric vector
    containing wavelength at which the spectrum is to be sampled. }
}
\value{
  An object of class \code{spectrum}. Most components are straight
  copies from \code{input}, except
  \item{lambda }{this is the wavelength vector \code{target} (or its
    \code{lambda} component) }
  \item{value, contdiv, contsub }{the interpolation of
    \code{input$lambda} and (if present) \code{input$contsub} and
    \code{contdiv} onto \code{lambda}}
  \item{noise }{Since the function does not deal with the \code{noise}
    component of \code{input}, this is set to \code{NULL}}
}
\author{Oliver Czoske}
\section{Warning }{
  Note that "resampling" actually means "interpolating". I have not
  written a proper resampling code yet. Also, the function does not deal
  with the noise vector associated with the \code{input} spectrum.
}
\seealso{ \code{\link{approx}} }
\keyword{ manip }
