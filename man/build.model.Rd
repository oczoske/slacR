\name{build.model}
\alias{build.model}

\title{ Build a model for a galaxy spectrum from a stellar template spectrum }
\description{
  This function convolves a stellar template spectrum with a Gaussian or
  Gauss-Hermite LOSVD and adjusts the continuum with additive and a
  multiplicative polynomial corrections to match a galaxy spectrum. 
}
\usage{
build.model(x, y, p,  LOSVD=c("gauss", "gh", "gauss-hermite", "shift"),
            o.mult = 4, o.add = 6, p.mult=NULL, p.add=NULL)
}

\arguments{
  \item{x}{ an object of class \code{spectrum}. This should be a
    high-resolution stellar template spectrum. }
  \item{y}{ an object of class \code{spectrum}. This should be a galaxy
    spectrum }
  \item{p}{ a four-component \code{vector} containing the parameters for
    the template fit: velocity \code{v} and velocity dispersion
    \code{sigma}, both in km/s, h3  and h4. }
  \item{LOSVD}{Shape of the LOSVD to be fit to the spectrum. Defaults to
    Gaussian but can also be set to a Gauss-Hermite series, to order 4. }
  \item{o.mult}{ order of polynomial multiplying the convolved template spectrum }
  \item{o.add}{ order of polynomial added to the convolved template spectrum }
  \item{p.mult, p.add}{polynomials over \code{y$lambda}. These are
    created with \code{\link{poly}} with an additional first column of 1.}
}
\details{
  The resulting model spectrum is resampled to the galaxy vector,
  \code{y$lambda}. The coefficients of the multiplicative and additive
  polynomials  are determined from a linear fit of the model spectrum to
  the galaxy spectrum for fixed parameters \code{p}.
  The model will typically be used by \code{\link{merit.function}} to
  compute the goodness of fit for the given kinematic parameters.
}
\value{
  an object of class \code{kinfit} (inheriting from \code{spectrum})
  with the convolved and continuum adjusted model spectrum resampled to
  the wavelength vector of the galaxy spectrum, the residuals, and all
  the parameters going into building the model
}
\references{ D. Kelson et al. 2000, ApJ 531, 159}
\author{ Oliver Czoske }
\seealso{ \code{\link{merit.function}}, \code{\link{fit.spectrum}} }
\examples{
  }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }

