\name{lambdaR}
\Rdversion{1.1}
\alias{lambdaR}

\title{
  Function to compute the angular momentum parameter
  lambda_R from the kinematic map of a galaxy
}
\description{
  Function to compute the angular momentum parameter
  \eqn{\lambda_R}{lambda_R} from the kinematic map of a galaxy. The
  parameter is defined by Emsellem et al. (2007) as
  \deqn{\lambda_R = \frac{\langle R\,|V|\rangle}{R\sqrt{V^2 +
	\sigma^2}}}
}
\usage{
lambdaR(kinmap, config, SN.lim = 0, SN.plot = FALSE, Lcent = NULL, Mcent
  = NULL, title = "NULL", errors = 0, error.scale = NULL, v.err = NULL,
  sig.err = NULL, ...)
}

\arguments{
  \item{kinmap}{a kinematic map with velocity and velocity dispersion,
    given as a file name, an object of class \code{kinmap} or as a
    \code{data.frame} containing the relevant columns}
  
  \item{config}{configuration file for the system at hand. The function
    requires the coordinates of the centre of the galaxy if these are
    not specified explicitely.
  }
  \item{SN.lim}{
    Lower signal-to-noise limit of data points that are to be used in
    the computation.
  }
  \item{SN.plot}{Create a plot of \eqn{\lambda_R} against
    signal-to-noise limit. This clarifies the dependence on
    \code{SN.lim} and may help in choosing an appropriate value.
  }
  \item{Lcent, Mcent}{Centre coordinates of the galaxy in the kinematic
    map. These values have priority over those in the configuration file.
  }
  \item{title}{
    A title for the \code{SN.plot}.
  }
  \item{errors}{number of Monte Carlo simulations to simulate errors on
    \code{lambda.R}}
  \item{error.scale}{If \code{v.err} and \code{sig.err} are not given,
    then they are computed from the interval between \code{dv.high} and
    \code{dv.low} (same for \code{sig}), scaled by \code{error.scale}}
  \item{v.err, sig.err}{dispersion of Gaussian distributions used to
    obtain Monte Carlo kinmaps for \code{v} and \code{sigma}}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  The value of \eqn{\lambda_R}{lambda_R} computed here is uncorrected. 
}
\value{a number}
\references{Emsellem, E. et al.: "The SAURON project - IX. A kinematic
  classification for early-type galaxies", MNRAS 379, 401 (2007)}
\author{
  Oliver Czoske
}
\seealso{\code{\link{binney}}}

\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ univar }

