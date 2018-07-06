\name{tvt}
\alias{v.sig.2d}
\alias{v.sig.1d}
\alias{delta.anisotropy}
\alias{Wxx.Wzz}
\alias{v.sig.eps.plot}
\title{The tensor virial theorem}
\description{These functions provide theoretical values and plots
  related to the tensor virial theorem. \code{v.sig.2d} and
  \code{v.sig.1d} compute \eqn{\langle v/\sigma \rangle}{<v/sigma>} as
  a function of ellipticity and anisotropy \eqn{\delta}{delta} as
  appropriate for two-dimensional and one-dimensional kinematic data,
  respectively. \code{v.sig.eps.plot} creates a plot to which data
  points can be added.}
\usage{
v.sig.2d(epsilon, delta = 0, alpha = 0.15)
v.sig.1d(epsilon, delta = 0)
delta.anisotropy(epsilon, V.sigma, alpha = 0.15)
Wxx.Wzz(epsilon)
v.sig.eps.plot(eps.data = NULL, vsig.data = NULL, twoD = TRUE, xlim,
ylim, \dots)
}
\arguments{
  \item{epsilon}{axis ratio, \eqn{\epsilon = b/a}{epsilon = b/a}}
  \item{delta}{the anisotropy parameter}
  \item{alpha}{parameter required to compute the two-dimensional
    \eqn{\langle v/\sigma \rangle}{<v/sigma>}; default value is as given
    by Binney (2005).}
  \item{V.sigma}{\eqn{\langle v/\sigma \rangle}{<v/sigma>}, computed
    from two-dimensional data}
  \item{eps.data,vsig.data}{vectors containing ellipticities and
    \eqn{\langle v/\sigma \rangle}{<v/sigma>} values derived from data;
    these will be added to the plot as points}
  \item{twoD}{logical: Plot \eqn{\langle v/\sigma \rangle}{<v/sigma>}
    curves for one-dimensional or two-dimensional data?}
  \item{xlim,ylim}{plot limits. If not present, these will take default
    values or will be derived from the data points.}
  \item{\dots}{parameters passed to other methods; in particular, these
    can be used to specify point types for plotting the data points.}
}
\value{All functions return numbers, except \code{v.sig.eps.plot}, which
  returns nothing.}
\references{Binney, J.: ``On the rotation of elliptical galaxies'',
  MNRAS 183, 501 (1978); Binney, J.: ``Rotation and anisotropy of
  galaxies revisited'', MNRAS 363, 937 (2005)}
\author{Oliver Czoske}
\seealso{\code{\link{binney}}, \code{\link{lambdaR}}}
\examples{
\dontrun{v.sig.eps.plot()}}
\keyword{math}
\keyword{hplot}
