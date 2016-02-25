\name{dtriang}
\alias{dtriang}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Triangular Density Function}
\description{
Symmetric triangular density with endpoints equal to \code{min} and \code{max}.
}
\usage{
dtriang(x, min = 0, max = 1)
}
\arguments{
  \item{x}{Vector of quantiles.}
  \item{min}{Left endpoint of the triangular distribution.}
  \item{max}{Right endpoint of the triangular distribution.}
}
\details{The triangular distribution has density
\eqn{4 (x-a) / (b-a)^2} for \eqn{a \le x \le \mu}, and
\eqn{4 (b-x) / (b-a)^2} for \eqn{\mu < x \le b}, where
\eqn{a} and \eqn{b} are the endpoints, and the mean of the distribution is \eqn{\mu = (a+b) / 2}.  }
\value{\code{dtriang} gives the density.}
%\references{ }
\author{Steven T. Garren, James Madison University, Harrisonburg, Virginia, USA}
%\note{ }

\seealso{\code{\link{ptriang}}, \code{\link{qtriang}}, and \code{\link{rtriang}}.}
\examples{dtriang( seq( 100, 200, length.out=11 ), 100, 200 )}
\keyword{ Triangular distribution }
