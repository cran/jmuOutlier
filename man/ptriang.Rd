\name{ptriang}
\alias{ptriang}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Triangular Cumulative Distribution Function}
\description{
Triangular cumulative distribution function with endpoints equal to \code{min} and \code{max}.
}
\usage{
ptriang(q, min = 0, max = 1, lower.tail = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{q}{Vector of quantiles.}
  \item{min}{Left endpoint of the triangular distribution.}
  \item{max}{Right endpoint of the triangular distribution.}
  \item{lower.tail}{Logical; if \code{TRUE} (default), probabilities are \eqn{P[X \le x]}; otherwise, \eqn{P[X > x]}.}
}
\details{The triangular distribution has density
\eqn{4 (x-a) / (b-a)^2} for \eqn{a \le x \le \mu}, and
\eqn{4 (b-x) / (b-a)^2} for \eqn{\mu < x \le b}, where
\eqn{a} and \eqn{b} are the endpoints, and the mean of the distribution is \eqn{\mu = (a+b) / 2}.  }
\value{\code{ptriang} gives the distribution function.}
%\references{ }
\author{Steven T. Garren, James Madison University, Harrisonburg, Virginia, USA}
%\note{ }

\seealso{\code{\link{dtriang}}, \code{\link{qtriang}}, and \code{\link{rtriang}}.}
\examples{
ptriang( seq( 100, 200, length.out=11 ), 100, 200 )

ptriang( seq( 100, 200, length.out=11 ), 100, 200, FALSE ) }
\keyword{ Triangular distribution }
