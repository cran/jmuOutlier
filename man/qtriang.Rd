\name{qtriang}
\alias{qtriang}
\title{Triangular Quantile Function}
\description{Symmetric triangular density with endpoints equal to \code{min} and \code{max}.}
\usage{ qtriang(p, min = 0, max = 1) }
\arguments{
  \item{p}{Vector of probabilities.}
  \item{min}{Left endpoint of the triangular distribution.}
  \item{max}{Right endpoint of the triangular distribution.}
}
\details{The triangular distribution has density
\eqn{4 (x-a) / (b-a)^2} for \eqn{a \le x \le \mu}, and
\eqn{4 (b-x) / (b-a)^2} for \eqn{\mu < x \le b}, where
\eqn{a} and \eqn{b} are the endpoints, and the mean of the distribution is \eqn{\mu = (a+b) / 2}.  }
\value{\code{qtriang} gives the quantile function.}
%\references{ }
\author{Steven T. Garren, James Madison University, Harrisonburg, Virginia, USA}
%\note{ }

\seealso{
\code{\link{dtriang}}, \code{\link{ptriang}}, and \code{\link{rtriang}}.
}
\examples{
# 5th, 15th, 25th, ..., 95th percentiles from a Triangular( 100, 200 ) distribution.
qtriang( seq( 0.05, 0.95, length.out=11 ), 100, 200 )
}
\keyword{ Triangular distribution }
