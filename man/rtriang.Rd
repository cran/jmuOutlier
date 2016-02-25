\name{rtriang}
\alias{rtriang}
\title{Triangular Random Generation}
\description{Symmetric triangular random generation with endpoints equal to \code{min} and \code{max}.}
\usage{
rtriang(n, min = 0, max = 1)
}
\arguments{
  \item{n}{Number of observations. If \code{length(n)>1}, the length is taken to be the number required.}
  \item{min}{Left endpoint of the triangular distribution.}
  \item{max}{Right endpoint of the triangular distribution.}
}
\details{The triangular distribution has density
\eqn{4 (x-a) / (b-a)^2} for \eqn{a \le x \le \mu}, and
\eqn{4 (b-x) / (b-a)^2} for \eqn{\mu < x \le b}, where
\eqn{a} and \eqn{b} are the endpoints, and the mean of the distribution is \eqn{\mu = (a+b) / 2}.  }
\value{\code{rtriang} generates random deviates.}
%\references{ }
\author{Steven T. Garren, James Madison University, Harrisonburg, Virginia, USA}
%\note{ }

\seealso{\code{\link{dtriang}}, \code{\link{ptriang}}, and \code{\link{qtriang}}.}
\examples{
# 20 random variates from a Triangular( 100, 200 ) distribution.
rtriang( 20, 100, 200 )  }
\keyword{ Triangular distribution }
