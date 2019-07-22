\name{plaplace}
\alias{plaplace}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Laplace (Double Exponential) Cumulative Distribution Function
}
\description{
Laplace (double exponential) cumulative distribution function with mean equal to \code{mean} and standard deviation equal to \code{sd}.
}
\usage{
plaplace(q, mean = 0, sd = 1, lower.tail = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{q}{Vector of quantiles.}
  \item{mean}{Population mean.}
  \item{sd}{Population standard deviation.}
  \item{lower.tail}{Logical; if \code{TRUE} (default), probabilities are \eqn{P[X \le x]}; otherwise, \eqn{P[X > x]}.}
}
\details{The Laplace distribution has density
\eqn{e^{-|x-\mu| \sqrt{2} / \sigma} / (\sigma \sqrt{2}),}
where \eqn{\mu} is the mean of the distribution and \eqn{\sigma} is the standard deviation.}
\value{\code{plaplace} gives the distribution function.}
\references{Higgins, J. J. (2004) \emph{Introduction to Modern Nonparametric Statistics}.}
\author{Steven T. Garren, James Madison University, Harrisonburg, Virginia, USA}
\note{The formulas computed within \code{plaplace} are based on the textbook by Higgins (2004).}

\seealso{\code{\link{dlaplace}}, \code{\link{qlaplace}}, and \code{\link{rlaplace}}.}
\examples{
plaplace( seq( 20, 80, length.out=11 ), 50, 10 )

plaplace( seq( 20, 80, length.out=11 ), 50, 10, FALSE )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Laplace }
\keyword{ Double exponential }% __ONLY ONE__ keyword per line
