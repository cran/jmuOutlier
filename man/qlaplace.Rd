\name{qlaplace}
\alias{qlaplace}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Laplace (Double Exponential) Quantile Function
}
\description{
Laplace (double exponential) quantile function with mean equal to \code{mean} and standard deviation equal to \code{sd}.
}
\usage{
qlaplace(p, mean = 0, sd = 1, lower.tail = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{Vector of probabilities.}
  \item{mean}{Population mean.}
  \item{sd}{Population standard deviation.}
  \item{lower.tail}{Logical; if \code{TRUE} (default), probabilities are \eqn{P[X \le x]}; otherwise, \eqn{P[X > x]}.}
}
\details{The Laplace distribution has density
\eqn{e^{-|x-\mu| \sqrt{2} / \sigma} / (\sigma \sqrt{2}),}
where \eqn{\mu} is the mean of the distribution and \eqn{\sigma} is the standard deviation.}
\value{\code{qlaplace} gives the quantile function.}
\references{Higgins, J. J. (2004) \emph{Introduction to Modern Nonparametric Statistics}.}
\author{Steven T. Garren, James Madison University, Harrisonburg, Virginia, USA}
\note{The formulas computed within \code{qlaplace} are based on the textbook by Higgins (2004).}

\seealso{\code{\link{dlaplace}}, \code{\link{plaplace}}, and \code{\link{rlaplace}}.}
\examples{
# 5th, 15th, 25th, ..., 95th percentiles from a Laplace( 50, 10 ) distribution.
qlaplace( seq( 0.05, 0.95, length.out=11 ), 50, 10 )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Laplace }
\keyword{ Double exponential }% __ONLY ONE__ keyword per line
