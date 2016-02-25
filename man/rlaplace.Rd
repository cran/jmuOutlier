\name{rlaplace}
\alias{rlaplace}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Laplace (Double Exponential) Random Generation}
\description{
Laplace (double exponential) random generation with mean equal to \code{mean} and standard deviation equal to \code{sd}.
}
\usage{
rlaplace(n, mean = 0, sd = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{Number of observations. If \code{length(n)>1}, the length is taken to be the number required.}
  \item{mean}{Population mean.}
  \item{sd}{Population standard deviation.}
}
\details{The Laplace distribution has density
\eqn{e^{-|x-\mu| \sqrt{2} / \sigma} / (\sigma \sqrt{2}),}
where \eqn{\mu} is the mean of the distribution and \eqn{\sigma} is the standard deviation.}
\value{
\code{rlaplace} generates random deviates.
}
\references{Higgins, J. J. (2004) \emph{Introduction to Modern Nonparametric Statistics}.}
\author{Steven T. Garren, James Madison University, Harrisonburg, Virginia, USA}
\note{The formulas computed within \code{rlaplace} are based on the textbook by Higgins (2004).}

\seealso{\code{\link{dlaplace}}, \code{\link{plaplace}}, and \code{\link{qlaplace}}.}
\examples{
# 20 random variates from a Laplace( 50, 10 ) distribution.
rlaplace( 20, 50, 10 )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Laplace }
\keyword{ Double Exponential }% __ONLY ONE__ keyword per line
