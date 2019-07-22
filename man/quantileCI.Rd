\name{quantileCI}
\alias{quantileCI}
\title{Confidence Intervals on Quantiles}
\description{Produces exact confidence intervals on quantiles corresponding to the stated probabilities,
based on the binomial test.  }
\usage{quantileCI(x, probs = 0.5, conf.level = 0.95)}
\arguments{
  \item{x}{Numeric vector of observations.}
  \item{probs}{Numeric vector of cumulative probabilities between 0 and 1.}
  \item{conf.level}{Confidence level of the interval.}
}
\details{If \code{probs=0.5} (default), then a confidence interval on the population median is produced.}
\value{Confidence interval for each quantile based on \code{probs}.}
\references{Higgins, J. J. (2004) \emph{Introduction to Modern Nonparametric Statistics}.}
\author{Steven T. Garren, James Madison University, Harrisonburg, Virginia, USA}
%\note{The formulas computed within \code{quantileCI} are based on the textbook by Higgins (2004).}

%\seealso{ }
\examples{
# Sample 20 observations from an Exponential distribution with mean=10.

print( sort( x <- rexp( 20, 0.1 ) ) ) 

# Construct 90% confidence intervals on the 25th, 50th, and 75th percentiles.  

quantileCI( x, c( 0.25, 0.5, 0.75 ), 0.9 ) }
\keyword{ Confidence interval }
\keyword{ Quantile }
\keyword{ Population median }
