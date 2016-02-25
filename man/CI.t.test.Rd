\name{CI.t.test}
\alias{CI.t.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Student's t-Confidence Interval with Finite Population Correction}
\description{
Performs two-sided confidence interval on population mean, allowing for a finite population correction.}
\usage{
CI.t.test(x, conf.level = 0.95, fpc = 1)
}
\arguments{
  \item{x}{A nonempty numeric vector of data values.}
  \item{conf.level}{Confidence level of the interval, and should be between 0 and 1.}
  \item{fpc}{The finite population correction, and should be between 0 and 1.}
}
\details{The \code{fpc} is typically defined as \eqn{1-n/N}, where \code{n} is the sample size,
and \code{N} is the population size, for simple random sampling without replacement.
When sampling with replacement, set \code{fpc=1} (default).}
\value{A confidence interval for the population mean.}
\references{
Scheaffer, R. L., Mendenhall, W., Ott, R. L., Gerow, K. G. (2012) \emph{Elementary Survey Sampling,} 7th edition.
}
\author{Steven T. Garren, James Madison University, Harrisonburg, Virginia, USA}
\note{The definition of \code{fpc} is based on the textbook by 
Scheaffer, Mendenhall, Ott, Gerow (2012), chapter 4.}

\seealso{\code{\link[stats]{t.test}} and \code{\link{plotCI}}. }
\examples{
# Sample 43 observations from a list of 200 numbers, and compute the 95\% confidence interval.
pop = sqrt(1:200) ; x1 = sample( pop, 43 ) ; list(sort(x1))
fpc = 1 - length(x1) / length(pop) ; CI.t.test( x1, fpc=fpc )

# Sample 14 observations from a Normal(mean=50, sd=5) distribution,
#    and compute the 90% confidence interval.
x2 = sample( 14, 50, 5 ) ; list(sort(x2)) ; CI.t.test( x2, 0.9 )
}

\keyword{ Student t }
\keyword{ Confidence interval }
\keyword{ Finite population correction }
