\name{power.binom.test}
\alias{power.binom.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Power Calculations for Exact Binomial Test}
\description{
Compute the power of the binomial test of a simple null hypothesis about a population median.}
\usage{
power.binom.test(n, alpha = 0.05, alternative = c("two.sided", "less", "greater"), 
                 null.median, alt.pdist, ...)
}
\arguments{
  \item{n}{The sample size.}
  \item{alpha}{Probability of Type I error.}
  \item{alternative}{A character string specifying the alternative hypothesis, and
     must be one of \code{"two.sided"} (default), \code{"greater"} or \code{"less"}.  
     Only the initial letter needs to be specified.}
  \item{null.median}{The population median under the null hypothesis.}
  \item{alt.pdist}{Name of the cumulative distribution function under the alternative distribution. 
      Some options include \code{
 \link[stats]{pnorm}, \link[stats]{pexp}, \link[stats]{pcauchy}, \link{plaplace}, 
 \link[stats]{pt}, \link[stats]{pchisq}, \link[stats]{pf}, \link{ptriang}, \link[stats]{punif}, \link[stats]{pbinom}, 
 \link[stats]{pgeom}, \link[stats]{ppois}.
}}
  \item{\dots}{Optional arguments to \code{alt.pdist}, excluding the first argument of \code{alt.pdist}.
See the examples below.}
}
%\details{ } 
\value{Power of the test.} 
\references{Higgins, J. J. (2004) \emph{Introduction to Modern Nonparametric Statistics}.}
\author{Steven T. Garren, James Madison University, Harrisonburg, Virginia, USA}
%\note{The formulas computed within \code{power.binom.test} are based on the textbook by Higgins (2004).}

\seealso{\code{\link[stats]{power.t.test}}}
\examples{
# Alternative distribution is Normal( mean=55.7, sd=2.5 ).
power.binom.test( 30, 0.05, "greater", 55, pnorm, 55.7, 2.5 )

# Alternative distribution is Laplace( mean=55.7, sd=2.5 ).
power.binom.test( 30, 0.05, "greater", 55, plaplace, 55.7, 2.5 )
}

\keyword{ Binomial test }
\keyword{ Power }
