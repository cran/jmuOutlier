\name{perm.f.test}
\alias{perm.f.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Permutation Test on the F-statistic}
\description{
A permutation F-test is performed, and a one-way analysis of variance F-test is performed.
}
\usage{
perm.f.test(response, treatment = NULL, num.sim = 20000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{response}{Numeric vector of responses if treatment is not \code{NULL}.
               If \code{treatment} is \code{NULL}, then \code{response} must be an N by 2 data frame or matrix,
               such that the first column represents response and the second column represents treatment.}
  \item{treatment}{Vector of treatments, which need not be numerical.
               If \code{response} is an N by 2 data frame or matrix, then \code{treatment} should be set to \code{NULL}.}
  \item{num.sim}{The number of simulations performed.
              If \code{num.sim} is smaller than one, then the permutation test is not performed.}
}
\details{The one-way analysis of variance F-test is performed, regardless of the value of \code{num.sim}.
The permutation F-test is performed whenever \code{num.sim} is at least 1.
The p-value of the permutation F-test is estimated by randomly generating the permutations,
and is hence not exact.
The larger the value of \code{num.sim} the more precise the estimate of
the p-value of the permutation F-test, but also the greater the computing time.
Thus, the p-value of the permutation F-test is not based on asymptotic approximation.
}
\value{The output consists of results from calling \code{\link[stats]{aov}} and from the permutation F-test.}

\references{Higgins, J. J. (2004) \emph{Introduction to Modern Nonparametric Statistics}.}
\author{Steven T. Garren, James Madison University, Harrisonburg, Virginia, USA}
\note{The formulas computed within \code{perm.f.test} are based on the textbook by Higgins (2004).}

\seealso{\code{\link[stats]{aov}} and \code{\link{perm.test}}.}
\examples{
perm.f.test( c( 14,6,5,2,54,7,9,15,11,13,12 ), rep( c("I","II","III"), c(4,4,3) ) )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Permutation test }
\keyword{ Analysis of Variance }
