\name{jmuOutlier-package}
\alias{jmuOutlier-package}
\alias{jmuOutlier}
\docType{package}
\title{Permutation Tests for Nonparametric Statistics
%\packageTitle{jmuOutlier}
}
\description{
Performs a permutation test on the difference between two location parameters,
a permutation correlation test, a permutation F-test, the Siegel-Tukey test, 
a ratio mean deviance test.
Also performs some graphing techniques, such as for confidence intervals,
vector addition, and Fourier analysis; and includes functions related to the
Laplace (double exponential) and triangular distributions.
Performs power calculations for the binomial test.}
\details{

\strong{(I)} Permutation tests
\itemize{
\item{\code{\link{perm.cor.test}} performs a permutation test based on Pearson and Spearman correlations.}
\item{\code{\link{perm.f.test}} performs a permutation F-test and a one-way analysis of variance F-test.}
\item{\code{\link{perm.test}} performs one-sample and two-sample permutation tests on vectors of data.}
\item{\code{\link{rmd.test}} performs a permutation test based on the estimated RMD,
the ratio of the mean of the absolute value of the deviances, using two datasets.}
\item{\code{\link{siegel.test}} performs the Siegel-Tukey test using two datasets.}
}
\strong{(II)} Confidence intervals
\itemize{
\item{\code{\link{CI.t.test}} produces two-sided confidence intervals on population mean, 
         allowing for a finite population correction.}
\item{\code{\link{quantileCI}} produces exact confidence intervals on quantiles corresponding 
         to the stated probabilities, based on the binomial test.}
}
\strong{(III)} Graphs
\itemize{
\item{\code{\link{coin.toss}} illustrates the Law of Large Numbers for proportions.}
\item{\code{\link{fourier}} determines the Fourier approximation for any function on domain \eqn{(0, 2\pi)} 
and then graphs both the function and the approximation.}
\item{\code{\link{lineGraph}} constructs a line graph on a vector of numerical observations.}
\item{\code{\link{plotCI}} plots multiple confidence intervals on the same graph,
and determines the proportion of confidence intervals containing
the true population mean.}
\item{\code{\link{plotEcdf}} graphs one or two empirical cumulative distribution functions on the same plot.}
\item{\code{\link{plotVector}} plots one or two 2-dimensional vectors along with their vector sum.}
\item{\code{\link{truncHist}} produces a truncated histogram, which may be useful if data contain
some extreme outliers.}
}
\strong{(IV)} Laplace (double exponential) and symmetric triangular distributions
\itemize{
\item{\code{\link{dlaplace}}, \code{\link{plaplace}}, \code{\link{qlaplace}}, and \code{\link{rlaplace}}
          give the density, the distribution function, the quantile function, and random deviates, respectively,
          of the Laplace distribution.}
\item{\code{\link{dtriang}}, \code{\link{ptriang}}, \code{\link{qtriang}}, and \code{\link{rtriang}}
          give the density, the distribution function, the quantile function, and random deviates, respectively,
          of the triangular distribution.}
}
\strong{(V)} Additional functions
\itemize{
\item{\code{\link{abbreviation}} determines if one character variable is an abbreviation among a 
         selection of other character variables.}
\item{\code{\link{latin}} generates a Latin square.}
\item{\code{\link{power.binom.test}} computes the power of the binomial test of a simple null hypothesis 
         about a population median.}
\item{\code{\link{score}} generates van der Waerden scores (i.e., normal quantiles) and exponential 
      (similar to Savage) scores.}
}
}
\author{
\packageAuthor{jmuOutlier}

Maintainer: \packageMaintainer{jmuOutlier}
}
\references{
Higgins, J. J. (2004) \emph{Introduction to Modern Nonparametric Statistics}.

Larson, R. (2013) \emph{Elementary Linear Algebra}, 7th edition.
}
\keyword{ Nonparametric statistics }
\keyword{ Fourier analysis }
\keyword{ Power }
%\seealso{ }
\examples{
fourier (function(x){ (x-pi)^3 }, 4 )
x = rtriang(20,50) ;  list(x) ;  perm.test( x, mu=25, stat=median )
quantileCI( x, c(0.25, 0.5, 0.75)  )
power.binom.test( 20, 0.05, "less", 47, plaplace, 45.2, 3.7 )
}
