\name{rmd.test}
\alias{rmd.test}
\title{Root Mean Deviance Test}
\description{A permutation test is performed based on the estimated RMD,
the ratio of the absolute value of the mean deviances, for data \code{x} and \code{y}.
}
\usage{
rmd.test(x, y, alternative = c("two.sided", "less", "greater"), all.perms = TRUE, 
         num.sim = 20000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Numeric vector of data values.}
  \item{y}{Numeric vector of data values.}
  \item{alternative}{A character string specifying the alternative hypothesis, and
     must be one of \code{"two.sided"} (default), \code{"greater"} or \code{"less"}.  
     Only the initial letter needs to be specified.}
  \item{all.perms}{Logical.  The exact p-value is attempted when \code{all.perms} (i.e., all permutations) 
     is \code{TRUE} (default), and is simulated when \code{all.perms} is \code{FALSE} or when
     computing an exact p-value requires more than \code{num.sim} calculations.}
  \item{num.sim}{The upper limit on the number of permutations generated.}
}
%\details{ }
\value{
\item{alternative}{Same as the input.}
\item{rmd.hat}{The value of the RMD test statistic.}
\item{p.value}{The p-value of the test.}
}
\references{Higgins, J. J. (2004) \emph{Introduction to Modern Nonparametric Statistics}.}
\author{Steven T. Garren, James Madison University, Harrisonburg, Virginia, USA}
\note{The formulas computed within \code{rmd.test} are based on the textbook by Higgins (2004).}

\seealso{\code{\link[stats]{ansari.test}}, \code{\link{siegel.test}}, and \code{\link{perm.test}}}
\examples{
rmd.test( c(13, 34, 2, 19, 49, 63), c(17, 29, 22) )
rmd.test( c(13, 34, 2, 19, 49, 63), c(17, 29, 22), "greater" )
}

\keyword{ Root Mean Deviance test }
\keyword{ Siegel-Tukey test }
\keyword{ Ansari-Bradley test }
\keyword{ permutation test }
