\name{siegel.test}
\alias{siegel.test}
\title{Siegel-Tukey Test}
\description{Performs the Siegel-Tukey test on data \code{x} and \code{y}, where ties are handled by averaging ranks,
  not by asymptotic approximations.  }
\usage{
siegel.test(x, y, alternative = c("two.sided", "less", "greater"), reverse = FALSE, 
            all.perms = TRUE, num.sim = 20000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Numeric vector of data values.}
  \item{y}{Numeric vector of data values.}
  \item{alternative}{A character string specifying the alternative hypothesis, and
     must be one of \code{"two.sided"} (default), \code{"greater"} or \code{"less"}.  
     Only the initial letter needs to be specified.}
  \item{reverse}{Logical; If \code{FALSE} (default), then assign rank 1 to the smallest observation.
                 If \code{TRUE}, then assign rank 1 to the largest observation.}
  \item{all.perms}{Logical.  The exact p-value is attempted when \code{all.perms} (i.e., all permutations) 
     is \code{TRUE} (default), and is simulated when \code{all.perms} is \code{FALSE} or when
     computing an exact p-value requires more than \code{num.sim} calculations.}
  \item{num.sim}{The upper limit on the number of permutations generated.}
}
\details{Since the logical value of \code{reverse} may affect the p-value,
yet neither logical value of \code{reverse} is preferred over the other, one should 
consider using \code{\link[stats]{ansari.test}} instead.}
\value{
\item{alternative}{Same as the input.}
\item{rank.x}{The Siegel-Tukey ranks of the data \code{x}.}
\item{rank.y}{The Siegel-Tukey ranks of the data \code{y}.}
\item{p.value}{The p-value of the test.}
}

\references{Higgins, J. J. (2004) \emph{Introduction to Modern Nonparametric Statistics}.}
\author{Steven T. Garren, James Madison University, Harrisonburg, Virginia, USA}
\note{The formulas computed within \code{siegel.test} are based on the textbook by Higgins (2004).}

\seealso{\code{\link[stats]{ansari.test}}, \code{\link{rmd.test}}, and \code{\link{perm.test}}}
\examples{
# The same data are used in the following two commands.

siegel.test( c(13, 34, 2, 19, 49, 63), c(17, 29, 22) )
siegel.test( c(13, 34, 2, 19, 49, 63), c(17, 29, 22), reverse=TRUE )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Siegel-Tukey test }
\keyword{ Ansari-Bradley test }
\keyword{ permutation test }% __ONLY ONE__ keyword per line
