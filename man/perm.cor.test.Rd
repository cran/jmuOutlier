\name{perm.cor.test}
\alias{perm.cor.test}
\title{Permutation Test on Correlation}
\description{A permutation test is performed based on Pearson and Spearman correlations.}
\usage{
perm.cor.test(x, y = NULL, alternative = c("two.sided", "less", "greater"), 
              method = c("pearson", "spearman"), num.sim = 20000)
}
\arguments{
  \item{x}{Numeric vector of design variable.}
  \item{y}{Numeric vector of response variable.}
  \item{alternative}{A character string specifying the alternative hypothesis, and
     must be one of \code{"two.sided"} (default), \code{"greater"} or \code{"less"}.  
     Only the initial letter needs to be specified.}
  \item{method}{A character string specifying the type of correlation, and
     must be one of \code{"pearson"} (default) or \code{"spearman"}.  
     Only the initial letter needs to be specified.}
  \item{num.sim}{The number of simulations generated.}
}
\details{The p-value is estimated by randomly generating the permutations,
and is hence not exact.  
The larger the value of \code{num.sim} the more precise the estimate of
the p-value, but also the greater the computing time.
Thus, the p-value is not based on asymptotic approximation. 
The output states more details about the permutation test, such as the values of \code{method} and \code{num.sim}.}
\value{
\item{alternative}{Same as the input.}
\item{p.value}{The p-value of the permutation test.}
}

\references{Higgins, J. J. (2004) \emph{Introduction to Modern Nonparametric Statistics}.}
\author{Steven T. Garren, James Madison University, Harrisonburg, Virginia, USA}
\note{The formulas computed within \code{perm.cor.test} are based on the textbook by Higgins (2004).}

\seealso{\code{\link[stats]{cor}}, \code{\link[stats]{cor.test}}, and \code{\link{perm.test}}.}
\examples{
x = c( 4, 6, 8, 11 ) ;   y = c( 19, 44, 15, 13 )
perm.cor.test( x, y, "less", "pearson" ) 
perm.cor.test( x, y, "less", "spearman" )
}
\keyword{ Correlation }
\keyword{ Permutation test }
