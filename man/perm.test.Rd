\name{perm.test}
\alias{perm.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Permutation Test}
\description{Performs one-sample and two-sample permutation tests on vectors of data.}
\usage{
perm.test(x, y = NULL, alternative = c("two.sided", "less", "greater"), mu = 0, 
      paired = FALSE, all.perms = TRUE, num.sim = 20000, plot = FALSE, stat = mean, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A (non-empty) numeric vector of data values.}
  \item{y}{An optional numeric vector data values.}
  \item{alternative}{A character string specifying the alternative hypothesis, and
     must be one of \code{"two.sided"} (default), \code{"greater"} or \code{"less"}.  
     Only the initial letter needs to be specified.}
  \item{mu}{A number indicating the null value of the location parameter 
     (or the difference in location parameters if performing a two-sample test).}
  \item{paired}{Logical, indicating whether or not a two-sample test should be paired,
     and is ignored for a one-sample test.}
  \item{all.perms}{Logical.  The exact p-value is attempted when \code{all.perms} (i.e., all permutations) 
     is \code{TRUE} (default), and is simulated when \code{all.perms} is \code{FALSE} or when
     computing an exact p-value requires more than \code{num.sim} calculations.}
  \item{num.sim}{The upper limit on the number of permutations generated.}
  \item{plot}{Logical. If \code{TRUE}, then plot the histogram of the permutation distribution;
     otherwise, list the p-value.}
  \item{stat}{Function, naming the test statistic, such as \code{mean} and \code{median}.}
  \item{\dots}{Optional arguments to \code{stat}; 
       and is the second argument to \code{stat} when unspecified.
       For example, if \code{stat} equals \code{mean}, then the second argument
      \code{trim} denotes the fraction (0 to 0.5) of observations to be trimmed 
      from each end of \code{x} and \code{y} before the mean is computed.}
}
\details{
A paired test using data \code{x} and nonNULL \code{y} is 
equivalent to a one-sample test using data \code{x-y}.
The output states more details about the permutation test, such as one-sample or two-sample,
and whether or not the \code{p.value} calculated was based on all permutations.
}
\value{
\item{alternative}{Same as the input.}
\item{mu}{Same as the input.}
\item{p.value}{The p-value of the permutation test.}
}

\references{Higgins, J. J. (2004) \emph{Introduction to Modern Nonparametric Statistics}.}
\author{Steven T. Garren, James Madison University, Harrisonburg, Virginia, USA}
\note{The formulas computed within \code{perm.test} are based on the textbook by Higgins (2004).}

%\seealso{ }
\examples{
# One-sample test

print( x <- rnorm(10,0.5) ) 

perm.test( x, stat=median )

# Two-sample unpaired test

print( y <- rnorm(13,1) )

perm.test( x, y )
}

\keyword{ Permutation test }
