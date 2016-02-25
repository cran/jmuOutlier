\name{score}
\alias{score}
\title{Generating van der Waerden and Exponential Scores}
\description{Generates van der Waerden scores (i.e., normal quantiles) and exponential 
      (similar to Savage) scores, for combined data \code{x} and \code{y}.}
\usage{ score(x, y = NULL, expon = FALSE) }
\arguments{
  \item{x}{A positive integer equal to the number of desired scores when \code{y} is \code{NULL},
       or \code{x} is a vector of observations.}
  \item{y}{An optional vector of observations, typically used with two-sample tests.}
  \item{expon}{Logical; if \code{FALSE} (default), van der Waerden scores are computed, even for ties.
                     If \code{TRUE}, Exponential scores are computed, and interpolation is used for ties.}
}
\details{The scored values for \code{x} are the output, when \code{y} is \code{NULL}.}
\value{
  \item{x}{Scored values for \code{x}, when \code{y} is not \code{NULL}.}
  \item{y}{Scored values for \code{y}, when \code{y} is not \code{NULL}.}
}
\references{Higgins, J. J. (2004) \emph{Introduction to Modern Nonparametric Statistics}.}
\author{Steven T. Garren, James Madison University, Harrisonburg, Virginia, USA}
\note{The formulas computed within \code{score} are based on the textbook by Higgins (2004).}

%\seealso{ }
\examples{
score( 10 )
score( 15, expon=TRUE )
score( c(4,7,6,22,13), c(15,16,7) )  # Two samples, including a tie. 
}

\keyword{ Scoring function }
\keyword{ Score function }
\keyword{ van der Waerden scores }
\keyword{ Savage scores }
