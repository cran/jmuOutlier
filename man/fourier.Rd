\name{fourier}
\alias{fourier}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Determining and Graphing Fourier Approximation}
\description{
The Fourier approximation is determined for any function on domain \eqn{(0, 2\pi)} and then graphed.
}
\usage{
fourier(f, order = 3, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f}{The function to be approximated by Fourier analysis.}
  \item{order}{Integer; the order of the Fourier transformation.}
  \item{\dots}{Optional arguments to be passed to the \code{\link[graphics]{plot}} function
      (see \code{\link[graphics]{par}}). }
}
\details{
The numerical output consists of \eqn{a_0/2, a_1, ..., a_n, b_1, ..., b_2.}
The equation is (constant) \eqn{+ a_1 cos(x) + ... + a_n cos(n x) + b_1 sin(x) + ... + b_n sin(n x).}
}
\value{
\item{constant}{The constant term.}
\item{cosine.coefficients}{The coefficients for the cosine terms.}
\item{sine.coefficients}{The coefficients for the sine terms.}
}
\references{Larson, R. (2013) \emph{Elementary Linear Algebra}, 7th edition.}
\author{Steven T. Garren, James Madison University, Harrisonburg, Virginia, USA}
\note{The formulas computed within \code{fourier} are based on the textbook by Larson (2013).}

%\seealso{ }
\examples{
par( mfrow=c(2,2) )
fourier( function(x){ exp(-x)*(x-pi) }, 4 )
fourier( function(x){ exp(-x) }, 7 )
fourier( function(x){ (x-pi) }, 5 )
fourier( function(x){ (x-pi)^2 } )
par( mfrow=c(1,1) )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Fourier approximation }
