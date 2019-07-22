\name{plotEcdf}
\alias{plotEcdf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plotting Two Empirical Cumulative Distribution Functions}
\description{
Graphs one or two empirical cumulative distribution functions on the same plot.}
\usage{
plotEcdf(x, y = NULL, col = c("black", "red"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Vector of numerical observations whose empirical cdf is to be graphed.}
  \item{y}{Optional vector of observations whose empirical cdf is to be graphed.}
  \item{col}{Scalar or vector of length two, specifying the colors of the two empirical distribution functions.
             The two colors correspond to \code{x} and \code{y}, respectively, and preferably should differ.
             Type \code{colors()} for selections.}
}
%\details{ }
%\value{ }
%\references{ }
\author{Steven T. Garren, James Madison University, Harrisonburg, Virginia, USA}
%\note{ }

\seealso{\code{\link[stats]{plot.ecdf}}}
\examples{
par( mfrow=c(2,2) )
plotEcdf( c(2,4,9,6), c(1,7,11,3,8) )
plotEcdf( c(2,4,9,6), c(1,7,11,3), col=c("navyblue", "orange") ) 
plotEcdf( c(11,5,3), c(3,7,9), col=c("tomato","darkgreen") ) 
plotEcdf( c(15,19,11,4,6), col="purple" ) 
par( mfrow=c(1,1) )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Empirical distribution }
