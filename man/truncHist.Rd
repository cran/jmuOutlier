\name{truncHist}
\alias{truncHist}
\title{Truncated Histograms}
\description{Produces a truncated histogram.}
\usage{
truncHist(x, xmin = NULL, xmax = NULL, trim = 0.025, main = NULL, xlab = "x", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Vector of numerical observations.}
  \item{xmin}{Minimum numerical value to be shown in graph.}
  \item{xmax}{Maximum numerical value to be shown in graph.}
  \item{trim}{The fraction (0 to 0.5) of observations to be trimmed from
              each end of \code{x} before the histogram is constructed.}
  \item{main}{An overall title for the histogram.}
  \item{xlab}{A title for the x-axis.}
  \item{\dots}{Optional arguments to \code{\link[graphics]{hist}}.}
}
\details{\code{truncHist} may be useful if data contain some extreme outliers.}
%\value{ }
%\references{}
\author{Steven T. Garren, James Madison University, Harrisonburg, Virginia, USA}
%\note{ }

\seealso{\code{\link[graphics]{hist}}}
\examples{
x1 = sort(rnorm(1000)) ;   c( head(x1), tail(x1)) 

x2 = sort(rnorm(1000)) ;   c( head(x2), tail(x2)) 

y1 = sort(rcauchy(1000)) ; c( head(y1), tail(y1)) 

y2 = sort(rcauchy(1000)) ; c( head(y2), tail(y2)) 

par( mfrow=c(2,2) )
truncHist(x1, main="Normal data; first simulation",  xlab="x1")
truncHist(x2, main="Normal data; second simulation", xlab="x2")
truncHist(y1, main="Cauchy data; first simulation",  xlab="y1")
truncHist(y2, main="Cauchy data; second simulation", xlab="y2")
par( mfrow=c(1,1) )
}
\keyword{ Histogram }
