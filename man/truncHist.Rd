\name{truncHist}
\alias{truncHist}
\title{Truncated Histograms}
\description{Produces a truncated histogram.}
\usage{
truncHist(x, xmin = NULL, xmax = NULL, trim = 0.025, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Vector of numerical observations.}
  \item{xmin}{Minimum numerical value to be shown in graph.}
  \item{xmax}{Maximum numerical value to be shown in graph.}
  \item{trim}{The fraction (0 to 0.5) of observations to be trimmed from
              each end of \code{x} before the histogram is constructed.}
  \item{\dots}{Optional arguments to \code{\link[graphics]{hist}}.}
}
\details{\code{truncHist} may be useful if data contain some extreme outliers.}
%\value{ }
%\references{}
\author{Steven T. Garren, James Madison University, Harrisonburg, Virginia, USA}
%\note{ }

\seealso{\code{\link[graphics]{hist}}}
\examples{
mfrow.old = par()$mfrow ;   par( mfrow = c( 2, 2 ) )

x1 = rnorm(1000) ;   c( head(x1), tail(x1)) ;   truncHist(x1, main="Normal data; first simulation")
x2 = rnorm(1000) ;   c( head(x2), tail(x2)) ;   truncHist(x2, main="Normal data; second simulation")
y1 = rcauchy(1000) ; c( head(y1), tail(y1)) ;   truncHist(y1, main="Cauchy data; first simulation")
y2 = rcauchy(1000) ; c( head(y2), tail(y2)) ;   truncHist(y2, main="Cauchy data; second simulation")

par( mfrow = mfrow.old )
}
\keyword{ Histogram }
