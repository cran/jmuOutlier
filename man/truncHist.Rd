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
x = rnorm(1000) ;   head(x) ;   tail(x) ;   truncHist(x)
y = rcauchy(1000) ; head(y) ;   tail(y) ;   truncHist(y)
}
\keyword{ Histogram }
