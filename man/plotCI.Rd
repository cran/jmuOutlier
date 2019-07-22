\name{plotCI}
\alias{plotCI}
\title{Confidence Interval Plot}
\description{Plots multiple confidence intervals on the same graph,
and determines the proportion of confidence intervals containing
the true population mean.}
\usage{
plotCI(CI, mu = NULL, plot.midpoints = TRUE, 
       col = c("black", "red", "darkgreen", "purple"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{CI}{N by 2 matrix or 2 by N matrix consisting of N two-sided confidence intervals.}
  \item{mu}{Numeric; the population mean, and is \code{NULL} if unknown.}
  \item{plot.midpoints}{Logical; plots the midpoints of the confidence intervals if \code{TRUE} (default);
   otherwise, does not plot the midpoints.}
  \item{col}{A vector of size four, specifying the colors of the line representing population mean, 
confidence intervals not containing the population mean, confidence intervals containing the population mean,
and the sample means, respectively.}
}
\details{The title of the graph states the proportion of the confidence intervals
containing the true population mean, when the population mean is not \code{NULL}. }
%\value{ }
%\references{ }
\author{Steven T. Garren, James Madison University, Harrisonburg, Virginia, USA}
%\note{}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{CI.t.test}} }
\examples{
# Plot fifty 90% confidence intervals, each based on 13 observations from a 
#    Normal( mean=70, sd=10 ) distribution.

plotCI( replicate( 50, t.test( rnorm( 13, 70, 10 ), conf.level=0.9 )$conf.int ), 70 )
}

\keyword{ Confidence interval }
